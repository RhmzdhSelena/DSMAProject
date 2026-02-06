# ------------------------------------------------------------
# Script: 04.02_google_places_full.py
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Enrich the Tucson restaurant dataset with selected
#   Google Places metadata using the legacy Places API
#   (Text Search + Details). This script extracts ratings,
#   review counts, price levels, types, hours per week,
#   and photo-based engagement metrics.
#
# Notes:
#   - Uses only the legacy Places endpoints
#   - Service-level fields (delivery, dine-in, etc.) are not
#     available in the legacy API.
#   - The script includes retry handling and progress saving
#     to avoid losing work.
# Output:
#   google_places_full.csv
# ------------------------------------------------------------
import os
os.chdir(r"C:\Users\selii\OneDrive\Goethe\1. Semester\DSMA\Data\Data")

import requests
import pandas as pd
import time
import os

API_KEY = "AIzaSyBnk4bSh-QRiLOCJ-lhsfbhq6vbz9Bw5Cg"

INPUT_FILE = "restaurants_tucson_clean.csv"
OUTPUT_FILE = "google_places_full.csv"

DELAY = 0.12            # safe delay for Google API
RETRY_DELAY = 2         # delay after an API failure
MAX_RETRIES = 3         # retry attempts


# ------------------------------------------------------------
# Compute hours per week from Google opening_hours.periods
# ------------------------------------------------------------
def compute_hours_per_week(opening_hours):
    if not opening_hours or "periods" not in opening_hours:
        return None

    total_minutes = 0

    for p in opening_hours["periods"]:
        if "open" in p and "close" in p:
            try:
                o_h = int(p["open"]["time"][:2])
                o_m = int(p["open"]["time"][2:])
                c_h = int(p["close"]["time"][:2])
                c_m = int(p["close"]["time"][2:])
            except:
                continue

            open_total = o_h * 60 + o_m
            close_total = c_h * 60 + c_m

            # Handle past-midnight closing
            if close_total < open_total:
                close_total += 24 * 60

            total_minutes += (close_total - open_total)

    return round(total_minutes / 60, 2)


# ------------------------------------------------------------
# Safe API request with retry
# ------------------------------------------------------------
def safe_request(url, params):
    for attempt in range(MAX_RETRIES):
        try:
            r = requests.get(url, params=params, timeout=10)
            return r.json()
        except Exception:
            time.sleep(RETRY_DELAY)
    return None


# ------------------------------------------------------------
# Text Search (legacy)
# ------------------------------------------------------------
def google_text_search(name):
    url = "https://maps.googleapis.com/maps/api/place/textsearch/json"
    params = {
        "query": name + " Tucson",
        "key": API_KEY
    }
    data = safe_request(url, params)
    if not data or data.get("status") != "OK":
        return None
    results = data.get("results", [])
    return results[0] if results else None


# ------------------------------------------------------------
# Place Details (legacy)
# ------------------------------------------------------------
def google_place_details(place_id):
    url = "https://maps.googleapis.com/maps/api/place/details/json"
    params = {
        "place_id": place_id,
        "key": API_KEY,
        "fields": (
            "place_id,name,rating,user_ratings_total,price_level,"
            "business_status,opening_hours,photos,types"
        )
    }
    data = safe_request(url, params)
    if not data or "result" not in data:
        return {}
    return data["result"]


# ------------------------------------------------------------
# Load restaurant dataset
# ------------------------------------------------------------
df = pd.read_csv(INPUT_FILE)
print(f"Loaded {len(df)} restaurants.\n")

records = []

# Resume if output already exists
if os.path.exists(OUTPUT_FILE):
    existing = pd.read_csv(OUTPUT_FILE)
    processed = set(existing["business_id"])
    records.extend(existing.to_dict("records"))
    print(f"Resuming: {len(processed)} restaurants already processed.\n")
else:
    processed = set()


# ------------------------------------------------------------
# MAIN SCRAPING LOOP
# ------------------------------------------------------------
for idx, row in df.iterrows():
    biz_id = row["business_id"]

    # Skip if already processed
    if biz_id in processed:
        continue

    yelp_name = row["name"]
    print(f"[{idx}] Searching: {yelp_name}")

    # STEP 1 — Text search
    base = google_text_search(yelp_name)

    if base is None:
        print(" → No match.")
        place_id = None
        details = {}
    else:
        place_id = base.get("place_id")
        print(f" → place_id: {place_id}")

        # STEP 2 — Details lookup
        details = google_place_details(place_id)

    # Photo contributor extraction
    photos = details.get("photos", [])
    contributors = []
    for ph in photos:
        for a in ph.get("html_attributions", []):
            contributors.append(a)

    # Final record
    rec = {
        "business_id": biz_id,
        "name": row["name"],
        "latitude": row["latitude"],
        "longitude": row["longitude"],

        # Google core fields
        "google_place_id": place_id,
        "google_name": details.get("name"),
        "google_rating": details.get("rating"),
        "google_user_ratings_total": details.get("user_ratings_total"),
        "google_price_level": details.get("price_level"),
        "google_business_status": details.get("business_status"),
        "google_types": ", ".join(details.get("types", [])),

        # Derived fields
        "google_hours_per_week": compute_hours_per_week(details.get("opening_hours")),
        "google_photo_count": len(photos),
        "google_unique_photo_contributors": len(set(contributors)),
        "google_has_photos": len(photos) > 0
    }

    records.append(rec)
    processed.add(biz_id)

    # Save progress every 20 restaurants
    if len(records) % 20 == 0:
        pd.DataFrame(records).to_csv(OUTPUT_FILE, index=False)
        print(f" → Saved {len(records)} rows so far.")

    time.sleep(DELAY)


# ------------------------------------------------------------
# FINAL SAVE
# ------------------------------------------------------------
pd.DataFrame(records).to_csv(OUTPUT_FILE, index=False)
print("\n✔ COMPLETED FULL SCRAPE")
print(f"Saved {len(records)} rows to {OUTPUT_FILE}")
