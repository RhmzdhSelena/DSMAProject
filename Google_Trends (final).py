import os
import time
import random
import pandas as pd
from pytrends.request import TrendReq

# ------------------------------------------------------------
# 1) Mac-Compatible Setup & Pathing
# ------------------------------------------------------------
BASE_PATH = os.path.expanduser("~/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
RESTAURANTS_FILE = "restaurants_tucson_clean.csv"
OUT_FILE = "google_trends_restaurants_final.csv"

if not os.path.exists(BASE_PATH):
    raise FileNotFoundError(
        f"Error: Path {BASE_PATH} not found. Please check your Mac folder path."
    )
os.chdir(BASE_PATH)

# ------------------------------------------------------------
# 2) Methodology Parameters
# ------------------------------------------------------------
# Reduced timeframe (2018–2022) + Arizona-level geo for higher data availability
TIMEFRAME = "2018-01-01 2022-12-31"
GEO = "US-AZ"

# ------------------------------------------------------------
# 3) Stealth Initialization (browser-like headers)
# ------------------------------------------------------------
headers = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/115.0.0.0 Safari/537.36"
    )
}

pytrends = TrendReq(
    hl="en-US",
    tz=0,
    retries=5,
    backoff_factor=1,
    requests_args={"headers": headers},
)

# ------------------------------------------------------------
# 4) Single-Term Fetch with Basic Backoff Handling
# ------------------------------------------------------------
def fetch_trend_safe(name: str):
    # Since we restrict via GEO, do NOT append "tucson"
    term = str(name).strip()
    if not term:
        return None

    try:
        pytrends.build_payload([term], geo=GEO, timeframe=TIMEFRAME)
        df_trend = pytrends.interest_over_time()

        if df_trend is None or df_trend.empty:
            return None

        df_trend = df_trend.reset_index()

        # Keep only date + interest column; drop isPartial if present
        keep_cols = ["date", term]
        df_trend = df_trend[keep_cols].rename(columns={term: "search_interest"})
        df_trend["search_term"] = term

        return df_trend

    except Exception as e:
        # pytrends often wraps rate limiting in generic exceptions
        if "429" in str(e):
            print("! Rate limited (429). Cooling down for 5 minutes...")
            time.sleep(300)
        else:
            print(f"! Error for '{name}': {e}")
        return None

# ------------------------------------------------------------
# 5) Load Restaurants
# ------------------------------------------------------------
df_rest = pd.read_csv(RESTAURANTS_FILE)

# Expecting columns: business_id, name
restaurants = df_rest[["business_id", "name"]].drop_duplicates()

records = []
total = len(restaurants)
print(f"Starting retrieval for {total} restaurants...")

# ------------------------------------------------------------
# 6) Loop + Checkpoint Saves
# ------------------------------------------------------------
for idx, row in restaurants.iterrows():
    rid = row["business_id"]
    rname = str(row["name"])

    trend_df = fetch_trend_safe(rname)

    if trend_df is not None:
        trend_df["business_id"] = rid
        trend_df["restaurant_name"] = rname
        records.append(trend_df)
        print(f"✓ ({idx+1}/{total}) {rname}")
    else:
        print(f"✗ ({idx+1}/{total}) No data: {rname}")

    # Random sleep to mimic human browsing behavior
    time.sleep(random.uniform(4, 6))

    # Save every 25 processed rows to prevent data loss
    if (idx + 1) % 25 == 0 and records:
        pd.concat(records, ignore_index=True).to_csv(OUT_FILE, index=False)
        print("--- Checkpoint saved ---")

# ------------------------------------------------------------
# 7) Final Save
# ------------------------------------------------------------
if records:
    final_df = pd.concat(records, ignore_index=True)
    final_df.to_csv(OUT_FILE, index=False)
    print(f"Finished! File saved to: {os.path.join(os.getcwd(), OUT_FILE)}")
else:
    print("Finished, but no trend data was retrieved. Try broader GEO or shorter timeframe.")
    
    
