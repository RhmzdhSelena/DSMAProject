# DSMA Project – Predicting Restaurant Success

This repository contains the full codebase for the **DSMA (Data Science & Machine Learning Applications)** course project:

**“Data-Driven Insights for Restaurant Success: What Yelp Reveals About Customer Satisfaction”**  
Goethe University Frankfurt  
Author: *Selena-Leila Rahimzadeh*

The project investigates short-term, within-restaurant deviations in customer satisfaction using Yelp data and supervised machine-learning models.

---

## Repository Contents

The code is provided in **two equivalent formats** for convenience:

- **Individual R scripts**, clearly numbered and documented
- **A ZIP archive** containing the same scripts bundled together

Both versions contain identical code and logic.

---

## Data Requirements (IMPORTANT)

### 1. Yelp Data (Primary Source)

The analysis is based on the **Yelp Open Dataset**, from which all initial raw tables are derived, including:
- Business information
- Reviews
- Check-ins

All feature engineering, modeling, and evaluation steps rely on these Yelp tables.

> The original Yelp raw data is **not redistributed** in this repository due to licensing restrictions.

---

### 2. External Data (Must Be Downloaded Separately)

Two external datasets are **essential** and are **not part of the Yelp dataset**:

- **Google Trends – restaurant-related search interest**
- **Google Places – cross-platform restaurant information**

These datasets are required for:
- Descriptive analysis of external digital attention
- Cross-platform validation
- Robustness checks

⚠️ **It is crucial to download or recreate these two tables separately** in order to fully reproduce the analysis.

---

### 3. Fully Prepared Tables (For Reproducibility)

To ensure that the code can be executed **without rebuilding the entire pipeline**, the repository includes:

- **`tables (FULL)/`**

This folder contains **all intermediate and final tables** required for the scripts to run end-to-end, including:
- Feature-engineered datasets
- Modeling inputs
- Final analytical tables

Using this folder allows the project to be reproduced **without re-running the full Yelp data extraction and preprocessing steps**.

---

## Running the Code

- Scripts are intended to be run **sequentially**, following their numbering.
- File paths may need to be adjusted to match your local directory structure.
- The code assumes that required tables are available either via:
  - Your own Yelp data extraction, **or**
  - The provided `tables (FULL)` folder.

---

## Notes

- The analysis is **predictive**, not causal.
- Model evaluation focuses on ranking-based metrics (AUC, Gini, Top-Decile Lift).
- Google Trends and Google Places data are used **descriptively**, not as primary predictors.

---

## License / Use

This repository is intended for **academic and educational purposes** only.

---

## Contact

For questions related to this project or the codebase, please use GitHub.
