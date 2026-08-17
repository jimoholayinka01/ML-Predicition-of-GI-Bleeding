# ML-Predicition-of-GI-Bleeding

A reproducible R-based project demonstrating development, evaluation, and deployment of a machine-learning model that predicts risk of gastrointestinal (GI) bleeding using the Eunomia OMOP OHDSI Common Data Model. The repository contains data extracts OHDSI/Eunomia, analysis and feature-engineering in an R Markdown workflow, and a Shiny application for patient-level risk assessment using the final XGBoost model.

---

## Key contents

- `OHDSI_V2_2026.Rmd` — Analysis notebook: data loading, cohort/outcome definition (GI bleeding), feature engineering, model training exploration and validation (V2 workflow).
- `OHDSI_2026_UI.R` — Shiny application (UI + server + custom CSS) for patient registration, risk prediction and model information pages. At the end of the file the app is launched with `shinyApp(ui, server)`.
- `data/GiBleed/` — (Not included in this repo) expected folder for OHDSI-style CSV files used by the Rmd (e.g., `PERSON.csv`, `CONDITION_OCCURRENCE.csv`, `DRUG_EXPOSURE.csv`, `MEASUREMENT.csv`, `OBSERVATION_PERIOD.csv`, `CONCEPT.csv`, `CONCEPT_ANCESTOR.csv`).
- `OBSERVATION.csv` — sample observation table extract (shown in repository).
- `variable_importance.png` — model variable-importance plot (image).
- Model artifacts expected by the Shiny app:
  - `v2_gi_bleeding_model.ubj` — final XGBoost model binary (XGBoost / ubj format).
  - `v2_model_metadata.rds` — metadata (feature names, formula, threshold, factor levels).
- `variable_importance` — binary PNG data (image file content).

---

## Project overview

The goal is to produce a patient-level classifier that identifies patients at risk of GI bleeding (outcome concept id used in the V2 workflow: `192671` — Gastrointestinal hemorrhage). The V2 pipeline:

1. Loads OHDSI/Eunomia-style CSVs and extracts the GI-bleed cohort.
2. Builds binary patient-level features from demographics, medications, clinical conditions and measurements.
3. Trains and evaluates multiple candidate models (Elastic Net, Random Forest, GBM, SVM, XGBoost, etc.) using cross-validation.
4. Selects an XGBoost model as final model (reported test AUROC ≈ 0.687, AUPRC ≈ 0.310) and exports the model and metadata for runtime use.
5. Provides a Shiny UI to register patients, store them in a local SQLite DB, and compute patient-level risk in real time using the exported model.

Note: This repository and application are research prototypes and demonstration software — predictions are not clinical advice.

---

## Requirements

- R >= 4.0
- Packages used in the analysis and app (a subset shown here; the full list is in `OHDSI_V2_2026.Rmd`):
  - DBI, DatabaseConnector, Eunomia, CommonDataModel, dplyr, dbplyr, tidyr, tibble
  - ggplot2, knitr, synthpop, rsample, glmnet, randomForest, xgboost, gbm, e1071, pROC, PRROC, caret, recipes, kernlab
- For model export/import: xgboost & base R serialization
- For the Shiny app: shiny, bslib, RSQLite, DBI, dplyr, xgboost

Install core packages (example):
```r
install.packages(c(
  "DBI","DatabaseConnector","dplyr","dbplyr","tidyr","tibble",
  "ggplot2","knitr","synthpop","rsample","glmnet","randomForest",
  "xgboost","gbm","e1071","pROC","PRROC","caret","recipes","kernlab",
  "shiny","bslib","RSQLite"
))
# OHDSI packages:
remotes::install_github("OHDSI/CommonDataModel")
remotes::install_github("OHDSI/Eunomia")
