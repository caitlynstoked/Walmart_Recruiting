# Walmart Recruiting – Store Sales Forecasting

## Project Overview

This project forecasts **weekly department-level sales** for Walmart stores using historical sales, economic indicators, and holiday markdown data. The goal is to make accurate predictions despite limited historical observations and highly seasonal demand patterns, particularly around major holidays.

The project is based on the **Walmart Recruiting – Store Sales Forecasting** Kaggle competition, designed to showcase applied forecasting and modeling skills on large-scale retail data.

---

## Business Problem

Retail forecasting is challenging because key events—such as holidays and markdowns—occur infrequently, limiting historical examples. Walmart provided sales data for **45 stores across multiple departments**, along with markdown events and macroeconomic variables, to simulate real-world decision-making under data constraints.

Accurate forecasts help:

* Optimize inventory and staffing decisions
* Anticipate holiday-driven demand spikes
* Reduce revenue loss from over- or under-stocking

---

## Objective

* Predict **weekly sales** for each store–department–date combination
* Incorporate holiday effects, markdowns, and external economic factors
* Minimize forecasting error using **Weighted Mean Absolute Error (WMAE)**

Holiday weeks receive higher importance during evaluation.

---

## Data Sources

* **train.csv** – Historical weekly sales by store and department
* **test.csv** – Store–department–date combinations requiring forecasts
* **features.csv** – Economic indicators, holiday flags, and markdown data

**Target Variable:**

* `Weekly_Sales`

---

## Exploratory Data Analysis

* Visualized missingness patterns across datasets
* Identified markdown variables with frequent missing values
* Verified sparsity across store–department combinations

---

## Feature Engineering

* Imputed missing markdown values with zeros and ensured non-negativity
* Created aggregate markdown features and log-transformed totals
* Imputed missing CPI and unemployment values using bagged tree imputation
* Added department metadata for interpretability
* Engineered time-based features (year, month, day-of-week)

---

## Modeling Strategy

Because data availability varies widely across store–department pairs, a **hybrid modeling strategy** was used:

### 1. Rule-Based Baselines

* No historical data → predict zero sales
* Fewer than 10 observations → predict historical mean

### 2. Time-Series Forecasting (Exploratory)

* Applied **Prophet** models for individual store–department pairs
* Included holiday indicators, temperature, and fuel price as regressors

### 3. Machine Learning Models

Used the **tidymodels** framework to compare multiple regression approaches:

* Random Forest (ranger)
* Gradient Boosted Trees (xgboost)
* Elastic Net Regression (glmnet)

Models were evaluated using **cross-validated RMSE** on sampled store–department pairs to balance accuracy and computational cost.

---

## Model Selection & Tuning

* Performed k-fold cross-validation with parallel processing
* Tuned hyperparameters using random grid search
* Compared performance and runtime across models
* Selected best-performing model based on RMSE

---

## Final Prediction Pipeline

For each store–department pair:

1. Determine data availability
2. Apply rule-based or trained ML model accordingly
3. Generate weekly sales forecasts
4. Track cross-validated RMSE for model reliability

Predictions were generated iteratively across all stores and departments with progress monitoring.

---

## Evaluation Metric

**Weighted Mean Absolute Error (WMAE)**:

* Holiday weeks weighted 5× more than non-holiday weeks
* Aligns model optimization with business-critical periods

---

## Submission Format

```text
Id,Weekly_Sales
1_1_2012-11-02,23145.32
1_1_2012-11-09,19872.11
```

* `Id`: Store_Dept_Date
* `Weekly_Sales`: Predicted sales value

---

## Tools & Technologies

* R
* tidyverse
* tidymodels
* ranger, xgboost, glmnet
* prophet
* vroom
* DataExplorer
* doParallel

---

## Competition Details

* **Competition:** Walmart Recruiting – Store Sales Forecasting
* **Host:** Kaggle
* **Evaluation Metric:** Weighted Mean Absolute Error (WMAE)
* **Year:** 2014

---

## Citation

Walmart Competition Admin & Cukierski, W. (2014). *Walmart Recruiting – Store Sales Forecasting*. Kaggle. [https://kaggle.com/competitions/walmart-recruiting-store-sales-forecasting](https://kaggle.com/competitions/walmart-recruiting-store-sales-forecasting)

---

## Author

Caitlyn Stokes

This project demonstrates applied retail forecasting, feature engineering for sparse time series, scalable model evaluation, and production-style prediction pipeli
