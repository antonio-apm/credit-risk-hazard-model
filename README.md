# Credit Risk Analysis: Retail Mortgage Loans
## **State-Dependent Logistic Hazard Model for 1-Month-Ahead Default Prediction**

*Antonio Melacini - December 2025*

## Overview
- This project builds an early-warning probability of default (PD) model that predicts whether a mortgage will become 90+ DPD (3+ missed payments) in the next month using information available in the current month. 
- The model is trained on loan-month panel data which is comprised of loan origination characteristics, monthly loan performance, and monthly macroeconomic variables.
    - Macroecnoomic variables are lagged to prevent leakage since the data for month $t$ is published in month $t+1$.
    - Due to the panel structure of the data, we use cluster-robust standard errors (clustered on loan IDs) to account for within-loan dependence over time.
- The model is state-dependent in the sense that one of the predictors is the current delinquency state (number of missed payments).
- In order to have a sufficient number of "events" in the data, we define default as the event of a loan having 3+ missed payments.
      - Also note that we have censoring at data cutoff and prepayment is treated as censoring.
- This is designed for applications to credit portfolio monitoring (flagging loans with rising risk), not "ever-default" origination pricing.

## Data
- **Freddie Mac Single-Family Loan-Level Dataset (SFLLD)**: vintages **2013, 2016, 2020**
- Train/test split:
  - **Train:** 2013/02 $-$ 2021/09  
  - **Test:** 2021/10 $-$ 2025/05  
- All loans are fixed-rate mortgages (FRMs) without prepayment penalties (non-PPM) in the sample.
- Missing DTI handled with:
  - `dti_missing` indicator
  - median-imputed DTI used inside `transf_dti = |DTI − 15|`



