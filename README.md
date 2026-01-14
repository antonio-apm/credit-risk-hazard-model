# Credit Risk Analysis: Retail Mortgage Loans
**State-Dependent Logistic Hazard Model for 1-Month-Ahead Default Prediction**
*Antonio Melacini - December 2025*

## Overview
- This project builds an early-warning probability of default (PD) model that predicts whether a mortgage will become 90+ DPD (3+ missed payments) in the next month using information available in the current month. 
- The model is trained on loan-month panel data which is comprised of loan origination characteristics, monthly loan performance variables, and monthly macroeconomic variables.
    - Due to the panel structure of the data, we use cluster-robust standard errors (clustered on loan IDs) to account for within-loan dependence over time. 
- This is designed for applications to credit portfolio monitoring (flagging loans with rising risk), not "ever-default" origination pricing.



