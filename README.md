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
- **Total size:** ~ 18 GB
- Missing DTI handled with:
  - `dti_missing` indicator
  - median-imputed DTI used inside `transf_dti = |DTI − 15|`

## Model
Discrete-time hazard / early-warning PD model:

$Y_{it} = 1_{\{\text{default in month }t+1\}}$

$\text{logit}(\pi_{it}) = X_{it}^\top\beta^{(L)} + Z_t^\top\beta^{(M)}$

Where $\pi_{it}$ is the **1-month-ahead PD** given survival up to month $t$.

## Key Predictors (high signal)
**State / behavior**
- `deliq_num` (missed-payments state): dominates short-horizon PD (orders-of-magnitude effect)

**Pricing / borrower risk**
- `cr_spread` (origination credit spread): strong positive association  
- `credit_score` (FICO): strong negative association  
- `dti_missing` and `transf_dti = |DTI − 15|`: capture underwriting/affordability effects

**Macro / market regime (lagged)**
- `last_vix` (volatility / uncertainty): higher VIX → higher default risk  
- `last_unrate_chg_pos = max(Δ12 UR, 0)` (positive unemployment change): strong positive association  
- `last_infl_yoy_low` (inflation ≤ 2.5% indicator): distributional shift in risk

**Collateral**
- `last_vtl_est` (estimated value-to-loan ratio, proxy for equity): higher equity → lower risk

Controls
- region, occupancy, loan purpose, long-term indicator, and **baseline hazard control** via `ns(loan_age, df=2)`.


