## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -------------------------------------------------------------------------------------------------
# Load libraries
library(tidyverse) # SQL-like data manipulation and more (e.g. dplyr)
library(ggplot2) # highly customizable visualizations
library(fredr) # FRED API
library(corrplot) # correlation matrix heat maps
library(performance) # VIF calculations and visualizations
library(knitr) # polished display of tables


## -------------------------------------------------------------------------------------------------
# Import data
orig_df <- read.csv("C:/Users/acubi/OneDrive/Desktop/Projects-Code-Certificates/Credit-Risk-Mortgages/credit_risk_pd/data/processed/Regression/orig_data.csv")
perf_df <- read.csv("C:/Users/acubi/OneDrive/Desktop/Projects-Code-Certificates/Credit-Risk-Mortgages/credit_risk_pd/data/processed/Regression/perf_data.csv")



## -------------------------------------------------------------------------------------------------
# Create indicator variable for event of default next month and
#   drop loans from data (remove from at-risk set) in months after defaulting 
perf_df <- perf_df %>% 
  arrange(loan_sequence_number, ymd(monthly_reporting_period)) %>% 
  group_by(loan_sequence_number) %>% 
  mutate(
    exit_now = (state %in% c("Default","Prepaid")), 
    keep = (cumsum(exit_now) == 0), # keep rows for a loan strictly before default 
    next_state = lead(state), 
    y = as.integer(next_state == "Default") # response variable
  ) %>% 
  ungroup() %>% 
  filter(keep, !is.na(y)) %>% 
  select(-exit_now, -keep)

# Merge origination and monthly data
full_df <- perf_df %>% left_join(orig_df, by = "loan_sequence_number")

# Delete perf_df to save space
rm(perf_df)

# Note: for any fixed loan, the origination variables are constant across months in full_df


# Drop right-censored rows (last time point for each loan)
full_df <- full_df %>% filter(!is.na(y))

# Sort on (loan ID, time)
full_df <- full_df %>% arrange(loan_sequence_number, ymd(monthly_reporting_period))

# Drop loans with NA values for LTV ratio at origination
na_ltv_ids <- full_df %>% 
  filter(orig_ltv == 999) %>% 
  pull(loan_sequence_number) %>% unique()
na_ltv_ids %>% length()
full_df %>% filter(loan_sequence_number %in% na_ltv_ids, y == 1) %>% 
  count() # check that dropping this wouldn't get rid of many default events (it doesn't)
full_df <- full_df %>% filter(!(loan_sequence_number %in% na_ltv_ids))

# Check out loans with NA values for DTI ratio at origination
na_dti_ids <- full_df %>% 
  filter(orig_dti == 999) %>% 
  pull(loan_sequence_number) %>% unique()
na_dti_ids %>% length()
full_df %>% filter(loan_sequence_number %in% na_dti_ids, y == 1) %>% 
  count() # check that dropping this wouldn't get rid of many default events (it does!)

# Create indicator for missing DTI values and impute the missing ones with median
clean_dti_vec <- orig_df$orig_dti[orig_df$orig_dti != 999]
med_dti <- median(clean_dti_vec)
full_df <- full_df %>% 
  mutate(dti_missing = (orig_dti == 999), 
         orig_dti_imp = ifelse(test=(orig_dti==999), 
                           yes=med_dti, no=orig_dti))


# Drop loans with NA values for credit score
na_cs_ids <- full_df %>% 
  filter(credit_score == 9999) %>% 
  pull(loan_sequence_number) %>% unique()
na_cs_ids %>% length()
full_df %>% filter(loan_sequence_number %in% na_cs_ids, y == 1) %>% 
  count() # check that dropping this wouldn't get rid of many default events (it doesn't)
full_df <- full_df %>% filter(!(loan_sequence_number %in% na_cs_ids))

# Normalize loan age into percentage of loan term
full_df$loan_age_pct <- full_df$loan_age / full_df$orig_loan_term

# Create variable for proportion of original unpaid principal left unpaid 
full_df$upb_pct <- full_df$current_actual_upb / full_df$orig_upb

# Create region variable (to use instead of state)
extra_abbs <- c("DC", "PR", "VI", "GU", "AS", "MP")
extra_regs <- c("South", "South", "South", "West", "West", "West")
custom_abb <- c(state.abb, extra_abbs)
custom_region <- c(as.character(state.region), extra_regs)
full_df <- full_df %>%
  mutate(region = custom_region[match(property_state, custom_abb)])

# Check levels of PPM flag and amortization type
orig_df$amortization_type %>% 
  as.factor() %>% levels()
orig_df$ppm_flag %>% 
  as.factor() %>% levels()

# Ensure orig_df only keeps IDs that we use for the full panel data
orig_df <- orig_df %>% 
  filter(loan_sequence_number %in% unique(full_df$loan_sequence_number))


# Select relevant variables for modeling
full_df <- full_df %>% select(
  loan_sequence_number, # ID
  monthly_reporting_period, # time
  y, # response (indicator for default next month)
  
  # monthly performance features (time-varying)
  deliq_num, # number of missed payments
  loan_age, # age of loan in months
  loan_age_pct, # age of loan as percentage of loan term
  upb_pct, # percentage of original unpaid principal left unpaid
  
  # origination features (constant over time for fixed loan)
  orig_upb, # unpaid balance at origination (original loan amount)
  quarter, # quarter of year when loan was originated
  credit_score, # FICO credit score
  orig_ltv, # loan-to-value ratio at origination
  orig_dti_imp, # debt-to-income ratio at origination (imputed with median)
  dti_missing, # indicator for DTI at origination being missing
  orig_interest_rate, # interest rate on loan at origination
  orig_loan_term, # length of the loan in months
  occupancy_status, # e.g. primary residence, investment property, etc.
  region, # region of USA (north/east/south/west) where property is located
  loan_purpose, # e.g. Cash-Out Refinance mortgage
) %>% rename(
  date = monthly_reporting_period, 
  orig_rate = orig_interest_rate, 
  orig_qtr = quarter
)



## ----include=FALSE--------------------------------------------------------------------------------
# Set FRED API key (this should be hidden from PDF output, and don't publish raw Rmd)
Sys.setenv(FRED_API_KEY = "0a2631fdb7b16e161ba0c36233ad34ff")


## -------------------------------------------------------------------------------------------------

months <- full_df %>% 
  select(date) %>% 
  unique() %>% 
  arrange(date)
months <- months$date

# View first month of time window
months[1]

# Set start and end date for macro variables 
start_date <- as.Date("2012-01-01") 
end_date <- as.Date(months[length(months)]) 

pull_fred <- function(series_id, monthly_avg=FALSE) {
  df <- fredr(
      series_id=series_id, 
      observation_start=start_date, 
      observation_end=end_date
    ) %>% 
    select(date, value) 
  if (monthly_avg) {
    df %>% mutate(
      ym = floor_date(date, "month") # month bucket
      ) %>% 
      group_by(ym) %>% 
      summarise(
        value = mean(value, na.rm=TRUE), .groups='drop'
        ) %>% 
      rename(date = ym, !!tolower(series_id) := value) # rename value column to the series_id
  } else {
    df %>% 
      rename(!!tolower(series_id) := value) # rename value column to the series_id
  }
}


# Get time series from FRED
unrate <- pull_fred("UNRATE")
cpi <- pull_fred("CPIAUCSL") %>% rename(cpi = cpiaucsl)
fedfunds <- pull_fred("FEDFUNDS") 
t10rate <- pull_fred("GS10") %>% rename(t10rate = gs10)
hpi <- pull_fred("CSUSHPINSA") %>% rename(hpi = csushpinsa)
slope <- pull_fred("T10Y3M", monthly_avg=TRUE) %>% rename(slope = t10y3m)
vix <- pull_fred("VIXCLS", monthly_avg=TRUE) %>% rename(vix = vixcls)

# Merge into a single dataframe
macro <- unrate %>% 
  left_join(cpi, by='date') %>% 
  left_join(fedfunds, by='date') %>% 
  left_join(hpi, by='date') %>%
  left_join(slope, by='date') %>% 
  left_join(vix, by='date') %>% 
  left_join(t10rate, by='date') %>% 
  arrange(date) %>% 
  mutate(
    # growth in CPI (inflation) YoY 
    infl_yoy = 100*(cpi / lag(cpi, 12) - 1), 
    # home inflation YoY
    hpi_yoy = 100*(hpi / lag(hpi, 12) - 1), 
    # change in UNRATE YoY
    unrate_chg_yoy = unrate - lag(unrate, 12), 
    # change in FEDFUNDS YoY
    fedfunds_chg_yoy = fedfunds - lag(fedfunds, 12), 
    # change in yield curve slope YoY
    slope_chg_yoy = slope - lag(slope, 12)
  ) 

# Prep data for plotting
macro_long <- macro %>%
  select(date, infl_yoy, hpi_yoy, unrate_chg_yoy, 
         fedfunds_chg_yoy, slope, slope_chg_yoy) %>%
  pivot_longer(cols = -date, names_to = "series", values_to = "value")
macro_long$series <- recode(
  macro_long$series, 
  infl_yoy = "CPI Inflation YoY",
  hpi_yoy = "HPI Inflation YoY",
  unrate_chg_yoy = "Unemployment YoY Change",
  fedfunds_chg_yoy = "Fed Funds YoY Change", 
  slope = "Yield Curve Slope (10Y-3M)", 
  slope_chg_yoy = "Yield Curve Slope YoY Change"
)
# Plot all the macro time series
ggplot(macro_long, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Value",
    color = "Macro Variable",
    title = "Selected Macroeconomic Time Series"
  ) 
rm(macro_long)
ggplot(vix, aes(x = date, y = vix)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Value",
    title = "CBOE Volatility Index (VIX)"
  ) 
index_levels <- data.frame(
  date = rep(cpi$date, 2), 
  value = c(hpi$hpi, cpi$cpi), 
  series = c(rep("hpi",length(cpi$date)), rep("cpi",length(cpi$date)))
)
ggplot(index_levels, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Value", 
    color = "Index", 
    title = "HPI and CPI Inflation YoY"
  ) 



macro <- macro %>% filter(!is.na(infl_yoy))
# Correlation matrix heat map for all macroeconomic variables
cor_macro <- macro %>% select(-date) %>% cor()
corrplot(cor_macro, method="color", tl.col="black", addCoef.col="black", 
         tl.srt=45, number.font=3, number.cex=0.65, 
         title="Correlations (Macroeconomic Variables)")


# Lag macro variables 1mo to prevent leakage
macro <- macro %>% mutate(
  last_t10rate = lag(t10rate, 1), 
  last_cpi = lag(cpi, 1), 
  last_infl_yoy = lag(infl_yoy, 1), 
  last_hpi = lag(hpi, 1), 
  last_hpi_yoy = lag(hpi_yoy, 1), 
  last_unrate_chg_yoy = lag(unrate_chg_yoy, 1), 
  last_fedfunds = lag(fedfunds, 1), 
  last_fedfunds_chg_yoy = lag(fedfunds_chg_yoy, 1), 
  # slope and vix are still lagged because we use monthly averages
  last_slope = lag(slope, 1), 
  last_slope_chg_yoy = lag(slope_chg_yoy, 1), 
  last_vix = lag(vix, 1)
) %>% 
  select(
   date, last_t10rate, last_cpi, last_infl_yoy, last_hpi_yoy, last_hpi, 
   last_vix, last_unrate_chg_yoy, last_fedfunds, last_fedfunds_chg_yoy, 
   last_slope, last_slope_chg_yoy
  )

# Merge macro data to loan-month data
full_df <- full_df %>% 
  mutate(date = as.Date(date))
full_df <- full_df %>% 
  left_join(macro, by="date")

# Derive date of origination
loan_vintages <- full_df %>%
  group_by(loan_sequence_number) %>%
  summarise(
    entry_year = year(min(date)),
    .groups = "drop"
  ) %>%
  mutate(
    vintage_year = case_when(
      entry_year %in% 2013:2015 ~ 2013,
      entry_year %in% 2016:2019 ~ 2016,
      entry_year %in% 2020:2025 ~ 2020,
      TRUE ~ entry_year
    )
  )
orig_df$vintage_year <- NULL
orig_df <- orig_df %>%
  left_join(loan_vintages %>% select(loan_sequence_number, vintage_year),
            by = "loan_sequence_number") 
orig_df <- orig_df %>%
  mutate(
    orig_quarter = case_when(
      quarter == "Q1" ~ 1,
      quarter == "Q2" ~ 2,
      quarter == "Q3" ~ 3,
      quarter == "Q4" ~ 4
    ),
    orig_month = case_when(
      orig_quarter == 1 ~ 1, # Jan
      orig_quarter == 2 ~ 4, # Apr
      orig_quarter == 3 ~ 7, # Jul
      orig_quarter == 4 ~ 10 # Oct
    ),
    orig_date = as.Date(sprintf("%d-%02d-01", vintage_year, orig_month))
  )
full_df <- full_df %>%
  left_join(orig_df %>% select(loan_sequence_number, orig_date),
            by = "loan_sequence_number")

# Approximate home price appreciation since origination ("HPAO")
hpi_df <- hpi %>% rename(orig_date = date, hpi_orig = hpi)
full_df$hpi_orig <- NULL
rm(hpi)
full_df <- full_df %>%
  left_join( # join HPI at origination
    hpi_df, by = "orig_date") %>%
  mutate(
    last_hpao = 100 * (last_hpi / hpi_orig - 1)
  )
rm(loan_vintages)



## Try hpi_yoy, hpao, orig_ltv, and orig_ltv * I(1/hpao) in the model.




## -------------------------------------------------------------------------------------------------
library(survival) # for KM estimator

# Aggregate (loan, month)-indexed binary data into loan-indexed count data
surv_df <- full_df %>% 
  group_by(loan_sequence_number) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(t = loan_age) %>% 
  summarise(
    time = ifelse(any(y==1), yes=min(t[y==1]), no=max(t)), # event or censor time
    status = as.integer(any(y == 1)), # 1 = observed event ; 0 = censored
    .groups = "drop"
  )

# Fit KM curve (estimate survival function)
fit <- survfit(Surv(time, status) ~ 1, data = surv_df)

rm(surv_df)

# Estimate CDF
cdf_fit <- data.frame(time = fit$time, surv = fit$surv, 
                     lower = 1 - fit$lower, upper = 1 - fit$upper) %>% 
  mutate(cdf = 1 - surv) %>% 
  select(-surv)

rm(fit)

# Plot CDF estimate with confidence limits
ggplot(cdf_fit, aes(x=time, y=cdf)) +
  geom_step() + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.25) +
  theme_minimal() + 
  labs(title="Estimated Cumulative Probability of Default vs. Time at Risk", 
       x="Time at Risk (Age of Loan in Months)", 
       y="Est. Cumulative PD")


## -------------------------------------------------------------------------------------------------

full_df <- full_df %>%
  group_by(loan_sequence_number) %>%
  arrange(date, .by_group = TRUE) %>% 
  ungroup()

full_df <- full_df %>% mutate(
  yq = paste(format(date, "%Y"), quarters(date))
)

haz_age_date <- full_df %>%
  group_by(loan_age, yq) %>%
  summarise(
    haz = mean(y), 
    n = n(), .groups = "drop")

# Plot calendar time and age of loan simultaneously
ggplot(haz_age_date %>% filter(n > 100),
       aes(x = loan_age, y = yq, fill = haz)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    x = "Loan Age (months)",
    y = "Calendar Time (Year-Quarter)",
    fill = "Empirical Hazard"
  ) +
  theme_minimal()
rm(haz_age_date)


## -------------------------------------------------------------------------------------------------
haz_date <- full_df %>%
  group_by(date) %>% 
  summarise(haz = mean(y), n_obs = n(), .groups = "drop") %>%
  left_join(macro, by="date")

macro_vars <- setdiff(names(macro), "date")
for (var_name in macro_vars) {
  p <- ggplot(haz_date %>% filter(haz > 0 & haz < 1), 
              aes(.data[[var_name]], qlogis(haz))) +
        geom_point(aes(size=n_obs), alpha=0.5) +
        geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
        theme_minimal() +
        labs(y = "Est. Logit(Hazard)", x = var_name, 
             title = var_name) + 
    scale_size_continuous(name ="size", range=c(0.5, 3))
  print(p)
}
rm(haz_date)

haz_rate <- full_df %>%
  group_by(orig_rate) %>% 
  summarise(
    haz = mean(y), .groups = "drop"
  )
p <- ggplot(haz_rate %>% filter(haz > 0 & haz < 1), 
            aes(orig_rate, qlogis(haz))) +
        geom_point() +
        geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
        theme_minimal() +
        labs(y = "Est. Logit(Hazard)", x = "Interest Rate at Origination", 
             title = "Interest Rate at Origination")
print(p)
rm(haz_rate)


## -------------------------------------------------------------------------------------------------
# Compute credit spread (risk premium) for each loan
orig_df <- orig_df %>% 
  left_join(t10rate %>% rename(orig_date = date, t10r = t10rate),
            by="orig_date") 
orig_df <- orig_df %>%
  mutate(cr_spread = pmax(orig_interest_rate - t10r, 0))
full_df <- full_df %>% 
  left_join(orig_df %>% select(loan_sequence_number, cr_spread), 
            by="loan_sequence_number")

# Plot logit hazard vs. credit spread
haz_spread_bin <- full_df %>%
  mutate(cr_bin = ntile(cr_spread, 40)) %>% 
  group_by(cr_bin) %>% 
  summarise(cr_mu = mean(cr_spread), haz = mean(y), n = n(), .groups = 'drop')
ggplot(haz_spread_bin %>% filter(haz > 0 & haz < 1), 
       aes(cr_mu, qlogis(haz))) +
  geom_point(alpha=0.7) +
  geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
  theme_minimal() +
  labs(y = "Est. Logit(Hazard)", x = "Spread", 
       title = "Credit Spread (Risk Premium)")
rm(haz_spread_bin)



## -------------------------------------------------------------------------------------------------

# Indicator for low inflation
full_df$last_infl_yoy_low <- factor(as.integer(full_df$last_infl_yoy < 0))
#full_df <- full_df %>% select(-last_infl_yoy)

# Positive part of unremployment rate change
full_df$last_unrate_chg_pos <- pmax(full_df$last_unrate_chg_yoy, 0)
#full_df <- full_df %>% select(-last_unrate_chg_yoy)

# Rate cut (negative part of change in fed funds rate)
full_df$last_rate_cut <- pmax(-full_df$last_fedfunds_chg_yoy, 0)
#full_df <- full_df %>% select(-last_fedfunds_chg_yoy)

# Indicator of change in yield curve slope being positive
full_df$last_slope_incr <- factor(as.integer(full_df$last_slope_chg_yoy > 0))
#full_df <- full_df %>% select(-last_slope_chg_yoy)

# Magnitude of slope (steepness)
full_df$last_slope_mag <- abs(full_df$last_slope)

# Inversion status of yield curve slope
full_df$last_invert <- factor(as.integer(full_df$last_slope < 0))
#full_df <- full_df %>% select(-last_slope)

# Binned VIX regimes
full_df$last_vix_bin <- factor(cut(full_df$last_vix, breaks=c(0,20,30,Inf)), 
                                  labels=c("Low", "Mid", "High"))
#full_df <- full_df %>% select(-last_vix)


new_vars <- c("last_unrate_chg_pos", "last_rate_cut", "last_slope_mag", 
              "last_slope_incr", "last_infl_yoy_low", "last_invert", 
              "last_vix_bin")

haz_date <- full_df %>%
  group_by(date) %>% 
  summarise(haz = mean(y),across(all_of(new_vars), ~ first(.x)), 
            .groups = "drop") 

for (var_name in new_vars) {
  if (var_name %in% c("last_infl_yoy_low", "last_slope_incr", 
                      "last_invert", "last_vix_bin")) {
    # overlapped histograms for categorical variables
    p <- ggplot(haz_date %>% filter(haz > 0 & haz < 1),
                aes(x = qlogis(haz), fill = factor(.data[[var_name]]))) +
      geom_density(alpha = 0.6, position = "identity") + 
      scale_fill_manual(values = c("steelblue", "lightpink", "darkred"), 
                        name = var_name, labels = c("0", "1", "2")) +
      theme_minimal() +
      labs(
        x = "Est. Logit(Hazard)",
        y = "Kernel Density Estimate",
        title = paste("Overlapping Densities by", var_name)
      )
  } else {
    smoother <- ifelse(test=(var_name=="last_unrate_chg_pos"), 
                     yes="lm", no="loess")
    # scatterplots for continuous variables
    p <- ggplot(haz_date %>% filter(haz > 0 & haz < 1), 
              aes(.data[[var_name]], qlogis(haz))) +
        geom_point() +
        geom_smooth(se = TRUE, method = smoother, formula = "y~x") +
        theme_minimal() +
        labs(y = "Est. Logit(Hazard)", x = var_name, 
             title = var_name)
  }
  print(p)
}
rm(haz_date)




## -------------------------------------------------------------------------------------------------

full_df <- full_df %>% select(-c(last_slope_incr, last_invert))

# Binned VIX regimes (only 2 now)
full_df$last_vix_bin <- factor(cut(full_df$last_vix, breaks=c(0,20,Inf)), 
                                  labels=c("Calm", "Stress"))

haz_date <- full_df %>%
  group_by(date) %>% 
  summarise(haz = mean(y), last_vix_bin = first(last_vix_bin), 
            .groups = "drop") 

ggplot(haz_date %>% filter(haz > 0 & haz < 1),
                aes(x = qlogis(haz), fill = factor(last_vix_bin))) +
      geom_density(alpha = 0.6, position = "identity") + 
      scale_fill_manual(values = c("steelblue", "lightpink"), 
                        name = "VIX Regime", labels = c("Calm", "Stress")) +
      theme_minimal() +
      labs(
        x = "Est. Logit(Hazard)",
        y = "Count",
        title = paste("Overlapping Histograms by", "VIX Regime")
      )


## -------------------------------------------------------------------------------------------------

haz_hpao <- full_df %>%
  group_by(last_hpao) %>%
  summarise(haz = mean(y), .groups = "drop")
ggplot(haz_hpao %>% filter(haz > 0 & haz < 1), 
       aes(x = last_hpao, y = qlogis(haz))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, formula = "y ~ x") +
  theme_minimal() +
  labs(
    x = "HPAO",
    y = "Est. Logit(Hazard)",
    title = "HPAO (Home Price Appreciation Since Origination)"
  )
rm(haz_hpao)


full_df <- full_df %>% 
  mutate(last_ltv_est = orig_ltv / last_hpao)

haz_eltv <- full_df %>%
  group_by(last_ltv_est) %>%
  summarise(haz = mean(y), .groups = "drop")
ggplot(haz_eltv %>% filter(haz > 0 & haz < 1), 
       aes(x = last_ltv_est, y = qlogis(haz))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, formula = "y ~ x") +
  theme_minimal() +
  labs(
    x = "Est. LTV ",
    y = "Est. Logit(Hazard)",
    title = "Est. LTV (LTV at Origination / HPAO)"
  )
ggplot(haz_eltv %>% filter(haz > 0 & haz < 1), 
       aes(x = log(last_ltv_est), y = qlogis(haz))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, formula = "y ~ x") +
  theme_minimal() +
  labs(
    x = "Log Est. LTV ",
    y = "Est. Logit(Hazard)",
    title = "Log Est. LTV"
  )
ggplot(haz_eltv %>% filter(haz > 0 & haz < 1), 
       aes(x = 1/last_ltv_est, y = qlogis(haz))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, formula = "y ~ x") +
  theme_minimal() +
  labs(
    x = "Inverse Est. LTV ",
    y = "Est. Logit(Hazard)",
    title = "Inverse Est. LTV (Est. Value-To-Loan Ratio)"
  )
rm(haz_eltv)




## -------------------------------------------------------------------------------------------------
origupb_plot_df <- full_df %>% 
  group_by(loan_sequence_number) %>% 
  summarise(amt = min(orig_upb), event=sum(y)) %>% 
  ungroup() %>% 
  mutate(event = factor(event, levels=c(0,1), labels=c("y = 0", "y = 1")))

ggplot(origupb_plot_df, aes(x=amt, fill=event)) +
  geom_density(position='identity', alpha=0.3) +
  labs(x="Original Unpaid Balance (Loan Amount)", y="Density", fill="Event") +
  theme_minimal()

rm(origupb_plot_df)


## -------------------------------------------------------------------------------------------------
# Remove variables already deemed to have no predictive power
full_df <- full_df %>% 
  select(-loan_age, -loan_age_pct, -orig_upb)

full_df$y <- full_df$y %>% as.integer()

# Encode categorical variables as factors
cat_vars <- c("deliq_num", "occupancy_status", "region", "loan_purpose")
for (v in cat_vars[1:length(cat_vars)]) {
  full_df[[v]] <- as.factor(full_df[[v]])
}

# Summarize response grouped by each categorical variable
results <- lapply(cat_vars, function(v) {
  full_df %>% group_by(.data[[v]]) %>% 
    summarise(mu = mean(y), sd = sqrt(mean(y)*(1-mean(y))), 
              n  = n(), .groups = "drop") %>%
    mutate(variable = v) %>% 
    rename(level = 1) %>% 
    select(variable, level, everything())
})
catsum_df <- bind_rows(results) %>% 
  group_by(variable) %>% 
  arrange(variable, mu) %>% 
  ungroup()
catsum_df %>% kable()


## -------------------------------------------------------------------------------------------------

# do FOR loop similar to for macro variables except will have to recompute haz df each time


haz_upb_bin <- full_df %>%
  mutate(upb_bin = ntile(upb_pct, 100)) %>% 
  group_by(upb_bin) %>% 
  summarise(upb_mu = mean(upb_pct), haz = mean(y), .groups = 'drop')
ggplot(haz_upb_bin %>% filter(haz > 0 & haz < 1), 
       aes(upb_mu, qlogis(haz))) +
  geom_point(alpha=0.7) +
  geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
  theme_minimal() +
  labs(y = "Est. Logit(Hazard)", x = "UPB %", 
       title = "Unpaid Principal Balance %")
rm(haz_upb_bin)


## -------------------------------------------------------------------------------------------------
# Looking at loan term
haz_term <- full_df %>%
  group_by(orig_loan_term) %>% 
  summarise(haz = mean(y), .groups = 'drop')
ggplot(haz_term %>% filter(haz > 0 & haz < 1), 
       aes(orig_loan_term, qlogis(haz))) +
  geom_point(alpha=0.7) +
  geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
  theme_minimal() +
  labs(y = "Est. Logit(Hazard)", x = "Loan Term", 
       title = "Loan Term")
rm(haz_term)

# Excess loan term above 25yrs
full_df$term_cap <- pmax(full_df$orig_loan_term - 300, 0)
haz_term <- full_df %>%
  group_by(term_cap) %>% 
  summarise(haz = mean(y), .groups = 'drop')
ggplot(haz_term %>% filter(haz > 0 & haz < 1), 
       aes(term_cap, qlogis(haz))) +
  geom_point(alpha=0.7) +
  geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
  theme_minimal() +
  labs(y = "Est. Logit(Hazard)", x = "Max(Loan Term, 25 Years)", 
       title = "Excess Loan Term Above 25 Years")
rm(haz_term)
full_df <- full_df %>% select(-term_cap)


# Indicator for loan term being at least 25 years
full_df$long_term <- factor(as.integer(full_df$orig_loan_term >= 300), 
                            labels=c("<25y", ">=25y"))
haz_term <- full_df %>%
  group_by(date, long_term) %>%
  summarise(haz = mean(y), .groups = "drop")
ggplot(haz_term %>% filter(haz > 0 & haz < 1),
       aes(x = qlogis(haz), fill = long_term)) +
  geom_density(alpha = 0.6, position = "identity") +
  scale_fill_manual(
    values = c("steelblue", "lightpink"),
    name   = "Loan Term",
    labels = c("<25y", ">=25y")
  ) +
  theme_minimal() +
  labs(x = "Est. Logit(Hazard)", y = "Kernel Density Estimate",
    title = "Overlapping Densities for Long vs. Short-Medium Term Loans"
  )
rm(haz_term)



## -------------------------------------------------------------------------------------------------
# Looking at credit score
haz_credit <- full_df %>%
  group_by(credit_score) %>%
  summarise(haz = mean(y), .groups = "drop")
ggplot(haz_credit %>% filter(haz > 0 & haz < 1),
       aes(y = qlogis(haz), x = credit_score)) +
  geom_point(alpha = 0.6, position = "identity") +
  geom_smooth(method='lm', se=TRUE, formula='y~x') +
  theme_minimal() +
  labs( x = "Credit Score", y = "Est. Logit(Hazard)",
    title = "FICO Credit Score at Origination"
  )
rm(haz_credit)


## -------------------------------------------------------------------------------------------------
# Looking at DTI ratio at origination
haz_dti <- full_df %>%
  group_by(orig_dti_imp) %>%
  summarise(haz = mean(y), .groups = "drop")
ggplot(haz_dti %>% filter(haz > 0 & haz < 1),
       aes(y = qlogis(haz), x = orig_dti_imp)) +
  geom_point(alpha = 0.6, position = "identity") +
  geom_smooth(method='loess', se=TRUE, formula='y~x') +
  theme_minimal() +
  labs( x = "DTI", y = "Est. Logit(Hazard)",
    title = "DTI Ratio at Origination"
  )
rm(haz_dti)

full_df$transf_dti <- abs(full_df$orig_dti_imp - 15)
haz_transf_dti <- full_df %>%
  group_by(transf_dti) %>%
  summarise(haz = mean(y), .groups = "drop")
ggplot(haz_transf_dti %>% filter(haz > 0 & haz < 1),
       aes(y = qlogis(haz), x = transf_dti)) +
  geom_point(alpha = 0.6, position = "identity") +
  geom_smooth(method='loess', se=TRUE, formula='y~x') +
  theme_minimal() +
  labs( x = "|DTI - 15|", y = "Est. Logit(Hazard)",
    title = "|DTI - 15|"
  )

haz_dti_miss <- full_df %>%
  group_by(dti_missing, date) %>%
  summarise(haz = mean(y), .groups = "drop")
ggplot(haz_dti_miss %>% filter(haz > 0 & haz < 1),
                aes(x = qlogis(haz), fill = factor(dti_missing))) +
      geom_density(alpha = 0.6, position = "identity") + 
      scale_fill_manual(values = c("steelblue", "lightpink"), 
                        name = "DTI Missing", labels = c("0", "1")) +
      theme_minimal() +
      labs(x = "Est. Logit(Hazard)", y = "Kernel Density Estimate",
        title = "Densities of Logit Hazard for Missing vs. Non-Missing DTI")
ggplot(haz_dti_miss %>% filter(haz > 0 & haz < 1),
       aes(x = factor(dti_missing), y = qlogis(haz), 
           fill = factor(dti_missing))) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_manual(values = c("steelblue", "lightpink"), name = "DTI Missing") +
  theme_minimal() +
  labs(
    x = "DTI Missing Indicator",
    y = "Est. Logit(Hazard)",
    title = "Distribution of Logit Hazard by DTI Missing Status"
  ) +
  theme(legend.position = "none")
rm(haz_dti_miss)

full_df %>%
  group_by(dti_missing) %>%
  summarise(haz = mean(y), sd = sqrt(haz*(1-haz)), n = n(), 
            .groups = "drop") %>% 
  kable()



## -------------------------------------------------------------------------------------------------
# can do GLM comparison via deviance/AIC right here with just DTI as predictor or do it later with all the variables (****prob just do it here since we did that before)

library(splines)

num_events <- full_df %>% filter(y == 1) %>% 
  pull(loan_sequence_number) %>% unique() %>% length()

num_all <- full_df %>% 
  pull(loan_sequence_number) %>% unique() %>% length()

(p_rec <- num_events/num_all)

train_sample <- function(p=p_rec) {
  events_df <- full_df %>% filter(y == 1)
  event_ids <- events_df %>% pull(loan_sequence_number) %>% unique()
  events_df <- full_df %>% filter(loan_sequence_number %in% event_ids)
  nonevents_df <- full_df %>% filter(!(loan_sequence_number %in% event_ids))
  nonevents_df <- nonevents_df %>% slice_sample(prop=p)
  return( bind_rows(events_df, nonevents_df) )
}

sample_df <- train_sample(0.06)



# Model with DTI linear and symmetric about 15
symlin_dti <- glm(y ~ transf_dti, family=binomial, 
                  data=sample_df, x = FALSE, y = FALSE, model = FALSE)

# Model with DTI quadratic and symmetric about 15
symquad_dti <- glm(y ~ transf_dti + I(transf_dti^2), family=binomial, 
                  data=sample_df, x = FALSE, y = FALSE, model = FALSE)

AIC(symlin_dti, symquad_dti)

symquad_dti$coefficients[['I(transf_dti^2)']]

rm(symquad_dti, symlin_dti)


## ----eval=FALSE-----------------------------------------------------------------------------------
## # Marginal distribution of each continuous variable
## num_df <- full_df %>% select_if(is.numeric)
## par(mfrow=c(3,3))
## for (vec_name in names(num_df)) {
##   boxplot(num_df[[vec_name]], main=vec_name, xlab=vec_name)
## }
## par(mfrow=c(1,1))
## 
## 
## # Correlation matrix heat map for all continuous variables
## cor_num <- cor(num_df)
## corrplot(cor_num, method="color", tl.col="black", tl.cex=0.9,
##          number.cex=0.4, addCoef.col="black", tl.srt=45, number.font=3,
##          title="Correlations (Continuous Variables)")
## 
## 
## 
## 
## # Check VIFs for all variables
## fit_all <- glm(y ~ loan_age + I(loan_age^2) + deliq_num +
##                  upb_pct + rate_change +
##                  last_unrate + last_unrate_chg_yoy +
##                  last_fedfunds + last_fedfunds_chg_yoy +
##                  last_hpi_lg_yoy + last_infl_lg_yoy,
##                family=binomial, data=full_df)
## 
## fit_all2 <- glm(
##   event ~
##     loan_age + I(loan_age^2) +
##     last_unrate + last_unrate_chg_yoy +
##     last_fedfunds + last_fedfunds_chg_yoy +
##     last_hpi_lg_yoy + last_infl_lg_yoy,
##   family = binomial,
##   data = tv_surv_df
## )
## 
## 
## check_collinearity(fit_adj)


## ----eval=FALSE-----------------------------------------------------------------------------------
## # Load libraries for custom CV implementation
## library(data.table)
## library(splines)
## library(glmnet) # for fitting regularized GLMs
## 
## # Create global training and holdout subsets
## train_prop <- 0.8
## global_train_end <- ceiling(length(months) * train_prop)
## train_mos <- months[1:global_train_end]
## df_train <- full_df %>% filter(date %in% train_mos)
## df_test <- full_df %>% filter(date %notin% train_mos)


## ----eval=FALSE-----------------------------------------------------------------------------------
## # Prepare data.table for CV
## dt <- as.data.table(df_train)
## dt[, date := as.IDate(date)]
## dt[, y := as.integer(y)]
## 
## # Encode categorical variables in data.table as factors
## cat_vars <- c("deliq_num", "occupancy_status", "region",
##               "loan_purpose")
## for (v in cat_vars) dt[, (v) := as.factor((v))]
## 
## 
## # Create random sample of loans to be used for CV
## set.seed(1) # reproducibility
## sample_frac <- 0.15
## all_ids <- unique(dt$loan_sequence_number)
## sample_ids <- sample(all_ids, size=ceiling(sample_frac*length(all_ids)), replace=FALSE)
## dts <- dt[loan_sequence_number %in% sample_ids] # sampled data
## 
## cat("Sampled", length(sample_ids), "loans out of", length(all_ids), "\n")
## cat("Sampled", nrow(dts), "rows out of", nrow(dt), "\n")
## 
## dts <- dt
## 
## # Create table telling us which rows correspond to each time period
## date_tab <- dts[, .N, by=date][order(date)]
## date_tab[, end_row := cumsum(N)]
## date_tab[, start_row := shift(end_row, fill = 0) + 1]
## 
## 
## # Function to map data into the indices defining each CV fold
## make_folds <- function(date_tab, init_periods, assess_periods, step) {
## 
##   m <- nrow(date_tab)
##   stopifnot(init_periods + assess_periods <= m)
## 
##   fold_splits <- seq(from=init_periods, to=m-assess_periods, by=step)
## 
##   return(
##     data.table(
##       fold = seq_along(fold_splits),
##       train_end_row  = date_tab$end_row[fold_splits],
##       test_start_row = date_tab$start_row[fold_splits + 1],
##       test_end_row   = date_tab$end_row[fold_splits + assess_periods],
##       train_end_date = date_tab$date[fold_splits]
##       )
##   )
## }
## 
## # Create CV folds
## folds <- make_folds(date_tab, init_periods=30, assess_periods=8, step=8)
## cat("Number of folds:", nrow(folds))
## 
## 
## 
## # Vector of all non-age covariate names
## other_x <- setdiff(names(dts),
##                    c("loan_sequence_number", "date", "y", "loan_age"))
## 
## # Right-hand-side formula string
## rhs <- paste(other_x, collapse=" + ")
## 
## # Create list of different model formulas
## specs <- list(
##   age_linear = as.formula(paste0("y ~ loan_age_pct + ", rhs)),
##   age_log1p = as.formula(paste0("y ~ log1p(loan_age_pct) + ", rhs)),
##   bins = as.formula(paste0("y ~ age_group_factor", rhs)),
##   age_spline = as.formula(paste0("y ~ ns(loan_age_pct, df=4) + ", rhs))
## )
## 
## 
## # Binary entropy loss function (with padding for numerical stability)
## be_loss <- function(y, p, eps=1e-15){
##   p <- pmin(pmax(p, eps), 1-eps)
##   return(-mean(y*log(p) + (1-y)*log(1-p)))
## }
## 
## 
## # Function to evaluate a model specification on CV folds
## eval_spec <- function(x, y, folds, loss_func) {
## 
##   n_folds <- nrow(folds)
##   out_list <- vector("list", n_folds)  # store per-fold results
## 
##   for (i in seq_len(n_folds)) {
## 
##     tr_end <- folds$train_end_row[i]
##     te_s <- folds$test_start_row[i]
##     te_e <- folds$test_end_row[i]
## 
##     x_train <- x[1:tr_end, , drop = FALSE]
##     x_test <- x[te_s:te_e, , drop = FALSE]
##     y_train <- y[1:tr_end]
##     y_test <- y[te_s:te_e]
## 
##     # Fit Lasso logistic regression
##     fit <- glmnet(
##       x = x_train,
##       y = y_train,
##       family = "binomial",
##       alpha = 1, # alpha = 1 for Lasso
##       nlambda = 50
##     )
## 
##     # Choose the last lambda for prediction
##     lambda_use <- tail(fit$lambda, 1)
## 
##     p <- as.numeric(
##       predict(fit, newx = x_test, type = "response", s = lambda_use)
##     )
## 
##     loss <- loss_func(y_test, p)
## 
##     # one-row data.table for this fold
##     out_list[[i]] <- data.table(
##       fold = folds$fold[i],
##       loss = loss,
##       train_end_month = folds$train_end_month[i]
##     )
## 
##     cat("\t", "Done fold ", i) # status update
##   }
## 
##   # combine all folds
##   rbindlist(out_list)
## }
## 
## 
## # Conduct CV (train and test the models on each fold)
## cv_results <- rbindlist(
##   lapply(names(specs), function(model) {
## 
##     cat("Working on model", model) # status update
## 
##     formula <- specs[[model]]
## 
##     needed_vars <- all.vars(formula)
##     dts_small <- dts[, ..needed_vars]
## 
##     x_mat <- model.matrix(formula, data = dts_small)[, -1, drop = FALSE]
##     y_vec <- dts_small$y
## 
##     res <- eval_spec(x_mat, y_vec, folds, be_loss)
##     res[, spec := model]
##     res
##   })
## )
## 
## 
## cv_summary <- cv_results[, .(
##   mean_loss = mean(loss),
##   sd_loss = sd(loss),
##   se_loss = sd(loss)/sqrt(.N)
##   ),
##   by = spec][order(mean_loss)]
## 
## cv_summary
## 
## 
## best_spec <- cv_summary$spec[1]
## best_formula <- specs[[best_spec]]
## cat("Best specification for loan age component of model:", best_spec)


## ----eval=FALSE-----------------------------------------------------------------------------------
## 
## 


## ----eval=FALSE-----------------------------------------------------------------------------------
## # Fit model 1: main effects
## #m1 <- glm(..., family=binomial, data=full_df)
## 

