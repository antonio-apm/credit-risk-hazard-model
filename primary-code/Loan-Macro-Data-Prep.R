####################################
# Loan and Macro Data Preparation  #
####################################

setwd("C:/Projects-Code-Certificates/Credit-Risk-Mortgages")


## LOAN-LEVEL DATA CLEANING

# Load libraries
library(tidyverse) # SQL-like data manipulation and more (e.g. dplyr)
library(ggplot2) # highly customizable visualizations
library(fredr) # FRED API
library(corrplot) # correlation matrix heat maps
library(performance) # VIF calculations and visualizations
library(knitr) # polished display of tables
library(glmnet) # regularized GLMs

# Import data
orig_df <- read.csv("C:/Projects-Code-Certificates/Credit-Risk-Mortgages/data/processed/Regression/orig_data.csv")
perf_df <- read.csv("C:/Projects-Code-Certificates/Credit-Risk-Mortgages/data/processed/Regression/perf_data.csv")

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



## MACROECONOMIC DATA CLEANING

# Read and set API key
fred_key <- readLines("config/fred_api_key.txt")
fredr::fredr_set_key(fred_key)

# Get vector of all monthly dates
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

saveRDS(t10rate, file="objects/t10rate.rds")

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
p1 <- ggplot(macro_long, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Value",
    color = "Macro Variable",
    title = "Selected Macroeconomic Time Series"
  ) 
rm(macro_long)
p2 <- ggplot(vix, aes(x = date, y = vix)) +
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
p3 <- ggplot(index_levels, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Value", 
    color = "Index", 
    title = "HPI and CPI Inflation YoY"
  ) 

# Save macro plots
ggsave("figures/macro_ts.pdf", plot = p1, width = 10, height = 8)
ggsave("figures/vix_ts.pdf", plot = p2, width = 10, height = 8)
ggsave("figures/infl_ts.pdf", plot = p3, width = 10, height = 8)


rm(p1, p2, p3)

macro <- macro %>% filter(!is.na(infl_yoy))

# Correlation matrix heat map for all macroeconomic variables
cor_macro <- macro %>% select(-date) %>% cor()
pdf("figures/macro_corr.pdf", width = 10, height = 10)
corrplot(cor_macro, method="color", tl.col="black", addCoef.col="black", 
         tl.srt=45, number.font=3, number.cex=0.65)
dev.off()

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
    last_hpao = last_hpi / hpi_orig - 1
  )
rm(loan_vintages)

# Save full merged performance + origination data
saveRDS(full_df, file="objects/full_df.rds")

# Save the cleaned origination loan data 
saveRDS(orig_df, file="objects/orig_df.rds")

# Save macro data
saveRDS(macro, file="objects/macro.rds")



rm(list = ls())
gc()



