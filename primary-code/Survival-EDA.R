#################################
# Univariate Survival Analysis  #
#################################

setwd("C:/Projects-Code-Certificates/Credit-Risk-Mortgages")

full_df <- readRDS("objects/full_df.rds")

# Load libraries
library(tidyverse) # SQL-like data manipulation and more (e.g. dplyr)
library(ggplot2) # highly customizable visualizations
library(fredr) # FRED API
library(corrplot) # correlation matrix heat maps
library(performance) # VIF calculations and visualizations
library(knitr) # polished display of tables
library(glmnet) # regularized GLMs
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
p <- ggplot(cdf_fit, aes(x=time, y=cdf)) +
  geom_step() + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.25) +
  theme_minimal() + 
  labs(title="Estimated Cumulative Probability of Default vs. Time at Risk", 
       x="Time at Risk (Age of Loan in Months)", 
       y="Est. Cumulative PD")

# Save cumulative incidence plot
ggsave("figures/km_cpd_plot.pdf", plot = p, width = 8, height = 6)



rm(list = ls())
gc()


