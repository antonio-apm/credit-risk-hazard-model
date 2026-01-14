##############################
# Investigating Age of Loan  #
##############################

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



full_df <- full_df %>%
  group_by(loan_sequence_number) %>%
  arrange(date, .by_group = TRUE) %>% 
  ungroup()

full_df <- full_df %>% mutate(
  yq = paste(format(date, "%Y"), quarters(date))
)

haz_age <- full_df %>%
  group_by(loan_age) %>%
  summarise(
    haz = mean(y), 
    n = n(), .groups = "drop")

poly_formula <- function(k) {
  paste0("I(x^", 1:k, ")", collapse = "+")
}

p1 <- ggplot(haz_age %>% filter(haz > 0 & haz < 1),
       aes(x = loan_age, y = qlogis(haz))) +
  geom_point(aes(size=n), alpha=0.5) +
  geom_smooth(se=TRUE, method="lm", 
              formula=paste0("y~",poly_formula(11))) +
  labs(x = "Loan Age (months)", y = "Est. Logit Hazard") +
  scale_size_continuous(name ="size", range=c(0.5, 3)) +
  theme_minimal()

rm(haz_age)

haz_age_date <- full_df %>%
  group_by(loan_age, yq) %>%
  summarise(
    haz = mean(y), 
    n = n(), .groups = "drop")

p2 <- ggplot(haz_age_date %>% filter(haz > 0 & haz < 1) %>% 
         mutate(yr = substr(yq, start=1,stop=4)), 
       aes(x = loan_age, y = qlogis(haz))) +
  geom_point(aes(size = n), alpha = 0.2) +
  geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
  facet_wrap(~ yr, ncol = 4) +
  scale_size_continuous(name = "n", range = c(0.4, 2.5)) +
  labs(
    x = "Loan Age (months)",
    y = "Empirical Hazard",
    title = "Empirical Default Hazard by Loan Age, Faceted by Calendar Year"
  ) +
  theme_minimal()


# Plot calendar time and age of loan simultaneously
p3 <- ggplot(haz_age_date %>% filter(n > 100),
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


# Save loan age plots
ggsave("figures/age_haz.pdf", plot = p1, width = 8, height = 6)
ggsave("figures/age_haz_year.pdf", plot = p2, width = 6, height = 6)
ggsave("figures/age_haz_time.pdf", plot = p3, width = 10, height = 10)


rm(list = ls())
gc()


