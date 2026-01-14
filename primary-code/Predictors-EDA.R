###########################################
# Investigating & Transforming Predictors #
###########################################

setwd("C:/Projects-Code-Certificates/Credit-Risk-Mortgages")

orig_df <- readRDS("objects/orig_df.rds")
full_df <- readRDS("objects/full_df.rds")
macro <- readRDS("objects/macro.rds")

# Load libraries
library(tidyverse) # SQL-like data manipulation and more (e.g. dplyr)
library(ggplot2) # highly customizable visualizations
library(fredr) # FRED API
library(corrplot) # correlation matrix heat maps
library(performance) # VIF calculations and visualizations
library(knitr) # polished display of tables
library(glmnet) # regularized GLMs
library(survival) # for KM estimator
library(survival) # Kaplan-Meier estimator
library(splines) # natural splines


## FIRST PLOTS FOR PRELIMINARY INSPECTION

haz_date <- full_df %>%
  group_by(date) %>% 
  summarise(haz = mean(y), n_obs = n(), .groups = "drop") %>%
  left_join(macro, by="date")

macro_vars <- setdiff(names(macro), "date")
saveRDS(macro_vars, file="objects/macro_vars.rds")
for (var_name in macro_vars) {
  p <- ggplot(haz_date %>% filter(haz > 0 & haz < 1), 
              aes(.data[[var_name]], qlogis(haz))) +
    geom_point(aes(size=n_obs), alpha=0.5) +
    geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
    theme_minimal() +
    labs(y = "Est. Logit(Hazard)", x = var_name, 
         title = var_name) + 
    scale_size_continuous(name ="size", range=c(0.5, 3))
  ggsave(paste0("figures/",var_name,".pdf"), plot = p, width = 6, height = 6)
  rm(p)
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
ggsave("figures/orig_rate.pdf", plot = p, width = 6, height = 6)
rm(p)
rm(haz_rate)



## SECOND PLOTS FOR TRANSFORMATIONS

t10rate <- readRDS("objects/t10rate.rds")

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
p <- ggplot(haz_spread_bin %>% filter(haz > 0 & haz < 1), 
       aes(cr_mu, qlogis(haz))) +
  geom_point(alpha=0.7) +
  geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
  theme_minimal() +
  labs(y = "Est. Logit(Hazard)", x = "Spread", 
       title = "Credit Spread (Risk Premium)")
ggsave("figures/cr_spread.pdf", plot = p, width = 6, height = 6)
rm(p)
rm(haz_spread_bin)


# Indicator for low inflation
full_df$last_infl_yoy_low <- factor(as.integer(full_df$last_infl_yoy < 2.5))
full_df <- full_df %>% select(-last_infl_yoy)

# Positive part of unemployment rate change
full_df$last_unrate_chg_pos <- pmax(full_df$last_unrate_chg_yoy, 0)
full_df <- full_df %>% select(-last_unrate_chg_yoy)

# Rate cut (negative part of change in fed funds rate)
full_df$last_rate_cut <- pmax(-full_df$last_fedfunds_chg_yoy, 0)
full_df <- full_df %>% select(-last_fedfunds_chg_yoy)

# Indicator of change in yield curve slope being positive
full_df$last_slope_incr <- factor(as.integer(full_df$last_slope_chg_yoy > 0))
full_df <- full_df %>% select(-last_slope_chg_yoy)

# Magnitude of slope (steepness)
full_df$last_slope_mag <- abs(full_df$last_slope)

# Inversion status of yield curve slope
full_df$last_invert <- factor(as.integer(full_df$last_slope < 0))
full_df <- full_df %>% select(-last_slope)


new_vars <- c("last_unrate_chg_pos", "last_rate_cut", "last_slope_mag", 
              "last_slope_incr", "last_infl_yoy_low", "last_invert")

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
  ggsave(paste0("figures/",var_name,".pdf"), plot = p, width = 6, height = 6)
  rm(p)
}
rm(haz_date)


haz_hpao <- full_df %>%
  group_by(last_hpao) %>%
  summarise(haz = mean(y), .groups = "drop")
p0 <- ggplot(haz_hpao %>% filter(haz > 0 & haz < 1), 
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
p1 <- ggplot(haz_eltv %>% filter(haz > 0 & haz < 1), 
       aes(x = last_ltv_est, y = qlogis(haz))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, formula = "y ~ x") +
  theme_minimal() +
  labs(
    x = "Est. LTV ",
    y = "Est. Logit(Hazard)",
    title = "Est. LTV (LTV at Origination / HPAO)"
  )
p2 <- ggplot(haz_eltv %>% filter(haz > 0 & haz < 1), 
       aes(x = log(last_ltv_est), y = qlogis(haz))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, formula = "y ~ x") +
  theme_minimal() +
  labs(
    x = "Log Est. LTV ",
    y = "Est. Logit(Hazard)",
    title = "Log Est. LTV"
  )
p3 <- ggplot(haz_eltv %>% filter(haz > 0 & haz < 1), 
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

full_df$last_vtl_est <- 1/full_df$last_ltv_est

ggsave("figures/hpao.pdf", plot = p0, width = 6, height = 6)
ggsave("figures/eltv.pdf", plot = p1, width = 6, height = 6)
ggsave("figures/log_eltv.pdf", plot = p2, width = 6, height = 6)
ggsave("figures/inv_eltv.pdf", plot = p3, width = 6, height = 6)

# Look at unpaid principal balance percentage
origupb_plot_df <- full_df %>% 
  group_by(loan_sequence_number) %>% 
  summarise(amt = min(orig_upb), event=sum(y)) %>% 
  ungroup() %>% 
  mutate(event = factor(event, levels=c(0,1), labels=c("y = 0", "y = 1")))

p <- ggplot(origupb_plot_df, aes(x=amt, fill=event)) +
  geom_density(position='identity', alpha=0.3) +
  labs(x="Original Unpaid Balance (Loan Amount)", y="Density", fill="Event") +
  theme_minimal()
ggsave("figures/orig_upb.pdf", plot = p, width = 8, height = 6)
rm(origupb_plot_df)



## SUMMARIZE REMAINING POTENTIAL PREDICTORS

# Remove variables that won't be used
full_df <- full_df %>% 
  select(-loan_age_pct, -orig_upb)

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

# Save categorical summary
saveRDS(catsum_df, file="objects/catsum_df.rds")


# Looking at loan term
haz_term <- full_df %>%
  group_by(orig_loan_term) %>% 
  summarise(haz = mean(y), .groups = 'drop')
p1 <- ggplot(haz_term %>% filter(haz > 0 & haz < 1), 
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
p2 <- ggplot(haz_term %>% filter(haz > 0 & haz < 1), 
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
p3 <- ggplot(haz_term %>% filter(haz > 0 & haz < 1),
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

ggsave("figures/loan_term1.pdf", plot = p1, width = 6, height = 6)
ggsave("figures/loan_term2.pdf", plot = p2, width = 6, height = 6)
ggsave("figures/loan_term3.pdf", plot = p3, width = 6, height = 6)


# Looking at credit score
haz_credit <- full_df %>%
  group_by(credit_score) %>%
  summarise(haz = mean(y), .groups = "drop")
p <- ggplot(haz_credit %>% filter(haz > 0 & haz < 1),
       aes(y = qlogis(haz), x = credit_score)) +
  geom_point(alpha = 0.6, position = "identity") +
  geom_smooth(method='lm', se=TRUE, formula='y~x') +
  theme_minimal() +
  labs( x = "Credit Score", y = "Est. Logit(Hazard)",
        title = "FICO Credit Score at Origination"
  )
ggsave("figures/credit_score.pdf", plot = p, width = 6, height = 6)
rm(haz_credit)


# Looking at DTI ratio at origination
haz_dti <- full_df %>%
  group_by(orig_dti_imp) %>%
  summarise(haz = mean(y), .groups = "drop")
p1 <- ggplot(haz_dti %>% filter(haz > 0 & haz < 1),
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
p2 <- ggplot(haz_transf_dti %>% filter(haz > 0 & haz < 1),
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
p3 <- ggplot(haz_dti_miss %>% filter(haz > 0 & haz < 1),
       aes(x = qlogis(haz), fill = factor(dti_missing))) +
  geom_density(alpha = 0.6, position = "identity") + 
  scale_fill_manual(values = c("steelblue", "lightpink"), 
                    name = "DTI Missing", labels = c("0", "1")) +
  theme_minimal() +
  labs(x = "Est. Logit(Hazard)", y = "Kernel Density Estimate",
       title = "Densities of Logit Hazard for Missing vs. Non-Missing DTI")
p4 <- ggplot(haz_dti_miss %>% filter(haz > 0 & haz < 1),
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

ggsave("figures/dti1.pdf", plot = p1, width = 6, height = 6)
ggsave("figures/dti2.pdf", plot = p2, width = 6, height = 6)
ggsave("figures/dti3.pdf", plot = p3, width = 6, height = 6)
ggsave("figures/dti4.pdf", plot = p4, width = 6, height = 6)


dti_sum <- full_df %>%
  group_by(dti_missing) %>%
  summarise(haz = mean(y), sd = sqrt(haz*(1-haz)), n = n(), 
            .groups = "drop") 
saveRDS(dti_sum, file = "objects/dti_sum.rds")

# Create rare-event-aware sub-sampling function
num_events <- full_df %>% filter(y == 1) %>% 
  pull(loan_sequence_number) %>% unique() %>% length()

num_all <- full_df %>% 
  pull(loan_sequence_number) %>% unique() %>% length()

(p_rec <- num_events/num_all)

train_sample <- function(df=full_df, p=p_rec) {
  events_df <- df %>% filter(y == 1)
  event_ids <- events_df %>% pull(loan_sequence_number) %>% unique()
  events_df <- df %>% filter(loan_sequence_number %in% event_ids)
  nonevents_df <- df %>% filter(!(loan_sequence_number %in% event_ids))
  nonevents_df <- nonevents_df %>% slice_sample(prop=p)
  return( bind_rows(events_df, nonevents_df) )
}

set.seed(10) # reproducibility
sample_df <- train_sample(p=0.06)


# Model with DTI linear and symmetric about 15
symlin_dti <- glm(y ~ transf_dti, family=binomial, 
                  data=sample_df, x = FALSE, y = FALSE, model = FALSE)

# Model with DTI quadratic and symmetric about 15
symquad_dti <- glm(y ~ transf_dti + I(transf_dti^2), family=binomial, 
                   data=sample_df, x = FALSE, y = FALSE, model = FALSE)

aic_df <- AIC(symlin_dti, symquad_dti)
quad_coef <- symquad_dti$coefficients[['I(transf_dti^2)']]

saveRDS(aic_df, file="objects/aic_df.rds")
saveRDS(quad_coef, file="objects/quad_coef.rds")

rm(symquad_dti, symlin_dti)



## DEALING WITH MULTICOLLINEARITY

predictor_names <- c(
  "loan_age" , "last_vix", "cr_spread", "last_unrate_chg_pos", "last_rate_cut", 
  "last_infl_yoy_low", "last_vtl_est", "deliq_num", "loan_purpose", 
  "occupancy_status", "region", "long_term", "credit_score", "dti_missing", 
  "transf_dti" 
)

# Save space by only selecting index variables or predictors that we plan to use
full_df <- full_df %>% select(loan_sequence_number, date, y, 
                              all_of(predictor_names))

num_df <- full_df %>% select_if(is.numeric) %>% select(-y)

# Correlation matrix heat map for all continuous variables
cor_num <- cor(num_df)
pdf("figures/num_pred_corr.pdf", width = 10, height = 10)
corrplot(cor_num, method="color", tl.col="black", tl.cex=0.9, 
         number.cex=0.9, addCoef.col="black", tl.srt=45, number.font=3)
dev.off()

# Check VIF for each variable
sample_fit <- glm(reformulate(predictor_names, response="y"), 
                  data = sample_df, family = binomial)

# Save console output to text file
output_text <- capture.output({
  print(check_collinearity(sample_fit))
  print(summary(sample_fit))
})
writeLines(output_text, "objects/vif_sum.txt")

predictor_names <- predictor_names[predictor_names != "last_rate_cut"]

sample_fit <- glm(reformulate(predictor_names, response="y"), 
                  data = sample_df, family = binomial)

sample_fit3 <- glm(reformulate(setdiff(predictor_names, "loan_age"), 
                               response="y"), 
                   data = sample_df, family = binomial)
aic_df3 <- AIC(sample_fit, sample_fit3)
saveRDS(aic_df3, file="objects/aic_df3.rds")

rm(sample_fit3)

sample_fit <- glm(reformulate(predictor_names, response="y"), 
                  data = sample_df, family = binomial)

sample_fit4 <- glm(reformulate(c(setdiff(predictor_names, "loan_age"), 
                                 "ns(loan_age, df=2)"), response="y"), 
                   data = sample_df, family = binomial)
sample_fit5 <- glm(reformulate(c(setdiff(predictor_names, "loan_age"), 
                                 "ns(loan_age, df=3)"), response="y"), 
                   data = sample_df, family = binomial)
sample_fit6 <- glm(reformulate(c(setdiff(predictor_names, "loan_age"), 
                                 "ns(loan_age, df=4)"), response="y"), 
                   data = sample_df, family = binomial)

aic_df46 <- AIC(sample_fit, sample_fit4, sample_fit5, sample_fit6) %>% 
  arrange(AIC)
saveRDS(aic_df46, file="objects/aic_df46.rds")
rm(sample_fit4, sample_fit5, sample_fit6)

predictor_names <- predictor_names[predictor_names != "loan_age"]
predictor_names <- c(predictor_names, "ns(loan_age, df=2)")

sample_fit <- glm(reformulate(predictor_names, response="y"), 
                  data = sample_df, family = binomial)

sample_fit8 <- glm(reformulate(c(predictor_names, 
                                 "loan_purpose * occupancy_status"), 
                               response="y"), 
                   data = sample_df, family = binomial)
aic_df8 <- AIC(sample_fit, sample_fit8)
saveRDS(aic_df8, file="objects/aic_df8.rds")
rm(sample_fit8)

sample_fit <- glm(reformulate(predictor_names, response="y"), 
                  data = sample_df, family = binomial)
sample_fit9 <- glm(reformulate(c(predictor_names, "last_vix * cr_spread"), 
                               response="y"), 
                   data = sample_df, family = binomial)
aic_df9 <- AIC(sample_fit, sample_fit9)
saveRDS(aic_df9, file="objects/aic_df9.rds")
rm(sample_fit, sample_fit9)

gc()


# Save updated full_df
saveRDS(full_df, file="objects/full_df.rds")


# Save vector of predictor names
saveRDS(predictor_names, file="objects/predictor_names.rdf")

rm(list = ls())
gc()


