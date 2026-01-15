############################################
# Final Training, Prediction, & Evaluation #
############################################

setwd("C:/Projects-Code-Certificates/Credit-Risk-Mortgages")

full_df <- readRDS("objects/full_df.rds")
predictor_names <- readRDS("objects/predictor_names.rds")

# Load libraries
library(tidyverse) # SQL-like data manipulation and more (e.g. dplyr)
library(ggplot2) # highly customizable visualizations
library(fredr) # FRED API
library(corrplot) # correlation matrix heat maps
library(performance) # VIF calculations and visualizations
library(knitr) # polished display of tables
library(glmnet) # regularized GLMs
library(survival) # for KM estimator
library(survival) # Kaplan-Meier esti
library(reshape2) # data.frame <-> matrix conversionsmator
library(splines) # natural splines
library(sandwich) # cluster-robust covariance matrix estimator
library(yardstick) # ROC plot and AUC calculation

library(scales) # for clean percentage formatting on plots


## TRAINING

all_dates <- full_df %>% pull(date) %>% unique()
train_prop <- 0.7
n <- length(all_dates)
train_n <- floor(n * train_prop)

train_dates <- all_dates[1:train_n]
test_dates <- all_dates[(train_n+1):n]

train_df <- full_df %>% 
  filter(date %in% train_dates)
test_df <- full_df %>% 
  filter(date %in% test_dates)

train_fit <- glm(reformulate(predictor_names, response="y"), 
                 data = train_df, family = binomial, 
                 x=FALSE, y=FALSE, model=FALSE)
train_sum <- summary(train_fit)
train_coefs <- coef(train_fit)

saveRDS(train_sum, file="objects/train_sum.rds")
saveRDS(train_coefs, file="objects/train_coefs.rds")



## INTERPRETATION

x_df0 <- data.frame(
  last_vix = 17, 
  cr_spread = 3, 
  last_unrate_chg_pos = 0.3, 
  last_infl_yoy_low = "0", 
  last_vtl_est = 0.4, 
  deliq_num = "0", 
  region = "South", 
  long_term = "<25y", 
  credit_score = 750, 
  dti_missing = FALSE, 
  transf_dti = 5, 
  loan_age = 4, 
  loan_purpose = "P", 
  occupancy_status = "P" 
)
prob_est0 <- predict(train_fit, newdata=x_df0, type="response")

x_df1 <- x_df0 %>% 
  mutate(deliq_num = "1")
prob_est1 <- predict(train_fit, newdata=x_df1, type="response")

x_df2 <- x_df1 %>% 
  mutate(deliq_num = "2")
prob_est2 <- predict(train_fit, newdata=x_df2, type="response")

prob_interp_df <- data.frame(
  deliq = c(0,1,2), 
  pd = c(prob_est0, prob_est1, prob_est2)
) %>% 
  mutate(
    rel_increase = pd / lag(pd, 1) - 1
  )
saveRDS(prob_interp_df, file="objects/prob_interp_df.rds")


get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

new_df <- data.frame(
  last_vix = median(train_df$last_vix), 
  cr_spread = median(train_df$cr_spread), 
  last_unrate_chg_pos = median(train_df$last_unrate_chg_pos), 
  last_infl_yoy_low = get_mode(train_df$last_infl_yoy_low), 
  last_vtl_est = median(train_df$last_vtl_est), 
  deliq_num = get_mode(train_df$deliq_num), 
  region = get_mode(train_df$region), 
  long_term = get_mode(train_df$long_term), 
  credit_score = median(train_df$credit_score), 
  dti_missing = FALSE, 
  transf_dti = median(train_df$transf_dti), 
  loan_age = seq(0, 120, by=1), 
  loan_purpose = get_mode(train_df$loan_purpose), 
  occupancy_status = get_mode(train_df$occupancy_status) 
)

all_indicators <- names(train_fit$xlevels)
for (col in all_indicators) {
  if (col %in% names(new_df)) {
    new_df[[col]] <- factor(new_df[[col]], levels = train_fit$xlevels[[col]])
  }
}

# Estimate cluster-robust covariance matrix
vcov_clustered <- vcovCL(train_fit, cluster = ~loan_sequence_number)

pred_link <- predict(train_fit, newdata = new_df, type = "link")

x_new <- model.matrix(delete.response(terms(train_fit)), data=new_df)
se_link <- sqrt(diag(x_new %*% vcov_clustered %*% t(x_new)))

age_pred_df <- data.frame(
  loan_age = new_df$loan_age,
  fit = pred_link,
  se = se_link
) %>%
  mutate(
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )

# map everything to probability scale (apply expit function)
age_pred_df <- age_pred_df %>%
  mutate(
    prob_fit = plogis(fit),
    prob_lower = plogis(lower),
    prob_upper = plogis(upper)
  )

p <- ggplot(age_pred_df, aes(x = loan_age, y = prob_fit)) +
  geom_ribbon(aes(ymin = prob_lower, ymax = prob_upper), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", linewidth = 1) +
  theme_minimal() +
  labs(
    x = "Loan age (months)",
    y = "Predicted 1-month PD",
    title = "Partial Dependence of Loan Age on PD", 
    subtitle = "Standard Errors clustered by Loan ID"
  )
ggsave("figures/age_pred.pdf", plot = p, width = 10, height = 10)


# Save selected CRSE for confidence interval calculation
se <- sqrt(diag(vcov_clustered)['credit_score']) %>% as.numeric() # std. error
est <- 10*train_coefs['credit_score'] %>% as.numeric() # estimate
z <- qnorm(1 - 0.99 / 2) # std. Normal quantile for 99% confidence interval
ci <- est + c(-1, 1) * z * se 
ci <- exp(ci) - 1 # 99% confidence interval (relative growth in odds scale)
saveRDS(ci, file="objects/credit_score_rel_ci.rds")



# Compare CRSE with regular MLE-based standard errors
vcov_reg <- train_sum$cov.unscaled
se_reg <-  sqrt(diag(vcov_reg))
se_clust <-  sqrt(diag(vcov_clustered))
se_growth <- se_clust / se_reg - 1

growth_df <- tibble(
  Predictor = names(se_growth),
  SE_Growth = as.numeric(se_growth),
  Col = "SE"
)
growth_df <- growth_df %>% arrange(desc(SE_Growth))

p <- ggplot(growth_df, aes(x = Col, y = Predictor, fill = SE_Growth)) +
  geom_tile() +
  scale_fill_viridis_c(
    option = "inferno", 
    direction = -1, 
    breaks = c(-0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5),
    labels = scales::percent_format(accuracy = 1),
    name = "SE Growth\n(CR / Reg − 1)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Clustered vs. Regular Std. Errors",
    subtitle = "Growth = Clustered / Regular − 1",
    y = "Predictor"
  )
ggsave("figures/se_growth.pdf", plot = p, width = 6, height = 10)



## PREDICTION EVALUATION

p_hat <- predict(train_fit, newdata=test_df, type="response")
y_test <- test_df$y

results_df <- data.frame(
  truth = factor(y_test, levels = c("1", "0")), 
  prob  = p_hat
)
auc_score <- results_df %>% 
  roc_auc(truth, prob)
p <- results_df %>%
  roc_curve(truth, prob) %>%
  autoplot() +
  labs(title = paste("ROC Curve ( AUC =", round(auc_score$.estimate, 4), ")"))
ggsave("figures/roc_curve.pdf", plot = p, width = 5, height = 5)


metrics_at_threshold <- function(t, p, y) {
  yhat <- as.integer(p >= t)
  TP <- sum(yhat == 1 & y == 1)
  FP <- sum(yhat == 1 & y == 0)
  FN <- sum(yhat == 0 & y == 1)
  TN <- sum(yhat == 0 & y == 0)
  
  precision <- ifelse(test = (TP + FP) == 0, 
                      yes = NA, 
                      no = TP / (TP + FP))
  recall <- ifelse(test = (TP + FN) == 0, 
                   yes = NA, 
                   no = TP / (TP + FN))
  f1 <- ifelse(test = is.na(precision) | is.na(recall) | (precision + recall) == 0, 
               yes = NA, 
               no = 2 * precision * recall / (precision + recall))
  
  list(threshold = t, TP = TP, FP = FP, FN = FN, TN = TN, 
       precision = precision, recall = recall, f1 = f1)
}

t_event_rate <- mean(train_df$y) # using empirical event rate from training set
m_event <- metrics_at_threshold(t_event_rate, p_hat, y_test)

conf_mat <- matrix(
  c(m_event$TN, m_event$FN, m_event$FP, m_event$TP), 
  nrow = 2, 
  dimnames = list(Predicted = c("No Default (0)", "Default (1)"), 
                  Actual = c("No Default (0)", "Default (1)"))
)
saveRDS(conf_mat, file="objects/conf_mat.rds")

metrics <- data.frame(
  Sensitivity = m_event$recall, 
  Precision = m_event$precision, 
  F1 = m_event$f1
)
saveRDS(metrics, file="objects/metrics.rds")


# Plot metrics vs. threshold
thr_grid <- unique(
  c(seq(0, 0.1, length.out = 500), 
    seq(0.1, 0.5, length.out = 500))
)
curve_list <- lapply(thr_grid, function(t) {
  m <- metrics_at_threshold(t, p_hat, y_test)
  return(
    data.frame(
      threshold = m$threshold,
      Precision = m$precision,
      Recall    = m$recall,
      F1        = m$f1)
  )
})
gc()
curve_df <- do.call(rbind, curve_list)
curve_long <- curve_df %>%
  pivot_longer(cols = c("Precision", "Recall", "F1"),
               names_to = "Metric", values_to = "Value")
plot_metrics <- function(df) {
  ggplot(df, aes(x = threshold, y = Value, color = Metric)) +
    geom_line(linewidth = 2, alpha = 0.7, na.rm = TRUE) +
    geom_vline(xintercept = t_event_rate, linetype = "dashed", linewidth = 0.6) +
    theme_minimal() +
    labs(
      title = "Classification Metrics vs Threshold",
      subtitle = "Dashed line = empirical event-rate threshold",
      x = "Threshold",
      y = "Metric value",
      color = ""
    )
}
p <- plot_metrics(curve_long)
ggsave("figures/threshold_metrics.pdf", plot = p, width = 7, height = 5)

curve_long_zoom <- curve_long %>%
  filter(threshold < 0.0015)
p <- plot_metrics(curve_long_zoom)
ggsave("figures/threshold_metrics_zoom.pdf", plot = p, width = 7, height = 5)


# Sort by predicted probabilities
ord <- order(p_hat)
p_ord <- p_hat[ord]
y_ord <- y_test[ord]

# Cumulative distributions
cdf_event <- cumsum(y_ord) / sum(y_ord)
cdf_nonevent <- cumsum(1 - y_ord) / sum(1 - y_ord)

# KS statistic and cutoff
diff_cdf <- abs(cdf_event - cdf_nonevent)
ks_stat <- max(diff_cdf)
ks_idx <- which(diff_cdf == ks_stat)
ks_cut <- p_ord[ks_idx]

cdf_df <- tibble(
  score = p_ord,
  event = cdf_event,
  nonevent = cdf_nonevent
)
cdf_long <- cdf_df %>%
  pivot_longer(cols = c(event, nonevent),
               names_to = "Group",
               values_to = "CDF")

p <- ggplot(cdf_long, aes(x = score, y = CDF, color = Group)) +
  geom_line(linewidth = 2, alpha=0.8) +
  geom_vline(xintercept = ks_cut,
             linetype = "dashed",
             linewidth = 0.8,
             color = "black") +
  annotate("text", x = ks_cut, y = 0.5,
           label = paste0("KS = ", round(ks_stat, 3)),
           hjust = -0.1, size = 4) +
  scale_color_manual(
    values = c(event = "firebrick", nonevent = "steelblue"),
    labels = c("Default", "Non-default")
  ) +
  theme_minimal() +
  labs(x = "Predicted PD", y = "Empirical CDF", color = "",
       title = "KS Plot: Empirical CDFs of Predicted PD",
       subtitle = paste("Maximum separation at PD =", signif(ks_cut, 3))
  )
ggsave("figures/ks_plot.pdf", plot = p, width = 5, height = 5)


# Calibration plot
p_fit <- train_fit$fitted.values
y_train <- train_df$y

cal_df <- data.frame(p = p_fit, y = y_train) %>% 
  arrange(p) %>% 
  mutate(event_cs = cumsum(y), 
         bin = pmax(1, ceiling(event_cs / 200))) %>% 
  group_by(bin) %>%
  summarise(
    p_mean = mean(p),
    y_rate = mean(y),
    .groups = "drop"
  )

x_axis = c(min(cal_df$p_mean), max(cal_df$p_mean))
y_axis = c(min(cal_df$y_rate), max(cal_df$y_rate))

p <- ggplot(cal_df, aes(x = p_mean, y = y_rate)) +
  geom_point(size=2, alpha = 0.7, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = 2, alpha = 0.5) +
  theme_minimal() +
  labs(x = "Mean predicted PD (bin)", y = "Observed default rate (bin)",
       title = "Calibration Plot")
ggsave("figures/cal_plot.pdf", plot = p, width = 6, height = 6)



rm(list = ls())
gc()



