haz_age <- full_df %>%
  group_by(loan_age) %>% 
  summarise(
    events  = sum(y), # number of loans that defaulted next month
    at_risk = n(),  # number of loans alive during this month
    hazard  = events / at_risk, # empirical discrete hazard
    .groups = "drop"
  )


ggplot(haz_age %>% filter(hazard > 0 & hazard < 1),
       aes(loan_age, qlogis(hazard))) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess") +
  theme_minimal() +
  labs(y = "Est. Logit(Hazard)", x = "Loan Age in Months", 
       title = "Estimated Logit(Hazard) vs. Loan Age")

rm(haz_age)


haz_age_month <- tv_surv_df %>%
  group_by(loan_age, date) %>%
  summarise(
    events  = sum(event),
    at_risk = n(),
    hazard  = events / at_risk,
    .groups = "drop"
  ) %>%
  left_join(macro, by = "date") # merge macro vars by date


macro_vars <- setdiff(names(macro), "date")
for (var_name in macro_vars) {
  vec <- macro[[var_name]]
  p <- ggplot(haz_age_month %>% filter(hazard > 0 & hazard < 1), 
              aes(.data[[var_name]], qlogis(hazard))) +
    geom_point() +
    geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
    theme_minimal() +
    labs(y = "Est. Logit(Hazard)", x = var_name, 
         title = var_name)
  print(p)
}


haz_age_rate <- tv_surv_df %>%
  group_by(loan_age, orig_rate) %>% 
  summarise(
    events  = sum(event),
    at_risk = n(),              # loans alive at start of month
    hazard  = events / at_risk, # empirical discrete hazard
    .groups = "drop"
  )
p <- ggplot(haz_age_rate %>% filter(hazard > 0 & hazard < 1), 
            aes(orig_rate, qlogis(hazard))) +
  geom_point() +
  geom_smooth(se = TRUE, method = "loess", formula = "y~x") +
  theme_minimal() +
  labs(y = "Est. Logit(Hazard)", x = "Interest Rate at Origination", 
       title = "Interest Rate at Origination")
print(p)
rm(haz_age_rate)


haz_age_qtr <- tv_surv_df %>%
  group_by(loan_age, orig_qtr) %>% 
  summarise(
    events  = sum(event),
    at_risk = n(),     
    hazard  = events / at_risk, 
    .groups = "drop"
  )
p <- ggplot(haz_age_qtr %>% filter(hazard > 0 & hazard < 1), 
            aes(orig_qtr, qlogis(hazard))) +
  geom_boxplot() +
  theme_minimal() +
  labs(y = "Est. Logit(Hazard)", x = "Quarter of Year at Origination", 
       title = "Quarter of Year at Origination")
print(p)
rm(haz_age_qtr)



p <- ggplot(haz_age_month %>% filter(hazard > 0 & hazard < 1), 
            aes(date, qlogis(hazard))) +
  geom_point() +
  theme_minimal() +
  labs(y = "Est. Logit(Hazard)", x = "Date", 
       title = "Date  (Calendar Time)")
print(p)
rm(haz_age_month)





####################################################################################
# OLD STUFF ABOUT LOAN AGE (WASN'T SIGNFICANT AFTER ADJUSTING FOR TIME/MACRO CONFOUNDING)
####################################################################################


The most apparent lack of marginal relationship between macro covariates and logit(hazard) seem to be with the lagged HPI log growth YoY and the lagged fed funds rate. The other plots appear to show weak-moderate marginal dependence, with most of them appearing nonlinear (in particular, sigmoidal or reflectionally sigmoidal), although the lagged unemployment rate seems to have a mostly-linear pattern with logit(hazard). 

Looking at the quarter at origination, we see median logit(hazard) values that are roughly constant across the quarters of the year, so this plot suggests very-little-to-no quarterly seasonality.

```
tv_surv_df <- tv_surv_df %>% select(-last_hpi_lg_yoy, -last_fedfunds)

fit_age <- glm(
  event ~ loan_age + I(loan_age^2) +
    orig_rate +
    last_unrate +
    last_infl_lg_yoy +
    last_fedfunds_chg_yoy,
  family = binomial,
  data = tv_surv_df, 
  x=FALSE, y=FALSE, model=FALSE # save some memory
)

lin_age_sum <- summary(fit_age)
print(lin_age_sum)



fit_age_time <- glm(
  event ~ loan_age + I(loan_age^2) + factor(yq) +
    orig_rate +
    last_unrate +
    last_infl_lg_yoy +
    last_fedfunds_chg_yoy,
  family = binomial,
  data = tv_surv_df, 
  x=FALSE, y=FALSE, model=FALSE # save some memory
)

lin_sum <- summary(fit_adj)
print(linear_sum)

age_grid <- expand.grid(
  loan_age = seq(0, 140, by=1),
  yq = unique(tv_surv_df$yq)
)
age_grid <- age_grid %>%
  mutate(
    last_unrate = median(tv_surv_df$last_unrate, na.rm = TRUE),
    last_hpi_lg_yoy = median(tv_surv_df$last_hpi_lg_yoy, na.rm = TRUE),
    last_fedfunds = median(tv_surv_df$last_fedfunds, na.rm = TRUE),
    last_infl_lg_yoy = median(tv_surv_df$last_infl_lg_yoy, na.rm = TRUE)
  )

pred <- predict(fit_adj, newdata = age_grid, se.fit=TRUE, type = "link")

# pdp = partial dependence plot
pdp_age <- age_grid %>%
  group_by(loan_age) %>%
  summarise(
    pred_mean = mean(pred),
    pred_lo = quantile(pred, 0.025),
    pred_hi = quantile(pred, 0.975),
    .groups = "drop"
  )

ggplot(pdp_age, aes(x = loan_age, y = pred_mean)) +
  geom_line(color = "steelblue") +
  geom_ribbon(aes(ymin = pred_lo, ymax = pred_hi), alpha = 0.2) +
  labs(
    x = "Loan Age (months)",
    y = "Logit(Hazard)",
    title = "Average Partial Dependence of Loan Age (over year-quarters)"
  ) +
  theme_minimal()




age_grid$logit_haz <- pred$fit
err <- 1.96*qnorm(1-0.05/2)*pred$se.fit
age_grid$lower <- age_grid$logit_haz - err
age_grid$upper <- age_grid$logit_haz + err

ggplot(age_grid, aes(x = loan_age, y = logit_haz)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper,
        fill = "95% CI"),
    alpha = 0.20, color = NA
  ) +
  geom_line(aes(color = "Model Fit"), linewidth = 1.1) +
  geom_point(aes(color = "Model Fit"), size = 1.5) +
  geom_point(
    data = haz_age,
    aes(x = loan_age, y = qlogis(hazard),
        color = "Empirical"),
    shape = 16, size = 2.0
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Model Fit" = "steelblue",
      "Empirical" = "darkgrey"
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("95% CI" = "steelblue")
  ) +
  theme_minimal() +
  labs(
    x = "Loan Age in Months",
    y = "Logit(Hazard)",
    title = "Macroeconomic-Adj. Linear Effect of Loan Age on Logit(Hazard)"
  ) +
  theme(
    axis.text.x = element_text(size = 6),
    legend.position = "bottom"
  )

cat("Linear Age Effect:", round(fit_adj$coefficients[['loan_age']]*100, 2), "% (age)")

linear_aic <- fit_adj$aic
linear_dev <- fit_adj$deviance

rm(fit_adj) # free up some memory


```





```

# Repeat the same partial dependence plot with squared term

fit_adj2 <- glm(
  event ~ 
    loan_age + I(loan_age^2) +
    orig_rate +
    last_unrate +
    last_infl_lg_yoy +
    last_fedfunds_chg_yoy,
  family = binomial,
  data = tv_surv_df
)

quadratic_sum <- summary(fit_adj2)
print(quadratic_sum)

pred2 <- predict(fit_adj2, newdata = age_grid, se.fit=TRUE, type = "link")

age_grid$logit_haz <- pred2$fit
err2 <- 1.96*qnorm(1-0.05/2)*pred2$se.fit
age_grid$lower <- age_grid$logit_haz - err
age_grid$upper <- age_grid$logit_haz + err


ggplot(age_grid, aes(x = loan_age, y = logit_haz)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper,
        fill = "95% CI"),
    alpha = 0.20, color = NA
  ) +
  geom_line(aes(color = "Model Fit"), linewidth = 1.1) +
  geom_point(aes(color = "Model Fit"), size = 1.5) +
  geom_point(
    data = haz_age,
    aes(x = loan_age, y = qlogis(hazard),
        color = "Empirical"),
    shape = 16, size = 2.0
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Model Fit" = "steelblue",
      "Empirical" = "darkgrey"
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("95% CI" = "steelblue")
  ) +
  theme_minimal() +
  labs(
    x = "Loan Age in Months",
    y = "Logit(Hazard)",
    title = "Macroeconomic-Adj. Quadratic Effect of Loan Age on Logit(Hazard)"
  ) +
  theme(
    axis.text.x = element_text(size = 6),
    legend.position = "bottom"
  )

cat("Quadratic Age Effect:", round(fit_ad2j$coefficients[['loan_age']]*100, 2), "%", 
    "(age) +", round(fit_adj2$coefficients[['I(loan_age^2)']]*100, 2), "%", "(age^2)")

quadratic_aic <- fit_adj2$aic
quadratic_dev <- fit_adj2$deviance



rm(fit_adj2)

cat("AIC for Model Quadratic in Age:", quadratic_aic, 
    "\nAIC for Model Linear in Age:", linear_aic)

# deviance test for H0: linear age model is adequate vs. quadratic age model
1 - pchisq(q=linear_deviance - quadratic_dev, df=1) # reject H0

```

*NOW ADJUST INTERPRETATION BASED ON THIS (PREV PARAGRAPHS BELOW)*
  
  Note: The logistic function is monotone increasing, so logit(hazard) increase iff hazard increase.


*STILL GOTTA MERGE DATA FROM OTHER SFLLD VINTAGES*
  
  
  
  In anticipation of a nonlinear association between age of loan and PD (i.e. a nonlinear term structure)\footnote{Li (2014) discusses modeling nonlinear age effects using polynomial terms or age-bucket dummy variables; see *Residential Mortgage Probability of Default Models and Methods*, Financial Institutions Commission of British Columbia.}, we try a few different functional forms for the loan age component of the model. 

For the first model, we will use a quadratic form for loan age; a linear term plus a squared term.

For the second model, we will use bins (discretize loan age into age group factor). This has the drawback that we lose a lot of information in the raw continuous loan age variable.

For the third model, we will use use natural cubic splines. This has the drawbacks that the model is harder to interpret. 
We will only select this as our final model if it outperforms the other two models enough to justify the reduction in interpretability.

We observe a slowly increasing proportion of defaults in the sample as age increases up to around $16\%$ loan age, at which point we see a huge spike (this is suspected to be around the post-COVID rate hikes era), and after that we see slow growth again with a local peak at around $31\%$ age. The sample proportion declines as age increases past $31\%$, although we do not have as much data for this period, and all observations are eventually right-censored at May 2025. 

This loosely matches the literature describing an increasing PD in early periods of a loan followed by a peak and then a decline in PD as the age of the loan approaches maturity. We are limited by the fact that we only sampled one vintage of loans. Also note that the relationship depicted in the plot above is suspected to be confounded by the effects of the macroeconomic environment during our sample period. This is adjusted for in our model by including macroeconomic covariates. 