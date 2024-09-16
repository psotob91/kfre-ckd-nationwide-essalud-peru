performance_measures <- function(data, horizon, primary_event, pred, ...) {
  
  ndata <- nrow(data)
  
  ## OE  ratio
  
  obj <- summary(survfit(Surv(time, eventdf) ~ 1, 
                         data = data), 
                 times = horizon)
  
  avg_obs <- obj$pstate[, primary_event + 1]
  
  se_avg_obs <- obj$std.err[, primary_event + 1]
  
  avg_pred <- mean(data[[pred]], na.rm = TRUE)
  
  ln_oe_ratio <- log(avg_obs) - log(avg_pred)
  #Fuente: anexo de https://www.bmj.com/content/356/bmj.i6460/related
  # anexo: https://www.bmj.com/content/bmj/suppl/2017/01/05/bmj.i6460.DC1/debt033157.ww_default.pdf
  
  se_ln_oe_ratio <- (1 / avg_obs ) * se_avg_obs
  
  oe_diff <- avg_obs - avg_pred
  
  se_oe_diff <- se_avg_obs # Por metodo delta se demustra
  
  ## Calibration Intercept
  
  score_vdata <- Score(
    list("csh_validation" = data[[pred]]),
    formula = Hist(time, eventd) ~ 1,
    cens.model = "km",
    data = data,
    conf.int = FALSE,
    times = horizon,
    summary = c("ipa"),
    cause = primary_event,
    plots = "calibration"
  )
  
  # First compute riskRegression::Score()
  
  pseudos <- tibble(score_vdata$Calibration$plotframe)
  
  # Use pseudo-observations calculated by Score() (can alternatively use pseudo::pseudoci)
  
  
  # Note:
  # - 'pseudos' is the data.frame with ACTUAL pseudo-observations, not the smoothed ones
  # - Column ID is not the id in vdata; it is just a number assigned to each row of
  # the original validation data sorted by time and event indicator
  
  pseudos$cll_pred <- log(-log(1 - pseudos$risk)) # add the cloglog risk ests 
  
  # Fit model for calibration intercept
  fit_cal_int <- geese(
    pseudovalue ~ offset(cll_pred),
    data = pseudos,
    id = ID,
    scale.fix = TRUE,
    family = gaussian,
    mean.link = "cloglog",
    corstr = "independence",
    jack = TRUE
  )
  
  # Fit model for calibration slope
  fit_cal_slope <- geese(
    pseudovalue ~ offset(cll_pred) + cll_pred,
    data = pseudos,
    id = ID,
    scale.fix = TRUE,
    family = gaussian,
    mean.link = "cloglog",
    corstr = "independence",
    jack = TRUE
  )
  
  ## AUC y Brier Score
  score_vdata <- Score(
    list("csh_validation" = as.matrix(data[[pred]])),
    formula = Hist(time, eventd) ~ 1,
    cens.model = "km",
    data = data,
    conf.int = TRUE,
    times = horizon,
    metrics = c("auc", "Brier"),
    cause = primary_event,
    plots = "calibration"
  )
  
  #Fuente: anexo de https://www.bmj.com/content/356/bmj.i6460/related
  # anexo: https://www.bmj.com/content/bmj/suppl/2017/01/05/bmj.i6460.DC1/debt033157.ww_default.pdf
  auc <- score_vdata[["AUC"]][["score"]][["AUC"]]
  logit_auc <- log(auc / (1 - auc))
  
  se_auc <- score_vdata[["AUC"]][["score"]][["se"]]
  se_logit_auc <- se_auc / (auc * (1 - auc)) 
  
  return(
    tibble(
      term = c("Average predicted risk", 
               "Overall observerd risk", 
               "Log OE ratio", 
               "OE difference", 
               "Calibration Intercept", 
               "Calibration Slope", 
               "Logit AUC", 
               "Brier" 
      ), 
      estimate_imp = c(avg_pred, avg_obs, ln_oe_ratio, oe_diff, 
                   summary(fit_cal_int)$mean$estimate, 
                   1 + summary(fit_cal_slope)$mean["cll_pred", "estimate"], 
                   logit_auc, 
                   score_vdata[["Brier"]][["score"]][["Brier"]][[2]] 
      ), 
      se = c(NA, se_avg_obs, se_ln_oe_ratio, se_oe_diff, 
             summary(fit_cal_int)$mean$san.se, 
             summary(fit_cal_slope)$mean["cll_pred", "san.se"], 
             se_logit_auc, 
             score_vdata[["Brier"]][["score"]][["se"]][[2]]
      ), 
      n = rep(ndata, 8)
    )
  )
}