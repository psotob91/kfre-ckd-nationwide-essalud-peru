# Intercepto de calibracion
cal_int_slope <- function(data, horizon, primary_event, pred) {
  
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
  
  return(tibble(
    term = c("Calibration Intercept", "Calibration Slope"), 
    estimate = c(summary(fit_cal_int)$mean$estimate, 
                 summary(fit_cal_slope)$mean["cll_pred", "estimate"]),
    se = c(summary(fit_cal_int)$mean$san.se, 
           summary(fit_cal_slope)$mean["cll_pred", "san.se"]),
    var = c(summary(fit_cal_int)$mean$san.se ^ 2, 
            summary(fit_cal_slope)$mean["cll_pred", "san.se"] ^ 2)
    ))
}