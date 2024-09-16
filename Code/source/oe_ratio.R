# Razon O/E
oe_ratio <- function(data, horizon, primary_event, pred) {
  
  data$eventd <- factor(data$eventd)
  
  obj <- summary(survfit(Surv(time, eventd) ~ 1, 
                         data = data), 
                 times = horizon)
  
  avg_obs <- obj$pstate[, primary_event + 1]
  
  se_avg_obs <- obj$std.err[, primary_event + 1]
  
  avg_pred <- mean(data[[pred]], na.rm = TRUE)
  
  oe_ratio <- avg_obs / avg_pred
  
  oe_diff <- avg_obs - avg_pred
  
  return(data.frame(term = c("Average predicted risk", 
                             "Overall observerd risk", 
                             "OE ratio", 
                             "OE difference"), 
                    estimate = c(avg_pred, avg_obs, oe_ratio, oe_diff), 
                    se = c(NA, se_avg_obs, NA, NA), 
                    var = c(NA, se_avg_obs ^ 2, NA, NA)
  ))
}