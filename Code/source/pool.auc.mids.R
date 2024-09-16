## Pooled de datos imputados
pool.auc.mids <- function(object, alpha = .05, horizon, primary_event, ...) {
  m <- length(object$analyses$se)
  
  logit <- function(x) log(x / (1-x))
  inv_logit <- function(x) {1/(1+exp(-x))}  
  
  # Estimates from individual studies
  ind <- list(auc = data.frame(est = object$analyses$estimate,
                               se = object$analyses$se,
                               ci.lb = object$analyses$ci.lb,
                               ci.ub = object$analyses$ci.ub))
  ind$logit.auc = data.frame(est = logit(ind$auc$est),
                             se = ind$auc$se / (ind$auc$est * (1 - ind$auc$est)),
                             ci.lb = logit(ind$auc$ci.lb),
                             ci.ub = logit(ind$auc$ci.ub))
  
  # Pooled across imputed data sets
  logit.auc <- list()
  logit.auc$estimate <- mean(ind$logit.auc$est)
  logit.auc$within <- mean(ind$logit.auc$se^2)
  logit.auc$between <- (1 + (1/m)) * var(ind$logit.auc$est)
  logit.auc$var <- logit.auc$within + logit.auc$between
  logit.auc$se <- sqrt(logit.auc$var)
  logit.auc$ci.lb <- logit.auc$est + qnorm(alpha/2)     * logit.auc$se
  logit.auc$ci.ub <- logit.auc$est + qnorm(1 - alpha/2) * logit.auc$se
  logit.auc$m <- m
  
  auc <- list()
  auc$estimate <- inv_logit(logit.auc$est)
  auc$ci.lb <- inv_logit(logit.auc$ci.lb)
  auc$ci.ub <- inv_logit(logit.auc$ci.ub)
  auc$m <- m
  
  return(list(individual = ind, 
              pooled = list(logit.auc = as.data.frame(logit.auc), 
                            auc = as.data.frame(auc))))
}
