## Pooled de datos imputados
pool.validate.mids <- function(object, alpha = .05, horizon, primary_event,...) {
  fit <- pool.scalar(Q = object$analyses$estimate, 
                     U = object$analyses$var, 
                     n = nrow(object$analyses))
  
  list("invididual" = data.frame(estimate = fit$qhat, 
                                 var = fit$u, 
                                 se = sqrt(fit$u),
                                 ci.lb = object$analyses$ci.lb,
                                 ci.ub = object$analyses$ci.ub),
       "pooled" = data.frame(estimate = fit$qbar,
                             variance = fit$t,
                             within = fit$ubar,
                             between = fit$b,
                             se = sqrt(fit$t),
                             ci.lb = fit$qbar + qnorm(alpha/2) * sqrt(fit$t),
                             ci.ub = fit$qbar + qnorm(1 - alpha/2) * sqrt(fit$t),
                             df = fit$df,
                             r = fit$r,
                             fmi = fit$fmi,
                             m = fit$m))
}