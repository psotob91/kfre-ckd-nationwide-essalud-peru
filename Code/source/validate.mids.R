# Calculo de cada medida en cada dataset imputado
validate.mids <- function(data, model, measure, medida, alpha = .05, horizon, primary_event, ...) {
  # From mice
  call <- match.call()
  if (!is.mids(data)) {
    stop("The data must have class mids")
  }
  
  # Make sure that these two are functions:
  model   <- match.fun(model)
  measure <- match.fun(measure)
  
  # Here we have two steps instead of one.
  # 1. Predict the outcome
  predicted_values <- lapply(seq_len(data$m), 
                             function(i) model(data = complete(data, i), horizon))
  
  # 2. Estimate performance
  analyses <- sapply(seq_len(data$m), 
                     function(i) measure(
                       data = complete(data, i), 
                       horizon = horizon, 
                       primary_event = primary_event,
                       pred = predicted_values[[i]], ...))
  
  if (medida == "oe_ratio") {
    
    analyses <- as.data.frame(t(analyses)) |> 
      mutate(estimate = unlist(estimate), 
             se = unlist(se), 
             var = unlist(var), 
             avg_obs = unlist(avg_obs), 
             avg_pred = unlist(avg_pred))
    
    analyses$ci.lb <- exp(log(analyses$estimate + qnorm(alpha/2) * analyses$se / analyses$avg_obs))
    analyses$ci.ub <- exp(log(analyses$estimate + qnorm(1-alpha/2) * analyses$se / analyses$avg_obs))
    
  } else {
    
    analyses <- as.data.frame(t(analyses)) |> 
      mutate(estimate = unlist(estimate), 
             se = unlist(se), 
             var = unlist(var))
    
    analyses$ci.lb <- analyses$estimate + qnorm(alpha/2)   * analyses$se
    analyses$ci.ub <- analyses$estimate + qnorm(1-alpha/2) * analyses$se    
    
  } 
  
  # From mice                   
  object <- list(call = call, 
                 call1 = data$call, 
                 nmis = data$nmis, 
                 analyses = analyses)
  oldClass(object) <- c("mira")
  object
}
