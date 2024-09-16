# Funcion que recalcula supervivencia basal usando Cox
recal.risk.basal <- function(data, horizon) {
  
  fc.aj.risk.basal <- rep(NA, max(data$.imp))
  imputations <- rep(NA, max(data$.imp))
  
  data <- data |> 
    mutate(beta.sum = kfre_pi(data))
  
  for (i in 1:max(data$.imp)) {
    
    datos_filter <- data |> 
      filter(.imp == i) 
    
    fit <- coxph(Surv(time5y, eventb5y) ~ offset(beta.sum), data = datos_filter, 
                 x = TRUE, y = TRUE)
    
    p1 <- predictSurvProb(fit, 
                          newdata = data.frame(beta.sum = 0), 
                          times = horizon, 
                          type = "survival", 
                          confint = TRUE, 
                          se = TRUE)
    imputations[i] <- i
    fc.aj.risk.basal[i] <- p1
  
  }
  
  recal_df_imp <- data.frame(
    year = horizon, 
    .imp = imputations, 
    st0_imp = fc.aj.risk.basal, 
    fc_coef_imp = 1
  )
  
  return(list(recal_df_imp = recal_df_imp, 
              st0_pool = mean(fc.aj.risk.basal), 
              fc_coef_pool = 1))
}
