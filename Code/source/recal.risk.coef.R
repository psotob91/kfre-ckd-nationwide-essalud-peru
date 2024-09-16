# Funcion que recalibra coeficientes usando Cox
recal.risk.coef <- function(data, horizon) {
  
  fc.aj.risk.basal <- rep(NA, max(data$.imp))
  fc.aj.coef <- rep(NA, max(data$.imp))
  imputations <- rep(NA, max(data$.imp))
  
  for (i in 1:max(data$.imp)) {
    
    datos_filter <- data |> 
      filter(.imp == i) 
    
    datos_filter <- datos_filter |> 
      mutate(beta.sum = kfre_pi(datos_filter))
    
    # Calculo del factor de ajuste a los coeficientes
    fit <- coxph(Surv(time5y, eventb5y) ~ beta.sum, data = datos_filter, 
                 x = TRUE, y = TRUE)
    
    datos_filter <- datos_filter |> 
      mutate(beta.sum2 = fit$coefficients * beta.sum)
    
    # Recalculo de la supervivencia basal
    fit2 <- coxph(Surv(time5y, eventb5y) ~ offset(beta.sum2), 
                  data = datos_filter, 
                  x = T, 
                  y = T)
    
    p2 <- predictSurvProb(fit2, 
                          newdata = data.frame(beta.sum2 = 0), 
                          times = horizon, 
                          type = "survival", 
                          confint = TRUE, 
                          se = TRUE)
    
    fc.aj.risk.basal[i] <- p2
    fc.aj.coef[i] <- fit$coefficients
    imputations[i] <- i
  }
  
  recal_df_imp <- data.frame(
    year = horizon, 
    .imp = imputations, 
    st0_imp = fc.aj.risk.basal, 
    fc_coef_imp = fc.aj.coef
  )
  
  return(list(recal_df_imp = recal_df_imp, 
              st0_pool = mean(fc.aj.risk.basal), 
              fc_coef_pool = mean(fc.aj.coef)))
}
