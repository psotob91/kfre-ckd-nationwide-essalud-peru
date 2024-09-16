# Funcion que recalcula supervivencia basal usando Competing risk model
recal.risk.basal.csh <- function(data, horizon) {
  
  fc.aj.risk.basal <- rep(NA, max(data$.imp))
  imputations <- rep(NA, max(data$.imp))
  
  for (i in 1:max(data$.imp)) {
    
    datos_filter <- data |> 
      filter(.imp == i) 
    
    datos_filter <- datos_filter |> 
      mutate(beta.sum = kfre_pi(datos_filter))  |>
      select(-time) |> 
      dplyr::rename(time = time5y)
    
    fit <- CSC(Hist(time, eventd5y) ~ offset(beta.sum), 
               data = datos_filter,
               cause = 1)
    
    p1 <- 1 - as.vector(predictRisk(fit, 
                                    newdata = data.frame(beta.sum = 0), 
                                    times = horizon, 
                                    cause = 1))
    
    fc.aj.risk.basal[i] <- p1
    imputations[i] <- i
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