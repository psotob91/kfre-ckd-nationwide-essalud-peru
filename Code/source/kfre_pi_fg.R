# Funcion que calcula indice pronostico
kfre_pi_fg <- function(data, coefs, means) {
  
  data$coef_age_cs * (data$age / 10 - data$means_age_cs) + 
    data$coef_male_c * (as.integer(data$sex == "Masculino") - data$means_male_c) + 
    data$coef_eGFR_cs * (data$eGFR_ckdepi / 5 - data$means_eGFR_cs) + 
    data$coef_log_acr_c * (data$log_acr - data$means_log_acr_c)
  
}