# Funcion que calcula indice pronostico
kfre_pi <- function(data) {
  -0.2201 * (data$age / 10 - 7.036) + 0.2467 * (as.integer(data$sex == "Masculino") - 0.5642) - 0.5567 * (data$eGFR_ckdepi / 5 - 7.222) + 0.4510 * (data$log_acr - 5.137)
}