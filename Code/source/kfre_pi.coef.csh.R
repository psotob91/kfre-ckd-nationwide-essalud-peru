# Funcion que calcula indice pronostico de recal.risk.coef,csh
# (de recalibracion de coeficientes usando Competing Risk Model)
kfre_pi.coef.csh <- function(data) {
  recal.risk.coef.csh(data, 5)$fc.aj.coef.csh * (-0.2201 * (data$age / 10 - 7.036) + 0.2467 * (as.integer(data$sex == "Masculino") - 0.5642) - 0.5567 * (data$eGFR_ckdepi / 5 - 7.222) + 0.4510 * (log(data$acr) - 5.137))
}