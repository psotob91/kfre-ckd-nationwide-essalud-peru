# Funcion que calcula probabilidad predicha a 2 o 5 years de recal.risk.coef.csh
# (de recalibracion de coeficientes usando Competing Risk Model)
kfre_pr.risk.coef.csh <- function(data, horizon) {
  if (horizon == 2) {
    1 - recal.risk.coef.csh(data, 2)$fc.aj.risk.basal.csh ^ exp(kfre_pi.coef.csh(data))
  } else if (horizon == 5) {
    1 - recal.risk.coef.csh(data, 5)$fc.aj.risk.basal.csh ^ exp(kfre_pi.coef.csh(data))
  }
}