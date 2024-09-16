# Funcion que calcula probabilidad predicha a 2 o 5 years de recal.risk.coef
# (de recalibracion de coeficientes usando Cox)
kfre_pr.risk.coef <- function(data, horizon) {
  if (horizon == 2) {
    1 - recal.risk.coef(data, 2)$fc.aj.risk.basal ^ exp(kfre_pi.coef(data))
  } else if (horizon == 5) {
    1 - recal.risk.coef(data, 5)$fc.aj.risk.basal ^ exp(kfre_pi.coef(data))
  }
}