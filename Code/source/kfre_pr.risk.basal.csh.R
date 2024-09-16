# Funcion que calcula probabilidad predicha a 2 o 5 years de recal.risk.basal.csh
# (de riesgo basal calculado por Competing risk model)
kfre_pr.risk.basal.csh <- function(data, horizon) {
  if (horizon == 2) {
    1 - recal.risk.basal.csh(data, 2)$fc.aj.risk.basal.csh ^ exp(kfre_pi(data))
  } else if (horizon == 5) {
    1 - recal.risk.basal.csh(data, 5)$fc.aj.risk.basal.csh ^ exp(kfre_pi(data))
  }
}