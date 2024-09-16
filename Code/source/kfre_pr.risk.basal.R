# Funcion que calcula probabilidad predicha a 2 o 5 years de recal.risk.basal
# (de riesgo basal calculado por Cox)
kfre_pr.risk.basal <- function(data, horizon) {
  if (horizon == 2) {
    1 - recal.risk.basal(data, 2)$fc.aj.risk.basal.mean ^ exp(kfre_pi(data))
  } else if (horizon == 5) {
    1 - recal.risk.basal(data, 5)$fc.aj.risk.basal.mean ^ exp(kfre_pi(data))
  }
}