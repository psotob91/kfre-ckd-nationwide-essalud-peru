# Funcion que calcula probabilidad predicha a 2 o 5 years
kfre_pr <- function(data, horizon) {
  if (horizon == 2) {
    1 - 0.9832 ^ exp(kfre_pi(data))
  } else if (horizon == 5) {
    1 - 0.9365 ^ exp(kfre_pi(data))
  }
}