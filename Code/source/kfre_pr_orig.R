# Funcion que calcula probabilidad predicha a 2 o 5 years
kfre_pr_orig <- function(data, horizon) {
  if (horizon == 2) {
    1 - 0.9750 ^ exp(kfre_pi(data))
  } else if (horizon == 5) {
    1 - 0.9240 ^ exp(kfre_pi(data))
  }
}