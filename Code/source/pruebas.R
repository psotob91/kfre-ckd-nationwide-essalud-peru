library(tidyverse)
datos <- imp.datosA |> filter(.imp == 1)

horizon <- 2
primary_event <- 1



score_vdata <- Score(
  list("csh_validation" = as.matrix(datos$risk2y)),
  formula = Hist(time, eventd) ~ 1,
  cens.model = "km",
  data = datos,
  conf.int = TRUE,
  times = horizon,
  metrics = c("auc", "Brier"),
  cause = primary_event,
  plots = "calibration"
)

auc_brier(datos, 5, 1, "risk2y")

performance_measures(datos, 2, 1, "risk2y")

tic()
score_vdata <- Score(
  list("csh_validation" = pred),
  formula = Hist(time5y, eventd5y) ~ 1,
  cens.model = "km",
  data = datos,
  conf.int = TRUE,
  times = horizon,
  summary = c("ipa"),
  cause = primary_event,
  plots = "calibration"
)
toc()

tic()
pseudovalues1 <- pseudoci(time = datos$time, event = datos$eventd, tmax = 2)
toc()

tic()
f <- prodlim(Hist(time,eventd) ~ 1, data = datos) 
jackknife(f, times = horizon, cause = 1)
toc()