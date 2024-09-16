# AUC
auc_brier <- function(data, horizon, primary_event, pred) {
  
  score_vdata <- Score(
    list("csh_validation" = as.matrix(data[[pred]])),
    formula = Hist(time, eventd) ~ 1,
    cens.model = "km",
    data = data,
    conf.int = TRUE,
    times = horizon,
    metrics = c("auc", "Brier"),
    cause = primary_event,
    plots = "calibration"
  )
  
  return(tibble(
    term = c("AUC", "Brier"), 
    estimate = c(score_vdata[["AUC"]][["score"]][["AUC"]], 
                 score_vdata[["Brier"]][["score"]][["Brier"]][[2]]), 
    se = c(score_vdata[["AUC"]][["score"]][["se"]], 
           score_vdata[["Brier"]][["score"]][["se"]][[2]]), 
    var = c(score_vdata[["AUC"]][["score"]][["se"]] ^ 2, 
            score_vdata[["Brier"]][["score"]][["se"]][[2]] ^ 2)
  ))
}
