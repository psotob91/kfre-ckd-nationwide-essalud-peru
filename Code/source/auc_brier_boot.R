auc_brier_boot <- function(split, horizon, primary_event, pred, ...) {
  
  score_vdata <- Score(
    list("csh_validation" = as.matrix(analysis(split)[[pred]])),
    formula = Hist(time, eventd) ~ 1,
    cens.model = "km",
    data = analysis(split),
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
