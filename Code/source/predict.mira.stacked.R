predict.mira.stacked <- function(fit, data) {
  
  # obtain predictions Q and prediction variance U
  predm <- lapply(getfit(fit), predict, se.fit = TRUE)
  Q <- sapply(predm, `[[`, "fit")
  U <- sapply(predm, `[[`, "se.fit")^2
  dfcom <- predm[[1]]$df
  
  # pool predictions
  pred <- matrix(NA, nrow = nrow(Q), ncol = 3,
                 dimnames = list(NULL, c("obs", "se.obs", "df")))
  
  for(i in 1:nrow(Q)) {
    pi <- pool.scalar(Q[i, ], U[i, ], n = dfcom + 1)
    pred[i, 1] <- pi[["qbar"]]
    pred[i, 2] <- sqrt(pi[["t"]])
    pred[i, 3] <- pi[["df"]]
  }
  
  pred <- as.data.frame(pred)
  
  risk <- data |>
    mice::complete(action = "long", include = FALSE) |>
    group_by(.id) |>
    summarise(risk = mean(risk)) |>
    ungroup()

  df_results <- pred |>
    bind_cols(risk) |>
    mutate(
      ll.obs = pmax(obs - qt(0.975, df) * se.obs, 0),
      ul.obs = pmin(obs + qt(0.975, df) * se.obs, 1)
    )

  return(df_results)
  
}