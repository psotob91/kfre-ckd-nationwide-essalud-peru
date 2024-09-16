tidy_pool_curve <- function(df_stack, conf = 0.95, est0 = 0) {
  
  alpha <- 1 - conf
  
  m <- length(unique(df_stack$.imp))
  # k <- c(1, 1, 1, 1, 1, 2, 1, 1)
  
  # Combina todos los tibbles en una lista en un único tibble  
  pool_predictions <- df_stack |> 
    group_by(risk) |> 
    summarise(
      estimate = mean(obs, na.rm = TRUE),
      ubar = mean(se ^ 2, na.rm = TRUE),
      b = var(obs, na.rm = TRUE),
      n = mean(n, na.rm = TRUE)
    ) |> 
    ungroup() |> 
    mutate(
      m = m, 
      t = ubar + b + b / m,
      lambda = (b + b / m) / t, # prop of variance attribut to missing data
      riv = (b + b / m) / ubar, # relat increa i var due to nonresp
      k = k, 
      dfold = (m - 1) / (lambda ^ 2),
      dfcom = n - k, 
      dfobs = ((dfcom + 1) / (dfcom + 3)) * dfcom * (1 - lambda), 
      df = if_else(lambda == 0, dfcom, (dfold * dfobs) / (dfold + dfobs)),
      fmi = ((riv + 2) / (df + 3)) / (1 + riv), 
      ll = estimate - qt(1 - alpha / 2, df) * sqrt(t), 
      ul = estimate + qt(1 - alpha / 2, df) * sqrt(t), 
      pval = pf((est0 - estimate) ^ 2 / t, 1, df, lower.tail = FALSE)
    ) |> 
    select(estimate, ll, ul, pval, ubar, b, t, dfcom, df, riv, lambda, fmi, n)
  
  return(pool_predictions)
}
