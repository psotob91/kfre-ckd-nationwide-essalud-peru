tidy_performance_stack <- function(data_imp, horizon, primary_event, pred, seed, 
                                   n_cores, ...) {

  pckges <- c("riskRegression", "survival", "splines", "cmprsk", "tidyverse", 
              "prodlim", "geepack")
  
  future::plan("multisession", workers = n_cores)
  
  resultados <- data_imp |> 
    group_by(.imp) |> 
    summarise(performance = list(
      future_map(
        .x = list(pick(everything())), 
        .f = ~ performance_measures(
          data = .x,
          horizon = horizon,  
          primary_event = primary_event,  
          pred = pred
        ), 
        .options = furrr_options(
          seed = seed, 
          packages = pckges
        ), 
        .progress = TRUE
      )
    ), .groups = "drop") 
  
  resultados <- resultados |> 
    mutate(performance = map(performance, `[[`, 1)) 
  
  combined_results <- bind_rows(resultados$performance, .id = ".imp")  # Combina los tibbles de rendimiento
  
  return(combined_results)
}