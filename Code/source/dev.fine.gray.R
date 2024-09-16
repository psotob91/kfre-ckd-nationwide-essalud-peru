# Función que recalcula supervivencia basal usando Competing risk model
dev.fine.gray <- function(data, n_cores, seed, ...) {
  
  pckges <- c("riskRegression", "survival", "splines", "cmprsk", "tidyverse", 
              "prodlim", "geepack", "tidycmprsk")
  
  # Asegurarse de que los cores están configurados para paralelismo
  plan(multisession, workers = n_cores)  # Ajustar el número de workers según los cores disponibles
  nimp <- max(data$.imp)
  
  data_sum <- data |> 
    summarise(age_mean_scal = mean(age) / 10, 
              male_mean = mean(male), 
              eGFR_mean_scal = mean(eGFR_ckdepi) / 5, 
              log_acr_mean = mean(log_acr), 
              .by = ".imp") |> 
    pivot_longer(cols = age_mean_scal:log_acr_mean, 
                 names_to = "vars", 
                 values_to = "means") |> 
    mutate(term = case_match(vars, 
                             "age_mean_scal" ~ "age_cs", 
                             "male_mean" ~ "male_c", 
                             "eGFR_mean_scal" ~ "eGFR_cs", 
                             "log_acr_mean" ~ "log_acr_c"))
 
  # Calcular los coeficientes para cada imputación
  coef_imp <- future_map(1:nimp, ~{
    
    datos_filter <- data %>% 
      filter(.imp == .x)
    
    n <- nrow(datos_filter)
    
    crr_mod <- tidycmprsk::crr(Surv(time, eventdf) ~ age_cs + 
                                 male_c + 
                                 eGFR_cs + 
                                 log_acr_c, 
                               data = datos_filter)
    
    dat <- datos_filter %>% 
      add_row(time = 2, eventdf = factor("0", levels = c("0", "1")), 
              age_cs = 0, male_c = 0, eGFR_cs = 0, log_acr_c = 0, 
              .before = 1) %>% 
      add_row(time = 5, eventdf = factor("0", levels = c("0", "1")), 
              age_cs = 0, male_c = 0, eGFR_cs = 0, log_acr_c = 0, 
              .before = 1) %>% 
      slice(1:3)
    
    st0_5 <- 1 - predict(crr_mod, times = 5, newdata = dat)[["time 5"]][1]
    st0_2 <- 1 - predict(crr_mod, times = 2, newdata = dat)[["time 2"]][1]
    
    crr_mod[["tidy"]] |> 
      select(term, estimate, std.error) |> 
      add_row(term = "st0_5", estimate = st0_5) |> 
      add_row(term = "st0_2", estimate = st0_2) |> 
      mutate(.imp = .x, 
             n = n) 
  }, 
  .options = furrr_options(seed = seed, 
                           packages = pckges), 
  .progress = TRUE
  )
  
  coef_stack <- bind_rows(coef_imp) |> 
    rename(estimate_imp = estimate) |> 
    left_join(data_sum, by = c(".imp", "term"))
  
  coefs <- coef_stack |> 
    select(.imp, term, estimate_imp) |>
    pivot_wider(id_cols = .imp, names_from = term, 
                values_from = estimate_imp, names_prefix = "coef_")
  
  means <- coef_stack |> 
    select(.imp, term, means) |>
    pivot_wider(id_cols = .imp, names_from = term, 
                values_from = means, names_prefix = "means_")
  
  return(list(coef_stack = coef_stack, 
              coefs = coefs, 
              means = means))
}
