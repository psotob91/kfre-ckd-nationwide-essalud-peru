print_equation <- function(data, st0) {
  
  eq <- data |> 
    mutate(sign = if_else(coefs < 0, "-", "+"), 
           term = str_glue("{sign} {abs(coefs)} \\times ({vars} / {scale} - {center})")) |> 
    select(vars, term) |> 
    pivot_wider(names_from = vars, values_from = term) |> 
    mutate(eq = str_glue("{age} {male} {eGFR} {logACR}")) |> 
    pull(eq)
  
  return(cat(paste0("$$1 - ", st0, "^{exp(", eq, ")}$$")))
}