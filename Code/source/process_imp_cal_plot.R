process_imp_cal_plot <- function(i, vdata, primary_event, horizon, 
                                 type, n_internal_knots = 5) {
  
  if (type == "pseudoval_loess") {
    
    vdata_filt <- vdata %>% 
      filter(.imp == i) 
    
    pred <- as.matrix(vdata_filt$pred)
    
    score_vdata <- Score(
      list("csh_validation" = pred),
      formula = Hist(time, eventd) ~ 1,
      cens.model = "km",
      data = vdata_filt,
      conf.int = TRUE,
      times = horizon,
      summary = c("ipa"),
      cause = primary_event,
      plots = "calibration"
    )
    
    datos <- data.frame(score_vdata$Calibration$plotframe) |> 
      mutate(.id = vdata_filt$.id, .imp = as.integer(i)) |> 
      select(.imp, .id, everything()) 
    
      return(datos)
    
  } else if (type == "subdist_hazard") {
    
    vdata_filt <- vdata %>% 
      filter(.imp == i) 
    
    pred <- vdata_filt$pred
    
    # cll_pp <- log(-log(1 - pp))
    # rcs_pp <- ns(cll_pp, df = n_internal_knots + 1)
    # colnames(rcs_pp) <- paste0("basisf_", colnames(rcs_pp))
    # 
    # vdata_bis_pp <- as.data.frame(rcs_pp)
    
    # 5 knots seems to give somewhat equivalent graph to pseudo method with bw = 0.05
    rcs_vdata <- ns(vdata_filt$cll_pred, df = n_internal_knots + 1)
    colnames(rcs_vdata) <- paste0("basisf_", colnames(rcs_vdata))
    
    # vdata_bis <- cbind.data.frame(vdata_filt, rcs_vdata)
    vdata_bis <- vdata_filt |> 
      bind_cols(as.data.frame(rcs_vdata))
    
    # Use subdistribution hazards (Fine-Gray) model
    form_fgr <- reformulate(
      termlabels = colnames(rcs_vdata),
      response = "Hist(time, eventd)"
    )
    
    # Regress subdistribution of event of interest on cloglog of estimated risks
    calib_fgr <- FGR(
      formula = form_fgr,
      cause = primary_event,
      data = vdata_bis, 
      variance = FALSE, 
      y = FALSE
    )
    
    obs <- predict(calib_fgr, times = horizon, newdata = vdata_bis)
    
    # obs_pp <- predict(calib_fgr, times = horizon, newdata = vdata_bis_pp)
    
    # Add observed and predicted together in a data frame
    datos <- data.frame(
      .imp = vdata_bis$.imp,
      .id = vdata_bis$.id,
      "obs" = obs[, 1],
      "risk" = vdata_bis$pred
    )
    
    # datos.pp <- data.frame(
    #   .imp = i, 
    #   "obs" = obs_pp, 
    #   "risk" = pp, 
    #   type = "fixed"
    # )
    
    # datos_mod <- datos |> 
    #   bind_rows(datos_pp = datos.pp)
    
    return(datos)
    
  } else {
    print("Error: Method not implemented")
  }
  
}