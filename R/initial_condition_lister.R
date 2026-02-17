IGP_models_lister <- function(igp_parms = igpParms, phi_values = phiVal){

maxIp_HV <- (igp_parms[["mup"]] * (igp_parms[["mp"]] + igp_parms[["mup"]])) /
  (igp_parms[["mp"]] * igp_parms[["Ep"]])
maxIp_PB <- igp_parms[["mup"]] / (igp_parms[["Ep"]] * phi_values[["phiP_PB"]]) # the 0.5 is phi

maxIn_HV <- (igp_parms[["mun"]] * (igp_parms[["mn"]] + igp_parms[["mun"]])) /
  (igp_parms[["mn"]] * igp_parms[["En"]])
maxIn_PB <- igp_parms[["mun"]] / (igp_parms[["En"]] * phi_values[["phiN_PB"]]) # the 0.5 is phi


# Define common initial values (this is not unified and could be unified in the equations)
common_init_4_var <- c(R = 1e-01, Nl = 1e-01, Na = 1e-01, P = 1e-01)
common_init_5_var <- c(R = 1e-01, Nl = 1e-01, Na = 1e-01, Pl = 1e-01, Pa = 1e-01)

####CHANGE THE EQATUIONS  


######here i am! 

igp_combinations <- list(
  # First set (PBPB variations)
  PB.PB = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_PB"]], phiN = phi_values[["phiN_PB"]])),
    igp_model = igp_model_PBPB,
    igp_init = common_init_4_var,
    i_max = c(maxIp = maxIp_PB, maxIn = maxIn_PB)
  ),
  PB.LB = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_PB"]], phiN = phi_values[["phiN_rest"]])),
    igp_model = igp_model_PBPB,
    igp_init = common_init_4_var,
    i_max = c(maxIp = maxIp_PB, maxIn = NA)
  ),
  LB.PB = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_rest"]], phiN = phi_values[["phiN_PB"]])),
    igp_model = igp_model_PBPB,
    igp_init = common_init_4_var,
    i_max = c(maxIp = NA, maxIn = maxIn_PB)
  ),
  LB.LB = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_rest"]], phiN = phi_values[["phiN_rest"]])),
    igp_model = igp_model_PBPB,
    igp_init = common_init_4_var,
    i_max = c(maxIp = NA, maxIn = NA)
  ),

  # Second set (PBHV variations)
  PB.HV = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_PB"]], phiN = phi_values[["phiN_rest"]])),
    igp_model = igp_model_PBHV,
    igp_init = common_init_4_var,
    i_max = c(maxIp = maxIp_PB, maxIn = maxIn_HV)
  ),
  LB.HV = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_rest"]], phiN = phi_values[["phiN_rest"]])),
    igp_model = igp_model_PBHV,
    igp_init = common_init_4_var,
    i_max = c(maxIp = NA, maxIn = maxIn_HV)
  ),

  # Third set (HVPB variations)
  HV.PB = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_rest"]], phiN = phi_values[["phiN_PB"]])),
    igp_model = igp_model_HVPB,
    igp_init = common_init_5_var,
    i_max = c(maxIp = maxIp_HV, maxIn = maxIn_PB)
  ),
  HV.LB = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_rest"]], phiN = phi_values[["phiN_rest"]])),
    igp_model = igp_model_HVPB,
    igp_init = common_init_5_var,
    i_max = c(maxIp = maxIp_HV, maxIn = NA)
  ),

  # Fourth set
  HV.HV = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_rest"]], phiN = phi_values[["phiN_rest"]])),
    igp_model = igp_model_HVHV,
    igp_init = common_init_5_var,
    i_max = c(maxIp = maxIp_HV, maxIn = maxIn_HV)
  ),

  ###parasitoid set fight
  LB.PA = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_rest"]], phiN = phi_values[["phiN_rest"]])),
    igp_model = igp_model_PBPA,
    igp_init = common_init_4_var,
    i_max = c(maxIp = NA, maxIn = NA)
  ),

  PB.PA = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_PB"]], phiN = phi_values[["phiN_rest"]])),
    igp_model = igp_model_PBPA,
    igp_init = common_init_4_var,
    i_max = c(maxIp = maxIp_PB, maxIn = NA)
  ),

  HV.PA = list(
    igp_parms = c(igp_parms, c(phiP = phi_values[["phiP_rest"]], phiN = phi_values[["phiN_rest"]])),
    igp_model = igp_model_HVPA,
    igp_init = common_init_5_var,
    i_max = c(maxIp = maxIp_HV, maxIn = NA)
  )
)

return(igp_combinations)
  
}
