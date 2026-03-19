##########################################################################################################
# Code that produce the data base of the simulations for the biocontrol
#' @param igpTimes (defined by the user)
#' @param predComb (defined by the user)
#' @functions biocontrol_databaser()
#' @return the data baser of biocontrol and saves two data frames if noy present (data/biocontrol/sval_sVec_K_KList/df_RAW and data/biocontrol/sval_sVec_K_KList/df_SUMMARIZED
#' @details run it only if you dont have it in your data base
#' @details The K and S are set, but those ones change within each simulation
#############################################################################################################

#######Import general combinations and define entries for the simulation
igpTimes <- seq(from = 1, to = 2000, by = .05) #this is the step for the integration
predComb <- c(
  "LB.LB",
  "PB.LB",
  "HV.LB",
  "LB.PB",
  "PB.PB",
  "HV.PB",
  "LB.HV",
  "PB.HV",
  "HV.HV",
  "LB.PA",
  "PB.PA",
  "HV.PA"
)

#sVec <- c(0.1)
#kList <- list(name = "K", min = 0, max = 1, res = 1) #change this to 8 for the analisis 

sVec <- c(0.1, 0.5, 0.9)
kList <- list(name = "K", min = 0, max = 8, res = 1) #change this to 8 for the analisis 

##checks if the data bases are present if not, runs it 
biocontrol_databaser(
  igp_combinations = igp_combinations,
  igp_times = igpTimes,
  pred_comb = predComb,
  s_vec = sVec,
  par_list = kList
)


