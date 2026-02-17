##########################################################################################################
# Code that produce the data base of the simulations for the biocontrol
#' @param igpParms (defined by the user)
#' @param phiVal (defined by the user)
#' @functions biocontrol_databaser()
#' @return the igp_combinations as a list saved in your data folder
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

sVec <- c(0.1)
kList <- list(name = "K", min = 0, max = 1, res = 1) #change this to 8 for the analisis 

#sVec <- c(0.1, 0.5, 0.9)
#kList <- list(name = "K", min = 0, max = 3, res = 1) #change this to 8 for the analisis 

#### run the data baser

DF_BIOCONTROL <- biocontrol_databaser(
  igp_combinations = igp_combinations,
  igp_times = igpTimes,
  pred_comb = predComb,
  s_vec = sVec,
  par_list = kList
)




