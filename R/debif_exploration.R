
debif_function <- function(chosen_P, chosen_N, ip_prop = NA, in_prop=NA, phi_P = 0.5, phi_N = 0.5){

combPred <- paste0(chosen_P, ".", chosen_N)

chosenParms = igp_combinations[[combPred]]$igp_parms
chosenInit = igp_combinations[[combPred]]$igp_init
chosenModel = igp_combinations[[combPred]]$igp_model
chosenParms["Ip"] <- 0 
chosenParms["In"] <- 0  

  #========================== set the specific values of Ip and In if PB or HV
# ============== this is, change 0.1 or 0.9 (low and high)
if (chosen_P %in% c("PB", "HV")){
  chosenParms["Ip"] <- ip_prop * igp_combinations[[chosenComb]]$i_max[["maxIp"]]
}  

if (chosen_N %in% c("PB", "HV")){
  chosenParms["In"] <- in_prop * igp_combinations[[chosenComb]]$i_max[["maxIn"]]
}  

  
## note, this is the default value, and it works without this loop, but if for the supll
  
if (chosen_P %in% c("PB")){
  chosenParms["phiP"] <- phi_P
} 
  
if (chosen_P %in% c("PB")){
  chosenParms["phiN"] <- phi_N
} 
  

deBif::bifurcation(chosenModel, chosenInit, chosenParms, resume = TRUE) #this creates myl1BifCurves

}