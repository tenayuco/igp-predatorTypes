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
sList <- list(name = "S", values = c(0.1, 0.5, 0.9)) #here is a vector of fixed initial conditions. 
kList <- list(name = "K", min = 0, max = 1, res = 8) #change this to 8 for the analisis 



#### test values (turn it for tests)
igpTimes <- seq(from = 1, to = 2000, by = .05) #this is the step for the integration
predComb <- c(
  "LB.LB",
  "PB.LB",
  "HV.LB",
  "LB.PB",
  "LB.HV",
  "LB.PA"
)
sList <- list(name = "S", values = c(0.1, 0.9)) #here is a vector of fixed initial conditions. 
kList <- list(name = "K", min = 0, max = 1, res = 1) #change this to 8 for the analisis 
###########



####
##checks if the data bases are present if not, runs it 

file_folder <- "./data/biocontrol/"
file_subfolder <- paste0("condpar_", paste0(sList[["name"]]), "_", paste0(sList[["values"]], collapse = "_"),  "_bifpar_", paste0(kList, collapse = "_"),"/")
file_name <- paste0("DF_BIOCONTROL_RAW", ".csv")

  ##this is a control BEFORE RUNNING THE ANALYSIS

if (file.exists(paste0(file_folder, file_subfolder, file_name))) {
    print(paste0(
      file_folder,
      file_subfolder,
      file_name,
      " exists already. Verifiy before running long analysis"
    ))
  } else {
    dir.create(paste0(file_folder, file_subfolder), recursive = TRUE)

DF_BIOCONTROL <-  bifurcation_databaser(
  igp_combinations = igp_combinations,
  igp_times = igpTimes,
  pred_comb = predComb,
  bif_par_list = kList,
  cond_par_list = sList
)
  
write_csv(DF_BIOCONTROL, paste0(file_folder, file_subfolder, file_name))

}

