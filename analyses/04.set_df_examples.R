##########################################################################################################
# Code that produce the data base of the simulations of the examples, high detai on K 
#' @param igpTimes (defined by the user)
#' @param predComb (defined by the user)
#' @functions biocontrol_databaser()
#' @return the data baser of biocontrol and saves two data frames if noy present (data/biocontrol/sval_sVec_K_KList/df_RAW and data/biocontrol/sval_sVec_K_KList/df_SUMMARIZED
#' @details run it only if you dont have it in your data base
#' @details The K and S are set, but those ones change within each simulation
#############################################################################################################

#######Import general combinations and define entries for the simulation
igpTimes <- seq(from = 1, to = 2000, by = .05) #this is the step for the integration
#predComb <- c(
 # "LB.LB"
#)

predComb <- c(
  "LB.LB", "PB.LB", "LB.PB", "PB.PB", "LB.PA"
)

###### this is the exampesl were I bifurcated K, with differetn values of s


sList <- list(name = "S", values = c(0.9)) #here is a vector of fixed initial conditions. 
kList <- list(name = "K", min = 0, max = 8, res = 0.1) #change this to 8 for the analisis 



####
##checks if the data bases are present if not, runs it 

file_folder <- "./data/bifurcations/"
file_subfolder <- paste0("condpar_", paste0(sList[["name"]]), "_", paste0(sList[["values"]], collapse = "_"),  "_bifpar_", paste0(kList, collapse = "_"),"/")
file_name <- paste0("DF_BIFURCATION_", paste0(predComb, collapse = "_"),  ".csv")

  ##this is a control BEFORE RUNNING THE ANALYSIS

if (file.exists(paste0(file_folder, file_subfolder, file_name))) {
    print(paste0(
      file_folder,
      file_subfolder,
      file_name,
      " exists already. Verify before running long analysis"
    ))
  } else {
    dir.create(paste0(file_folder, file_subfolder), recursive = TRUE)

DF_BIF_1 <-  bifurcation_databaser(
  igp_combinations = igp_combinations,
  igp_times = igpTimes,
  pred_comb = predComb,
  bif_par_list = kList,
  cond_par_list = sList
)
  
write_csv(DF_BIF_1, paste0(file_folder, file_subfolder, file_name))

}



##################and this where I bifurcat S iwth diferent intial K


sList <- list(name = "S", min = 0, max = 1, res = 0.01) #change this to 8 for the analisis 
kList <- list(name = "K", values = c(4)) #here is a vector of fixed initial conditions. 
predComb <- c(
  "LB.LB", "PB.LB", "LB.PB", "PB.PB", "LB.PA"
)


####
##checks if the data bases are present if not, runs it 

file_folder <- "./data/bifurcations/"
file_subfolder <- paste0("condpar_", paste0(kList[["name"]]), "_", paste0(kList[["values"]], collapse = "_"),  "_bifpar_", paste0(sList, collapse = "_"),"/")
file_name <- paste0("DF_BIFURCATION_", paste0(predComb, collapse = "_"),  ".csv")

  ##this is a control BEFORE RUNNING THE ANALYSIS

if (file.exists(paste0(file_folder, file_subfolder, file_name))) {
    print(paste0(
      file_folder,
      file_subfolder,
      file_name,
      " exists already. Verify before running long analysis"
    ))
  } else {
    dir.create(paste0(file_folder, file_subfolder), recursive = TRUE)

DF_BIF_2 <-  bifurcation_databaser(
  igp_combinations = igp_combinations,
  igp_times = igpTimes,
  pred_comb = predComb,
  bif_par_list = sList,
  cond_par_list = kList
)
  
write_csv(DF_BIF_2, paste0(file_folder, file_subfolder, file_name))

}







