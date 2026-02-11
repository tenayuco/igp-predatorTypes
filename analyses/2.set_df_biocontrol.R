###The goal of this code is:

#produce the biocontrol database
#run it only if you dont have it in your data base 


###I. Biocontrol measurements 

source("./R/bifPlotters.R")
source("./R/bifurcationF.R")


igp_combinations<- readRDS(file = "./data/igp_combinationsCOMPLETE.RData")
igp_times <- seq(from=1,to=2000,by=.05)  #this is the step for the integration 

PRED_COMB <- c("LB.LB", "PB.LB", "HV.LB",
               "LB.PB", "PB.PB", "HV.PB",
               "LB.HV", "PB.HV", "HV.HV",
               "LB.PA", "PB.PA", "HV.PA")

#Slist <- c(0.1, 0.5, 0.9)
Slist <- c(0.5)



BIOCON_COMB <- data.frame()


##this loops creates a data frame with the different conditions of s, and of ip 
for (comb in PRED_COMB){
  chosenComb <- comb
  ipMax <- igp_combinations[[chosenComb]]$i_max[["maxIp"]]
  inMax <- igp_combinations[[chosenComb]]$i_max[["maxIn"]]
  
  if (is.na(ipMax)){Ip_seq = 0}
  else{Ip_seq = signif(ipMax * c(0.1, 0.9),2)}  ###now we will remove high and low values 
  if (is.na(inMax)){In_seq = 0}
  else{In_seq = signif(inMax * c(0.1, 0.9),2)}
  
  BIOCON_I <- expand.grid(Ip = Ip_seq, In= In_seq, S= Slist)
  BIOCON_I$combPred <- chosenComb
  BIOCON_COMB <- rbind(BIOCON_COMB, BIOCON_I) }


###this one runs the simulation 

DF_BIOCON <- data.frame()

for (i in seq(1, dim(BIOCON_COMB)[1])){
  
  chosenComb <- BIOCON_COMB$combPred[i]
  Ip_var = BIOCON_COMB$Ip[i]
  In_var = BIOCON_COMB$In[i]
  S_var = BIOCON_COMB$S[i]
  
  print(chosenComb)
  
  #eqution characteristic 
  chosenParms = igp_combinations[[chosenComb]]$igp_parms
  chosenInit = igp_combinations[[chosenComb]]$igp_init
  chosenModel = igp_combinations[[chosenComb]]$igp_model
  chosenParms[["S"]] <- S_var
  chosenParms[["Ip"]] <- Ip_var
  chosenParms[["In"]] <- In_var
  
  
  BIOCON_temp <- bif_backFor(model = chosenModel, minPar = 0, maxPar =2, parSw = "K", resolution = 1, mod_parameters = chosenParms, mod_times =   igp_times, mod_init = chosenInit, estCr =1e-6)
  
  #this is to homogeneize the value we want to use to see the exclusion from one another. 
  if ("Rn" %in% names(BIOCON_temp)) {BIOCON_temp$N <- BIOCON_temp$Rn +BIOCON_temp$Na}
  if ("Nl" %in% names(BIOCON_temp)) {BIOCON_temp$N <- BIOCON_temp$Nl +BIOCON_temp$Na}
  if ("Pl" %in% names(BIOCON_temp)) {BIOCON_temp$P <- BIOCON_temp$Pl +BIOCON_temp$Pa}
  if ("Rp" %in% names(BIOCON_temp)) {BIOCON_temp$P <- BIOCON_temp$Rp +BIOCON_temp$Pa}
  
  BIOCON_temp <- BIOCON_temp %>% dplyr::select(-any_of(c("Rn", "Nl", "Pl", "Rp", "Pa", "Na")))
  
  
  BIOCON_temp <- merge(BIOCON_COMB[i,], BIOCON_temp)
  
  DF_BIOCON <- rbind(DF_BIOCON, BIOCON_temp)
  
}


##Im going to export it raw
write_csv(DF_BIOCON, "./output/numericalIGP/biocontrol/DF_biocon_raw_changingK.csv")


##First we gonna take the decision of summarise. This means that we take the mean EVEN if we did not get to the equilibria here.. 
### im gonna export it raw...v


DF_BIOCON_N <- DF_BIOCON %>%
  dplyr::group_by(Ip, In, K, combPred, type, S, direccion)%>%
  dplyr::summarise(meanR = mean(R), meanN= mean(N), meanP = mean(P))



DF_BIOCON_N  <- DF_BIOCON_N  %>%
  dplyr::group_by(combPred) %>%
  dplyr::mutate(normIp = Ip/max(Ip), normIn = In/max(In))



DF_BIOCON_N$IpCat <- "NA"
DF_BIOCON_N$InCat <- "NA"


DF_BIOCON_N$IpCat[DF_BIOCON_N$normIp %in% sort(unique(DF_BIOCON_N$normIp))[1:2]] <- "IpMin" ##agarara los dos valoes menores y sobre eso los catwegotri como los inimos (el de pb y el hv)
DF_BIOCON_N$IpCat[DF_BIOCON_N$normIp %in% sort(unique(DF_BIOCON_N$normIp))[3:4]] <- "IpMed" ##agarara los dos valoes menores y sobre eso los catwegotri como los inimos (el de pb y el hv)
DF_BIOCON_N$IpCat[DF_BIOCON_N$normIp %in% sort(unique(DF_BIOCON_N$normIp))[5]] <- "IpMax"


DF_BIOCON_N$InCat[DF_BIOCON_N$normIn %in% sort(unique(DF_BIOCON_N$normIn))[1:2]] <- "InMin"
DF_BIOCON_N$InCat[DF_BIOCON_N$normIn %in% sort(unique(DF_BIOCON_N$normIn))[3:4]] <- "InMed"
DF_BIOCON_N$InCat[DF_BIOCON_N$normIn %in% sort(unique(DF_BIOCON_N$normIn))[5]] <- "InMax"


write_csv(DF_BIOCON_N, "./output/numericalIGP/biocontrol/DF_biocon_full_moreK.csv")





