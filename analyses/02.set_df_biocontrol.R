###The goal of this code is:

#produce the biocontrol database
#run it only if you dont have it in your data base 


###I. Biocontrol measurements 

source("./R/bifPlotters.R")
source("./R/bifurcationF.R")
source("./R/biocontrolSummary.R")


igpComb<- readRDS(file = "./data/igp_combinationsCOMPLETE.RData")
igpTimes <- seq(from=1,to=2000,by=.05)  #this is the step for the integration 

predComb <- c("LB.LB", "PB.LB", "HV.LB",
               "LB.PB", "PB.PB", "HV.PB",
               "LB.HV", "PB.HV", "HV.HV",
               "LB.PA", "PB.PA", "HV.PA")

#Slist <- c(0.1, 0.5, 0.9)
sVec <- c(0.1, 0.5)

kList  <- list(name="K", min =0, max=2, res= 1)


biocontrol_databaser(igp_combinations = igpComb, igp_times= igpTimes, pred_comb= predComb, s_vec= sVec, par_list= kList)
