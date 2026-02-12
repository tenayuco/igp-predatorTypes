###The goal of this code is:

#produce the initial codes
#run it only if you dont have it in your data base
#for the preferences phi, the Ip and the In it will be changed for each model (outside the initial conditions)
# The K and S are set, but those ones change within each simulation

###1This calls the functions from RNP_functions
source("./R/RNP_functions.R")
source("./R/initial_condition_lister.R")


##2. then you define the parameters common to all the models
igpParms <- c(
  rho = 1,
  K = 1.5,
  En = 0.5,
  Ep = 0.5,
  Cn = 10,
  Cp = 2.5,
  mun = 1.1,
  mup = 0.33, ## to compare with hin, they have mun = 1 (Tp) + 0.3,  and mup = 0.3 (maintenance) + 0.03 death rate
  Hn = 1,
  Hp = 1,
  mn = 0.2,
  mp = 0.2, #here is dfi
  S = 0.5
) #here the Sr different


###3. Then I defined the preferences for the PB cases
phiVal <- c(phiP_PB = 0.5, phiN_PB = 0.5, phiP_rest = 0, phiN_rest = 0)

###
igp_combinations <- IGP_models_lister(igp_parms = igpParms, phi_values = phiVal)
###here you save it for all
saveRDS(igp_combinations, "./data/igp_combinationsCOMPLETE.RData")
