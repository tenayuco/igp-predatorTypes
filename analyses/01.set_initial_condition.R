##########################################################################################################
# Analysis that produce a database with all the characteristic of each model
#' @param igpParms (defined by the user)
#' @param phiVal (defined by the user)
#' @return the igp_combinations as a list saved in your data folder
#' @details run it only if you dont have it in your data base
#' @details The K and S are set, but those ones change within each simulation
#############################################################################################################


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

###run it and save it in the local environment 
igp_combinations <- IGP_models_lister(igp_parms = igpParms, phi_values = phiVal)
###here you save it for all

rm(igpParms)
rm(phiVal)
