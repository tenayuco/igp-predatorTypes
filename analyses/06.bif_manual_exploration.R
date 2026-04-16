

##=====================manual explorations======================
# So this code helps to use the debif package with our model. But it is all maually with a shiny app



#1. We call the bifurcation 

#So the procedure is always the same. First we chose the model we want to explore. 
#a.Then we run for K. Given anything else, we always have like the control were S=0.5, Cn=10. 
#b. We try to find ALL bifurcation plots in relation to the basic model 
#we save the image 
#c. Then we compare the other parameters in the 2d plots. Once all the boundary curves have been draw, we close. And save the bifurcation plot in a generic rdata. And we move it to the good file. 


#====================================USER SECTION=============================================

## 1. First you chose the parameters you want MANUALLY

chosenP <- "LB" 
chosenN <- "PA"
ipProp <- 0.1  ##or 0.9
inProp <- 0.1  ## or 0.9
phiP <- 0
phiN <-  0


##. 2. Then you run the function that will opne a shiny app. In this you can:
##2.1 Check some time series
##2.2 Use the bifurcation plot. You fix the parameter you want to bifurcate (K for example) and run it for any variable R, N, P. yo
# will find point called bp, lp or hp. 
##2.3 change the initial conditions to start from these values and rerun it until you find all bifurcation points
##2.4 For 2d bifurcation, change to the phasepot window, put the parameters you want and run from the bifrucation point to generate a bifrucation curve 
##.2.5 save the plots if you want, but then close everything
##. 3. Before closing chose BifCurves to save the data

debif_function(chosen_P= chosenP, chosen_N = chosenN, ip_prop = ipProp, in_prop = inProp, phi_P = phiP, phi_N = phiN)


bifDATA <- bifCurves

output_dir <- paste0("outputs/manualBifurcation/rData_ALL/",paste0(chosenP, ".", chosenN)
,"/")

# Create directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
} else{
  print("folder already exists, check for any change")
}


saveRDS(bifDATA , paste0(output_dir, "/",
                        paste0(chosenP, ".", chosenN), "_bif_K_S_", 
                        "IpProp_" ,ipProp, 
                        "phiP_", phiP, 
                        "InProp_" , inProp, 
                        "phiN_", phiN, ".RData"))

##4. you will be able to use this data to extract information from it. 



##########################33





LBLB_LV_log_model <- function (t, state, parms) {##includes PB, LB as predators and as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
  
    ###predator IGP P functional responses
    ### prey IGP N functional response
    
    #these are the equations 
    dPdt <- Ep*((frp*S*R + fnp*N*(1-S))*P)- mup*P
    dNdt <- En*(frn*R*N)- fnp*P*N-mun*N 
    dRdt <- rho*R*(1-R/K)- (frn*N- frp*P)*R  #logisstic
    
    # z[R, Nl, Na, P] is the order of the vector
    return(list(c(dRdt, dNdt,  dPdt)))        
  })
}


LBLB_LV_parms <- c(
  rho = 1,
  K = 5,
  En = 0.5,
  Ep = 0.5,
  frp = 0.6,   ### if wwe suppose both r and n to be biomass, we extract that when n=r=1/2 hn (to reach the linear assumtoption), then frp should be cp/4
  fnp = 0.6,
  frn = 5, ## hrtr is cn/2
  mun = 1.1,
  mup = 0.33, ## to compare with hin, they have mun = 1 (Tp) + 0.3,  and mup = 0.3 (maintenance) + 0.03 death rate
 #here is dfi
  S = 0.5
) #here the Sr different


deBif::bifurcation(LBLB_LV_log_model, c(R=0.1, N=0.1, P=0.1), LBLB_LV_parms,  resume = TRUE) #this creates myl1BifCurves


