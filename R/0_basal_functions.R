#' Functions to bifurcate that creates the database of biocontrol. As it works on back and forth simulations it takes a lot of time
#' @param model the IGP model used
#' @param mod_times the integration times used by the solver (ODE)
#' @param mod_parameters the parameters of the ODE
#' @param mod_init the initial conditions
#' @param parSw the parameter to be swapped
#' @param maxPar the max value of the parameter
#' @param minPar the min value of the para
#' @param resolution the step of the parameter to be swapped
#' @param estCr the criteria from wchih we derived no chnage in the folowewing point
#' @return 1 data frames.with the bifurcation 
#' @uses ODE() function
#'  @examples


################################################################################
#############################BASAL CODES###################
################################################################################
## double checked

bif_backFor <-function(model, parSw, maxPar, minPar=0, resolution=0.5, mod_parameters, mod_times, mod_init, estCr = 1e-10){
  
  #you first establish the first sweep
  preSweep<-seq(minPar,maxPar,resolution)                          
  zinit <- mod_init
  parms <-mod_parameters
  
  FULL_DF <- data.frame()  ## this data frame will store the equilibrium values. 
  
  
  ##we establish the back and forth. We begin by ida. 
  for (dir in c("ida", "regreso")) {
    
## this chunk will revert the order of the sweep of the parameter to go on the opposite direction
    ## and stablish as its firs value, the last value of the IDA data frame
 if(dir =="regreso") {sweep <- rev(preSweep) 
 newCI <- as.numeric(tail(FULL_DF, 1)[,1:length(mod_init)]) #for the vector to be the size of the variables
 names(newCI) <- names(mod_init)
 zinit <- newCI }
    else{sweep <- preSweep}
  
    
    ### now for both ida y vuleta, this: 
    
  
  for(i in 1:length(sweep)) {
    keyPar<-sweep[i]  #this is the value of the parameter that is going to change parSw
    parms[[parSw]] <- keyPar  #the paramater that is changing
    
    #afeter the firdt index it will take for initial condition the last value of the prevoius equilibria
     # with small perturbation to travel away from the stable point (now unstable)
     if (i>1){
      newCI <- as.numeric(tail(FULL_DF, 1)[,1:length(mod_init)])
      per <- estCr* 10  ##lo perturbas un orden de magnitud mas de la maxima resolucio 
      newCI <- newCI + per
      names(newCI) <- names(mod_init)
      zinit <- newCI}
    
    
    #print(c('i=',i, 'zinit=', zinit))
    out<-deSolve::ode(func=model, y=zinit, times=mod_times,parms=parms) |> 
      as.data.frame()
    
    #we round to the ten digit

    out_last100 <-subset(out, time> max(mod_times-100))
    out_last100 <-round(out_last100,-log10(estCr))
    
    
    ## w checked if the alst values 
    maxDIF <- estCr
    
    ##{2,2 is the second row of R}
    #ig it converges, then it keeps the avergae
    #it it does not converge yet, it take the last 100 points
    
    if((out_last100[2,2]- out_last100[1,2]) < maxDIF){
      dyn_DF <- out_last100 |>
        dplyr::summarise_all(mean)
      dyn_DF$type <- '1-point'
    }
    else {
      dyn_DF <- out_last100
      dyn_DF$type <-'2-other'
    }
    
    ##here i remove the time, then I add the value of the param, and the direction 
    dyn_DF$time <- NULL
    dyn_DF$parmValue<-keyPar
    dyn_DF$direccion <- dir
    FULL_DF <- rbind(FULL_DF, dyn_DF)
  }
 
  }  
    
  ###
  names(FULL_DF)[which(names(FULL_DF)=="parmValue")]<- parSw
  
  return(FULL_DF)
}

