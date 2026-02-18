#####LOAD packages
library(manipulate)
library(deSolve)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Rmisc)
library(reshape2)
library(ggthemes)
library(gridExtra)
library(plotrix)
library(gdata)

#library(devtools)
#install_github("coolbutuseless/ggpattern")
#library(ggpattern)

################################################################################
#############################TIME SERIES PLOTTERS AND SOLVER###################
################################################################################


np_plot <- function(out){
  ############# this ones needs a dataframe to plot it ##############################33
  #miluys
  RNP_NP <- out %>% 
    ggplot(aes(x=N, y=P)) +
    geom_point(aes()) + 
    xlab("") +
    scale_color_colorblind()+
    theme_bw()
  RNP_NP
}

### this one computes the model  for different parameters 
### it needs the model, the parameters, the times and initial conditions
### the parameter that will change, its maximum value and resolution
ts_solve <- function(model, mod_parms, mod_times, mod_init, facet_1, minPar,  maxPar, resolution){
  out_total <- data.frame()
  for (parVar in seq(minPar, maxPar, resolution)){
    
    mod_parms[[facet_1]] <- parVar
    
    out<-ode(func=model, y=mod_init, times=mod_times,parms=mod_parms) %>% 
      as.data.frame()
    out$parameterValue <- parVar
    
    out_total <- rbind(out_total, out)
  }
  return(out_total)
}




### this one computes the model  for two changing parameters

### it needs the model, the parameters, the times and initial conditions
### the parameter that will change, its maximum value and resolution
ts_solve_2p <- function(model, mod_parms, mod_times, mod_init, li_pair){
  out_total <- data.frame()

  
  for (pair in li_pair){
    
    mod_parms[[names(pair)[1]]] <- pair[[1]]
    mod_parms[[names(pair)[2]]] <- pair[[2]]
    
    
    out<-ode(func=model, y=mod_init, times=mod_times,parms=mod_parms) %>% 
      as.data.frame()
    out[[names(pair)[1]]] <- pair[[1]] ### esto asi sencillo para nombrar cosas dentro de funciones, no todo el desmadre que hice
    out[[names(pair)[2]]] <-pair[[2]]  
    
    out_total <- rbind(out_total, out)
    
  }
  return(out_total)
}


################################################################################
#############################BIFURCATION SWEEPERS###################
################################################################################
#tis is the base from jonno
k_sweep_eq <-function(model=myl1, maxK=10, resolution=0.5){
  full<-list()
  EQ<-list()
  outALL<-list()                               ###Data output for BIFURCATION PLOT
  sweep<-seq(0,maxK,resolution)                           ###Set range and resolution of sweep
  
  for(i in 1:length(sweep)) {
    
    k<-sweep[i]
    parms <- c(r=0.5,K=k, Arp=500, Arn=5000, Anp=100, Erp=0.00001, Ern=0.00001, Enp=0.3, Hrp=0.00005, Hrn=0.00005, Hnp=0.11, Mn=.05, Mp=.05)            ###Set parameter values
    ###Set parameter values
    ###Set parameter values
    times <- seq(from=1,to=1000,by=.5)                                                                  ###Set run time
   # zinit <- c(R=0.0001, N=0.0001, P=0.0001)                                                                ###Set starting conditions
    zinit <- c(R=runif(1, min = 0, max=0.0001), N=runif(1, min = 0, max=0.0001), P=runif(1, min = 0, max=0.0001))    #random starting conditions                                                            ###Set starting conditions
    
    
    out<-ode(
      func=model,
      y=zinit,
      times=times,
      parms=parms
    ) %>% 
      as.data.frame()
    
    
    
    outALL[[i]]<-out                          ###CODE NOTE: 1 added to i to deal with 0 value - R will not store 0ith values in lists
    
    tmeltEQ<-melt(subset(out, time>900), id.vars=c("time"))
    sumtmeltEQ<-summarySE(tmeltEQ, measurevar="value", groupvars=c("variable"), na.rm=TRUE)
    sumtmeltEQ$parmvalue<-k
    
    if (i==1){EQ_DF <- sumtmeltEQ}
    else{EQ_DF <- rbind(EQ_DF, sumtmeltEQ)}
    
    #EQ[[i]]<-sumtmeltEQ #if you want to keep it in a list shape
  }
  return(EQ_DF)
}


#this model is the brute force sweeper with channging initial conditions
k_sweep_model <-function(model, parSw, maxPar, resolution=1, 
                         mod_parameters, mod_times, random = FALSE, 
                         numRandomCI=1, lenBar){
  
  sweep<-seq(0,maxPar,resolution)                           ###Set range and resolution of sweep
  
  if (random ==TRUE) {
    Rini <- runif(numRandomCI, min= 0, max =30)
    Nini <- runif(numRandomCI, min=0, max = 1)
    Pini <- runif(numRandomCI, min= 0, max = 1)
    condini_DF <- data.frame(R, N, P)
    
  }
  
  else{
    R <-seq(from = 0.0001, to=15, length.out=lenBar)
    N <- seq(from = 0.0001, to=0.005, length.out=lenBar)
    P <- seq(from = 0.0001, to=0.002, length.out=lenBar)
    condini_DF <- expand.grid(R, N, P)
    names(condini_DF)<- c("Rini", "Nini", "Pini")
    
  }
  
  
  condini_DF$condInit <- seq(1, dim(condini_DF)[1])
  
  for(i in 1:length(sweep)) {
    if (i %%10 ==0) {print(i)}
    for (ci in condini_DF$condInit){
      keyPar<-sweep[i]  #this is the value of the parameter that is going to change parSw
      

      ###Set parameter values
      zinit <- c(R=condini_DF$R[ci], N=condini_DF$N[ci], P=condini_DF$P[ci])
      parms <-mod_parameters
      parms[[parSw]] <- keyPar  #the paramater that is changing
      
      out<-ode(
        func=model,
        y=zinit,
        times=mod_times,
        parms=parms
      ) %>% 
        as.data.frame()
      
      tmeltEQ<-melt(subset(out, time>(max(out$time)-100)), id.vars=c("time"))
      tmeltEQ$parmvalue<-keyPar
      tmeltEQ$condInit<-ci
      
      ##ALGO
      if (i==1 & ci==1){FULL_DF <- tmeltEQ}
      else{FULL_DF <- rbind(FULL_DF, tmeltEQ)}
    
    }
  }
  
 FULL_DF<- merge(FULL_DF, condini_DF, by= "condInit")
 
  return(FULL_DF)
}












#this model is the brute force sweeper with channging initial conditions
stablePoint_doubleSweep <-function(par1, par2, minPar1, minPar2, maxPar1, maxPar2, resPar1, resPar2,
                                   model, mod_parameters, mod_times, mod_init){
  
  sweep1 <- seq(minPar1, maxPar1, length= resPar1)
  sweep2 <- seq(minPar2, maxPar2, length = resPar2)
                           ###Set range and resolution of sweep
  
  totalDF <- data.frame()
  
  parms <- mod_parameters
  
  for(i in 1:length(sweep1)) {
    if (i %%10 ==0) {print(i)}
    par1_var<-sweep1[i]  #this is the value of the parameter that is going to change parSw
    parms[[par1]] <- par1_var  #the paramater that is changing
    
    for(j in 1:length(sweep2)) {
      par2_var<-sweep2[j]  #this is the value of the parameter that is going to change parSw
      parms[[par2]] <- par2_var  #the paramater that is changing
      

        out<-ode(
          func=model,
          y=mod_init,
          times=mod_times,
          parms=parms
        ) %>% 
          as.data.frame()
        
        
        #this suppose we get an equi
      
          
      
        tmeltEQ<-as.data.frame(melt(subset(out, time>(max(out$time)-100)), id.vars=c("time")))
      
        
        tmeltEQ <-  tmeltEQ %>%
          dplyr::group_by(variable)%>%
          dplyr::summarise(valueMean =mean(value))
        
       
  
        
        tmeltEQ$paruno <- par1_var
        tmeltEQ$pardos <- par2_var
        
  
        
        totalDF <- rbind(totalDF, tmeltEQ)
        
        
      
        
    }
     
  }
  
  names(totalDF)[names(totalDF) == "paruno"] <-as.character(par1)
  names(totalDF)[names(totalDF) == "pardos"] <-as.character(par2)
  
 
  
  return(totalDF)
  }

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      

#This might calculate min and max, but for now, only identifies the points
bif_min_max <- function(fullDF, varName){
  MIN_MAX_DF<-  data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("varName", "minMax", "parmvalue", "value"))))
  for (k in unique(fullDF$parmvalue)){
    for(ci in unique(fullDF$condInit)){
      fullDF_sample <- fullDF%>%
        filter(condInit == ci)%>%
        filter(parmvalue == k)%>%
        filter(variable ==varName)
      
      maxError <- 1e-6 # this is the max error to say they are the same values in percentage but we should normalize
      
      for (i in 2:(dim(fullDF_sample)[1]-1)){
        if (abs(fullDF_sample$value[i+1] - fullDF_sample$value[i]) <maxError) {
          
          ROW_DF <- data.frame("varName" = varName, "minMax" = "point", "parmvalue"= k, "condInit"= ci, 'value'=fullDF_sample$value[i])
          MIN_MAX_DF<- rbind(MIN_MAX_DF, ROW_DF)
          break}
        }
      }
    }
  MIN_MAX_DF <- unique(MIN_MAX_DF)
  return(MIN_MAX_DF)
}



####back and for method. Ill comment this one a bit more. 
bif_backFor <-function(model, parSw, maxPar, minPar=0, resolution=0.5, mod_parameters, mod_times, mod_init, 
                       estCr = 1e-10){
  
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
    out<-ode(func=model, y=zinit, times=mod_times,parms=parms) %>% 
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
      dyn_DF <- out_last100 %>%
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




############################double sweep#########################


doubleSweep_main <- function(model, 
                             lipar,
                             mod_parameters, mod_times, mod_init, 
                             estCr = 1e-10){
  
  start1<-proc.time()
  
  DIR_A <- bif_backFor_double(model = model,
                                        lipar =lipar,
                                        mod_parameters = mod_parameters,
                                        mod_times = mod_times, 
                                        mod_init = mod_init, 
                                        estCr =estCr)
  
  print("First direction finished in")
  print(proc.time()- start1)
  
  
  start2<-proc.time()
  
  DIR_B <- bif_backFor_double(model = model,
                              lipar = rev(lipar), #here is the small change between both 
                              mod_parameters = mod_parameters,
                              mod_times = mod_times, 
                              mod_init = mod_init, 
                              estCr =estCr)
  
  print("second direction finished in")
  print(proc.time()- start2)
  
  DIR_A$corte <-"a" 
  DIR_B$corte <-"b" 
  DO <- rbind(DIR_A, DIR_B)
  
  return(DO)
  
  
}



bif_backFor_double <-function(model, 
                              lipar,
                              mod_parameters, mod_times, mod_init, 
                       estCr = 1e-10){
  
  #you first stablish the first and second sweep
  
  lipar1 <- lipar[[1]]
  lipar2 <- lipar[[2]]
  sweep1<-seq(lipar1[["minPar"]],lipar1[["maxPar"]],lipar1[["res"]])
  preSweep2<-seq(lipar2[["minPar"]],lipar2[["maxPar"]], lipar2[["res"]]) #i do it like this to then revert it only one time and not each time for the return
  par1Sw <- lipar1[["parSw"]]
  par2Sw <- lipar2[["parSw"]]
  
  maxCounter <- length(sweep1) * length(preSweep2) * 2
  counter <- 0
  
  # this is the one for the back and forth procedure..
  
  #So we start with the initial mod parameters (step 0)
  zinit <- mod_init
  parms <-mod_parameters
  
  FULL_DF <- data.frame()  ## this data frame will store the equilibruium valiues. 
  
#We take the first parameter that is relatively fixed. And within each value of this parameter
  #we will run the back and forth with the ither parameter 2. 
  
  for(i in 1:length(sweep1)) {
    keyPar1<-sweep1[i]  #this is the value of the parameter that is going to change parSw
    parms[[par1Sw]] <- keyPar1  #the paramater that is changing
    
  #Now we start the cycle of back and forth
  for (dir in c("ida", "regreso")) {
    
    ## this chunk will revert the order of the sweep of the parameter to go on the opposite direction
    ## and stablish as its firs value, the last value of the IDA data frame
    if(dir =="regreso") {sweep2 <- rev(preSweep2)
    newCI <- as.numeric(tail(FULL_DF, 1)[,1:length(mod_init)]) #for the vector to be the size of the variables
    names(newCI) <- names(mod_init)
    zinit <- newCI }
    else{sweep2 <- preSweep2} 
    
    
    ### now for both ida y vuleta, we bifurcate both parameters: 
    
    for(j in 1:length(sweep2)) {

      keyPar2<-sweep2[j]
      parms[[par2Sw]] <- keyPar2  #the paramater that is changing
      
      #afeter the firdt index it will take for initial condition the last value of the prevoius equilibria
      # with small perturbation to travel away from the stable point (now unstable) This overscripts the previous initial condition 
      if (j>1){
        newCI <- as.numeric(tail(FULL_DF, 1)[,1:length(mod_init)])
        per <- estCr* 10  ##lo perturbas un orden de magnitud mas de la maxima resolucio 
        newCI <- newCI + per
        names(newCI) <- names(mod_init)
        zinit <- newCI}
    
      #print(c('i=',i, 'zinit=', zinit))
      out<-ode(func=model, y=zinit, times=mod_times,parms=parms) %>% 
        as.data.frame()
      
      #we round to the ten digit
      
    # digit we keep -log10(estCr) as the number of digits
      out_last100 <-subset(out, time> max(mod_times-100))
      out_last100 <-round(out_last100, -log10(estCr))
      

      ## w checked if the alst values 
      maxDIF <- estCr
      
      ##{2,2 is the second row of R}
      #ig it converges, then it keeps the avergae
      #it it does not converge yet, it take the last 100 points
      
      if((out_last100[2,2]- out_last100[1,2]) < maxDIF){
        dyn_DF <- out_last100 %>%
          dplyr::summarise_all(mean)
        dyn_DF$type <- '1-point'
      }
      else {
        dyn_DF <- out_last100
        dyn_DF$type <-'2-other'
      }
      
      ##here i remove the time, then I add the value of the param, and the direction 
      dyn_DF$time <- NULL
      dyn_DF$parm1Value<-keyPar1
      dyn_DF$parm2Value<-keyPar2
      dyn_DF$direccion <- dir
      FULL_DF <- rbind(FULL_DF, dyn_DF)
      
      counter <- counter +1 
    }  
    print(paste(counter, "out of", maxCounter))
  }  
  }
  
  names(FULL_DF)[which(names(FULL_DF)=="parm1Value")]<- lipar1[["parSw"]]
  names(FULL_DF)[which(names(FULL_DF)=="parm2Value")]<- lipar2[["parSw"]]
  
  
  return(FULL_DF)
}

#######################



################################################################################
#############################CODES FOR EQUILIBI+RIA A COLORING###################
################################################################################


simple_ass_coex <- function(simpleDF, par_sw, facet_1=NULL, ncrit =2){

  ###first we have to extract the means without removing the other aspect!

  if("Nl" %in% colnames(simpleDF)){
    simpleDF$N <- simpleDF$Na + simpleDF$Nl} ###so we dont care about the stages for coexistnece 
  if("Pl" %in% colnames(simpleDF)){
    simpleDF$P <- simpleDF$Pa + simpleDF$Pl}
  
  
  DF_NORM <-simpleDF  %>%
    dplyr::group_by(across(-any_of(c("R", "Na", "Nl", "Pl", "Pa,", "N", "P"))))%>%
    summarise_all(mean)
  

  ###despues vamos a ver cuales son ASS (solo tienen sentido en los puntos)
  ## y vamos a juntar la ida y el regreso. Es decir, aqui pierdo info, pero dejo que es ASS
  
  ## we add the equilibri
  DF_NORM$EQR <-""
  DF_NORM$EQN <-""
  DF_NORM$EQP <-""
  
  ###chacun a son critere de maximumm...
  DF_NORM$EQR[round(DF_NORM$R/max(DF_NORM$R), ncrit) >0] <- "R"
  DF_NORM$EQN[round(DF_NORM$N/max(DF_NORM$N), ncrit) >0] <- "N"  ##here I assume no stage
  DF_NORM$EQP[round(DF_NORM$P/max(DF_NORM$P), ncrit) >0] <- "P"
  
  DF_NORM <- DF_NORM %>%
    unite("EQ", EQR:EQP, sep = "", remove = T)
  
  DF_NORM$EQ[DF_NORM$EQ ==""] <- 0
  
  DF_NORM$Rnorm <- round(DF_NORM$R/max(DF_NORM$R), ncrit) #this is the max criteria for differences (1*10-3)
  
  DF_NORM <- DF_NORM %>%
    dplyr::group_by(across(any_of(c(par_sw, facet_1)))) %>%
    dplyr::mutate(ASS = n_distinct(Rnorm))
  
  return(DF_NORM)
  
}


minMax_coex <- function(simpleDF, par_sw, facet_1=NULL, ncrit =2){
  
  ###first we have to extract the means without removing the other aspect!
  
  if("Nl" %in% colnames(simpleDF)){
    simpleDF$N <- simpleDF$Na + simpleDF$Nl} ###so we dont care about the stages for coexistnece 
  if("Rn" %in% colnames(simpleDF)){
    print("Rn is here")
    simpleDF<- dplyr::rename(simpleDF, Nl = Rn)
    simpleDF$N <- simpleDF$Na + simpleDF$Nl} ###so we dont care about the stages for coexistnece 
  if("Pl" %in% colnames(simpleDF)){
    simpleDF$P <- simpleDF$Pa + simpleDF$Pl}
  
  
  DF_NORM <-simpleDF  %>%
    dplyr::group_by(across(-any_of(c("R", "Na", "Nl", "Pl", "Pa", "N", "P", "Rn"))))%>%
    summarise_all(list(maxValue = max, minValue = min, meanValue=mean))  
  
  ###despues vamos a ver cuales son ASS (solo tienen sentido en los puntos)
  ## y vamos a juntar la ida y el regreso. Es decir, aqui pierdo info, pero dejo que es ASS
  
  ## we add the equilibri
  DF_NORM$EQR <-""
  DF_NORM$EQN <-""
  DF_NORM$EQP <-""
  
  ###chacun a son critere de maximumm...BUT HERE I HAN CHEAITNG AND PUTTNG CPEXITE AONY IF THE MEAN IS GREEN (so it take the average of oscilaiotns)
  DF_NORM$EQR[round(DF_NORM$R_meanValue/max(DF_NORM$R_meanValue), ncrit) >0] <- "R"
  DF_NORM$EQN[round(DF_NORM$N_meanValue/max(DF_NORM$N_meanValue), ncrit) >0] <- "N"  ##here I assume no stage
  DF_NORM$EQP[round(DF_NORM$P_meanValue/max(DF_NORM$P_meanValue), ncrit) >0] <- "P"
  
  DF_NORM <- DF_NORM %>%
    unite("EQ", EQR:EQP, sep = "", remove = T)
  
  DF_NORM$EQ[DF_NORM$EQ ==""] <- 0
  
  DF_NORM$Rnorm <- round(DF_NORM$R_meanValue/max(DF_NORM$R_meanValue), ncrit) #this is the max criteria for differences (1*10-3)
  
  DF_NORM <- DF_NORM %>%
    dplyr::group_by(across(any_of(c(par_sw, facet_1)))) %>%
    dplyr::mutate(ASS = n_distinct(Rnorm))
  
  ###NOW we going to save the min and max column and put them as worw (even if repetitive..)
  
  #done by deepseek
  DF_NORM_LONG <- DF_NORM %>%
    gather(key = "var_minmax", value = "value", 
           R_minValue, R_maxValue, Nl_minValue, Nl_maxValue, 
           Na_minValue, Na_maxValue, P_minValue, P_maxValue) %>%
    separate(var_minmax, into = c("variable", "minMax"), sep = "_") %>%
    spread(key = variable, value = value)
  
  DF_NORM_LONG <-DF_NORM_LONG %>%
    select(type, S, direccion, K, EQ, ASS, minMax, Na, Nl, P, R)
  
  return(DF_NORM_LONG)
  
}

##########this is almost the same as the previus function, but changin the names of facet by other parameter
### eventually this two functions can be the samee.. (this and the )

double_ass_coex <- function(simpleDF, li_par, ncrit =2){
  
  li1 <- li_par[[1]]
  li2 <- li_par[[2]]
  par1 <- li1[["parSw"]]
  par2 <- li2[["parSw"]]
  
  ###first we have to extract the means without removing the other aspect!
  
  if("Nl" %in% colnames(simpleDF)){
    simpleDF$N <- simpleDF$Na + simpleDF$Nl} ###so we dont care about the stages for coexistnece 
  if("Pl" %in% colnames(simpleDF)){
    simpleDF$P <- simpleDF$Pa + simpleDF$Pl}
  
  ### here we extract the mean values!!
  DF_NORM <-simpleDF  %>%
    dplyr::group_by(across(-any_of(c("R", "Na", "Nl", "Pl", "Pa,", "N", "P"))))%>%
    summarise_all(mean)
  
  
  ###despues vamos a ver cuales son ASS (solo tienen sentido en los puntos)
  ## y vamos a juntar la ida y el regreso. Es decir, aqui pierdo info, pero dejo que es ASS
  
  ## we add the equilibri
  DF_NORM$EQR <-""
  DF_NORM$EQN <-""
  DF_NORM$EQP <-""
  
  ###chacun a son critere de maximumm...
  DF_NORM$EQR[round(DF_NORM$R/max(DF_NORM$R), ncrit) >0] <- "R"
  DF_NORM$EQN[round(DF_NORM$N/max(DF_NORM$N), ncrit) >0] <- "N"  ##here I assume no stage
  DF_NORM$EQP[round(DF_NORM$P/max(DF_NORM$P), ncrit) >0] <- "P"
  
  DF_NORM <- DF_NORM %>%
    unite("EQ", EQR:EQP, sep = "", remove = T)
  
  DF_NORM$EQ[DF_NORM$EQ ==""] <- 0
  
  DF_NORM$Rnorm <- round(DF_NORM$R/max(DF_NORM$R), ncrit) #this is the max criteria for differences (1*10-3)
  
  DF_NORM <- DF_NORM %>%
    dplyr::group_by(across(any_of(c(par1, par2)))) %>%
    dplyr::mutate(ASS = n_distinct(Rnorm))
  
  return(DF_NORM)
  
}
############################3


###############sumarize

double_sumarise <- function(doubleDF, li_par){
  li1 <- li_par[[1]]
  li2 <- li_par[[2]]
  par1 <- li1[["parSw"]]
  par2 <- li2[["parSw"]]
  
  DF <- doubleDF
  
  DF_summary <- DF %>%
    select(!direccion)%>%
    select(!corte)%>%
    dplyr::group_by(across(all_of(c(par1, par2))), type, EQ, ASS) %>%
    summarise_all(mean)
  
  return(DF_summary)
}



