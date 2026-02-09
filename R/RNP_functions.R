

###############SIMPLIFIED FUNCTIONS ONLY R and N###############################33

### Mylius - Direct transition from N2 to N1. And temporal transition (thourg m) from N1 to N2
### Mymodel - direc transition from N2 to N1 and from N1 to N2
###LB-LB both larva and adult feed on R
### HV, only the larva feed on R



###### LB LB cases###############

###Mylius, normal reduced version. 
redN_str_1 <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frn <- Crn * R /(Hn+ R)  ###########here i dont have the srn since it is the only prey
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*(N1+N2)
    dN1dt <- Ern*Frn*(N1+a*N2)-m*N1 -mu1 *N1
    dN2dt <- m*N1 - mu2*N2
    
    return(list(c(dRdt, dN1dt, dN2dt)))        
  })
}

###Mylis with partition: n <- mu+m, x <- m/n
redN_str_1_v2 <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frn <- Crn * R /(Hn+ R)  ###########here i dont have the srn since it is the only prey
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*(N1+N2)
    dN1dt <- Ern*Frn*(N1+a*N2)-n*N1
    dN2dt <- n*(x*N1 - (1-x)*N2)
    
    return(list(c(dRdt, dN1dt, dN2dt)))        
  })
}

###My structure, without the m. just direct transitions
redN_str_2 <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {

    Frn <- Crn * R /(Hn+ R)
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*(N1+N2)
    dN1dt <- Ern*Frn*N2-mu1*N1
    dN2dt <- a*Ern*Frn*N1-mu2*N2
    
    return(list(c(dRdt, dN1dt, dN2dt)))        
  })
}


### invesrted Mylius, normal reduced version. we assumed growtght and maintenance are the same
##here m means the number of children alive, per adult.. 


redN_str_3 <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frn <- Crn * R /(Hn+ R)  ###########here i dont have the srn since it is the only prey
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*(N1+N2)
    dN1dt <- m*N2 - mu1*N1
    dN2dt <- Ern*Frn*(N1+N2) -mu2 *N2
    
    
    
    return(list(c(dRdt, dN1dt, dN2dt)))        
  })
}



####################Hover flies cases######

#### HV based on Mylius structre
redN_str_1_hv <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frn <- Crn * R /(Hn+ R)  ###########here i dont have the srn since it is the only prey
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*(N1)
    dN1dt <- Ern*Frn*(N1)-m*N1 -mu1 *N1
    dN2dt <- m*N1 - mu2*N2
    
    return(list(c(dRdt, dN1dt, dN2dt)))        
  })
}

###### HV based on Mylis with partition: n <- mu+m, x <- m/n

redN_str_1_hv_2 <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frn <- Crn * R /(Hn+ R)  ###########here i dont have the srn since it is the only prey
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*(N1)
    dN1dt <- Ern*Frn*(N1) - n*N1
    dN2dt <-  n*(x*N1 - (1-x)*N2)
    
    return(list(c(dRdt, dN1dt, dN2dt)))        
  })
}

##############hover flies with my basic structrue BUT an imput from external that makes N2 survives (eats and reproduce)
redN_str_2_hv <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frn <- Crn * R /(Hn+ R)
    
    dRdt <- r*(K-R)- Frn*(N1)
    dN1dt <- m*N2 -mu1*N1
    dN2dt <- Ern*Frn*N1-mu2*N2
    
    return(list(c(dRdt, dN1dt, dN2dt)))        
  })
}

############hoverfiles with own regulation as an adult with logistic (i dont like it)

redN_self_hv <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frn <- Crn * R /(Hn+ R)  ###########here i dont have the srn since it is the only prey
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*N1
    dN1dt <- rn2 * N2 - mu1*N1
    dN2dt <-  Ern*Frn*N1 - mu2*N2*N2
    
    return(list(c(dRdt, dN1dt, dN2dt)))        
  })
}
########################################

############hoverfiles with own regulation as an adult with monomoleclar for N

redN_self_hv_mono <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frn <- Crn * R /(Hn+ R)  ###########here i dont have the srn since it is the only prey
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*N1
    dN1dt <- rn2 *Kn - mu1*N1
    dN2dt <-  Ern*Frn*N1 - rn2*N2
    
    return(list(c(dRdt, dN1dt, dN2dt)))        
  })
}
########################################

################COMPLETE FUNCTIONS@#####################33

## myl with attack rate and handling times. 
myl1 <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frp <- Arp*R/(1+Hrp*Arp*R+Hnp*Anp*N)
    Fnp <- Anp*N/(1+Hrp*Arp*R+Hnp*Anp*N)
    Frn <- Arn*R/(1+Hrn*Arn*R)
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*N - Frp*P
    dNdt <- Ern*Frn*N- Fnp*P -Mn*N
    dPdt <- Erp*Frp*P + Enp*Fnp*P -Mp*P
    return(list(c(dRdt, dNdt, dPdt)))        
  })
}

## myl with attack rate and handling times. 
myl2 <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frp <- Arp*R/(1+hrp*Arp*R+hnp*Anp*N1)
    Fnp <- Anp*N1/(1+hrp*Arp*R+hnp*Anp*N1)
    Frn <- Arn*R/(1+hrn*Arn*R)
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*(N1+N2) - Frp*P
    dN1dt <- Ern*Frn*(N1+N2)- Fnp*P -Mn*N1 - m*N1
    dN2dt <- m*N1 -Mn*N2
    dPdt <- Erp*Frp*P + Enp*Fnp*P -Mp*P
    return(list(c(dRdt, dN1dt, dN2dt, dPdt)))        
  })
}


## myl2 with attack rate and handling times. with dirrect transitions
myl2_modified <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frp <- Arp*R/(1+hrp*Arp*R+hnp*Anp*N1)
    Fnp <- Anp*N1/(1+hrp*Arp*R+hnp*Anp*N1)
    Frn <- Arn*R/(1+hrn*Arn*R)
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*(N1+N2) - Frp*P
    dN1dt <- Ern*Frn*N2- Fnp*P-Mn*N1  ## I remove the alpha and beta adding ones..
    dN2dt <- alpha*Ern*Frn*N1- Mn*N2
    dPdt <- (Erp* Frp + Enp*Fnp)*P - Mp*P
    
    return(list(c(dRdt, dN1dt, dN2dt, dPdt)))        
  })
}



###########################################################3#############3
######################################################################33
########################################################################33
# MY MODULES WIT SELECTIVTY



lblb_model <- function (t, state, parms) {
  
  with(as.list(c(state, parms)), {
    
    Frn <- Cn*R/(Hn + R)
    Frp <- Cp*S*R/(Hp + S*R + (1-S)*Nl)
    Fnp <- Cp*(1-S)*Nl/(Hp + S*R + (1-S)*Nl)
    
    # z[R, N, P] este es el orden del vector
    dPdt <- Ep*(Frp +Fnp)*P - Mup*P
    dNadt <- m*Nl- Mun*Na
    dNldt <- En*Frn*(Na+Nl)- Fnp*P-Mun*Nl -m*Nl  ##
    dRdt <- rho*(K-R)- Frn*(Na+Nl) - Frp*P
    
    return(list(c(dRdt, dNldt, dNadt, dPdt)))        
  })
}




LBHV_model <- function (t, state, parms) {  #here the difference is that the N2 predator does not feed on aphids (will die slowly)
  
  with(as.list(c(state, parms)), {
    
    Frn <- Crn*R/(Hn + R)
    Frp <- Crp*Srp*R/(Hp + Srp*R + (1-Srp)*N1)
    Fnp <- Cnp*(1-Srp)*N1/(Hp + Srp*R + (1-Srp)*N1)
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*N1 - Frp*P
    dN1dt <- I*N2 - Fnp*P-Mn1*N1
    dN2dt <- Ern*Frn*N1- Mn2*N2
    dPdt <- (Erp* Frp + Enp*Fnp)*P - Mp*P
    
    return(list(c(dRdt, dNldt, dNadt, dPdt)))        
  })
}


#######################################################33

symLBLB <- function (t, state, parms) {  #here the difference is that the N2 predator does not feed on aphids (will die slowly)
  
  with(as.list(c(state, parms)), {
    
    #Srn <- (Srp-1)*phin+1
    
    Frn <- Crn*Srn*R/(Hn + Srn*R + (1-Srn)*P)
   
    Frp <- Crp*Srp*R/(Hp + Srp*R + (1-Srp)*N)
    
    Fnp <- Cnp*(1-Srp)*N/(Hp + Srp*R + (1-Srp)*N)
    
    Fpn <- phin*Cpn*(1-Srn)*P/(Hn + Srn*R + phin*(1-Srn)*P)
    
    
    # z[R, N, P] este es el orden del vector
    
    dRdt <- r*(K-R)- Frn*N - Frp*P
    dNdt <- (Ern*Frn + Epn*Fpn)*N - Fnp*P -Mn*N
    dPdt <- (Erp*Frp + Enp*Fnp)*P - Fpn*N -Mp*P
    

    return(list(c(dRdt, dNdt, dPdt)))        
  })
}





igp_model_MOD1 <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ## here is the change and also in the nominator. 
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn + (1-phiN)*R)
    
    # z[R, N, P] este es el orden del vector
    
    
    dPdt <- Ep*((1-phiP)*(Frp +Fnp)*P + phiP*Ip*P)- mup*P
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*(Frn*(Nl+ (1-phiN)*Na) + phiN*In*Na)- (1-phiP)*Fnp*P-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*(Nl+ (1-phiN)*Na) - (1-phiP)*Frp*P
    
    
    return(list(c(dRdt, dNldt, dNadt,  dPdt)))        
  })
}




igp_model_MOD2 <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + S*R + (1-S)*Nl)  ## here is the change and also in the nominator. 
    Fnp <- Cp*(1-S)*Nl/(Hp + S*R + (1-S)*Nl)  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn + (1-phiN)*R)
    
    
    # z[R, N, P] este es el orden del vector
    
    dPadt <- mp*Pl-mup*Pa
    dPldt <- Ep*((Frp +Fnp)*Pl + Ip*Pa) -mp*Pl - mup*Pl
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*(Frn*(Nl+ (1-phiN)*Na) + phiN*In*Na)- (1-phiP)*Fnp*Pl-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*(Nl+ (1-phiN)*Na) - (1-phiP)*Frp*Pl
    
    
    return(list(c(dRdt, dNldt, dNadt,  dPldt, dPadt)))        
  })
}






















###################OLDDD




igp_model_Pst <- function (t, state, parms) {  ##NOT YET
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + S*R + (1-S)*Nl)  #########ACA ESTOY 
    Fnp <- Cp*(1-S)*Nl/(Hp +S*R + (1-S)*Nl)  

    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn + R)


    # z[R, N, P] este es el orden del vector
    dPadt <- mp*Pl-mup*Pa
    dPldt <- Ep*((Frp +Fnp)*(Pl+wp*Pa) + Ip*Pa) -mp*Pl - mup*Pl
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*(Frn*(Nl+ wn*Na) + In*Na)- Fnp*Pl-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*(Nl+ wn*Na) - Frp*(Pl+ wp*Pa)
    
    
    return(list(c(dRdt, dNldt, dNadt,  dPldt, dPadt)))        
  })
}



#################
# This model includes Ip and In as a new growth factor food income 
### this does not has P structure, and only works for PB and LB as predators. 

##############3

igp_model <- function (t, state, parms) {  ##NOT YET
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + S*R + (1-S)*Nl)  
    Fnp <- Cp*(1-S)*Nl/(Hp +S*R + (1-S)*Nl)  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn + R)
    
    
    # z[R, N, P] este es el orden del vector
   
    
    dPdt <- Ep*((Frp +Fnp)*P + Ip*P)- mup*P
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*(Frn*(Nl+ wn*Na) + In*Na)- Fnp*P-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*(Nl+ wn*Na) - Frp*P
    
    
    return(list(c(dRdt, dNldt, dNadt,  dPdt)))        
  })
}


########### IGP model where both stages in pb can take on the external ressources


###############OLDDDD##################

##############33IGO model with preference ()


igp_model_pref <- function (t, state, parms) {  ##FOR NOW ONLY FOR THE PB LB EXAMPLE
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ## here is the change and also in the nominator. 
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn + R)
    
    
    # z[R, N, P] este es el orden del vector
    
    
    dPdt <- Ep*((1-phiP)*(Frp +Fnp)*P + phiP*Ip*P)- mup*P
    
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*(Frn*(Nl+ wn*Na) + In*Na)- (1-phiP)*Fnp*P-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*(Nl+ wn*Na) - (1-phiP)*Frp*P
    
    
    return(list(c(dRdt, dNldt, dNadt,  dPdt)))        
  })
}


igp_model_pref_2 <- function (t, state, parms) {  ##FOR NOW ONLY FOR THE PB LB EXAMPLE
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ## here is the change and also in the nominator. 
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn + (1-phiN)*R)
    
    
    # z[R, N, P] este es el orden del vector
    
    
    dPdt <- Ep*((1-phiP)*(Frp +Fnp)*P + phiP*Ip*P)- mup*P
    
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*((1-phiN)*Frn*(Nl+ wn*Na) + phiN*In*Na)- (1-phiP)*Fnp*P-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*(1-phiN)*(Nl+ wn*Na) - (1-phiP)*Frp*P
    
    
    return(list(c(dRdt, dNldt, dNadt,  dPdt)))        
  })
}

############GENERAL USED MODELSSS #################333



#############################################################################33
######################### GENERAL MODEL ########################################3



#################
# This model includes Ip and In as a new growth factor food income 
# the pl and p are divided into to subpopulations
# the is no preference for the external inpuut 
##############3


igp_model_PBPB <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ## here is the change and also in the nominator. 
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn + (1-phiN)*R)
    
    # z[R, N, P] este es el orden del vector
    
    
    dPdt <- Ep*((1-phiP)*(Frp +Fnp)*P + phiP*Ip*P)- mup*P
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*((1-phiN)*Frn*(Nl+Na) + phiN*In*(Nl+Na))- (1-phiP)*Fnp*P-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- (1-phiN)*Frn*(Nl+Na) - (1-phiP)*Frp*P
    
    
    return(list(c(dRdt, dNldt, dNadt,  dPdt)))        
  })
}


igp_model_PBHV <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ## here is the change and also in the nominator. 
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn +R)
    
    # z[R, N, P] este es el orden del vector
    dPdt <- Ep*((1-phiP)*(Frp +Fnp)*P + phiP*Ip*P)- mup*P
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*(Frn*Nl + In*Na)- (1-phiP)*Fnp*P-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*(Nl) - (1-phiP)*Frp*P
    return(list(c(dRdt, dNldt, dNadt,  dPdt)))        
  })
}


igp_model_HVPB <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + S*R + (1-S)*Nl)  ## here is the change and also in the nominator. 
    Fnp <- Cp*(1-S)*Nl/(Hp + S*R + (1-S)*Nl)  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn + (1-phiN)*R)
    
    
    # z[R, N, P] este es el orden del vector
    
    dPadt <- mp*Pl-mup*Pa
    dPldt <- Ep*((Frp +Fnp)*Pl + Ip*Pa) -mp*Pl - mup*Pl
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*((1-phiN)*Frn*(Nl+Na) + phiN*In*(Nl+Na))- Fnp*Pl-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- (1-phiN)*Frn*(Nl+Na) - Frp*Pl
    return(list(c(dRdt, dNldt, dNadt,  dPldt, dPadt)))        
  })
}



igp_model_HVHV <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + S*R + (1-S)*Nl)  ## here is the change and also in the nominator. 
    Fnp <- Cp*(1-S)*Nl/(Hp + S*R + (1-S)*Nl)  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn + R)
    
    
    # z[R, N, P] este es el orden del vector
    
    dPadt <- mp*Pl-mup*Pa
    dPldt <- Ep*((Frp +Fnp)*Pl + Ip*Pa) -mp*Pl - mup*Pl
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*(Frn*Nl + In*Na)- Fnp*Pl-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*Nl - Frp*Pl
    return(list(c(dRdt, dNldt, dNadt,  dPldt, dPadt)))        
  })
}


###PARASITODI MODEL


igp_model_PBPA <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Rn))  ## here is the change and also in the nominator. 
    Frnp <- Cp*(1-S)*Rn/(Hp + (1-phiP)*(S*R + (1-S)*Rn))  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn +R)
    
    # z[R, N, P] este es el orden del vector
    
    
    dPdt <- Ep*((1-phiP)*(Frp +Frnp)*P + phiP*Ip*P)- mup*P
    dNadt <- mn*Rn- mun*Na
    dRndt <- En*(Frn*Na)- (1-phiP)*Frnp*P-murn*Rn-mn*Rn  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*Na - (1-phiP)*Frp*P
    
    
    return(list(c(dRdt, dRndt, dNadt,  dPdt)))        
  })
}


igp_model_HVPA <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + S*R + (1-S)*Rn)  ## here is the change and also in the nominator. 
    Frnp <- Cp*(1-S)*Rn/(Hp + S*R + (1-S)*Rn)  
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn +R)
    
    # z[R, N, P] este es el orden del vector
    
    
    dPadt <- mp*Pl-mup*Pa
    dPldt <- Ep*((Frp +Frnp)*Pl + Ip*Pa) -mp*Pl - mup*Pl
    dNadt <- mn*Rn- mun*Na
    dRndt <- En*(Frn*Na)- Frnp*Pl-murn*Rn-mn*Rn  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*Na -Frp*Pl
    
    
    return(list(c(dRdt, dRndt, dNadt,  dPldt, dPadt)))        
  })
}



igp_model_PAPA <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + S*R + (1-S)*Rn)  ## here is the change and also in the nominator. 
    Frnp <- Cp*(1-S)*Rn/(Hp + S*R + (1-S)*Rn)  ##here the S is gthe choice of parasitoid to parasite pararisetex or not parasited insect
    
    ### prey IGP N functional responses adult
    Frn <- Cn*R/(Hn +R)
    
    # z[R, N, P] este es el orden del vector
    
    
    dPadt <- mp*Rp-mup*Pa
    dRpdt <- Ep*((Frp +Frnp)*Pa) -mp*Rp - murp*Rp
    dNadt <- mn*Rn- mun*Na
    dRndt <- En*(Frn*Na)- Frnp*Pa-mn*Rn  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*Na -Frp*Pa
    
    
    return(list(c(dRdt, dRndt, dNadt,  dRpdt, dPadt)))        
  })
}

##################33



