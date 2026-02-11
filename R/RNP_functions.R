######################### GENERAL MODEL ########################################3

#################
# This model includes Ip and In as a new growth factor food income 
# the pl and p are divided into to subpopulations
##############


igp_model_PBPB <- function (t, state, parms) {##includes PB, LB as predators and as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ##the value of phiP changes according to the model
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    ### prey IGP N functional response
    Frn <- Cn*R/(Hn + (1-phiN)*R)
    
    #these are the equations 
    dPdt <- Ep*((1-phiP)*(Frp +Fnp)*P + phiP*Ip*P)- mup*P
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*((1-phiN)*Frn*(Nl+Na) + phiN*In*(Nl+Na))- (1-phiP)*Fnp*P-mun*Nl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- (1-phiN)*Frn*(Nl+Na) - (1-phiP)*Frp*P
    
    # z[R, Nl, Na, P] is the order of the vector
    return(list(c(dRdt, dNldt, dNadt,  dPdt)))        
  })
}


igp_model_PBHV <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
   ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ##the value of phiP changes according to the model
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    ### prey IGP N functional response
    Frn <- Cn*R/(Hn + (1-phiN)*R)

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
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ##the value of phiP changes according to the model
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    ### prey IGP N functional response
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
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ##the value of phiP changes according to the model
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    ### prey IGP N functional response
    Frn <- Cn*R/(Hn + (1-phiN)*R)
    
    
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
     ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ##the value of phiP changes according to the model
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    ### prey IGP N functional response
    Frn <- Cn*R/(Hn + (1-phiN)*R)
    
    # z[R, N, P] este es el orden del vector
    
    
    dPdt <- Ep*((1-phiP)*(Frp +Fnp)*P + phiP*Ip*P)- mup*P
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*(Frn*Na)- (1-phiP)*Fnp*P-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*Na - (1-phiP)*Frp*P
    
    
    return(list(c(dRdt, dNldt, dNadt,  dPdt)))        
  })
}


igp_model_HVPA <- function (t, state, parms) {##includes PB, LB as predators, PB,LB and HV as preys
  
  with(as.list(c(state, parms)), {
    ##the i is the same for both predators 
    
    ###predator IGP P functional responses
    Frp <- Cp*S*R/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  ##the value of phiP changes according to the model
    Fnp <- Cp*(1-S)*Nl/(Hp + (1-phiP)*(S*R + (1-S)*Nl))  
    ### prey IGP N functional response
    Frn <- Cn*R/(Hn + (1-phiN)*R)
    # z[R, N, P] este es el orden del vector
    
    
    dPadt <- mp*Pl-mup*Pa
    dPldt <- Ep*((Frp +Fnp)*Pl + Ip*Pa) -mp*Pl - mup*Pl
    dNadt <- mn*Nl- mun*Na
    dNldt <- En*(Frn*Na)- Fnp*Pl-mn*Nl  ## I remove the alpha and beta adding ones..
    dRdt <- rho*(K-R)- Frn*Na -Frp*Pl
    
    
    return(list(c(dRdt, dNldt, dNadt,  dPldt, dPadt)))        
  })
}



##################33



