

#############################################################################33
######################### GENERAL MODEL ########################################3

#################
# This model includes Ip and In as a new growth factor food income 
# the pl and p are divided into to subpopulations
# there is no preference for the external inpuut 
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



