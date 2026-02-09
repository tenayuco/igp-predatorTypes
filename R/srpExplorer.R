library(dplyr)
library(tidyr)
library(ggplot2)
##
run_equation_and_save <- function(parms, Rvalues, Nvalues) {
  # Calculate y values using the equation y = mx + b
  
  Srp <- parms["Srp"]
  Crp <- parms["Crp"]
  Cnp <- parms["Cnp"]
  Hp <- parms["Hp"]
  
  R= Rvalues
  N = Nvalues
  
  results_df<- data.frame()
  
  # Loop through each combination of x and y values
  for (R in Rvalues) {
    for (N in Nvalues) {
      # Evaluate the function F(x, y)

  Frp <- Crp*Srp*R/(Hp + Srp*R + (1-Srp)*N)
  Fnp <- Cnp*(1-Srp)*N/(Hp + Srp*R + (1-Srp)*N)
  
  results_df_t <- data.frame(R= R, N= N, Frp= Frp, Fnp= Fnp)
  
  results_df <- rbind(results_df, results_df_t)
  
  
    }}  
  
  return(results_df)
  
  # Create a dataframe with x and y values
  
  
  # Return the dataframe
  return(results_df)
}

plotSrp <- function(D_F){
  plotDF<- D_F %>%
  ggplot(aes(x= R, y =Fnp)) +
  geom_line(aes(colour = as.factor(N)))+
  facet_wrap(~Srp, labeller = label_both)+
  scale_color_viridis_d() +
  theme_bw()
  return(plotDF)
}




DF_eq_Srp <- data.frame()

for (SrpV in seq(0, 1, 0.2)){
  param <- c(Srp=0.5, Crp=10, Cnp=10, Hp =1)
  param["Srp"] <- SrpV
  
  DF_eq <- run_equation_and_save(parms = param, Rvalues = seq(0, 10, 1), Nvalues = seq(0, 10, 1))
  DF_eq$Srp <- SrpV
  DF_eq_Srp <- rbind(DF_eq_Srp, DF_eq)
  
}

#DF_eq_Srp_g <- DF_eq_Srp %>%
 # gather(key= "variable", value= "value" , -c(Frp, Fnp, Srp))

muestraSrp <- plotSrp(D_F= DF_eq_Srp)
muestraSrp


ggsave(muestraSrp,filename=paste("./Documents/enemyCocktail/structureModels/output/SrpExplorer/", "muestraFnp_R.png", sep=""),  height = 8, width = 12, create.dir = T)



##############simple

###########simple case 

param <- c(Srp=0.5, Crp=10, Cnp=10, Hp =1)

DF_eq <- run_equation_and_save(parms = param, Rvalues = seq(0, 10, 1), Nvalues = seq(0, 10, 1))
DF_eq$Srp <- param["Srp"]

DF_eq_g <- DF_eq %>%
  gather(key= "variable", value= "value" , -c(Frp, Fnp, Srp))

plotSrp(D_F= DF_eq_g)
##################################3



