
#clean this code!!

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


#this one plot the time series  for different parameters value

ts_plot <- function(outDF, xmax, facet_1, plotted_var = c("R", "N", "P")){
  
  outLong <-  outDF  %>%
    gather(key = "varName", "value",-time, -parameterValue)
  #gather(key = "varName", "value", R, N, P)
  
  RNP_timeSeries <- outLong %>%
    mutate(orden=case_when(varName=="P" ~ 6,
                           varName=="Pa" ~ 5,#this ones works cause they are in a if 
                           varName=="Pl" ~ 4,
                           varName=="Rp" ~ 4,
                           varName=="Na" ~3,
                           varName=="Nl" ~2,
                           varName=="Rn" ~2,
                           varName=="N" ~1,
                           varName == "R" ~0))%>%
    filter(varName %in% plotted_var)%>%
    ggplot(aes(x=time, y=value)) +
    geom_line(color="black", linewidth=1) + 
    xlab("Time") +
    xlim(0, xmax)+
    facet_grid(fct_reorder(varName, orden, .desc = TRUE)~parameterValue, scales = "free")+
    
    scale_color_colorblind()+
    theme_bw()+ 
    #ggtitle(label= "", subtitle = as.character(facet_1)) +
    theme(plot.subtitle = element_text(hjust = 0.5, size = 12))+
    theme(text = element_text(size = 12),
          axis.text.x=element_text(angle=60, hjust=1, size = 12))+
    theme(axis.text.y=element_text(size = 12))+
    
    
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.text.y  = element_blank()
    ) +
  theme(legend.position = "none")+
    labs(y="", x= "Time") # this depends on the sr parameter 
  

  return(RNP_timeSeries)
}

#############3plot specific valuesss. 

ts_plot_p2 <- function(outDF, li_pair, plotted_var = c("R", "N", "P"), tmax){
  
  par1_name<- names(li_pair$pair1)[1]
  par2_name<- names(li_pair$pair1)[2]
  
  outLong <-  outDF  %>%
    gather(key = "varName", "value",-time, -par1_name, -par2_name)
  #gather(key = "varName", "value", R, N, P)
  
  RNP_timeSeries <- outLong %>%
    mutate(orden=case_when(varName=="P" ~ 6,
                           varName=="Pa" ~ 5,#this ones works cause they are in a if 
                           varName=="Pl" ~ 4,
                           varName=="Na" ~3,
                           varName=="Nl" ~2,
                           varName=="N" ~1,
                           varName == "R" ~0))%>%
    filter(varName %in% plotted_var)%>%
    ggplot(aes(x=time, y=value)) +
    geom_line(aes(col=varName), linewidth=1) + 
    xlab("Time") +
    xlim(0, tmax)+
    facet_wrap(vars(!!sym(par1_name), !!sym(par2_name)),  # Facet by both parameters
      scales = "free", 
      labeller = label_both  # This adds both variable name and value
    ) +
   # facet_grid(fct_reorder(varName, orden, .desc = TRUE)~par1_name*par2_name, scales = "free")+
    scale_color_colorblind()+
    theme_bw()+ 
    #ggtitle(label= "", subtitle = as.character(facet_1)) +
    theme(plot.subtitle = element_text(hjust = 0.5))+
    theme(axis.text.x=element_text(angle=60, hjust=1))
  
  RNP_timeSeries
  
  return(RNP_timeSeries)
}


################################################################################
#############################BIFURCATION PLOTTERS###################
################################################################################


###so this function plot the output of the debif function after some operators changes

plot_debif <-function(full_debif, onlyStab=TRUE){
  
  long_debif_DF <- full_debif %>%
    gather(key = "varName", "value", R, N, P)
  
  if (onlyStab ==TRUE) {
    long_debif_DF <- long_debif_DF %>%
      filter(stability == TRUE)
  }
  
  BF_plot <-long_debif_DF%>%
    ###CHECK WICH MUTATE IS THIS
    
    mutate(orden=case_when(varName=="P" ~ 3,
                           varName=="N" ~2,
                           varName == "R" ~1))%>%
    ggplot(aes(x=K, y=value), na.rm=TRUE) + 
    geom_point(aes(color= as.factor(stability)))+
    facet_wrap(~fct_reorder(varName, orden, .desc = TRUE), ncol = 1, scales = "free")+
    scale_shape_manual(values= c(1, 19)) +
    xlim(0, 100)+
    theme_classic()
  return(BF_plot)
}





## the [plot]
plot_min_max <-function(min_max_df){
  
  BF_minMax <-min_max_df %>%
    mutate(orden=case_when(varName=="P" ~ 3,
                           varName=="N" ~2,
                           varName == "R" ~1))%>%
    ggplot(aes(x=parmvalue, y=value), na.rm=TRUE) + 
    geom_point(aes(color= as.factor(condInit)))+
    facet_wrap(~fct_reorder(varName, orden, .desc = TRUE), ncol = 1, scales = "free")+
    
    #geom_point(shape=19, size=.5, stroke=0, alpha=.1, color="red") +
    #geom_point(shape=19, size=1, stroke=0, alpha=.1, color="black") +
    scale_shape_manual(values= c(1,2, 19))+
    scale_x_continuous("Charge capacity K") +
    #scale_color_viridis_d()+
    # scale_color_manual(values = c("black", "blue", "red"))+
    #scale_x_continuous(expression(paste("") +
    theme_classic()
  return(BF_minMax)
}

###############IMPORTNAT

plot_bif_sweep <- function(full_sweep, par_sw, plotVar) {
  
  #we remove the rnorm column
  
  # Define variable ordering for faceting
  varOrder <- c("P" = 3, "Na" = 2, "Nl" = 1, "N" = 1, "R" = 0)
  
  # Convert to long format and create plot
  bifPlot <- full_sweep %>%
    tidyr::gather(key = "varName", value = "value", -any_of(c(par_sw, "direccion", "type", "ASS", "EQ", "Rnorm"))) %>%  #the all_off is a inmportant to select the variables we want (or dont want) and dont confuse them 
    mutate(
      orden = case_when(
        varName %in% names(varOrder) ~ varOrder[varName],
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::filter(varName %in% (plotVar))%>%
    ggplot(aes(x = !!sym(par_sw), y = value)) + ## the !!sym is for make the parsw a symbol 
    geom_point(aes(
      shape = as.factor(type),
      color = as.factor(direccion)
    )) +
      facet_wrap(
        ~ fct_reorder(varName, orden, .desc = TRUE),
        ncol = 1,
        scales = "free"
      ) +
      scale_shape_manual(values = c(19, 1)) +
      scale_color_viridis_d(
        option = "inferno",
        end = 0.8,
        direction = -1
      ) +
      theme_classic()
  return(bifPlot)
}



#### here is more generic with some facets (in case we want it)

## wew try the same bifurcation but with facets
plot_bif_sweep_facet <-function(full_sweep, facet_1, par_sw, plotVar, xmax){
  
  # Define variable ordering for faceting
  varOrder <- c("Pa_Pl"=5, "P" = 5, "Pa" = 4, "Pl" = 3, "N" = 2, "Na" = 2, "Nl" = 1 , "Rn"=1,  "R" = 0)
  
  #wide to long format
  bifPlot <- full_sweep %>%
   # filter(type=="1-point")%>% ## to remove non finished dynamics, for instance. 
  tidyr::gather(key = "varName", value = "value", -any_of(c(par_sw, facet_1, "direccion", "type","EQ", "ASS", 'Rnorm', 'minMax'))) %>%  #the all_off is a inmportant to select the variables we want (or dont want) and dont confuse them 
    mutate(
      orden = case_when(
        varName %in% names(varOrder) ~ varOrder[varName],
        TRUE ~ NA_real_
      )
    ) %>%   
    dplyr::filter(varName %in% (plotVar))%>% ##here just in case I have more variables.. 
    #and then directly to the ggplot 
    ggplot(aes(x = !!sym(par_sw), y = value), na.rm=TRUE) + ## the !!sym is for make the parsw a symbol 
    geom_line(aes(
      group =as.factor(interaction(minMax, type, direccion)),
      #shape = as.factor(type),
      #color = as.factor(direccion)
    ), size =0.5) +
    facet_grid(
      vars(fct_reorder(varName, orden, .desc = TRUE)), 
      vars(!!sym(facet_1)),
      scales = "free") +
    #scale_y_discrete(breaks = full_sweep$value) +
    scale_shape_manual(values = c(19, 1)) +
    scale_color_viridis_d(
      option = "inferno",
      end = 0.8,
      direction = -1
    ) +
    theme_bw()+
   # ggtitle(label= "", subtitle = as.character(facet_1)) +
    #theme(plot.subtitle = element_text(hjust = 0.5))+
    theme(  axis.text = element_text(size= 12),       # Change tick labels for all axes
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.text.y  = element_blank()
    )+
    labs(y="", x= "IGP Symmetry (S)") # this depends on the sr parameter 
  
  
  
    xlim(0, xmax)

  return(bifPlot)
}

add_eq_gray <-function(gg_plot, par_sw, reso=1){  ##In progress
 newPlot <- gg_plot + geom_rect(aes(xmin=!!sym(par_sw), 
                         xmax =!!sym(par_sw)+reso, 
                         ymin = 0, 
                         ymax = Inf, 
                         fill = EQ, 
                         alpha= EQ))+ ## que cada rectangulo use todo su valor
 
   
   # I can change this if needed

   scale_alpha_manual(values = colsEQ <- c("0"= 0, 
                                          "R"= 0, 
                                          "RN"= 0, 
                                          "RNP"= 0.4, 
                                          "RP"=0))+
   
   
  scale_fill_manual(values = colsEQ <- c("0"= "white", 
                                        "R"= "#e6e6e6", 
                                     "RN"= "#b5b5b5", 
                                     "RNP"=  "darkgreen", #ffff67ff", #"#00b61bff", "#18474dff",
                                    "RP"="gray"))+
  geom_point(aes(shape= as.factor(type)), color= "black")+
   geom_line(aes(group=interaction(type, direccion, minMax)), color= "black")+
   
   theme(legend.position="none") 
 
 return(newPlot)
}



add_eq <-function(gg_plot, par_sw, reso=1){  ##In progress
  newPlot <- gg_plot + geom_rect(aes(xmin=!!sym(par_sw), 
                                     xmax =!!sym(par_sw)+reso, 
                                     ymin = 0, 
                                     ymax = Inf, 
                                     fill = EQ), alpha=0.2)+ ## que cada rectangulo use todo su valor
    
    scale_fill_manual(values = colsEQ <- c("0"= "white", 
                                           "R"= "gray", 
                                           "RN"= "blue", 
                                           "RNP"= "darkgreen", 
                                           "RP"="yellow"))+
    geom_point(aes(shape= as.factor(type)), color= "black")
  
  return(newPlot)
}

##########################double sweep ploters
###############################################3



p_doubleBif <-function(doubleDF, par1, par2, par1lim, par2lim){
  
  
  DF <- doubleDF
  
  
  
  plot_double <- ggplot(DF, aes(x=parm1Value, y=parm2Value))+
    geom_tile(aes(fill=as.factor(EQ), alpha=R))+
    scale_fill_manual(values = colsEQ <- c("0"= "white", 
                                           "R"= "gray", 
                                           "RN"= "blue", 
                                           "RNP"= "darkgreen", 
                                           "RP"="yellow"))+
    # scale_alpha(range=c(0,max(doubleDF$R)), limits=c(0,10), na.value=1)+
    geom_point(data=subset(DF, type == "1-point" & ASS>1),
               aes(x=parm1Value, y=parm2Value, size=ASS))+
    facet_grid(~direccion*corte)+
    # scale_color_manual(values = c("TRUE"="black"))+
    labs(title = "Coexistence and R value",
         x = par1,
         y = par2) +
    theme_bw()
  
  return(plot_double)
}




p_doubleBif_hatch <-function(doubleDF, li_par){
  
  
  li1 <- li_par[[1]]
  li2 <- li_par[[2]]
  par1 <- li1[["parSw"]]
  par2 <- li2[["parSw"]]
  
  DF <- doubleDF
  
  
  plot_double <- ggplot(DF, aes(x = !!sym(par1), y = !!sym(par2))) +
    geom_tile_pattern(aes(alpha=R,
                          fill= as.factor(EQ), 
                          pattern= as.factor(type)),
                      color = "lightgray", 
                      pattern_fill = "black",
                      pattern_angle = 45,
                      pattern_density = 0.1,
                      pattern_spacing = 0.025,
                      pattern_key_scale_factor = 0.6) +
    scale_fill_manual(values = colsEQ <- c("0"= "white", 
                                           "R"= "gray", 
                                           "RN"= "blue", 
                                           "RNP"= "darkgreen", 
                                           "RP"="yellow"))+
    scale_pattern_manual(values = c("1-point"="none", "2-other" = "stripe"))+
    # scale_alpha(range=c(0,max(doubleDF$R)), limits=c(0,10), na.value=1)+
    
    geom_point(data=subset(DF, type == "1-point" & ASS>1),
               aes(x = !!sym(par1), y = !!sym(par2)), color= "black")+
    
    facet_grid(direccion~corte)+
    # scale_color_manual(values = c("TRUE"="black"))+
    labs(fill= "Coexistence",
         alpha= "R value",
         color = "ASS",
         x = par1,
         y = par2) +
    ggtitle(label= "", subtitle = "Coexistence and R value") +
    theme(plot.subtitle = element_text(hjust = 0.5))+
    guides(pattern = guide_legend(override.aes = list(fill = "white")),
           fill=guide_legend(override.aes=list(pattern="none")),
           alpha=guide_legend(override.aes=list(pattern="none")))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
    ) 
  
  return(plot_double)
}



p_doubleBif_hatch_sum <-function(doubleDF, li_par){
  
  
  li1 <- li_par[[1]]
  li2 <- li_par[[2]]
  par1 <- li1[["parSw"]]
  par2 <- li2[["parSw"]]
  
  DF <- doubleDF
  
  
  plot_double <- ggplot(DF, aes(x=!!sym(par1), y=!!sym(par2),  ##esto para que cache que son simboloss
  ))+
    geom_tile_pattern(aes(fill= as.factor(EQ), 
                          pattern= as.factor(type)),
                      alpha = 0.5, 
                      color = "lightgray", 
                      pattern_fill = "black",
                      pattern_angle = 45,
                      pattern_density = 0.1,
                      pattern_spacing = 0.025,
                      pattern_key_scale_factor = 0.6) +
    geom_point(data=subset(DF, type == "1-point" & ASS>1),
               aes(x = !!sym(par1), y = !!sym(par2)), color= "black")+
   
    scale_fill_manual(values = colsEQ <- c("0"= "white", 
                                           "R"= "gray", 
                                           "RN"= "blue", 
                                           "RNP"= "darkgreen", 
                                           "RP"="yellow", 
                                           "ASS" = "black"))+
    scale_pattern_manual(values = c("1-point"="none", "2-other" = "stripe"))+
    labs(fill= "Coexistence",
         # pattern = "Stability"
         x = par1,
         y = par2) +
    ggtitle(label= "", subtitle = "Coexistence Summary") +
    theme(plot.subtitle = element_text(hjust = 0.5))+
    guides(pattern = guide_legend(override.aes = list(fill = "white")),
           fill=guide_legend(override.aes=list(pattern="none")),
           alpha=guide_legend(override.aes=list(pattern="none")))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          #panel.background = element_blank()
    ) 
  
  return(plot_double)
}




#####
############# rquilibrim values plotter


heatMap_eq <-function(eq_DF, par1, par2, varia_plot, onlyCoex =FALSE){
  

  if (onlyCoex ==TRUE){
    eq_DF <- eq_DF %>%
      filter(coexistence ==1)
  }
  
  
  DF <- eq_DF %>%
    filter(variable %in% varia_plot)
  
  
  
  
  
  
  plot_double <- ggplot(DF, aes(x=!!sym(par1), y=!!sym(par2),  ##esto para que cache que son simboloss
  ))+
    geom_tile(aes(fill= valueMean),
                      alpha = 1) +

    labs(fill= "EquValue",
         # pattern = "Stability"
         x = par1,
         y = par2) +
    facet_wrap(~comb)+
    scale_fill_viridis_c()+
    ggtitle(label= "", subtitle = "Equili values") +
    theme(plot.subtitle = element_text(hjust = 0.5)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
          #panel.border = element_blank(),
          #panel.background = element_blank()
    ) 
  
  return(plot_double)
}


#################################EXPER





p_doubleBif_debif <-function(doubleDF, li_par, criteria = c("neg_all", "pos_one")){
  
  
  li1 <- li_par[[1]]
  li2 <- li_par[[2]]
  par1 <- li1[["parSw"]]
  par2 <- li2[["parSw"]]
  
  DF <- doubleDF %>%
    filter(!!sym(par1) >0) %>%
    filter(!!sym(par2) >0)%>%
    filter(eigenCriteria %in% criteria)
  
  #%>%
   # filter(typeBif == "BP")
  
  
  plot_double <- ggplot(DF, aes(x=!!sym(par1), y=!!sym(par2),  ##esto para que cache que son simboloss
  ))+
    
    #geom_line(aes(group =as.factor(BC), linetype = typeBif))+  #linetype = "dashed"
    geom_point(aes(fill=typeBif), shape= 21)+
    labs(fill= "Coexistence",
         # pattern = "Stability"
         x = par1,
         y = par2) +
    ggtitle(label= "", subtitle = "Coexistence Summary") +
    theme(plot.subtitle = element_text(hjust = 0.5))+
    guides(pattern = guide_legend(override.aes = list(fill = "white")),
           fill=guide_legend(override.aes=list(pattern="none")),
           alpha=guide_legend(override.aes=list(pattern="none")))+
    scale_color_manual(values = c("neg_all"= "black", 
                                           "pos_one"= "gray" 
                                           ))+
    scale_fill_manual(values = c("BP"= "black",
                                     "LP"= "blue",
                                     "HP" = "red"
    ))+
    scale_x_continuous(limits = c(0, max(DF[[par1]])), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, max(DF[[par2]])), expand = c(0, 0)) +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          #panel.background = element_blank()
    ) 
  
  return(plot_double)
}



##################bio control PLOT
biocontrol_plotter_viejo <-function(data_biocon, kvalue =2){
  
  
  DF <- data_biocon
  
  DF <- DF[c(TRUE, FALSE, FALSE, FALSE), ] #this remove every two lines in general. 
  
  order <- c("LB", "PB", "HV", "PA")
  
  DF_PLOT <- DF %>%
    filter(K == kvalue) %>%
    filter(IpCat != "IpMed") %>%
    filter(InCat != "InMed") %>%
    filter(combPred != "PA.PA")%>%
    filter(combPred != "HV.LB")%>%
    filter(combPred != "LB.HV")%>%
    filter(combPred != "HV.HV")%>%
    filter(combPred != "HV.PB")%>%
    filter(combPred != "PB.HV")%>%
    filter(combPred != "HV.PA")%>%
    tidyr::separate(combPred, c("IGP_P", "IGP_N"), remove = FALSE)%>%
    mutate(
      IGP_N = factor(IGP_N, levels = order),
      IGP_P = factor(IGP_P, levels = order),
      fill_color = ifelse(EQ == "RNP", paste(IpCat, InCat, sep = "_"), "NoCol")) %>%
    ggplot(aes(x = meanR, y = S)) +
    geom_path(aes(group = interaction(IpCat, InCat)), color= "black")+  #quite la dire
    geom_point(aes(fill = fill_color, shape= EQ), size=3) +
    #facet_grid(IGP_N~IGP_P)+
    #facet_wrap(~combPred, ncol=3) +
    scale_fill_manual(values = c("0_0"= "black", 
                                 "0_InMax"= "orange", 
                                 "IpMin_InMin"= "green", 
                                 "IpMax_InMin"= "darkolivegreen1",
                                 "0_InMin"= "yellow",
                                 "IpMin_InMax"= "darkolivegreen", 
                                 "IpMax_InMax"= "darkgreen",
                                 "IpMin_0"= "blue",
                                 "IpMax_0"= "darkblue",
                                 "NoCol" = "white"))+
    scale_shape_manual(values = c("R" = 22,
                                  "RN"= 23, 
                                  "RP" = 24,
                                  "RNP"= 21))+
    
    ggtitle(label = paste("Biocontrol", "K (productivity) =", kvalue, sep = " "), subtitle = "Biocontrol") +
    theme_bw() +
    theme(
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(legend.position = "none")
  
  
  
  return(DF_PLOT)
}



biocontrol_plotter<-function(data_biocon, kvalue =2){
  
  
  DF <- data_biocon
  
  DF <- DF[c(TRUE, FALSE, FALSE, FALSE), ] #this remove every two lines in general. 
  
  order <- c("LB", "PB", "HV", "PA")

  
  DF_PLOT <- DF %>%
    tidyr::separate(combPred, c("IGP_P", "IGP_N"), remove = FALSE)%>%
    #filter(K == kvalue) %>%
    filter(IpCat != "IpMed") %>%
    filter(InCat != "InMed") %>%
    filter(IGP_P != "HV")%>%
    filter(IGP_P != "PA")%>%
    filter(IGP_N != "HV")%>%
    filter(EQ == "RNP")%>%
    filter(IpCat != "IpMax")%>% #we remove it cause it just one observation! at very low S
    mutate(
      IGP_N = factor(IGP_N, levels = order),
      IGP_P = factor(IGP_P, levels = order))   %>%  ##TOdo esto se puede quitar
    ggplot(aes(x = S, y = meanR)) +
    
    geom_point(aes(fill = combPred, shape = InCat), color="black", size=3) +

    facet_wrap(~K, ncol=3) +
    scale_fill_viridis_d()+


   scale_shape_manual(values = c("0"= 21,
                                 "InMin" = 24,
                                 "InMax"= 22) 
                      ) +
    
    
    # Create combined legend for shape and fill
    guides(
      fill = guide_legend(override.aes = list(shape = 21)),  # Set default shape for fill legend
      shape = guide_legend(override.aes = list(fill = "gray50"))) +

    labs(title = "Biocontrol", subtitle = "K (productivity)",  x = "S (IGP gradient)", 
         y = "Ressource R at coexistence equilibrium (RNP)", 
         fill= "Predator combination", 
         shape="External food for the predator N") +
    theme_bw() +
    theme(
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) 
  
 
  
  return(DF_PLOT)
}

biocontrol_plotter_GUILDS<-function(data_biocon, RNP_var= "meanR"){
  
  #new version oct 31
  
    order <- c("LB", "PB", "HV", "PA")
  
  
  DF <- data_biocon
  
  DF <- DF[c(TRUE, FALSE), ] #this remove every two lines in general. 
  DF <- DF[c(TRUE, FALSE), ] #this remove every two lines in general. 
  DF <- DF[c(TRUE, FALSE), ] #this remove every two lines in general. 
  
  
  DF_PLOT <- DF %>%
   # filter(combPred == chosen_comb)%>%
     tidyr::separate(combPred, c("IGP_P", "IGP_N"), remove = FALSE)%>%
    filter(K %in% c(4)) %>%
  # filter(IGP_P != "HV")%>%
    filter(IGP_P != "PA")%>%
   # filter(IGP_N != "HV")%>%
    filter(IpCat != "IpMed")%>%
   filter(IpCat != "IpMax")%>% #cause yeah, it does not works
    filter(InCat != "InMed")%>%#we remove it cause it just one observation! at very low S
    mutate(
      IGP_N = factor(IGP_N, levels = order),
      IGP_P = factor(IGP_P, levels = order)) %>%
    unite("IpCat_InCat", IpCat:InCat, remove = F) %>%
    mutate(fill_color = ifelse(EQ == "RNP", IpCat_InCat, "NoCol")) %>%
    ggplot(aes(x = S, y = !!sym(RNP_var))) +
    #ggplot(aes(x =S, y = meanR))+
    #geom_line(aes(group = interaction(InCat, K), color = as.factor(K), linetype = as.factor(K)))+
    
    geom_line(aes(group = interaction(IpCat_InCat, K)), color= "black")+
    geom_point(aes(fill = as.factor(fill_color), shape= EQ), size=3, color="black") +
    
    facet_grid(IGP_N~IGP_P)+
      
    scale_shape_manual(values = c("RN" = 22, 
                                  "RP" =  24, 
                                  "RNP" = 21)) +
    
    scale_fill_manual(values = c("0_0"= "black",
                                 "0_InMin" = "#78c3d0ff",
                                 "0_InMax" = "#18474dff",
                                 "IpMin_0" = "#ffff67ff",
                                 "IpMax_0" = "orange",
                                 "IpMin_InMin" = "lightgreen",
                                 "IpMin_InMax" = "#00b61bff",     
                                 "IpMax_InMin" = "red",
                                 "IpMax_InMax" = "#bcffc9ff", 
                                 "NoCol" = "white"
                                  )) +

    #scale_fill_viridis_d(option = "inferno", begin = 0.4, end = 0.8, direction = -1)+
    scale_color_viridis_d(option = "inferno", begin = 0.4, end = 0.8, direction = -1) +
  
    
    labs(x = " IGP Symmetry (S)", 
         y = paste0( "Resource R at coexistence equilibrium (RNP)"), 
         color= "Productivity K",
         fill= "Productivity K", 
         linetype = "Productivity K",
         alpha= "External food for IGP Prey N", 
         shape="External food for IGP Prey N") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill="white"), 
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "None",
    ) 
  
  
  
  return(DF_PLOT)
}




biocontrol_plotter_sameI<-function(data_biocon){
  
  
  DF <- data_biocon
  
 # DF <- DF[c(TRUE, FALSE), ] #this remove every two lines in general. 
  #DF <- DF[c(TRUE, FALSE), ] #this remove every two lines in general. 
  
  order <- c("LB", "PB", "HV", "PA")
  
  DF_ag_eq <- DF %>%
    filter(IpCat != "IpMed")%>%
    filter(InCat != "InMed")%>%#we remove it cause it just one observation! at very low S
    #filter(EQ == "RNP")%>%  #agarro solo el equilibrio
    select(!IpCat)%>%
    select(!InCat)%>%
    select(!EQ)%>%
    select(!type)%>%
    select(!direccion)%>%
    dplyr:: group_by(K, combPred, S)%>%
    dplyr::summarise_all(list(mean, sd))
  
  DF_PLOT <- DF_ag_eq %>%
    tidyr::separate(combPred, c("IGP_P", "IGP_N"), remove = FALSE)%>%
    filter(IGP_P != "HV")%>%
    filter(IGP_P != "PA")%>%
    filter(IGP_N != "HV")%>%
    mutate(
      IGP_N = factor(IGP_N, levels = order),
      IGP_P = factor(IGP_P, levels = order)) %>%
    
    #alpha_color = ifelse(EQ == "RNP", "COEX", "NoCol")) %>%
    #filter(IpCat != "IpMax")%>% #we remove it cause it just one observation! at very low S
    ggplot(aes(x = S, y = meanR_fn1)) +
    
    geom_ribbon(aes(fill= as.factor(K), ymin=meanR_fn1- meanR_fn2, ymax=meanR_fn1+ meanR_fn2), alpha=0.4)+
    geom_line(aes(color = as.factor(K), linetype = as.factor(K)))+
    
    #scale_colour_colorblind()+
    #scale_fill_colorblind()+
    
  #  scale_fill_gradient(low = "yellow", high = "red", na.value = NA)
   # scale_color_gradient(low = "yellow", high = "red", na.value = NA)

    scale_fill_viridis_d(option = "inferno", begin = 0.4, end = 0.8, direction = -1)+
    scale_color_viridis_d(option = "inferno", begin = 0.4, end = 0.8, direction = -1) +
    
    #geom_line(aes(group = InCat), color="black")+
   # geom_point(aes(fill = as.factor(K), alpha = alpha_color), shape=21, color="black", size=3) +
    
    facet_grid(IGP_N~IGP_P)+
   
    
    labs(subtitle = "IGP Predator P",  x = " IGP Symmetry (S)", 
         y = "Resource R at coexistence equilibrium (RNP)", 
         fill= "Productivity (K)", 
         col= "Productivity (K)", 
         linetype = "Productivity (K)") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill="white"), 
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) 
  
  
  
  return(DF_PLOT)
}



biocontrol_plotter_all_Var<-function(data_biocon, chosen_comb = "PB.PB", chosen_K =4, excluded_Ip, excluded_In){
  
  order <- c("meanP", "meanN", "meanR")
  orderIn <- c("InMin", "InMax")
  
  
  DF <- data_biocon
  
 
  
  DF_SHORT <- DF %>%
    filter(combPred == chosen_comb)%>%
    filter(K == chosen_K)%>%
    filter(!(IpCat %in% excluded_Ip))%>%
   # filter(meanN < 10)%>%  ### IMPRTATN E QUITE PARA VISUA
    
    filter(!(InCat %in% excluded_In))%>%#we remove it cause it just one observation! at very low S
    #filter(EQ == "RNP")%>%  #agarro solo el equilibrio
    select(c(K, combPred, S, meanR, meanN, meanP, In, Ip, IpCat, InCat, direccion))
  
  
  DF_LONG <-  DF_SHORT %>%
    gather(key = "meanVariable", value = "value",
                    meanR, meanN, meanP)
  
  #DF_LONG <- DF_LONG %>%
   # dplyr::group_by(K, combPred, S, In, Ip, IpCat, InCat, meanVariable)%>%
    #mutate(normValue = value/max(value))
  
  
    
  DF_LONG <- DF_LONG %>%
    filter(combPred == chosen_comb)%>%
    filter(K == chosen_K)%>%
    tidyr::separate(combPred, c("IGP_P", "IGP_N"), remove = FALSE)%>%
    filter(IGP_P != "HV")%>%
    filter(IGP_P != "PA")%>%
    filter(IGP_N != "HV")%>%
    # filter(K == kvalue) %>%
    #filter(EQ == "RNP")%>%
    filter(IpCat != "IpMed")%>%
    filter(IpCat != "IpMax")%>% #cause yeah, it does not works
    filter(InCat != "InMed")%>%#we remove it cause it just one observation! at very low S
    mutate(
      meanVariable = factor(meanVariable, levels = order))%>%
  mutate(
    InCat = factor(InCat, levels = orderIn))
  
 # DF_LONG_FEW <- DF_LONG[c(TRUE, FALSE), ] #this remove every two lines in general. 
  #DF_LONG_FEW <- DF_LONG_FEW[c(TRUE, FALSE), ] #this remove every two lines in general. 
  #DF_LONG_FEW <- DF_LONG_FEW[c(TRUE, FALSE), ]
  #DF_LONG_FEW <- DF_LONG_FEW[c(TRUE, FALSE), ]
  
 
  DF_PLOT <- DF_LONG %>% ggplot(aes(x = S, y = value)) +

    #geom_line(aes(group = interaction(InCat, K), color = as.factor(K), linetype = as.factor(K)))+
    
   
   #geom_point(data = DF_LONG_FEW, aes(shape= InCat), fill="black", color= "black", alpha= 1,  size=2) +
    geom_line(aes(group = interaction(InCat, direccion)), color="black", size=1)+
    
    facet_grid(rows = vars(meanVariable), scales = "free_y") +
    #ylim(0, 10)+
  
    scale_color_viridis_d(option = "inferno", begin = 0.4, end = 0.8, direction = -1) +
    
    #scale_shape_manual(values = c("0"= 21,
     #                             "InMin" = 24,
      #                            "InMax"= 22, 
       #                           "NoCol"= NA)) +
    
    labs(x = " IGP Symmetry (S)", 
         y = paste0( ""), 
         color= "Productivity K",
         fill= "Productivity K", 
         linetype = "Productivity K",
         alpha= "External food for IGP Prey N", 
         shape="External food for IGP Prey N") +
    theme_bw() +
    theme(
     # strip.background = element_rect(fill="white"), 
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "None",
     strip.text = element_blank(),
     strip.background = element_blank()
    ) 
  
  
  
  return(DF_PLOT)
}


