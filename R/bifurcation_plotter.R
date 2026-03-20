

################################################################################
#############################CODES FOR EQUILIBI+RIA A COLORING###################
################################################################################


simple_ass_coex <- function(simpleDF, ncrit =2){

 
  ### this is just if you have a lot of points (not convergent)
  DF_NORM <-simpleDF  |>
    dplyr::group_by(across(-any_of(c("R", "Na", "Nl", "P"))))|>
    dplyr::summarise_all(mean)
  

  ###despues vamos a ver cuales son ASS (solo tienen sentido en los puntos)
  ## y vamos a juntar la ida y el regreso. Es decir, aqui pierdo info, pero dejo que es ASS
  
  ## we add the equilibri
  DF_NORM$EQR <-""
  DF_NORM$EQN <-""
  DF_NORM$EQP <-""
  
  ###chacun a son critere de maximumm...
  DF_NORM$EQR[round(DF_NORM$R/max(DF_NORM$R), ncrit) >0] <- "R"
  DF_NORM$EQN[round(DF_NORM$Nl/max(DF_NORM$Nl), ncrit) >0] <- "N"  ##here I assume no stage
  DF_NORM$EQP[round(DF_NORM$P/max(DF_NORM$P), ncrit) >0] <- "P"
  
  DF_NORM <- DF_NORM |>
    tidyr::unite("EQ", EQR:EQP, sep = "", remove = T)
  
  DF_NORM$EQ[DF_NORM$EQ ==""] <- 0
  
  DF_NORM$Rnorm <- round(DF_NORM$R/max(DF_NORM$R), ncrit) #this is the max criteria for differences (1*10-3) this is, simple criteria to distinguish two ass
  
  DF_NORM <- DF_NORM |>
    dplyr::group_by(dplyr::across(-c(direccion, R, Nl, Na, P, Rnorm, EQ))) |>  #we group by aevetyghin excepet for the
    dplyr::mutate(ASS = n_distinct(Rnorm))
  
  return(DF_NORM)
  
}



#### here is more generic with some facets (in case we want it)

## wew try the same bifurcation but with facets
plot_bif_sweep_facet <-function(full_sweep, facet_1, par_sw, xmax, combPred){
  
  # Define variable ordering for faceting
  varOrder <- c("P" = 3, "Na" = 2, "Nl" = 1 ,  "R" = 0) 
  
  #wide to long format
  bifPlot <- full_sweep |> 
  tidyr::pivot_longer(c(R, Nl, Na, P), names_to = "varName", values_to = "value") |> 
    dplyr::mutate(
      orden = case_when(
        varName %in% names(varOrder) ~ varOrder[varName])
    ) |> 
   # ggplot(aes(x = !!sym(par_sw), y = value), na.rm=TRUE) + ## the !!sym is for make the parsw a symbol 
  ggplot(aes(x = S, y = value)) + ## the !!sym is for make the parsw a symbol 
    geom_line(aes(
      group =as.factor(interaction(type, direccion)),
      #shape = as.factor(type),
      #color = as.factor(direccion)
    ), size =0.5)  +
    facet_grid(
      vars(fct_reorder(varName, orden, .desc = TRUE)), 
      #vars(!!sym(facet_1)),
      vars(K),
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
