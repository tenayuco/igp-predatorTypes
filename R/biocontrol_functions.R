##Procedure to new figure of biocontrol

############FUNCTIONS RELATED TO BIOCONTROL



###########here the function to create the data base

biocontrol_databaser <- function(igp_combinations, igp_times, pred_comb, s_vec, par_list){

file_folder <- "./data/biocontrol/"
file_subfolder <- paste0("sval_",  paste0(s_vec, collapse = "_"), "_", paste0(par_list, collapse = "_"), "/")  
file_name <- paste0("DF_BIOCONTROL_RAW", ".csv")

  
##this is a control BEFORE RUNNING THE ANALYSIS
  
if(file.exists(paste0(file_folder, file_subfolder, file_name))){
  print(paste0(file_folder, file_subfolder, file_name, " exists already. Verifiy before running long analysis"))} else{
     
dir.create(paste0(file_folder, file_subfolder), recursive = TRUE)
  
BIOCON_COMB <- data.frame()


##this loops creates a data frame with the different conditions of s, and of ip 
for (comb in pred_comb){
  chosenComb <- comb
  ipMax <- igp_combinations[[chosenComb]]$i_max[["maxIp"]]
  inMax <- igp_combinations[[chosenComb]]$i_max[["maxIn"]]
  
  if (is.na(ipMax)){Ip_seq = 0}
  else{Ip_seq = signif(ipMax * c(0.1, 0.9),2)}  ###now we will remove high and low values 
  if (is.na(inMax)){In_seq = 0}
  else{In_seq = signif(inMax * c(0.1, 0.9),2)}
  
  BIOCON_I <- expand.grid(Ip = Ip_seq, In= In_seq, S= s_vec)
  BIOCON_I$combPred <- chosenComb
  BIOCON_COMB <- rbind(BIOCON_COMB, BIOCON_I) }


###this one runs the simulation 

DF_BIOCON <- data.frame()

for (i in seq(1, dim(BIOCON_COMB)[1])){
  
  chosenComb <- BIOCON_COMB$combPred[i]
  Ip_var = BIOCON_COMB$Ip[i]
  In_var = BIOCON_COMB$In[i]
  S_var = BIOCON_COMB$S[i]
  
  print(chosenComb)
  
  #eqution characteristic 
  chosenParms = igp_combinations[[chosenComb]]$igp_parms
  chosenInit = igp_combinations[[chosenComb]]$igp_init
  chosenModel = igp_combinations[[chosenComb]]$igp_model
  chosenParms[["S"]] <- S_var
  chosenParms[["Ip"]] <- Ip_var
  chosenParms[["In"]] <- In_var
  
  
  BIOCON_temp <- bif_backFor(model = chosenModel, minPar = par_list[["min"]], maxPar =par_list[["max"]], parSw = par_list[["name"]], resolution = par_list[["res"]], mod_parameters = chosenParms, mod_times =   igp_times, mod_init = chosenInit, estCr =1e-6)
  
  #this is to homogeneize the value we want to use to see the exclusion from one another. 
  if ("Rn" %in% names(BIOCON_temp)) {BIOCON_temp$N <- BIOCON_temp$Rn +BIOCON_temp$Na}
  if ("Nl" %in% names(BIOCON_temp)) {BIOCON_temp$N <- BIOCON_temp$Nl +BIOCON_temp$Na}
  if ("Pl" %in% names(BIOCON_temp)) {BIOCON_temp$P <- BIOCON_temp$Pl +BIOCON_temp$Pa}
  if ("Rp" %in% names(BIOCON_temp)) {BIOCON_temp$P <- BIOCON_temp$Rp +BIOCON_temp$Pa}
  
  BIOCON_temp <- BIOCON_temp %>% dplyr::select(-any_of(c("Rn", "Nl", "Pl", "Rp", "Pa", "Na")))
  
  
  BIOCON_temp <- merge(BIOCON_COMB[i,], BIOCON_temp)
  
  DF_BIOCON <- rbind(DF_BIOCON, BIOCON_temp)
}

 
  
write_csv(DF_BIOCON, paste0(file_folder, file_subfolder, file_name))
}



##First we gonna take the decision of summarise. This means that we take the mean EVEN if we did not get to the equilibria here.. 
### im gonna export it raw...v


  
DF_BIOCON_N <- read.csv(paste0(file_folder, file_subfolder, file_name))%>%
  dplyr::group_by(Ip, In, K, combPred, type, S, direccion)%>%
  dplyr::summarise(meanR = mean(R), meanN= mean(N), meanP = mean(P))



DF_BIOCON_N  <- DF_BIOCON_N  %>%
  dplyr::group_by(combPred) %>%
  dplyr::mutate(normIp = Ip/max(Ip), normIn = In/max(In))



DF_BIOCON_N$IpCat <- "NA"
DF_BIOCON_N$InCat <- "NA"


DF_BIOCON_N$IpCat[DF_BIOCON_N$normIp %in% sort(unique(DF_BIOCON_N$normIp))[1:2]] <- "IpMin" ##agarara los dos valoes menores y sobre eso los catwegotri como los inimos (el de pb y el hv)
DF_BIOCON_N$IpCat[DF_BIOCON_N$normIp %in% sort(unique(DF_BIOCON_N$normIp))[3:4]] <- "IpMed" ##agarara los dos valoes menores y sobre eso los catwegotri como los inimos (el de pb y el hv)
DF_BIOCON_N$IpCat[DF_BIOCON_N$normIp %in% sort(unique(DF_BIOCON_N$normIp))[5]] <- "IpMax"


DF_BIOCON_N$InCat[DF_BIOCON_N$normIn %in% sort(unique(DF_BIOCON_N$normIn))[1:2]] <- "InMin"
DF_BIOCON_N$InCat[DF_BIOCON_N$normIn %in% sort(unique(DF_BIOCON_N$normIn))[3:4]] <- "InMed"
DF_BIOCON_N$InCat[DF_BIOCON_N$normIn %in% sort(unique(DF_BIOCON_N$normIn))[5]] <- "InMax"


  
file_name <- paste0("DF_BIOCONTROL_SUMMARIZED_", ".csv")

##this is a control AFTER RUNNING THE ANALYSIS, but it is not really important 

if(file.exists(paste0(file_folder, file_subfolder, file_name))){
  print("DF_BIOCON_SUMMARIZED exists already")} else{
write_csv(DF_BIOCON_N, paste0(file_folder, file_subfolder, file_name))
  
}

}



coexistence_adder <- function(da_ta){
  
DF_BIO <- da_ta

## transform the NA in 0
DF_BIO[is.na(DF_BIO)] <- 0

####1 we add and fill the columns of bioexistnece #################
DF_BIO$EQR <-""
DF_BIO$EQN <-""
DF_BIO$EQP <-""
###I fill the columns according to a criteria for coexistnece, where if more than 10**-4 then it exists
DF_BIO$EQR[round(DF_BIO$meanR/max(DF_BIO$meanR), 4) >0] <- "R"
DF_BIO$EQN[round(DF_BIO$meanN/max(DF_BIO$meanN), 4) >0] <- "N"  ##here I assume no stage
DF_BIO$EQP[round(DF_BIO$meanP/max(DF_BIO$meanP), 4) >0] <- "P"

DF_BIO <- DF_BIO |>
  tidyr::unite("EQ", EQR:EQP, sep = "", remove = T)

DF_BIO$EQ[DF_BIO$EQ ==""] <- 0

### 2. now we simplify to yes or not coexistence in another column 
DF_BIO$coexistence <-  0
DF_BIO$coexistence[DF_BIO$EQ =="RNP"] <- 1

#############################################333
### 3. here we remove the medium and also filter for the best sceanrio for exositence, 
#his is high In, low Ip
### we also name the categories of S for comun plotting 

DF_BIO <- DF_BIO |>
  dplyr::mutate(s_cat = ifelse(S>0.66, "competitive", ifelse(S>0.33, "symmetric", "tritrophic")))|>
  dplyr::select(Ip, In, K, combPred, S, meanR, IpCat, InCat, s_cat, coexistence, meanN, meanP)|>
  dplyr::filter(combPred != "PA.PA")


#### 4. But now lets take only  s=0.1, 0.5, 0.9  (we can run the simulations that way..)
DF_BIO <- DF_BIO |>
 dplyr::group_by(S, Ip, In, K, combPred, IpCat, InCat, coexistence)|>  ###we remove the bistabilities that are not from coexistence or not.. 
dplyr::filter(S %in% c(0.1, 0.5, 0.9))
#dplyr::filter(S %in% c(0.1, 0.9))

###5. We remove the bistabilities and create some last columns


DF_BIO  <- DF_BIO |> 
  tidyr::separate(combPred, c("IGP_P", "IGP_N"), remove = FALSE) 

DF_BIO <- unique(DF_BIO)

return(DF_BIO)
}



##########now lets plot 

plotter_absolute <- function(da_ta_coex, var_plot, ip_values= c("IpMin", "IpMax"), in_values= c("InMin", "InMax")){

if(!(var_plot %in% c("meanR", "meanP", "meanN"))){print("error: var_plot shoud be meanR, meanP or meanN")
  return()} else {  
plotted_var <- var_plot  
named_var <- c("meanP"= "IG predator P", "meanN"= "IG prey N", "meanR"= "Herbivore H")
  

DF_PLOT <- da_ta_coex
  
DF_PLOT[DF_PLOT == "PB"] <- "Predatory bug type (PB)"
DF_PLOT[DF_PLOT == "HV"] <- "Hoverfly type (HF)"
DF_PLOT[DF_PLOT == "LB"] <- "Ladybird type (LB)"
DF_PLOT[DF_PLOT == "PA"] <- "Parasitoid type (PA)"
  
order <- c("Ladybird type (LB)", "Predatory bug type (PB)", "Hoverfly type (HF)", "Parasitoid type (PA)")
com_pred_col <-  c("LB.LB"= "black",
  "LB.PB" = "#1f6168ff",
  "LB.HV" = "#1f6168ff",
  "PB.LB" = "#ffff67ff",
  "HV.LB" = "#ffff67ff",
  "PB.PB" = "#00b61bff",
  "PB.HV" = "#00b61bff",
  "HV.PB" = "#00b61bff",
  "HV.HV" = "#00b61bff",
  "LB.PA" = "darkgrey",
  "PB.PA" = "#ffff67ff",
  "HV.PA" = "#ffff67ff",
  "NoCol" = "white")


for (Ip_chosen in ip_values){
  for(In_chosen in in_values){
    
BIO_PLOT <- DF_PLOT |> 
  dplyr::filter(IpCat %in% c("0", Ip_chosen))|>
  dplyr::filter(InCat %in% c("0", In_chosen)) |>
  dplyr::mutate(
    IGP_N = factor(IGP_N, levels = order),
    IGP_P = factor(IGP_P, levels = order)) |> 
  dplyr::mutate(fill_color = ifelse(coexistence == 1, combPred, "NoCol"))|> 
  ggplot(aes(x=K, y= !!sym(plotted_var))) + 
  geom_line(aes(linetype =  s_cat), linewidth = 0.5, color= "black")+
  geom_point(aes(fill=fill_color, shape = s_cat), size=3, alpha=1) +  ###opcion1
 # geom_point(aes(fill=as.character(coexistence), shape = s_cat), size=2, alpha=1) +  ##opcion 2
  facet_grid(IGP_N~IGP_P) +
  scale_shape_manual(values = c("competitive" = 22, 
                               "symmetric" =  21, 
                               "tritrophic"= 24))+
  scale_fill_manual(values = com_pred_col) +  ##opcion 1
  #scale_fill_manual(values = s_cat_col) +  #opcion2
  
  labs(x = "Productivity (K)", 
       y = paste0("Population density of the ", named_var[plotted_var], " at equilibrium"), 
       fill= "IGP module",
       shape= "IGP symmetry (s)",
       linetype= "IGP symmetry (s)")+
  theme_bw(base_size = 14) +  # Just change this number
  theme(
    strip.background = element_rect(fill="white"), 
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(legend.position = "none")
  
  
ggsave(BIO_PLOT, filename= paste0("./output/numericalIGP/biocontrol/s_cat/", 
                                  plotted_var,
                                  "_scat_allK_S=", 
                                  paste(unique(DF_BIO_EQ$S), collapse = "_"),
                                  Ip_chosen, "_",
                                  In_chosen, ".png"),  
       height = 9, width = 10, create.dir = T)

  }
}
#return(BIO_PLOT)

}
}


##############This plot will plot the differences with the LBLB reference 


differencer_lblb <- function(da_ta_coex, criteria_change){
### we gonna remove the bistabilities for this species plot 

DF <- da_ta_coex |>
  dplyr::group_by(Ip, In, K, combPred, IGP_P, IGP_N, S, IpCat, InCat, s_cat, coexistence)|>
  dplyr::summarise(meanR = mean(meanR), meanN=mean(meanN), meanP = mean(meanP)) |>
  dplyr::ungroup()  ## this is to allow future select

## 2. we then subset the LBLB and we will paste it and thenn substract it 
DF_LBLB  <- DF |>
  dplyr::filter(combPred == "LB.LB") |>
  dplyr::select(K, S, s_cat, meanR, meanN, meanP, coexistence)

## here is a left join that wil lmatch the ids colum in a many to one fashion
DF_CHANGE <- dplyr::left_join(x=DF, y=DF_LBLB, 
                                      by=c("K", "S", "s_cat"),
                                      relationship = "many-to-one")
##here i substract                                 
DF_CHANGE <- DF_CHANGE|>
  dplyr::mutate(meanR_change = (meanR.x-meanR.y),
                meanN_change =(meanN.x-meanN.y),
                meanP_change = (meanP.x-meanP.y),
                meanR_rel_change = 100*(meanR.x-meanR.y)/(meanR.x+meanR.y),
                meanN_rel_change =100*(meanN.x-meanN.y)/(meanN.x+meanN.y),
                meanP_rel_change = 100*(meanP.x-meanP.y)/(meanP.x+meanP.y),
                coexi_change=  coexistence.x-coexistence.y  ##This is binary
                ) 

##here I create a criteria were for each K I say if it increase or not (if 100*x-xef/x=xref > crteriachage)

DF_CHANGE$meanR_rel_change[is.na(DF_CHANGE$meanR_rel_change)] <- 0 ###porque es porque la suma de lblbl y otro dio 0, entonces no cambio 

DF_CHANGE$meanR_rel_change_cat <- 0
DF_CHANGE$meanR_rel_change_cat[DF_CHANGE$meanR_rel_change>criteria_change] <- 1  ### the 10 percet is 
DF_CHANGE$meanR_rel_change_cat[DF_CHANGE$meanR_rel_change< criteria_change*(-1)] <- -1  ### the 10 percet is 


return(DF_CHANGE)
}
#############here is the ploit

plotter_relative_lblb <- function(da_ta_rel, var_plot, ip_values= c("IpMin", "IpMax"), in_values= c("InMin", "InMax")){

DF_PLOT <- da_ta_rel  

if(!(var_plot %in% c("meanR_change", "meanP_change", "meanN_change", "meanR_rel_change", "meanP_rel_change", "meanN_rel_change")))
  {print("error: var_plot shoud be meanR_change, meanP_change or meanN_change, meanR_rel_change, meanP_rel_change or meanN_rel_change")
  return()} 
else { 

DF_PLOT[DF_PLOT == "PB"] <- "Predatory bug (PB)"
DF_PLOT[DF_PLOT == "HV"] <- "Hoverfly (HF)"
DF_PLOT[DF_PLOT == "LB"] <- "Ladybird (LB)"
DF_PLOT[DF_PLOT == "PA"] <- "Parasitoid (PA)"

order <- c("Ladybird (LB)", "Predatory bug (PB)", "Hoverfly (HF)", "Parasitoid (PA)")
    
plotted_var_change <- var_plot  ##meanP, meanR
named_var_change <- c("meanP_change"= "Change in IG predator P", "meanN_change"= "Change in IG prey N", "meanR_change"= "Change in Herbivore H")


for (Ip_chosen in ip_values){
  for(In_chosen in in_values){

BIO_PLOT_CHANGE<- DF_PLOT |> 
  dplyr::filter(IpCat %in% c("0", Ip_chosen))|>
  dplyr::filter(InCat %in% c("0", In_chosen)) |>
  dplyr::mutate(
    IGP_N = factor(IGP_N, levels = order),
    IGP_P = factor(IGP_P, levels = order)) |> 
  ggplot(aes(x=K, y= !!sym(plotted_var_change))) + 
  geom_line(aes(linetype =  s_cat), linewidth = 0.5, color= "black")+
  geom_segment(aes(x=0, y=0, xend=8, yend=0), color="darkred", linetype = 1)+
  geom_point(aes(fill= as.factor(coexi_change), shape = s_cat), size=2.5, alpha=0.8) +  ###opcion1
  # geom_point(aes(fill=as.character(coexistence), shape = s_cat), size=2, alpha=1) +  ##opcion 2
  
  facet_grid(IGP_N~IGP_P) +
  scale_shape_manual(values = c("competitive" = 22, 
                                "symmetric" =  21, 
                                "tritrophic"= 24))+
  scale_fill_manual(values = c("1"= "darkblue",
                               "0"= "white",
                               "-1"= "darkred")) +  ##opcion 1
  #scale_fill_manual(values = s_cat_col) +  #opcion2
  
  labs(x = "Productivity (K)", 
       y = paste0("Population density of the ", named_var_change[plotted_var_change], " at equilibrium"), 
       fill= "IGP module",
       shape= "IGP symmetry (s)",
       linetype= "IGP symmetry (s)")+
  theme_bw() +
  theme_bw(base_size = 14) +  # Just change this number
  theme(
    strip.background = element_rect(fill="white"), 
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(legend.position = "none")



ggsave(BIO_PLOT_CHANGE, filename= paste0("./output/numericalIGP/biocontrol/change/", 
                                         plotted_var_change,"_scat_allK_S=", 
                                         paste(unique(DF_PLOT$S), collapse = "_"), 
                                         Ip_chosen, "_",
                                         In_chosen, ".png"),  
       height = 9, width = 10, create.dir = T)
#return(BIO_PLOT_CHANGE)
  }
}
}
}




##########then im gonna sumarisze again across big cate

summariser_categories <- function(da_ta_rel, ip_values= c("IpMin", "IpMax"), in_values= c("InMin", "InMax")){
 
  DF <- da_ta_rel 
  DF$combPred_cat <- "Baseline"
  DF$combPred_cat[DF$IGP_N %in% c("PB", "HV")] <-"Changing IG N"
  DF$combPred_cat[DF$IGP_P %in% c("PB", "HV")] <-"Changing IG P"
  DF$combPred_cat[(DF$IGP_P %in% c("PB", "HV"))
                                 & (DF$IGP_N %in% c("PB", "HV"))] <-"Changing both"
  DF$combPred_cat[DF$IGP_N %in% c("PA")] <-"Parasitoid cases"
  

##here I summarise within categorries


DF_TOTAL_SUM <- data.frame()  

  for (Ip_chosen in ip_values){
    for(In_chosen in in_values){
  
DF_SUM <- DF|>
  dplyr::filter(IpCat %in% c("0", Ip_chosen))|>
  dplyr::filter(InCat %in% c("0", In_chosen)) |>
  dplyr::ungroup()|>
  dplyr::group_by(combPred_cat, S, s_cat, K)|>
  dplyr::summarise(total_meanR_cat= mean(meanR_rel_change_cat),
                   total_coexistence_cat= mean(coexi_change))

DF_SUM$total_coexistence_cat[abs(DF_SUM$total_coexistence_cat)<0.5] <-  0
DF_SUM$total_coexistence_cat[DF_SUM$total_coexistence_cat>=0.5] <-  1
DF_SUM$total_coexistence_cat[DF_SUM$total_coexistence_cat<= 0.5*(-1)] <-  -1

###here I put in terms of biocontrol

DF_SUM$biocontrol <- 0

DF_SUM$biocontrol[abs(DF_SUM$total_meanR_cat)<0.5] <- 0
DF_SUM$biocontrol[DF_SUM$total_meanR_cat>=0.5] <- -1
DF_SUM$biocontrol[DF_SUM$total_meanR_cat<= 0.5*(-0.1)] <- 1

DF_SUM$IpCat <- Ip_chosen
DF_SUM$InCat <- In_chosen

DF_TOTAL_SUM <- rbind(DF_TOTAL_SUM, DF_SUM)
    }
  }

return(DF_TOTAL_SUM)

####I have to do this again toc ope with the 0 (of just doing within the muutat) #we summa acroos K



#write.csv(DF_BIO_CHANGE_SUM, file=paste0("./output/numericalIGP/biocontrol/change/", "table_acrossK", ".csv"))



#### here an even more compacted csv where I summarize by categories



}


######here the categorical plot

plotter_relative_lblb_cat <- function(da_ta_cat, ip_values= c("IpMin", "IpMax"), in_values= c("InMin", "InMax")){
  
  DF_PLOT <- da_ta_cat  
  
  ordercat <- c("Baseline", "Changing IG P",   "Changing IG N" , "Changing both", "Parasitoid cases")
  
  
  for (Ip_chosen in ip_values){
    for(In_chosen in in_values){
  
  BIO_CAT_CHANGE<- DF_PLOT |>
    dplyr::filter(IpCat %in% c(Ip_chosen))|>
    dplyr::filter(InCat %in% c(In_chosen)) |>
    dplyr::filter(!(combPred_cat=="Baseline"))|>
    dplyr::mutate(
      combPred_cat = factor(combPred_cat, levels = ordercat))|> 
    ggplot(aes(x=K, y= biocontrol)) + 
    geom_rect(aes(xmin=0, xmax=8, ymin=0, ymax = 1), fill="darkgreen", alpha=0.1)+
    geom_line()+
    geom_point(aes(fill =  as.factor(total_coexistence_cat)), shape=21, size=3)+
    
    facet_grid(combPred_cat~S) +
    scale_fill_manual(values = c("1"= "black",
                                  "0"= "white",
                                  "-1"= "white")) +
  

    labs(x = "Productivity (K)", 
         y = "Mean biocontrol effect", 
         fill= "Coexistence",
         )+
    theme_bw() +
    theme_bw(base_size = 14) +  # Just change this number
    theme(
      strip.background = element_rect(fill="white"), 
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(legend.position = "none")
  
  
  
  ggsave(BIO_CAT_CHANGE, filename= paste0("./output/numericalIGP/biocontrol/change_cat/", 
                                           "biocat_allK_S=", 
                                           paste(unique(DF_PLOT$S), collapse = "_"), 
                                          Ip_chosen, "_",
                                          In_chosen,".png"), 
         
         height = 9, width = 10, create.dir = T)
  #return(BIO_CAT_CHANGE)
    }
  }
}



##############33here is a simplified converter. That should results in the same

differencer_cat_lblb_treatment <- function(da_ta_coex, ip_chosen= c("IpMin", "IpMax"), in_chosen= c("InMin", "InMax")){
  ### we gonna remove the bistabilities for this species plot 
  
  ##here I add the categories of the IGP groups
  DF <- da_ta_coex
  DF$combPred_cat <- "Baseline"
  DF$combPred_cat[DF$IGP_N %in% c("PB", "HV")] <-"Changing IG N"
  DF$combPred_cat[DF$IGP_P %in% c("PB", "HV")] <-"Changing IG P"
  DF$combPred_cat[(DF$IGP_P %in% c("PB", "HV"))
                  & (DF$IGP_N %in% c("PB", "HV"))] <-"Changing both"
  DF$combPred_cat[DF$IGP_N %in% c("PA")] <-"Parasitoid cases"
  
  ###here I add the K categories
  DF$K_cat <- "lowK"
  DF$K_cat[DF$K >4] <- "highK"
  

  DF <- DF |>
    dplyr::filter(IpCat %in% c(ip_chosen, 0))|>
    dplyr::filter(InCat %in% c(in_chosen, 0))|>
    dplyr::filter(K != 0)|>
    dplyr::group_by(IpCat, InCat, K_cat, combPred_cat, s_cat)|>
    dplyr::summarise(meanR = mean(meanR), 
                     meanN=mean(meanN), 
                     meanP = mean(meanP),
                     meanCoexistence= mean(coexistence)) |>
    dplyr::ungroup()  ## this is to allow future select
  
  ## 2. we then subset the LBLB and we will paste it and thenn substract it 
  DF_LBLB  <- DF |>
    dplyr::filter(combPred_cat == "Baseline") 
  
  ## here is a left join that wil lmatch the ids colum in a many to one fashion
  DF_CHANGE <- dplyr::left_join(x=DF, y=DF_LBLB, 
                                by=c("K_cat", "s_cat"),
                                relationship = "many-to-one")
  ##here i substract                                 
  DF_CHANGE <- DF_CHANGE|>
    dplyr::mutate(meanR_change = (meanR.x-meanR.y),
                  meanN_change =(meanN.x-meanN.y),
                  meanP_change = (meanP.x-meanP.y),
                  meanR_rel_change = 100*(meanR.x-meanR.y)/(meanR.x+meanR.y),
                  meanN_rel_change =100*(meanN.x-meanN.y)/(meanN.x+meanN.y),
                  meanP_rel_change = 100*(meanP.x-meanP.y)/(meanP.x+meanP.y),
                  mean_coexi_change1= 100* (meanCoexistence.x-meanCoexistence.y)/(meanCoexistence.x+meanCoexistence.y),
                  mean_coexi_change2= meanCoexistence.x-meanCoexistence.y##This is binary
    ) 
  
  ##here I create a criteria were for each K I say if it increase or not (if 100*x-xef/x=xref > crteriachage)
  DF_CHANGE <- DF_CHANGE |>
    dplyr::select("IpCat.x", "InCat.x",  "K_cat", "combPred_cat.x", "s_cat",
                  "meanR.x" , "meanCoexistence.x", "meanR_change" ,"meanR_rel_change", "mean_coexi_change1") 
  
  DF_CHANGE$mean_coexi_change1[is.na(DF_CHANGE$mean_coexi_change1)] <- 0
  return(DF_CHANGE)
}


  