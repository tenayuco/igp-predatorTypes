#the ncrit telle us the max difference
## add the one you want to analize and plot

data_folder <- "./data/bifurcations/"###here you pik the subfolder you want
data_subfolder <- "condpar_K_2_bifpar_S_0_1_0.01/" ##you modify the one you want
data_name <- "DF_BIFURCATION_LB.LB_PB.LB_LB.PB_PB.PB_LB.PA.csv"  #this is the fullest data base

DF_BIF <- read.csv(paste0(data_folder, data_subfolder, data_name))

#this changes according to the data base you are using. Eventually it would have to know from the data base but not now
parSw <- "S"
resPar <- 0.01
facetPar <- "K"
maxPar <- 1


#DF_BIF_EQ <- simple_ass_coex(DF_BIF, ncrit = 2) ##this normalizes and calcualte the mean so it removes the "other multiple points

##This takes into account the min and max vaues and is more general 
DF_BIF_EQ_minMax <- minMax_coex(simpleDF = DF_BIF, ncrit = 3)


#############folder to save the plots
file_folder <- "./outputs/bifurcation/"
file_subfolder <- data_subfolder

predVec <- c("LB.LB", "LB.PB", "PB.LB", "PB.PB", "LB.PA")

for (predChosen in predVec) {
  
      DF_BIF_SAMPLE <- DF_BIF_EQ_minMax |>
        dplyr::ungroup()|>
        dplyr::filter(combPred == predChosen)
  
  for (ipChosen in unique(DF_BIF_SAMPLE$Ip)) {
    for (inChosen in unique(DF_BIF_SAMPLE$In)) {
        DF_BIF_SAMPLE2 <- DF_BIF_SAMPLE |>
        dplyr::filter(Ip %in% c(ipChosen)) |>
        dplyr::filter(In %in% c(inChosen))


      PT_BIF_EQ <- plot_bif_sweep_facet(
        DF_BIF_SAMPLE2,
        par_sw = parSw,
        facet_1 = facetPar
      )

      PT_BIF_EQ_AREAS <- add_eq_gray(
        PT_BIF_EQ,
        par_sw = parSw,
        reso = resPar,
        only_coex = FALSE
      )

      file_name <- paste0(
        predChosen,
        "_Ip_",
        ipChosen,
        "_In_",
        inChosen,
        "_BIFURCATION.png"
      )

      if (file.exists(paste0(file_folder, file_subfolder, file_name))) {
        print(paste0(file_name, " exists already"))
      } else {
        ggsave(
          PT_BIF_EQ_AREAS,
          filename = paste0(file_folder, file_subfolder, file_name),
          height = 8,
          width = 6,
          create.dir = T
        )
      }
}
  }
}
