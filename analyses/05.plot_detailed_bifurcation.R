#the ncrit telle us the max difference
## add the one you want to analize and plot

#====================================USER SECTION=============================================


data_folder <- "./data/bifurcations/"###here you pik the subfolder you want
#data_subfolder <- "condpar_K_4_bifpar_S_0_1_0.001/" ##you modify the one you want
data_subfolder <- "condpar_S_0.5_bifpar_K_0_8_0.1/" ##you modify the one you want

parSw <- "K"
resPar <- 0.1
maxPar <- 4
facetPar <- "S"

#==========================================================================================


data_name <- "DF_BIFURCATION_LB.LB_PB.LB_LB.PB_PB.PB_LB.PA.csv"  #this is the fullest data base
DF_BIF <- read.csv(paste0(data_folder, data_subfolder, data_name))

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
        facet_1 = facetPar, 
        max_par = maxPar
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
