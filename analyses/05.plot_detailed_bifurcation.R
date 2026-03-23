#the ncrit telle us the max difference
## add the one you want to analize and plot

data_folder <- "./data/bifurcations/"###here you pik the subfolder you want
data_subfolder <- "condpar_K_2_bifpar_S_0_1_0.5/" ##you modify the one you want
data_name <- "DF_BIFURCATION_LB.LB_PB.LB_LB.PB_PB.PB_LB.PA.csv"  #this is the fullest data base


#this changes according to the data base you are using. Eventually it would have to know from the data base but not now
parSw <- "S"
resPar <- 0.5
facetPar <- "K"
maxPar <- 1

DF_BIF <- read.csv(paste0(data_folder, data_subfolder, data_name))


###write the name of the parameter (explicit) you bifur, and the multiple cond parameter

DF_BIF_EQ <- simple_ass_coex(DF_BIF, ncrit = 2) ##this normalizes and calcualte the mean so it removes the "other multiple points

predVec <- c("LB.LB", "LB.PB")



###here I need to just check the combination and the ip and in selected to plot, or subfilter before plotting

file_folder <- "./outputs/bifurcation/"
file_subfolder <- data_subfolder


for (predChosen in predVec) {
  
      DF_BIF_SAMPLE <- DF_BIF_EQ |>
        dplyr::ungroup()|>
        dplyr::filter(combPred == predChosen)
      print(DF_BIF_SAMPLE)
  
  for (ipChosen in unique(DF_BIF_SAMPLE$Ip)) {
    for (inChosen in unique(DF_BIF_SAMPLE$In)) {
        DF_BIF_SAMPLE2 <- DF_BIF_SAMPLE |>
        dplyr::filter(Ip %in% c(ipChosen)) |>
        dplyr::filter(In %in% c(inChosen))
      print(DF_BIF_SAMPLE2)
      print("dai")

      PT_BIF_EQ <- plot_bif_sweep_facet(
        DF_BIF_SAMPLE2,
        par_sw = parSw,
        facet_1 = facetPar,
        xmax = maxPar
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
          height = 4,
          width = 8,
          create.dir = T
        )
      }
}
  }
}
