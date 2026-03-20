#the ncrit telle us the max difference
## add the one you want to analize and plot

data_folder <- "./data/bifurcations/"
data_name <- "DF_BIFURCATION.csv"
###here you pik the subfolder you want
data_subfolder <-  "condpar_K_2_bifpar_S_0_1_0.1/"  ##you modify the one you want

DF_BIF <- read.csv(paste0(data_folder, data_subfolder, data_name))


###write the name of the parameter (explicit) you bifur, and the multiple cond parameter

DF_BIF_EQ <- simple_ass_coex(DF_BIF, ncrit = 2)  ##this normalizes and calcualte the mean so it removes the "other multiple points

###here I need to just check the combination and the ip and in selected to plot, or subfilter before plotting

PT_BIF_EQ <- plot_bif_sweep_facet(DF_BIF_EQ, par_sw  = "S", facet_1 = "K", xmax = 4)

PT_BIF_EQ_AREAS  <- add_eq_gray(PT_BIF_EQ, par_sw = "S", reso = 0.1, only_coex = FALSE)


##this does not work 
Ip_chosen <- 0
In_chosen <- 0

file_folder <- "./outputs/bifurcation/"
file_subfolder <- data_subfolder
file_name <- paste0(
          "PLOT_BIFURCATION_",
          Ip_chosen,
          "_",
          In_chosen,
          ".png"
        )

        if (file.exists(paste0(file_folder, file_subfolder, file_name))) {
          print(paste0(file_name, " exists already"))
        } else {
          ggsave(
            BIO_PLOT,
            filename = paste0(file_folder, file_subfolder, file_name),
            height = 9,
            width = 10,
            create.dir = T
          )
        }





