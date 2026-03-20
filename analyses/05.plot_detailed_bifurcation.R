#the ncrit telle us the max difference

DF_BIF <- read.csv("data/bifurcations/condpar_K_2_bifpar_S_0_1_0.1/DF_BIOCONTROL_RAW.csv")







BIF_Sfix_K_EQ<- simple_ass_coex(BIF_Sfix_K, par_sw = "K", facet_1 = "S", ncrit = 2)  ##this normalizes and calcualte the mean so it removes the "other multiple points

PT_BIF_Sfix_K_EQ <- plot_bif_sweep_facet(full_sweep =  BIF_Sfix_K_EQ, par_sw  = "K", plotVar = names(chosenInit), facet_1 = "S", xmax = 4)

PT_BIF_Sfix_K_EQ  <- add_eq_gray(PT_BIF_Sfix_K_EQ, par_sw = "K", reso = resoK)

#PT_BIF_S_K_EQ  <- PT_BIF_S_K_EQ   + ggtitle(label = as.character(igp_name))

#full_path <- paste0(output_dir, import_name_Sfix)

ggsave(PT_BIF_Sfix_K_EQ,filename=paste0("output/numericalIGP/bifExamples/plots/", igp_name, "_Sfix0.5_", ".png"),  height = 8, width = 6, create.dir = TRUE)

BIF_Kfix_S_EQ<- simple_ass_coex(BIF_Kfix_S, par_sw = "S", facet_1 = "K", ncrit = 2)  ##this normalizes and calcualte the mean so it removes the "other multiple points
PT_BIF_Kfix_S_EQ <- plot_bif_sweep_facet(full_sweep =  BIF_Kfix_S_EQ, par_sw  = "S", plotVar = names(chosenInit), facet_1 = "K", xmax = 1)
PT_BIF_Kfix_S_EQ  <- add_eq_gray(PT_BIF_Kfix_S_EQ, par_sw = "S", reso = resoS)

#PT_BIF_S_K_EQ  <- PT_BIF_S_K_EQ   + ggtitle(label = as.character(igp_name))

#ggsave(PT_BIF_Kfix_S_EQ,filename=paste0("output/numericalIGP/bifExamples/plots/", igp_name, "_Kfix4_", ".png"),  height = 8, width = 6, create.dir = TRUE)



