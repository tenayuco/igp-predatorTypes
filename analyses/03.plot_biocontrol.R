##########################################################################################################
# Code that produce the data base of the simulations for the biocontrol
#' @param igpParms (defined by the user)
#' @param phiVal (defined by the user)
#' @functions biocontrol_databaser()
#' @return the igp_combinations as a list saved in your data folder
#' @details run it only if you dont have it in your data base
#' @details The K and S are set, but those ones change within each simulation
#############################################################################################################

##change the name of the file depending which degree of precision you use

DF_BIO_IMPORT <- read.csv("./data/biocontrol/sval_0.1_0.5_0.9_K_0_8_1/DF_BIOCONTROL_SUMMARIZED.csv")  ###arreglar con todo el proyecto

DF_BIO_EQ <- coexistence_adder(da_ta = DF_BIO_IMPORT)

plotter_absolute(da_ta_coex= DF_BIO_EQ, var_plot = "meanR", 
ip_values= c("IpMin", "IpMax"), in_values= c("InMin", "InMax"))


################his one will  summarize and do the table


differencer_cat_lblb_treatment(da_ta_coex =DF_BIO_EQ, ip_chosen= c("IpMin"), in_chosen= c("InMax"))
