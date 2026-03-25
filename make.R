#=======================================ALWAYS RUN THIS=================================================
# Look for every package used in the project and add them to DESCRIPTION under Imports
rdeps::add_deps()

# Install/update packages listed in DESCRIPTION
devtools::install_deps(upgrade = "never")

# Load packages under Depends
devtools::load_all()

#Set the inital conditions used for the rest of the simulations (within the environment)
source("analyses/01.set_initial_condition.R")


#=======================================ANALYSIS=================================================

#=============BIOCONTROL ANALYSES AND PLOTS=====================================

#################################################
# Calculate a plot the herbivore density for different values of s, and K. 
# Note: it check if the plots and analyses exists before doing the calcuarion
# Enter that script to modify the conditions of s and K

start<-proc.time()
source("analyses/02.set_df_biocontrol.R")
print(proc.time()- start)

# Plot thegeneral figures (of biocoontr)
source("analyses/03.analyze_biocontrol.R")

#======================================================================================


#============================ DETAILED NUMERIC BIFURCATIONS======================

### in the default setting it run
# total 38000 scenarios * 0.1 s
# And it does around 3800 s, around 1 hour
# Note: it check if the plots and analyses exists before doing the calcuarion
# Enter that script to modify the conditions of s and K


start<-proc.time()
#################################################
#Derives the detailed bifurcations of examples.. enter the script to change the conditons 
source("analyses/04.set_df_examples.R")
####################3
print(proc.time()- start)

#################################################
#Plots detailed bifurcations of examples.. enter the script to change the conditons 
source("analyses/05.plot_detailed_bifurcation.R")
####################3


#==========================================================================================


#================ MANUAL EXPLORATIONS===========================

#Dont run this from makeR, enter the code for the manual exploration 
#"analyses/06.bif_manual_exploration.R")#


