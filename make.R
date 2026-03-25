

#====================ALWAYS RUN THIS===================

# Look for every package used in the project
# Add them to DESCRIPTION under Imports
rdeps::add_deps()

# Install/update packages listed in DESCRIPTION
devtools::install_deps(upgrade = "never")


# Load packages under Depends
devtools::load_all()

### these instructions are not linear, follow them according to your needs

###
#################################################
#1. Set the inital conditions used for the rest of the simulations (within the environment)
source("analyses/01.set_initial_condition.R")





#=============USER OPTIONAL FUNCTIONS======================

##=======================BIOCONTROL PLOTS============================

#################################################
# Calculate a plot the herbivore density for different values of s, and K. (enter that script to modify the conditions)
source("analyses/02.set_df_biocontrol.R")
####################3

# plot thegeneral figures (of biocoontr)
source("analyses/03.analyze_biocontrol.R")



#============================ DETAILED NUMERIC BIFURCATIONS======================

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


#================ MANUAL EXPLORATIONS===========================

source("analyses/06.bif_manual_exploration.R")
