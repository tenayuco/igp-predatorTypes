

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
### in the default setting it run
## 30 PT scenarios (12 combinations, but with different ip and in (2 for each PB and each HF))
## 9 values of K  (back and forth so 18)
## 3 values of S
## 1620 scenarios
## by secanrio 40 000 time steps (from 0 to 2000, by 0.05)
### And it does it in 1300 s. So, 0.8 s per scenario. 
## 

start<-proc.time()
source("analyses/02.set_df_biocontrol.R")
print(proc.time()- start)



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

### in the default setting it run
## scenario 1 
# 10 scenarios PT
## 900 values of K  (back and forth so 1800)
## 1 values of S
## 18000 scenarios
######
## scenario 1 
# 10 scenarios PT
## 1000 values of S  (back and forth so 2000)
## 1 values of S
## 20000 scenarios
## by secanrio 40 000 time steps (from 0 to 2000, by 0.05)

#####
#total 38000 scenarios * 0.1 s
### And it does around 3000 s, this is 0.83 hours (almost an hour)


#################################################
#Plots detailed bifurcations of examples.. enter the script to change the conditons 
source("analyses/05.plot_detailed_bifurcation.R")
####################3


#================ MANUAL EXPLORATIONS===========================

source("analyses/06.bif_manual_exploration.R")
