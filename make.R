
# Look for every package used in the project
# Add them to DESCRIPTION under Imports
rdeps::add_deps()

# Install/update packages listed in DESCRIPTION
devtools::install_deps(upgrade = "never")


# Load packages under Depends
devtools::load_all()

### these instructions are not linear, follow them according to your needs

###
#roxygen2::roxygenise()
#################################################
#0. Set the inital conditions used for the rest of the simulations (within the environment)
source("analyses/01.set_initial_condition.R")

#################################################
#1. Calculate a plot the herbivore density for different values of s, and K. (enter that script to modify the conditions)
source("analyses/02.set_df_biocontrol.R")
####################3


#2. plot thegeneral figures (of biocoontr)
source("analyses/03.analyze_biocontrol.R")

