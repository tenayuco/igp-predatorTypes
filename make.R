
# Look for every package used in the project
# Add them to DESCRIPTION under Imports
rdeps::add_deps()

# Install/update packages listed in DESCRIPTION
devtools::install_deps()

# Load packages under Depends
devtools::load_all()

source("analyses/01.set_initial_condition.R")
source("analyses/02.set_df_biocontrol.R")