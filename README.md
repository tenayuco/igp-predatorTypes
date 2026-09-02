# Code for Functional dissimilarity modulates coexistence and prey suppression in intraguild predation modules

[![License:
GPL-2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)


This project aims to reproduce the data and the figures used in the paper "Functional dissimilarity modulates coexistence and prey suppression in intraguild predation modules". by E.M. 

## Content

This project is structured as follow:

```
.
├─ README.md                                  # Presentation of the project
├─ DESCRIPTION                                # Project metadata
├─ LICENSE.md                                 # License of the project
|
├─ data/                                      # Contains raw data
|  ├─ bifurcation/                              # stores the data of bifurcation
|  |
|  |
|  └─ bicontrol/                             # stores the data of biocontorl
|     ├─ 
|
├─ R/                                         # Contains R functions (only)
|  ├─ 0_basal_functions.R                     # functions used everywhere
|  └─ 01_ic_functions.R                    # functions create a list with the initial conditions of each model 
|  └─ 02_df_bifurcations.R                    # functions to create the database of biocontrol.
|  └─ 03_biocontrol_plotter.R                    # function to plot the biocontrol data 
|  └─ 05_bifurcation_plotter.R                    # function plots the bifurcation data
|  └─ 06_debif_explorations.R                    # functions to create database of bifurcations
|  └─ RNP_functions.R                    # functions with the models
|  └─ old_bifPlotter.R                    #old functions that could be useful 
|  └─ old_bifurcationF.R                    # old functions that could be useful 


├─ analyses/                                  # Contains R scripts
|  └─ 01.set_initial_condition.R                      # Script to create initial conditions
|  └─ 02.set_df_biocontrol.R                   # Script to create the data frame of biocontrol
|  └─ 03.analyze_biocontrol.R                 # Script to analyze biocontrol
|  └─ 04.set_df_examples.R                 # Script to create thedata frame of bifurcations
|  └─ 05.plot_detailed_bifurcation.R                 # Script to plot bifrucations
|  └─ 06.bif_manual_exploration.R                 # Script to do manual bifurcations with the debif
└─ make.R                                     # Script to setup & run the project
```


> [!NOTE]  
> The folder **data/** **output/** are not present in this repository (listed in the `.gitignore`) 
> but we provide the code to recreate it



#

## Usage

Open this project in Positron and either run the makeR. or each of the analyses manually

```r
source("make.R")
```

- All packages will be automatically installed and loaded
- Raw data will be saved in the `data/` directory



## License

This project is released under the 
[GPL-2](https://choosealicense.com/licenses/gpl-2.0/) license.



## Citation



## References
