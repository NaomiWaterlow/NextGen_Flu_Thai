# UK model specifications
#set the file path to the main folder <- change here and all other should be read in. 
setwd("~/Documents/GitHub/NextGen_Flu_Thai/")
library(here)
library(fluEvidenceSynthesis)
library(tidyverse)
library(data.table)
library(MASS)
library(ggplot2)
library(fluEvidenceSynthesis)
library(plyr)
library(reshape2)
library(ISOweek)
library(qs)
library(gridExtra)
library(grid)
library(wpp2019)
# this overrides a needed function  
if("tictoc" %in% (.packages())){
  detach("package:tictoc", unload=TRUE) 
}



####### VARIABLES #####

###### these are for the economics and have not yet been adapted to Thailand
base_scenario_to_use <- 1 # for the economics
societal_perspective <- TRUE

disc.rate.dalys <- c(0,0.03) #0.03
disc.rate.costs <- 0.03
thai.gdppc <- 7066.2 # Thai 2021 GDP per capita from World Bank.
# threshold <- 0.5 * thai.gdppc
threshold <- 160000/31.98 #Thai CET in USD

# the rest

location <- "Thailand"
n_samples <- posterior_sample_size <- 200

target_scenarios <- scenarios_to_include
# target_scenarios <- c(1,12,132,253,371,610) # seasonal targeting under 5s
# target_scenarios <- c(1,136,156,277,395,634) # seasonal targeting under 11s

use_presampled <- F # this is always false for UK - as only have 1000 samples of each
save_samples <- F # this is always false for UK - as only have 1000 samples of each
change_susceptibility_switch <- "OFF" # whether to take account reduction of infection from previous year
name_run <- "default"
create_plots <- "Yes"

num_parameters_posteriors <- 4
transmisibility_location <- 2 # position in posterior of the transmissibility parameter
infection_delays <- c(0.8,1.8)
initial_infection_location <- 4

age_groups_model <- c(2, 6, 12, 18, 60) 
no_risk_groups <- 1
max_age <- 77 # average life expectancy - used only for ageing out of the last compartment
num_age_groups <- 6
end_first_year_vaccination <- as.Date("2006-04-01") 
high_risk <- rep(0,6)

susceptibility_pattern <- c(NA, 3,3,3,3,3)
calculate_FOI <- "Yes" # this should always be No for the UK (as no inter-epidemic period)
risk_ratios_input <- matrix(c(high_risk,0,0,0,0,0,0),
                            ncol = num_age_groups , byrow = T)

num_years <- 4
years <- c(2005:2010)
#     v3:<2 yr,  v4:2-5 yrs,  v5:6-11 yrs,  v6:12-17 yrs, v7:18-59 yrs, v8:>=60 yrs
# load in contacts etc. 
source(here::here("Fitting", "creating_contacts.R"))
contact_ids_input <-NA # not required as using a fixed contact matrix
relevant_polymod <- polymod.thai

age_group_labels <- data.frame(id =c(1:18), 
                               label =rep(c("Age0-1","Age2-5","Age6-11","Age12-17",
                                        "Age18-59","Age60+"),3))
risk_group_labels <- data.frame(id =c(1:18), 
                               label =c(rep("Risk_group1",num_age_groups),
                               rep("Risk_group2",num_age_groups),
                               rep("Risk_group3",num_age_groups)))
vaccine_scenario_names <- c("No vaccine","Current seasonal","Improved (minimal)","Improved (efficacy)","Improved (breadth)",
"Universal")
set.seed(5042)
input_contacts_fixed <- fluEvidenceSynthesis::contact_matrix(as.matrix(relevant_polymod),
                                     popthai[,2], age_groups_model )


##### SOURCE AND RUN ######

# The vaccination scenario 
source(here::here("Vacc_epi_model","1_0_vaccination_scenario_list_Thai_LSHTM.R"))
#The list of epidemics in Thailand
source(here::here("Vacc_epi_model","2_0_epidemics_list_thai.R"))# needs updating from UK 

# Run all the modelling!
source(here::here("Vacc_epi_model", "0_Main_NextGen.R"))
# Run all the economics!

# source(here::here("Economics", "5_0_run_economics.R"))

