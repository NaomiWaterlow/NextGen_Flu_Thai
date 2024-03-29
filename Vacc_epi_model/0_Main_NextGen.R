# Main script for the Next Gen models 

####### LOAD AND SOURCE ########

# Load packages

###### VACCINATION MODEL ######
# the vaccination list will have been loaded from the specifications sheet

# The vaccination model functions
source(here::here("Vacc_epi_model","1_1_model_vaccine_waning.R"))
# Setup and run the vaccination model
source(here::here("Vacc_epi_model","1_2_Vaccination_Model.R"))

###### EPIDEMIC MODEL #######

#The epidemic model function
source(here::here("Vacc_epi_model","2_1b_model_epidemic_yearcross.R"))
# Run the vaccination model for different epidemics and vaccine scenarios
source(here::here("Vacc_epi_model","2_2_Epidemic_Model.R"))

# run the Foi inclusion scripts
if(calculate_FOI== "Yes"){
  source(here::here("Vacc_epi_model","3_0_data_for_FOI.R"))
  source(here::here("Vacc_epi_model","3_1_FOI_calculations.R"))
  source(here::here("Vacc_epi_model","4_0_combining_epidemics_background.R"))
}


