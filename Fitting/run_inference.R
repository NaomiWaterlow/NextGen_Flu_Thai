###### packages ######

library(fluEvidenceSynthesis)
library(rriskDistributions)
library(data.table)
library(socialmixr)
library(ggplot2)
library(gridExtra)
library(wpp2019)
library(beepr)
library(tictoc)

###### things to specify #####

epidemic_to_run <- 1 # <<<<< TO UPDATE: NAOMI RUN 1 TO 6, SIMON RUN 7 TO 12, NEED TO BE SET RUNNING MANUALLY SINCE CODE ISN'T PARALLELISED
post_size <- 10000
thinning_steps <- 100
burn_in <- 500000
seed_to_use <- 55 # <<<<< TO UPDATE: NAOMI USE 55, SIMON USE 99)
save <- T

#### loading in all the neccesary things #####
source(here::here("Fitting","data_for_fitting.R"))
source(here::here("Fitting","epidemics.R"))
source(here::here("Vacc_epi_model","2_0_epidemics_list_Thai.R"))
epidemics_list[7:12] <- epidemics_list[1:6] 
source(here::here("Vacc_epi_model","2_1b_model_epidemic_yearcross.R"))
source(here::here("Fitting","inference_function.R"))
source(here::here("Fitting","creating_contacts.R"))
source(here::here("Fitting","prior_distribution_specification.R"))
infection_delays <- c(0.8,1.8)
high_risk <- rep(0,6)
risk_ratios_input <- matrix(c(high_risk,0,0,0,0,0,0),
                            ncol = 6 , byrow = T)
set.seed(seed_to_use) #set.seed(5042)
contacts_matrixformat <- fluEvidenceSynthesis::contact_matrix(
  as.matrix(polymod.thai),
  popthai[,2], 
  c(2, 6, 12, 18, 60)  
)

if(epidemic_to_run >6){
  epidemic_no = epidemic_to_run-6
}

dates_to_run <- c(epidemics_to_fit[[epidemic_to_run]]$start,epidemics_to_fit[[epidemic_to_run]]$end)

vaccine_calendar <- as_vaccination_calendar(efficacy = c(0,0,0,0,0,0), 
                                            dates = dates_to_run,
                                            coverage = matrix(0, nrow = 3,#length(dates_to_run), 
                                                              ncol = 6), 
                                            no_age_groups = 6, no_risk_groups = 1)

# for some reason the function seems to require at least 
initial_parameters <- epidemics_to_fit[[epidemic_to_run]]$initial_params
names(initial_parameters) <- c("reporting", "transmissibility","susceptibility","initial_infected")
# initial infected to the power of, reporting is on log scale

##### Run the fit #####

tic()
set.seed(seed_to_use)

output <- custom_inference(
  input_demography = pop_by_age, 
  vaccine_calendar = vaccine_calendar, 
  input_polymod = polymod.thai, 
  ili = NULL, 
  mon_pop = NULL, 
  n_pos = epidemics_to_fit[[epidemic_to_run]]$data_points,
  n_samples = NULL,
  initial = initial_parameters,
  mapping = NULL, 
  nbatch = post_size, 
  nburn = burn_in, 
  blen = thinning_steps
  )


output_list <- list (
  epidemic_ran = epidemic_to_run,
  posterior = output, 
  post_size = post_size, 
  thinning_steps = thinning_steps, 
  burn_in = burn_in, 
  seed = seed_to_use
)

if(save == T){
  saveRDS(output_list, file = here::here(
    "Fitting", "Fits", 
    paste0("mcmc_", epidemic_to_run,"_", post_size, "_", Sys.time(),".Rdata"))
  )
}
toc()
beep(4)



