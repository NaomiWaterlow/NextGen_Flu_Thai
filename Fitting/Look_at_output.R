library(fluEvidenceSynthesis)
library(ggplot2)

# sepcify how many samples to take for spaghetti plot
n_samples <- 100
# can use this to load one
# epidemic <- epidemic_to_run

DENSITY <- list()
TRACE_THINNED <- list()
SPAGHETTI <- list()

 # counter
for (epidemic_to_run in 1:6){
  
  if(epidemic_to_run >6){
    epidemic_no = epidemic_to_run-6
  }
  output_list <-  readRDS(file = here::here("Fitting", "Fits", paste0("mcmc_",epidemic_to_run,"_to_use.Rdata"))) #"_to_use.Rdata"))) # "_10000_2023-01-25.Rdata")))
  
  colnames(output_list$posterior$batch) <- c("reporting", "transmissibility", "sus1",
                                             "infected", "blank", "blank")
  post_samples <- data.table(output_list$posterior$batch)
  # remove the blank ones
  post_samples[, blank := NULL ]
  post_samples[, blank := NULL ]
  # add the likelihoods
  post_samples[, ll := output_list$posterior$llikelihoods]
  post_samples$timestep <- 1:nrow(post_samples)
  post_samples_m <- melt.data.table(post_samples, id.vars = "timestep")
  
  DENSITY[[epidemic_to_run]] <- ggplot(post_samples_m) + 
    facet_wrap( ~ variable, ncol=5, scales="free" ) +
    geom_histogram(aes(x=value), bins=25) + 
    labs(title = paste0("Epidemic ", epidemic_to_run))
  DENSITY[[epidemic_to_run]]
  
  TRACE_THINNED[[epidemic_to_run]] <- ggplot(post_samples_m, aes(x = timestep, y = value)) + 
    facet_grid(variable~., scales = "free_y") + 
    geom_line() + 
    labs(title = paste0("Epidemic ", epidemic_to_run))
  
  TRACE_THINNED[[epidemic_to_run]]
  
  # Function that runs the model given a set of parameters. Most of the function
  # has todo with loading the correct inputs for the ODE model
  ode.results <- function( pars, input_demography = pop_by_age ) 
  {
    
    age.group.limits <- c(2,6,12,18,60)
    
    contacts <- fluEvidenceSynthesis::contact_matrix(as.matrix(polymod.thai),
                                                     input_demography, age.group.limits ) 
    
    age.groups <- stratify_by_age(input_demography, age.group.limits)
    # Ppulation size initially infected by age and risk group
    initial.infected <- rep( 10^pars[4], 6 ) 
    
    dates_to_run <- c(epidemics_to_fit[[epidemic_to_run]]$start,epidemics_to_fit[[epidemic_to_run]]$end)
    
    vaccine_calendar <- as_vaccination_calendar(efficacy = c(0,0,0,0,0,0), 
                                                dates = dates_to_run,
                                                coverage = matrix(0, nrow = 3,#length(dates_to_run), 
                                                                  ncol = 6), 
                                                no_age_groups = 6, no_risk_groups = 1)
    prop_vacc_start <- list(prop_vaccine_compartments = rep(0,18),
                            prop_R_vaccinated =rep(0,18), 
                            prop_R = rep(0,18))
    # Run simulation
    # Note that to reduce complexity 
    # by using the same susceptibility parameter for multiple age groups
    pars[2] <- pars[2]/100
    odes2 <- incidence_function_fit(demography_input =popthai[,2], 
                                    parameters = pars,
                                    calendar_input= vaccine_calendar,
                                    contact_ids_sample = as.matrix(polymod.thai),
                                    waning_rate = 0,
                                    vaccination_ratio_input = prop_vacc_start,
                                    begin_date = dates_to_run[1], 
                                    end_date = dates_to_run[2] ,  
                                    year_to_run = year(dates_to_run[1]), 
                                    efficacy_now =rep(0,18) , 
                                    efficacy_next=rep(0,18) ,
                                    efficacy_next2 =rep(0,18), 
                                    previous_summary =NA, 
                                    age_groups_model = age.group.limits)
    
    
    
    # For simplicity we sum the low and high risk group
    odes <- data.table(odes2)
    # Ignore times row
    odes[,Month := month(as.Date(as.character(time)))]
    odes[,time := NULL]
    odes[, lapply(.SD, sum, na.rm=TRUE), by="Month" ]
    monthly_cases <-  odes[, sum(.SD, na.rm=TRUE), by="Month" ]
    
    return( monthly_cases )}
  
  
  sample_no<- sample(c(1:nrow(post_samples)), n_samples, replace=T)
  out_run <- list()
  
  for(i in 1:n_samples){
    
    test_params <- unlist(post_samples[sample_no[i],])
    temp_out <-  ode.results(unlist(test_params)[1:4])
    temp_out$timestep <- 1:nrow(temp_out)
    temp_out$V1 <-temp_out$V1* exp(test_params[1])
    out_run[[i]] <- temp_out 
    
  }
  
  spaghetti_out <- rbindlist(out_run, use.names = T, idcol = "sample")
  
  data_fitted <- data.frame(V1 = epidemics_to_fit[[output_list$epidemic_ran]]$data_points)
  data_fitted$timestep <- 1:nrow(data_fitted)                    
  
  # print(
 SPAGHETTI[[epidemic_to_run]] <- ggplot(spaghetti_out, aes(x = timestep, y = V1, group = sample)) + 
    geom_line(alpha = 0.5)+ 
    geom_point(data = data_fitted,aes(x = timestep, y = V1, group =NULL),
               colour = "red") + 
    labs(title = paste0("Epidemic ", epidemic_to_run), x="Month", y="Cases")
  # )
 
}
# c(-7, 0.07, 0.9, 0.9, 0.9, 0.4)# gives an outbreak

# DENSITY <- grid.arrange(DENSITY[[1]],DENSITY[[2]],DENSITY[[3]],DENSITY[[4]],DENSITY[[5]],DENSITY[[6]],ncol=1,nrow=6)
SPAGHETTI_ALL <- grid.arrange(SPAGHETTI[[1]],SPAGHETTI[[2]],SPAGHETTI[[3]],SPAGHETTI[[4]],SPAGHETTI[[5]],SPAGHETTI[[6]],ncol=2,nrow=3)
