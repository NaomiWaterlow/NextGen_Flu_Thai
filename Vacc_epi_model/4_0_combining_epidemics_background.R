### combining background and epidemics components

colnames(overall_store)[which(colnames(overall_store) == "Vacc_scenario")] <- "scenario"
colnames(total_cases_time)[which(colnames(total_cases_time) == "Virus")] <- "virus_type"
overall_store <- overall_store[, c("Date", "scenario", "virus_type", "variable", "infections")]
total_cases_time$scenario <- as.numeric(as.character(total_cases_time$scenario))

total_cases_time_m <- melt.data.table(total_cases_time, id.vars = c("scenario", "sample", "week",
                                                         "Year", "Date", "virus_type", "epidemic"), 
                           measure.vars = c("X1", "X2", "X3", "X4", "X5", "X6"))
# summing across epidemics
total_cases_to_join <- total_cases_time_m[, sum(value), by =c("scenario", "sample", "week", "Date", "variable", "virus_type")]
colnames(total_cases_to_join)[7] <- "total_cases"

temp_list_store <- list()
for(i in 1:posterior_sample_size){
  
  overall_store[,sample := i]
  temp_list_store[[i]] <- copy(overall_store)
  
}

overall_store <- rbindlist(temp_list_store)
overall_store[variable == "rel_sus_1", variable := "X1"]
overall_store[variable == "rel_sus_2", variable := "X2"]
overall_store[variable == "rel_sus_3", variable := "X3"]
overall_store[variable == "rel_sus_4", variable := "X4"]
overall_store[variable == "rel_sus_5", variable := "X5"]
overall_store[variable == "rel_sus_6", variable := "X6"]

#overall_store$scenario <- as.factor(overall_store$scenario)

overall_store[total_cases_to_join, on = c("Date", "scenario", "virus_type", "sample", "variable"), epidemic_cases := i.total_cases]
overall_store[is.na(epidemic_cases), epidemic_cases := 0]

# don't want background infections when epidemic is running
overall_store[epidemic_cases != 0, infections := 0]
# combine
overall_store[, total_infections := infections + epidemic_cases]

#check it's combined and looks cageuly appropiate. 
# ggplot(overall_store, aes(x = Date, y = total_infections, group = sample)) + 
#   geom_line() + 
#   facet_grid(variable~virus_type)

# # Need to store it in the way the economics expects
# overall_store[variable == "rel_sus_1", variable := "X1"]
# overall_store[variable == "rel_sus_2", variable := "X2"]
# overall_store[variable == "rel_sus_3", variable := "X3"]
# overall_store[variable == "rel_sus_4", variable := "X4"]
# overall_store[variable == "rel_sus_5", variable := "X5"]
# overall_store[variable == "rel_sus_6", variable := "X6"]
overall_store[,scenario := as.factor(scenario)]
# INFECTIONS_TIME <- ggplot(overall_store[sample==1],
#        aes(x = Date, y = total_infections, group = interaction(sample, scenario), colour =scenario)) + 
#   geom_line(alpha=1) +
#   # geom_ribbon() +
#   facet_grid(virus_type~variable, scales= "free_y")  + scale_colour_manual(values = c("#d73027","orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) +
#   theme_linedraw() + 
#   theme(
#         axis.text.x = element_text(angle = -90), 
#         strip.text.y = element_text(size = 7)) + 
#   labs(x = "", y = "Infections")
# 
# print(INFECTIONS_TIME)
                 
total_infections_all <- dcast.data.table(overall_store, 
                                         Date + scenario + virus_type + sample ~ variable, 
                                         value.var = "total_infections")


total_infections_all[, Year := year(Date)]
colnames(total_infections_all)[which(colnames(total_infections_all) == "virus_type")] <- "Virus"

total_infections_all[, scenario := as.factor(scenario)]



