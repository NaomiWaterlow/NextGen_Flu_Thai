# look at results

library(cowplot)
n_samples <- posterior_sample_size
total_infections_time <- as.data.table(copy(total_infections_all))

# total_infections_time[scenario ==target_scenarios[1], scenario_nice := vaccine_scenario_names[1]]
# total_infections_time[scenario ==target_scenarios[2], scenario_nice := vaccine_scenario_names[2]]
# total_infections_time[scenario ==target_scenarios[3], scenario_nice := vaccine_scenario_names[3]]
# total_infections_time[scenario ==target_scenarios[4], scenario_nice := vaccine_scenario_names[4]]
# total_infections_time[scenario ==target_scenarios[5], scenario_nice := vaccine_scenario_names[5]]
# total_infections_time[scenario ==target_scenarios[6], scenario_nice := vaccine_scenario_names[6]]

total_infections_time <- merge(
  total_infections_time,
  vaccine_scenarios_tab[,.(scenario_id,NGIVtype,CoveragePercent,TargetAge)],
  by.x="scenario", by.y="scenario_id"
)

# total_infections_time$scenario_nice <- factor(total_infections_time$scenario_nice, 
#                                          levels = vaccine_scenario_names)

total_infections_time[, total_cases := X1+X2+X3+X4+X5+X6]
total_infections_time_sub <- total_infections_time[sample %in% seq(1,n_samples, n_samples/10),]
 
DETAIL_EPIS <- ggplot(total_infections_time[scenario==1 & Date < "2009-04-01"], aes(x = Date, y = total_cases, group = sample)) + 
  geom_line(alpha=0.03) + 
  # facet_grid(scenario_nice~Virus) + 
  facet_grid(Virus~.) + 
  # scale_colour_manual(values = c("orange1", "#91CF60", "#3288BD")) +
  theme_linedraw() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = -90), 
        strip.text.y = element_text(size = 7)) + 
  labs(x = "Date", y = "Infections", title="")

EPIS <- grid.arrange(AH1_EPIS,AH3_EPIS,B_EPIS,ncol=1)
COMBINED_EPIS <- grid.arrange(EPIS, DETAIL_EPIS, ncol=1)

 tiff(here::here(paste0(name_run,"_DETAILED_TIMELINE.tiff")),  width = 3250, height = 2000, res = 300, )
DETAIL_EPIS
 dev.off()
 
 
summary_table <- total_infections_time[,sum(total_cases), by = c("Date", "scenario", "NGIVtype","TargetAge", "sample")]
summary_table[,cumulative_sum := cumsum(V1), by = c("scenario", "NGIVtype","TargetAge", "sample")]

summary_table3 <- merge(
  summary_table[!scenario==1],
  summary_table[scenario==1,.(Date,sample,cumulative_sum)],
  by=c("Date","sample")
)
summary_table3[,cumulative_sum := cumulative_sum.y - cumulative_sum.x]

summary_table2 <- summary_table3[,quantile(cumulative_sum, 0.5), by = c("Date", "scenario", "NGIVtype","TargetAge")]
summary_table2$upper <- summary_table3[,quantile(cumulative_sum, 0.975), by = c("Date", "scenario", "NGIVtype","TargetAge")]$V1
summary_table2$lower <- summary_table3[,quantile(cumulative_sum, 0.025), by = c("Date", "scenario", "NGIVtype","TargetAge")]$V1

summary_table2$TargetAge <- factor(
  summary_table2$TargetAge,
  levels = c(
    "0 to 5 years",
    "6 to 11 years",  
    "12 to 17 years", 
    "2 to 11 years", 
    "0 to 11 years", 
    "0 to 17 years" 
  )
)


SUMMARY <- ggplot(
  summary_table2[Date<"2009-05-01"], 
  aes(x = as.Date(Date, origin = "1970-01-01"))
) + 
  geom_line(aes(y = V1/1000000)) + 
  geom_ribbon(aes(ymin = lower/1000000, ymax = upper/1000000,
                  fill= NGIVtype), alpha = 0.5)+
  facet_grid(TargetAge~NGIVtype) + 
  theme_bw() +
  scale_fill_manual(values = c("#d73027","orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) +
  labs(x = "Date", y = "Cumulative Infections averted (in millions)", fill = "Vaccine", title = "") + 
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 11), 
        strip.text = element_text(size = 12), 
        legend.text =  element_text(size = 11), 
        axis.text.x = element_text(angle = -90, vjust = 0.5), 
        legend.title = element_text(size = 12), 
        strip.background = element_blank(),
        # strip.text.x = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        legend.key.size = unit(0.5, "cm"))

legend1 <- get_legend(SUMMARY)

vac_summary <- merge(
  one_set_c,
  vaccine_scenarios_tab[,.(scenario_id=as.numeric(scenario_id),NGIVtype,CoveragePercent,TargetAge)],
  by.x="Vacc_scenario", by.y="scenario_id"
)
vac_summary$TargetAge <- factor(
  vac_summary$TargetAge,
  levels = c(
    "0 to 5 years",
    "6 to 11 years",  
    "12 to 17 years", 
    "2 to 11 years", 
    "0 to 11 years", 
    "0 to 17 years" 
  )
)

VACCS_GIVEN <- ggplot(
  vac_summary[!Vacc_scenario==1], 
  aes(x=Year,y=Vaccinations/1E6,fill=age_group)
  ) +
  geom_bar(position="stack",stat="identity") +
  theme_linedraw() +
  labs(y = "Total vaccinations given (millions)",
       title = "",
       colour = "Age group") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 9),
        legend.text =  element_text(size = 11),
        axis.text.x = element_text(angle = -90, vjust = 0.5),
        legend.title = element_text(size = 12),
        legend.key.size = unit(0.5, "cm"),
       panel.spacing = unit(0.5, "lines"),
       strip.background =element_rect(fill="gray31")) +
  facet_grid(TargetAge ~ NGIVtype)
  
legend2 <- get_legend(VACCS_GIVEN)

tiff(here::here(paste0(name_run,"_INFECTIONS_VACCINATIONS.tiff")),  width = 3500, height = 2000, res = 300, )
grid.arrange(SUMMARY + theme(legend.position = "NONE"),legend1,
             VACCS_GIVEN+ theme(legend.position = "NONE"), legend2,  layout_matrix = 
               rbind(c(1,1,1,1,2), 
                     c(3,3,3,3,4)))
dev.off()


# save(total_cases_time, file = here::here("UK_output", paste0(name_run,"_total_cases_time.Rdata")))

summary_for_text <- total_infections_time[,sum(total_cases),
                                     by = c("sample", "scenario", "scenario_nice")]
base_summary <- summary_for_text[scenario == base_scenario_to_use]
summary_for_text[base_summary, on = c("sample" ), base_total := i.V1 ]
summary_for_text[ ,percent_reduc := round(((base_total - V1) / base_total)*100, digits = 1)]
print("% of infections averted")
print(summary_for_text[, quantile(percent_reduc, probs = c(0.025, 0.5, 0.975)), by = c("scenario_nice")])

temp_c[is.na(Vaccinations), Vaccinations := 0]
total_vacc <- temp_c[, sum(Vaccinations), by = "Vacc_scenario"]

print("total vaccines given (millions)")
print(total_vacc$V1/1000000)
print(paste0("ratio of scenarios 4 and 6 is ", total_vacc[4,"V1"]/
               total_vacc[6,"V1"]))

colnames(total_vacc)[1] <- "scenario"
total_vacc$scenario <- factor(total_vacc$scenario)
summary_for_text[total_vacc, on = "scenario", tot_vacc := i.V1]
summary_for_text[, extra_averted :=  (base_total-V1)/tot_vacc ]

print("Extra cases averted per vaccine, compared to current scenario")
print("min")
print(summary_for_text[, min(extra_averted), by = "scenario"])
print(summary_for_text[, max(extra_averted), by = "scenario"])


