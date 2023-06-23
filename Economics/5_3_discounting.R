# Discounting and combining all the economics
# base_scenario_to_use <- 1
# thai.gdppc <- 7066.2 # Thai 2021 GDP per capita from World Bank. 
# disc.rate.costs <- 0.03

# calculate years since base
for (i in 1:length(seasons)){
  outcomes[Season==seasons[i],discounting_years := i-1]
}
outcomes[, disc_factor.dalys := (1 + disc.rate.dalys)^-discounting_years] # uses disc.rate that was used to calculated the discounted life expectancy
outcomes[, disc.rate.costs := disc.rate.costs]
outcomes[, disc_factor.costs := (1 + disc.rate.costs)^-discounting_years]

outcomes[,disc_costs.total := costs.total * disc_factor.costs]
outcomes[,disc_costs.outcomes := costs.outcomes * disc_factor.costs]
outcomes[,disc_DALYs := DALYs * disc_factor.dalys]

outcomes[,disc_vaccs := Vaccinations * disc_factor.costs] #used for threshold price calculations
outcomes[,disc_costs.vacc_delivery := (costs.vacc - costs.vacc_purchase) * disc_factor.costs]
outcomes[,disc_costs.vacc_purchase := costs.vacc_purchase * disc_factor.costs]

# sum across age groups
sum_outcomes <- outcomes[
  , c(
    "sample",
    "scenario",
    "Season",
    "disc.rate.costs",
    "disc.rate.dalys",
    "infections",
    "Symptomatic",
    "Deaths",
    "IPcases",
    "OPcases",
    "DALYs",
    "costs.vacc",
    "costs.vacc_purchase",
    "costs.total",
    "disc_DALYs",
    "disc_costs.total",
    "disc_costs.outcomes",
    "disc_vaccs",
    "disc_costs.vacc_purchase"
  )
][
  ,lapply(.SD, sum, na.rm=T),
  by = c("sample","scenario","Season","disc.rate.dalys","disc.rate.costs")
] 

# DALYS <- ggplot(sum_outcomes[sample==1],
#                 aes(x = scenario, y = disc_DALYs, colour = Season)) +
#   geom_point() +
#   # facet_grid(scenario~disc.rate.dalys) +
#   labs(y = "discounted DALYs") +
#   theme_linedraw()
# 
# COSTS <- ggplot(sum_outcomes,
#                 aes(x = Season, y = disc_costs.outcomes, colour = scenario)) +
#   geom_point() +
#   facet_grid(scenario~.) +
#   labs(y = "discounted healthcare costs") + theme_linedraw()

# combine across seasons
summary_stats <- copy(sum_outcomes)
summary_stats <- summary_stats[,Season:=NULL][
  ,lapply(.SD, sum, na.rm=T),
  by = c("sample","scenario","disc.rate.dalys","disc.rate.costs")
]

base_scenario <- summary_stats[scenario==base_scenario_to_use]
summary_stats[base_scenario, on = c("sample","disc.rate.dalys"), base_DALYs := i.disc_DALYs]
summary_stats[base_scenario, on = c("sample","disc.rate.dalys"), base_costs.total := i.disc_costs.total]

summary_stats[,inc_DALYs := disc_DALYs - base_DALYs]
summary_stats[,inc_costs.total := disc_costs.total - base_costs.total]

summary_stats[, icer := inc_costs.total/inc_DALYs]
summary_stats[, icer_gained := -icer] # DALYs averted

summary_stats <- merge(summary_stats,vaccine_scenarios_tab[,.(scenario_id,CoveragePercent,TargetAge,NGIVtype)],by.x="scenario",by.y="scenario_id")

summary_stats$NGIVtype <- factor(
  summary_stats$NGIVtype,
  levels = c("No vaccine","Current seasonal","Improved (minimal)","Improved (efficacy)" ,"Improved (breadth)","Universal")
)
summary_stats$TargetAge <- factor(
  summary_stats$TargetAge,
  levels = c("0 to 5 years","0 to 11 years","0 to 17 years","2 to 11 years","6 to 11 years","12 to 17 years")
)


# summary_stats[scenario == target_scenarios[1], scenario_name := vaccine_scenario_names[1]]
# summary_stats[scenario == target_scenarios[2], scenario_name := vaccine_scenario_names[2]]
# summary_stats[scenario == target_scenarios[3], scenario_name := vaccine_scenario_names[3]]
# summary_stats[scenario == target_scenarios[4], scenario_name := vaccine_scenario_names[4]]
# summary_stats[scenario == target_scenarios[5], scenario_name := vaccine_scenario_names[5]]
# summary_stats[scenario == target_scenarios[6], scenario_name := vaccine_scenario_names[6]]
# summary_stats$scenario_name <- factor(summary_stats$scenario_name, 
#                                       levels = vaccine_scenario_names)



ICERS <- ggplot(
  summary_stats[!(scenario %in% c(1))], #& CoveragePercent == "50%"], 
  aes(x = NGIVtype, y = icer_gained, fill = NGIVtype)
) +
  geom_boxplot(outlier.shape = NA, notch=TRUE) +
  # geom_violin() +
  theme_linedraw() +
  labs(x = "Scenario", y = "ICER (Inc cost per DALY averted in USD)", title="Target age group and coverage", legend="Vaccine type") + 
  scale_fill_manual(values = c("orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) +
  theme(axis.text.x = element_blank(),
        legend.position = "bottom") +
  facet_grid(disc.rate.dalys~TargetAge+CoveragePercent)


# INMB
summary_stats[, INMB :=  ((-inc_DALYs * threshold) - inc_costs.total)/1E6] # INMB in millions

INMBS <- ggplot(
  summary_stats[scenario != 1,], 
  aes(x = NGIVtype, y = INMB, fill = NGIVtype)
) + 
  geom_boxplot(outlier.shape = NA, notch=TRUE) +
  theme_linedraw() + 
  labs(x = "Scenario", y = "INMB (in millions USD)", title="Target age group and coverage") + 
  scale_fill_manual(values = c("orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) +
  theme(legend.position = "bottom", 
        # axis.text.x = element_text(angle = -90),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  scale_x_discrete(labels = function(y) str_wrap(y, width = 10)) +
  facet_grid(.~TargetAge+CoveragePercent)


ICERS
INMBS

PLANE <- ggplot(
  data = summary_stats[!(scenario %in% c(1))],
  aes(x = -inc_DALYs/1E3, y = inc_costs.total/1E6, colour = NGIVtype)
) + 
  geom_point(shape=".") + 
  stat_ellipse() +
  theme_linedraw() + 
  labs(x = "Incremental DALYs averted (thousands)", y = "Incremental costs (in millions USD)", 
       colour = "Scenario", title = "Target age group and coverage") + 
  scale_colour_manual(values = c("orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) +
  geom_abline(intercept=0, slope=threshold/1000, linetype="dashed") +
  facet_grid(CoveragePercent~TargetAge)


# Vaccine threshold price
summary_stats[
  !(scenario %in% c(1)), 
  threshold_price := 
    ((-inc_DALYs * threshold) - inc_costs.total + disc_costs.vacc_purchase) / # this is the INMB with a vaccine price of zero
    disc_vaccs                                                              # dividing this through by the discounted number of vaccines gives the threshold price
]

THRESHOLD_PRICES <- ggplot(
  data = summary_stats[!(scenario %in% c(1))], 
  aes(x = NGIVtype, y = threshold_price, fill = NGIVtype)
) + 
  geom_boxplot(outlier.shape = NA, notch=TRUE) +
  theme_linedraw() + 
  labs(x = "Scenario", y = "Vaccine Threshold Price (USD)", title="Target age group and coverage") + 
  scale_fill_manual(values = c("orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) + 
  theme(legend.position = "bottom", 
        # axis.text.x = element_text(angle = -90), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  scale_x_discrete(labels = function(y) str_wrap(y, width = 10)) +
  facet_grid(.~TargetAge+CoveragePercent)

# tables

thresh_price <- summary_stats[, quantile(threshold_price, probs = c(0.025, 0.5, 0.975), na.rm=T), by = c("scenario_name","disc.rate.dalys")]
thresh_price$type <- rep(c("lower", "median", "upper"), 6)
thresh_price <- dcast.data.table(thresh_price, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Infections")
print(thresh_price)

infs <- summary_stats[, quantile(infections, probs = c(0.025, 0.5, 0.975), na.rm=T), by = c("scenario_name","disc.rate.dalys")]
infs$type <- rep(c("lower", "median", "upper"), 6)
infs <- dcast.data.table(infs, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Infections")
print(infs)

op_cases <- summary_stats[, quantile(OPcases, probs = c(0.025, 0.5, 0.975), na.rm=T), by = c("scenario_name","disc.rate.dalys")]
op_cases$type <- rep(c("lower", "median", "upper"), 6)
op_cases <- dcast.data.table(op_cases, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Outpatient cases")
print(op_cases)

ip_cases <- summary_stats[, quantile(IPcases, probs = c(0.025, 0.5, 0.975), na.rm=T), by = c("scenario_name","disc.rate.dalys")]
ip_cases$type <- rep(c("lower", "median", "upper"), 6)
ip_cases <- dcast.data.table(ip_cases, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Inpatient cases")
print(ip_cases)


Deaths <- summary_stats[, quantile(Deaths, probs = c(0.025, 0.5, 0.975), na.rm=T), by = c("scenario_name","disc.rate.dalys")]
Deaths$type <- rep(c("lower", "median", "upper"), 6)
Deaths <- dcast.data.table(Deaths, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Deaths")
print(ip_cases)

vac_price <- summary_stats[, quantile(threshold_price, probs = c(0.025, 0.5, 0.975), na.rm=T), by = c("scenario_name","disc.rate.dalys")]
vac_price$type <- rep(c("lower", "median", "upper"), 6)
vac_price <- dcast.data.table(vac_price, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Threshold vaccine price")
print(vac_price)

tot_costs <- summary_stats[, quantile(disc_costs.total, probs = c(0.025, 0.5, 0.975)), by = c("scenario_name","disc.rate.dalys")]
tot_costs$type <- rep(c("lower", "median", "upper"), 6)
tot_costs <- dcast.data.table(tot_costs, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Total Costs")
print(tot_costs)

out_costs <- summary_stats[, quantile(disc_costs.outcomes, probs = c(0.025, 0.5, 0.975)), by = c("scenario_name","disc.rate.dalys")]
out_costs$type <- rep(c("lower", "median", "upper"), 6)
out_costs <- dcast.data.table(out_costs, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Health Outcome Costs")
print(out_costs)

inc_costs <- summary_stats[, quantile(inc_costs.total, probs = c(0.025, 0.5, 0.975)), by = c("scenario_name","disc.rate.dalys")]
inc_costs$type <- rep(c("lower", "median", "upper"), 6)
inc_costs <- dcast.data.table(inc_costs, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Incremental Costs")
print(inc_costs)

vac_costs <- summary_stats[, quantile(costs.vacc, probs = c(0.025, 0.5, 0.975)), by = c("scenario_name","disc.rate.dalys")]
vac_costs$type <- rep(c("lower", "median", "upper"), 6)
vac_costs <- dcast.data.table(vac_costs, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Total Vaccinattion Costs")
print(vac_costs)

pur_costs <- summary_stats[, quantile(costs.vacc_purchase, probs = c(0.025, 0.5, 0.975)), by = c("scenario_name","disc.rate.dalys")]
pur_costs$type <- rep(c("lower", "median", "upper"), 6)
pur_costs <- dcast.data.table(pur_costs, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Total Vaccine Purchase Costs")
print(pur_costs)

tot_DALYs <- summary_stats[, quantile(disc_DALYs, probs = c(0.025, 0.5, 0.975)), by = c("scenario_name","disc.rate.dalys")]
tot_DALYs$type <- rep(c("lower", "median", "upper"), 6)
tot_DALYs <- dcast.data.table(tot_DALYs, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Total DALYs")
print(tot_DALYs)

inc_DALYs <- summary_stats[, quantile(inc_DALYs, probs = c(0.025, 0.5, 0.975)), by = c("scenario_name","disc.rate.dalys")]
inc_DALYs$type <- rep(c("lower", "median", "upper"), 6)
inc_DALYs <- dcast.data.table(inc_DALYs, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("Incremental DALYs")
print(inc_DALYs)

inmbs <- summary_stats[, quantile(INMB, probs = c(0.025, 0.5, 0.975)), by = c("scenario_name","disc.rate.dalys")]
inmbs$type <- rep(c("lower", "median", "upper"), 6)
inmbs <- dcast.data.table(inmbs, scenario_name + disc.rate.dalys ~ type, value.var = "V1")
print("INMBs")
print(inmbs)

icers <- summary_stats[, quantile(icer_gained, probs = c(0.025, 0.5, 0.975), na.rm = T), by = c("scenario_name","disc.rate.dalys")]
icers$type <- rep(c("lower", "median", "upper"), 6)
icers <- dcast.data.table(icers, scenario_name + disc.rate.dalys~ type, value.var = "V1")
print("icers")
print(icers)




INMBS <- ggplot(summary_stats[scenario_name != vaccine_scenario_names[1],], aes(x = scenario_name, y = INMB, fill = scenario_name)) + 
  geom_boxplot() + 
  theme_linedraw() + 
  labs(x = "Scenario", y = "INMB (in millions)", title="") + 
  scale_fill_manual(values = c("orange1", "#91CF60", "#92C5DE", "#3288BD","purple" )) + 
  theme(legend.position = "None", 
        axis.text.x = element_text(angle = -90), 
        axis.title.x = element_blank()) + 
  scale_x_discrete(labels = function(y) str_wrap(y, width = 10)) +
  facet_grid(disc.rate.dalys~.)

tot_costs$lower  <- tot_costs$lower/1E6
tot_costs$median <- tot_costs$median/1E6
tot_costs$upper  <- tot_costs$upper/1E6
tot_costs[, `Total Costs (millions)` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
tot_costs[, c("disc.rate.dalys","lower", "median", "upper") := NULL]

out_costs$lower  <- out_costs$lower/1E6
out_costs$median <- out_costs$median/1E6
out_costs$upper  <- out_costs$upper/1E6
out_costs[, `Total Costs (millions)` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
out_costs[, c("scenario_name", "disc.rate.dalys","lower", "median", "upper") := NULL]

vac_costs$lower  <- vac_costs$lower/1E6
vac_costs$median <- vac_costs$median/1E6
vac_costs$upper  <- vac_costs$upper/1E6
vac_costs[, `Vaccine Costs (millions)` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
vac_costs[, c("scenario_name", "disc.rate.dalys","lower", "median", "upper") := NULL]

inc_costs$lower  <- inc_costs$lower/1E6
inc_costs$median <- inc_costs$median/1E6
inc_costs$upper  <- inc_costs$upper/1E6
inc_costs[, `Incremental Costs (millions)` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
inc_costs[, c("scenario_name", "disc.rate.dalys","lower", "median", "upper") := NULL]

ip_cases$lower  <- ip_cases$lower/1E3
ip_cases$median <- ip_cases$median/1E3
ip_cases$upper  <- ip_cases$upper/1E3
ip_cases[, `Inpatient cases (thousands)` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
ip_cases[, c("scenario_name", "disc.rate.dalys","lower", "median", "upper") := NULL]

op_cases$lower  <- op_cases$lower/1E3
op_cases$median <- op_cases$median/1E3
op_cases$upper  <- op_cases$upper/1E3
op_cases[, `Outpatient cases (thousands)` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
op_cases[, c("scenario_name", "disc.rate.dalys","lower", "median", "upper") := NULL]

Deaths$lower  <- Deaths$lower/1E3
Deaths$median <- Deaths$median/1E3
Deaths$upper  <- Deaths$upper/1E3
Deaths[, `Deaths (thousands)` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
Deaths[, c("scenario_name", "disc.rate.dalys","lower", "median", "upper") := NULL]

tot_DALYs$lower  <- tot_DALYs$lower/1E3
tot_DALYs$median <- tot_DALYs$median/1E3
tot_DALYs$upper  <- tot_DALYs$upper/1E3
tot_DALYs[, `Total DALYs (thousands)` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
tot_DALYs[, c("scenario_name", "disc.rate.dalys","lower", "median", "upper") := NULL]

inc_DALYs$lower  <- inc_DALYs$lower/1E3
inc_DALYs$median <- inc_DALYs$median/1E3
inc_DALYs$upper  <- inc_DALYs$upper/1E3
inc_DALYs[, `Incremental DALYs (thousands)` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
inc_DALYs[, c("scenario_name", "disc.rate.dalys","lower", "median", "upper") := NULL]

infs$lower  <- infs$lower/1E6
infs$median <- infs$median/1E6
infs$upper  <- infs$upper/1E6
infs[, `Total infections (millions)` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
infs[, c("scenario_name", "disc.rate.dalys","lower", "median", "upper") := NULL]

thresh_price[, `Threshold_price` := sprintf("%.4g (%.4g, %.4g)",median,lower,upper)]
thresh_price[, c("disc.rate.dalys","lower", "median", "upper") := NULL]


summary_table <- cbind(tot_costs, vac_costs, inc_costs, ip_cases, op_cases, Deaths, tot_DALYs, inc_DALYs)
