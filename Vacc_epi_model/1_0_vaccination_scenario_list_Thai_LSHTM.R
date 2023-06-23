# List of the vccination scenarios required

# NOTE: All the dates are releative to a year start date of 2005-01-01

# specify the number of days per year (leap years!) (2005 - 2009)
year_days <- c(365,365,365,366,365)

#find values from literature
# from https://www.cdc.gov/flu/vaccines-work/past-seasons-estimates.html
# assuming SH and NH the same, and differetn viral subtypes the saem

# Have updated for Thailand using Table 3 from Chittaganpitch et al.

H1_matches <- c(
  "2005_SH" = "MATCH", #"2005_SH" = "NOMATCH",
  "2005_NH" = "MATCH", #"2005_NH" = "NOMATCH",
  "2006_SH" = "MATCH",
  "2006_NH" = "MATCH",
  "2007_SH" = "NOMATCH",
  "2007_NH" = "MATCH", #"2007_NH" = "NOMATCH",
  "2008_SH" = "NOMATCH", #"2008_SH" = "MATCH",
  "2008_NH" = "MATCH",
  "2009_SH" = "NOMATCH", #"2009_SH" = "MATCH", 
  "2009_NH" = "NOMATCH", #"2009_NH" = "MATCH",
  "20010_SH" = NA
)
# 
H3_matches <- c(
  "2005_SH" = "NOMATCH",
  "2005_NH" = "MATCH", #"2005_NH" = "NOMATCH",
  "2006_SH" = "NOMATCH", #"2006_SH" = "MATCH",
  "2006_NH" = "MATCH",
  "2007_SH" = "NOMATCH",
  "2007_NH" = "NOMATCH",
  "2008_SH" = "MATCH",
  "2008_NH" = "MATCH",
  "2009_SH" = "MATCH", 
  "2009_NH" = "MATCH",
  "20010_SH" = NA
)
# 
B_matches <- c(
  "2005_SH" = "NOMATCH",
  "2005_NH" = "NOMATCH",
  "2006_SH" = "NOMATCH", #"2006_SH" = "MATCH",
  "2006_NH" = "NOMATCH", #"2006_NH" = "MATCH",
  "2007_SH" = "NOMATCH",
  "2007_NH" = "NOMATCH",
  "2008_SH" = "MATCH",
  "2008_NH" = "MATCH",
  "2009_SH" = "NOMATCH", #"2009_SH" = "MATCH", 
  "2009_NH" = "MATCH",
  "20010_SH" = NA
)


efficacies <- list(
  efficacy_constant_0.9 = c(0.9,0.9),
  efficacy_constant_0.7 = c(0.7,0.7),
  efficacy_NOMATCH_0.4 = c(0.7,0.4),
  efficacy_NOMATCH_0.7 = c(0.9,0.7)
)

# start in march as this is the "year end" / "birthday"
campaigns <- list(
  dates_campaign = c("-05-01","-07-31")#, # May to July (Making it -07-31, so that 90 days generated in model)
  # dates_yearround = c("-03-02") # all year round
)

# this is the coverage over the dates period
# i.e. over one year in the year round scenario, and over the 3 months in the 3 month scenario
# NOTE: Have added 10% coverage in over 60s to all scenarios in line with Meeyai et al.
coverages <- list(
  coverage_low_0to5    = c(0.25, 0.25, 0.00, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_mid_0to5    = c(0.5 , 0.50, 0.00, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_high_0to5   = c(0.75, 0.75, 0.00, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_low_0to11   = c(0.25, 0.25, 0.25, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_mid_0to11   = c(0.50, 0.50, 0.50, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_high_0to11  = c(0.75, 0.75, 0.75, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_low_0to17   = c(0.25, 0.25, 0.25, 0.25, 0.00, 0.10, rep(0,12)),
  coverage_mid_0to17   = c(0.50, 0.50, 0.50, 0.50, 0.00, 0.10, rep(0,12)),
  coverage_high_0to17  = c(0.75, 0.75, 0.75, 0.75, 0.00, 0.10, rep(0,12)),
  coverage_low_2to11   = c(0.00, 0.25, 0.25, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_mid_2to11   = c(0.00, 0.50, 0.50, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_high_2to11  = c(0.00, 0.75, 0.75, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_low_6to11   = c(0.00, 0.00, 0.25, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_mid_6to11   = c(0.00, 0.00, 0.50, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_high_6to11  = c(0.00, 0.00, 0.75, 0.00, 0.00, 0.10, rep(0,12)),
  coverage_low_12to17  = c(0.00, 0.00, 0.00, 0.25, 0.00, 0.10, rep(0,12)),
  coverage_mid_12to17  = c(0.00, 0.00, 0.00, 0.50, 0.00, 0.10, rep(0,12)),
  coverage_high_12to17 = c(0.00, 0.00, 0.00, 0.75, 0.00, 0.10, rep(0,12))
)

durations <- c(
  six_months = 1/(365.25*0.5),
  one_year = 1/(365.25*1),
  two_year = 1/(365.25*2),
  three_year = 1/(365.25*3),
  four_year = 1/(365.25*4),
  five_year = 1/(365.25*5)
)

vaccine_scenarios <- list()
i <- 1


efficacy_H3 <- matrix(nrow= 18, ncol = 11)
efficacy_H3[1,] <- efficacy_H3[2,] <- efficacy_H3[3,] <- efficacy_H3[4,] <- 
  efficacy_H3[5,] <- efficacy_H3[6,] <- efficacy_H3[7,] <- efficacy_H3[8,] <- 
  efficacy_H3[9,] <- efficacy_H3[10,] <- efficacy_H3[11,] <- efficacy_H3[12,] <- 
  efficacy_H3[13,] <- efficacy_H3[14,] <- efficacy_H3[15,] <- efficacy_H3[16,] <- 
  efficacy_H3[17,] <- efficacy_H3[18,]  <- H3_matches
efficacy_H3_copy <- matrix(nrow= 18, ncol = 11)

efficacy_H1 <- matrix(nrow= 18, ncol = 11)
efficacy_H1[1,] <- efficacy_H1[2,] <- efficacy_H1[3,] <- efficacy_H1[4,] <- 
  efficacy_H1[5,] <- efficacy_H1[6,] <- efficacy_H1[7,] <- efficacy_H1[8,] <- 
  efficacy_H1[9,] <- efficacy_H1[10,] <- efficacy_H1[11,] <- efficacy_H1[12,] <- 
  efficacy_H1[13,] <- efficacy_H1[14,] <- efficacy_H1[15,] <- efficacy_H1[16,] <- 
  efficacy_H1[17,] <- efficacy_H1[18,]  <- H1_matches
efficacy_H1_copy <- matrix(nrow= 18, ncol = 11)

efficacy_B <- matrix(nrow= 18, ncol = 11)
efficacy_B[1,] <- efficacy_B[2,] <- efficacy_B[3,] <- efficacy_B[4,] <- 
  efficacy_B[5,] <- efficacy_B[6,] <- efficacy_B[7,] <- efficacy_B[8,] <- 
  efficacy_B[9,] <- efficacy_B[10,] <- efficacy_B[11,] <- efficacy_B[12,] <- 
  efficacy_B[13,] <- efficacy_B[14,] <- efficacy_B[15,] <- efficacy_B[16,] <- 
  efficacy_B[17,] <- efficacy_B[18,]  <- B_matches
efficacy_B_copy <- matrix(nrow= 18, ncol = 11)

# no vaccine scenario
vaccine_scenarios[[1]] <- list(waning_rate = 1, 
                      efficacy_H3 = matrix(0,nrow=18,ncol=10), 
                      efficacy_H1 = matrix(0,nrow=18,ncol=10),
                      efficacy_B = matrix(0,nrow=18,ncol=10), 
                      dates = c("-03-02"),  # all year round
                      coverage = rep(0,18), 
                      prop_group_vacc = rep(0,6))



for(scen_duration in durations){
  for(scen_coverage  in coverages){
    for(scen_campaign in campaigns){
      for(scen_efficacy in efficacies){
        
        
        
       efficacy_B_copy[which(efficacy_B=="MATCH")] <- as.numeric(scen_efficacy[1])
       efficacy_B_copy[which(efficacy_B=="NOMATCH")] <- as.numeric(scen_efficacy[2])
       
       efficacy_H3_copy[which(efficacy_H3=="MATCH")] <- as.numeric(scen_efficacy[1])
       efficacy_H3_copy[which(efficacy_H3=="NOMATCH")] <- as.numeric(scen_efficacy[2])
       
       efficacy_H1_copy[which(efficacy_H1=="MATCH")] <- as.numeric(scen_efficacy[1])
       efficacy_H1_copy[which(efficacy_H1=="NOMATCH")] <- as.numeric(scen_efficacy[2])
        
       waning_years <- 1/(scen_duration*365.25)
       if(waning_years <1 ) {prop_group_vacc = rep(1, 6)} else{
      prop_group_vacc = c(1/waning_years,1/waning_years,1/waning_years,1/waning_years,1/waning_years,1/waning_years)}
       
        vaccine_scenarios[[i+1]] = list(
          waning_rate = scen_duration, 
          efficacy_H3 = efficacy_H3_copy,
          efficacy_H1 = efficacy_H1_copy,
          efficacy_B = efficacy_B_copy,
          dates = scen_campaign, 
          coverage = scen_coverage,
          prop_group_vacc = prop_group_vacc
        )
        i <- i+1
      }
    }
  }
}

lookup_year <- names(B_matches)

# Tabulate vaccine scenarios
i <- 1
# No vaccine scenarios
vaccine_scenarios_tab <-  cbind(
  scenario_id = as.integer(i),
  immunity_duration = "no vaccine",
  coverage = "no vaccine",
  campaign = "no vaccine",
  efficacy = "no vaccine"
)
i <- i + 1


for(d in 1:length(durations)){
  for(cov  in 1:length(coverages)){
    for(camp in 1:length(campaigns)){
      for(eff in 1:length(efficacies)){
        
        vaccine_scenarios_tab <- rbind(
          cbind(
            scenario_id = as.integer(i),
            immunity_duration = names(durations[d]),
            coverage = names(coverages[cov]),
            campaign = names(campaigns[camp]),
            efficacy = names(efficacies[eff])
          ),
          vaccine_scenarios_tab
        ) 
        i <- i+1
      }
    }
  }
}
vaccine_scenarios_tab <- as.data.table(vaccine_scenarios_tab)

vaccine_scenarios_tab[
  immunity_duration=="six_months" & efficacy=="efficacy_NOMATCH_0.4",
  NGIVtype := "Current seasonal"
]
vaccine_scenarios_tab[
  immunity_duration=="one_year" & efficacy=="efficacy_NOMATCH_0.4",
  NGIVtype := "Improved (minimal)"
]
vaccine_scenarios_tab[
  immunity_duration=="two_year" & efficacy=="efficacy_NOMATCH_0.7",
  NGIVtype := "Improved (efficacy)"
]
vaccine_scenarios_tab[
  immunity_duration=="three_year" & efficacy=="efficacy_constant_0.7",
  NGIVtype := "Improved (breadth)"
]
vaccine_scenarios_tab[
  immunity_duration=="five_year" & efficacy=="efficacy_constant_0.9",
  NGIVtype := "Universal"
]
vaccine_scenarios_tab[
  immunity_duration=="no vaccine",
  NGIVtype := "No vaccine"
]

vaccine_scenarios_tab[
  coverage %like% "_high_",
  CoveragePercent := "75%"
]
vaccine_scenarios_tab[
  coverage %like% "_mid_",
  CoveragePercent := "50%"
]
vaccine_scenarios_tab[
  coverage %like% "_low_",
  CoveragePercent := "25%"
]

vaccine_scenarios_tab[
  coverage %like% "_0to5",
  TargetAge := "0 to 5 years"
]
vaccine_scenarios_tab[
  coverage %like% "_0to11",
  TargetAge := "0 to 11 years"
]
vaccine_scenarios_tab[
  coverage %like% "_0to17",
  TargetAge := "0 to 17 years"
]
vaccine_scenarios_tab[
  coverage %like% "_2to11",
  TargetAge := "2 to 11 years"
]
vaccine_scenarios_tab[
  coverage %like% "_6to11",
  TargetAge := "6 to 11 years"
]
vaccine_scenarios_tab[
  coverage %like% "_12to17",
  TargetAge := "12 to 17 years"
]



scenarios_to_include <- sort(
  as.numeric(
    vaccine_scenarios_tab[
      !is.na(NGIVtype) & (coverage=="no vaccine" | coverage %like% "coverage_mid_"),
      scenario_id
    ])
)

vaccine_scenarios_tab <- vaccine_scenarios_tab[scenario_id %in% scenarios_to_include]
