# Investigation and cleaning of EA in SSB 
# Perline Demange 
# Mai 2025
# Update June, correction of the EA4 definition (9 being No education rather than missing)
# Update June with alternative EA definition based on Norwegian system 

rm(list=ls())

# Set up 
library(data.table)
library(tidyverse)

# 1. Get educational attainment from register #########
#edu <- fread("//tsd-evs/p805/data/durable/data/registers/original/csv/w19_0634_utd_1970_2023_ut.csv")
edu <- fread("N:/durable/data/registers/SSB/01_data/data_v5.0/EDUCATION_ADVANCED/csv/EDUCATION_BU_UTD.csv")
head(edu)

# 2. Keep 10 years of higher education ########
years <- 2023:2014
cols_to_keep <- c("w19_0634_lnr", paste0("BU_", years))

edu <- edu %>%
  select(all_of(cols_to_keep))

# 3. Keep first digit of NUS2000 code and remove Nas ###########

# TEST FOR ERROR IN CODE
# as the column is read as numeric, it removes the 0 before 09901 which makes this code looks like a 9 "missing" rather than 0 "no edu"
# this is corrected below
# I also checked that this did not impact any individual used for the within-sibling GWAS of EA in MoBA and the file was corrected. 
summary(edu[(edu$BU_2023 < 100000 | edu$BU_2023 > 900000), ]$BU_2023)

edu <- edu %>%
  mutate(across(starts_with("BU_"),
                ~ ifelse(is.na(.), NA, as.integer(substr(as.character(.), 1, 1))),
                .names = "edu_{substr(.col, 4, 7)}"))


summary(duplicated(edu$w19_0634_lnr)) # no duplicates 


table(edu$edu_2023)
# 1       2       3       4       5       6       7       8       9 
# 27452 2325082  953069 1447894  175874 1324480  561831   59232   18336 
#all 9 here should be 0 

# the labels are
# 0 "no edu" 1 "1-7gr" 2 "8-10gr" 3 "some high school" 4 "complete high school" 
# 5 "extra high school" 6 "BA" 7 "MA" 8 "PhD" 9 "unknown" 

edu <- edu %>%
  mutate(across(starts_with("edu_"), ~ ifelse(. == 9, 0, .)))
table(edu$edu_2023) # 9 are now zeros
eduNO <- edu

# 4.1 Convert these codes to ISCED 2011 ###########################
# the labels are
# 0 "no edu" 1 "1-7gr" 2 "8-10gr" 3 "some high school" 4 "complete high school" 
# 5 "extra high school" 6 "BA" 7 "MA" 8 "PhD" 9 "unknown" 
# 3-4 are combined together in ISCED  3
# while 5 is unclear if 4 or, here chose 5

for (yr in years) {
  edu <- edu %>%
    mutate(
      !!paste0("ISCED11_", yr) := case_when(
        .data[[paste0("edu_", yr)]] == 0 ~ 0, # combines ppl with edu up to age 2 & up to age 5
        .data[[paste0("edu_", yr)]] == 1 ~ 1, # primary
        .data[[paste0("edu_", yr)]] == 2 ~ 2, # secondary lower
        .data[[paste0("edu_", yr)]] == 3 ~ 3, # secondary upper AND post secondary non tertiary shorter than 2 yrs
        .data[[paste0("edu_", yr)]] == 4 ~ 3, # secondary upper AND post secondary non tertiary shorter than 2 yrs
        .data[[paste0("edu_", yr)]] == 5 ~ 5, # postsecondary vocational 0.5-1.5 or 2 years tertiary--not distinguished so dunno if isced 4 vs 5
        .data[[paste0("edu_", yr)]] == 6 ~ 6, #undergrad
        .data[[paste0("edu_", yr)]] == 7 ~ 7, #MSc/MA
        .data[[paste0("edu_", yr)]] == 8 ~ 8, #PhD
        TRUE ~ NA_real_
      )
    )
}
table(edu$edu_2023)
table(edu$ISCED11_2023)


# 4.2 Convert to US years of schooling equivalent ######
# PRE PRIMARY=1 ::0
# PRIMARY=7::1
# LOWER SECONDARY =10 :2
# UPPER SECONDARY 13 : 3
# POST SECONDARY NON TERITARY 15 : 4/5
# FIRST STAGE TERTIARY NOT LEADING TO ADVANCEED RESEARCH 19: ba: 6
# FIRST OR SECOND STAGE CANNOT DISTINGUISH 20 () ma: 7
# SECOND STAGE TERTIARY LEADING TO ADVANCED RESEARCH QUAL 22: 8

for (yr in years) {
  edu <- edu %>%
    mutate(
      !!paste0("EduYears11_", yr) := case_when(
        .data[[paste0("ISCED11_", yr)]] == 0 ~ 1, # combines ppl with edu up to age 2 & up to age 5
        .data[[paste0("ISCED11_", yr)]] == 1 ~ 7, # primary
        .data[[paste0("ISCED11_", yr)]] == 2 ~ 10, # secondary lower
        .data[[paste0("ISCED11_", yr)]] == 3 ~ 13, # secondary upper AND post secondary non tertiary shorter than 2 yrs
        .data[[paste0("ISCED11_", yr)]] == 5 ~ 15, # postsecondary vocational 0.5-1.5 or 2 years tertiary--not distinguished so dunno if isced 4 vs 5
        .data[[paste0("ISCED11_", yr)]] == 6 ~ 19, # undergrad
        .data[[paste0("ISCED11_", yr)]] == 7 ~ 20, # MSc/MA
        .data[[paste0("ISCED11_", yr)]] == 8 ~ 22, # PhD
        TRUE ~ NA_real_
      )
    )
}

table(edu$EduYears11_2023)

# 5. Convert to Norway years of schooling equivalent ########
# Here I follow the translation made by Tarjei, following Insungse
# Vis/skjul:0 - Ingen utdanning og førskoleutdanning
# Vis/skjul:1 - Barneskoleutdanning
# Vis/skjul:2 - Ungdomsskoleutdanning
# Vis/skjul:3 - Videregående, grunnutdanning
# Vis/skjul:4 - Videregående, avsluttende utdanning
# Vis/skjul:5 - Påbygging til videregående utdanning
# Vis/skjul:6 - Universitets- og høgskoleutdanning, lavere nivå
# Vis/skjul:7 - Universitets- og høgskoleutdanning, høyere nivå
# Vis/skjul:8 - Forskerutdanning
# Vis/skjul:9 - Uoppgitt
for (yr in years) {
  eduNO <- eduNO %>%
    mutate(
      !!paste0("EduYearsNO_", yr) := case_when(
        .data[[paste0("edu_", yr)]] == 0 ~ 0,
        .data[[paste0("edu_", yr)]] == 1 ~ 6, 
        .data[[paste0("edu_", yr)]] == 2 ~ 9, 
        .data[[paste0("edu_", yr)]] == 3 ~ 10, 
        .data[[paste0("edu_", yr)]] == 4 ~ 12, 
        .data[[paste0("edu_", yr)]] == 5 ~ 13, 
        .data[[paste0("edu_", yr)]] == 6 ~ 16, 
        .data[[paste0("edu_", yr)]] == 7 ~ 18, 
        .data[[paste0("edu_", yr)]] == 8 ~ 23, # PHD default is 3 years but average is 5 years. Tarjei checked in his analyssi and it makes no difference 
        TRUE ~ NA_real_
      )
    )
}
table(eduNO$EduYearsNO_2023)

# 6. Remove people who died before 25 years old from the data. ########
basic <- fread("N:/durable/data/registers/SSB/01_data/data_v5.0/CORE/csv/POPULATION_FASTE_OPPLYSNINGER_reduced.csv", 
               strip.white=TRUE)
head(basic)

edu <- merge(edu, basic, by="w19_0634_lnr")
head(edu)
edu$birth_year <- as.integer(substr(edu$foedsels_aar_mnd, 1, 4))
edu$death_year <- as.integer(substr(edu$doeds_aar_mnd, 1, 4))
edu <- edu[is.na(edu$death_year) | (edu$death_year - edu$birth_year >= 25), ]
#from 6931045 to 6916148 
table(edu$EduYears11_2023)

eduNO <- merge(eduNO, basic, by="w19_0634_lnr")
head(eduNO)
eduNO$birth_year <- as.integer(substr(eduNO$foedsels_aar_mnd, 1, 4))
eduNO$death_year <- as.integer(substr(eduNO$doeds_aar_mnd, 1, 4))
eduNO <- eduNO[is.na(eduNO$death_year) | (eduNO$death_year - eduNO$birth_year >= 25), ]
#from 6931045 to 6916148 
table(eduNO$EduYearsNO_2023)

# 7. Are people increasing their education between 25 and 30 years old? ####### 

edu <- edu %>%
  mutate(
    age_25_year = birth_year + 25,
    age_30_year = birth_year + 30
  ) 

table(edu$age_25_year, useNA="always")

#takes a while...
edu <- edu %>%
  rowwise() %>%
  mutate(
    edu_25 = if (between(age_25_year, 2014, 2023)) {
      get(paste0("EduYears11_", age_25_year))
    } else {
      NA_real_
    },
    edu_30 = if (between(age_30_year, 2014, 2023)) {
      get(paste0("EduYears11_", age_30_year))
    } else {
      NA_real_
    },
    change = case_when(
      is.na(edu_25) | is.na(edu_30) ~ NA_character_,
      edu_30 > edu_25 ~ "increase",
      edu_30 < edu_25 ~ "decrease",
      edu_30 == edu_25 ~ "no_change"
    ),
    size_change = edu_30 - edu_25  # Calculating the size of the change
  ) %>%
  ungroup()


# Summary
table(edu$change, useNA = "ifany")
# increase no_change      <NA> 
#   77605    299624   6538919 
77605*100/299624
summary(edu$size_change, na.rm = TRUE)
hist(edu$size_change)
table(edu$size_change)


edu_25plus <- edu[2023 - edu$birth_year >= 25, ]
plot(table(edu_25plus$EduYears11_2023))
edu_30plus <- edu[2023 - edu$birth_year >= 30, ]
plot(table(edu_30plus$EduYears11_2023))

# Save data ####
save(edu, file="educational_attainement_250512.rda")
save(eduNO, file="educational_attainement_Norwaystandards.rda")

# add norway standard to edu file 
load("educational_attainement_250512.rda")
load("educational_attainement_Norwaystandards.rda")
eduNO <- eduNO[,c("w19_0634_lnr", "EduYearsNO_2023", "EduYearsNO_2022", "EduYearsNO_2021", 
                 "EduYearsNO_2020","EduYearsNO_2019","EduYearsNO_2018","EduYearsNO_2017",
                 "EduYearsNO_2016","EduYearsNO_2015","EduYearsNO_2014"
                 )]
edu <- merge(edu, eduNO, by="w19_0634_lnr")
save(edu, file="educational_attainement_250517.rda")
