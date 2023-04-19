rm(list=ls())

library(here)
library(tidyverse)
library(readr)
library(haven)
library(lubridate)

master <- readRDS("/Users/sophiatan/Downloads/bangladesh-cleaned-master-data.RDS")
master

main <- read_dta("/Users/sophiatan/Downloads/1. WASHB_Baseline_main_survey.dta")
main <- main %>% select(dataid, q105years, q105months, q105days)

master <- master %>% left_join(main %>% mutate(dataid=as.numeric(dataid)))
master <- master %>%
  mutate(menstrual_date = as.Date(ifelse(q105years %>% is.na(), NA,
                                         paste(q105years, q105months, q105days, sep="/"))))
master
master <- master %>% mutate(gest_age_weeks = difftime(samplecoldate_blood_t0, menstrual_date, units="weeks"))
master

saveRDS(master, "/Users/sophiatan/Downloads/bangladesh-cleaned-master-data.RDS")
