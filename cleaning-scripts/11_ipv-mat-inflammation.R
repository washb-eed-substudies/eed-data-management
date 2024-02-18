rm(list=ls())
source(here::here("0-config.R"))

#box_write(d, "bangladesh-maternal-pregnancy-exposures-cleaned.RDS", dir_id = 140726526642)

# box_write(mom.sumscore,
#           "bangladesh-maternal-immune-sum-score.csv", dir_id = 140726526642)

#use codes from scripts 6 and 7 or load and merge those cleaned variables into the master data with the IPV data

dfull <- readRDS("C:/Users/andre/Documents/EE/eed-substudy-data/bangladesh-cleaned-master-data.RDS")


d <- dfull

saveRDS(d, file="C:/Users/andre/Documents/EE/ipv-maternal-inflammation/data/bangladesh-cleaned-mat-infl-full-data.RDS")
