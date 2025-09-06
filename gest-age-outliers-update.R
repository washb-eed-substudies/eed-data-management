#######################################
# WASH Benefits Bangladesh
# Maternal Micronutrient-Microbiome investigation
# Updating sample collection and menstrual dates for gestational age outliers
#######################################

# Clean workspace
rm(list = ls())

# Load config and libraries
source(paste0(here::here(), "/0-config.R"))

# Load the dataset
ee_full <- readRDS(paste0(data_dir, "/bangladesh-cleaned-master-data.RDS"))

# Ensure columns are Date or numeric as needed
ee_full <- ee_full %>%
  mutate(
    samplecoldate_blood_t0 = as.Date(samplecoldate_blood_t0),
    menstrual_date = as.Date(menstrual_date),
    q105years = as.numeric(q105years),
    q105months = as.numeric(q105months),
    q105days = as.numeric(q105days)
  )

#Update q105 fields where menstrual_date is being corrected
ee_full_corrected <- ee_full %>%
  mutate(
    q105years = case_when(
      dataid == 31304 ~ 2012,
      dataid == 37905 ~ 2013,
      dataid == 42206 ~ 2013,
      dataid == 64406 ~ 2013,
      dataid == 45701 ~ 2013,
      TRUE ~ q105years
    ),
    q105months = case_when(
      dataid == 31304 ~ 11,
      dataid == 37905 ~ 1,
      dataid == 42206 ~ 1,
      dataid == 64406 ~ 2,
      dataid == 45701 ~ 1,
      TRUE ~ q105months
    ),
    q105days = case_when(
      dataid == 31304 ~ 1,
      dataid == 37905 ~ 15,
      dataid == 42206 ~ 1,
      dataid == 64406 ~ 7,
      dataid == 45701 ~ 13,
      TRUE ~ q105days
    )
  )

#Recompute menstrual_date from q105 fields
ee_full_corrected <- ee_full_corrected %>%
  mutate(
    menstrual_date = case_when(
      !is.na(q105years) & !is.na(q105months) & !is.na(q105days) ~
        dmy(paste(q105days, q105months, q105years, sep = "/")),
      TRUE ~ menstrual_date
    )
  )

#Update samplecoldate_blood_t0 for specific participants
ee_full_corrected <- ee_full_corrected %>%
  mutate(
    samplecoldate_blood_t0 = case_when(
      dataid == 22708 ~ dmy("25/1/2013"),
      dataid == 43707 ~ dmy("12/4/2013"),
      TRUE ~ samplecoldate_blood_t0
    )
  )

#Recalculate gestational age in weeks
ee_full_corrected <- ee_full_corrected %>%
  mutate(
    gest_age_weeks = as.numeric(difftime(samplecoldate_blood_t0, menstrual_date, units = "weeks"))
  )

#Save the corrected dataset
saveRDS(ee_full_corrected, paste0(data_dir, "clean-data/bangladesh-cleaned-master-data_corrected_sampledates.RDS"))

#Inspect corrected participants
ee_full_corrected %>%
  filter(dataid %in% c(22708, 31304, 37905, 43707, 42206, 64406, 45701)) %>%
  select(dataid, q105years, q105months, q105days, menstrual_date, samplecoldate_blood_t0, gest_age_weeks)
