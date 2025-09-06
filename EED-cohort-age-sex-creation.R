
# Clean workspace
rm(list = ls())

# Load config and libraries
source(paste0(here::here(), "/0-config.R"))

# Load the dataset
ee_full <- readRDS(paste0(data_dir, "/bangladesh-cleaned-master-data.RDS"))
head(ee_full)
ee_full$ent
ee_full$sex
ee_full$dataid

#load dob's
med_history <- read.csv(paste0(data_dir, "/BD-EE-medhistory.csv"))
med_history <- med_history %>% select(dataid, childNo,DOB) %>% mutate(DOB=dmy(DOB), childid=as.numeric(paste0(as.character(dataid),as.character(childNo)))) %>% select(-childNo, -dataid)
head(med_history)



colnames(ee_full)[grepl("date",colnames(ee_full))]

df <- ee_full %>% select(childid, dataid, sex, starts_with("ageday")) %>% distinct(childid, .keep_all = TRUE)

#merge dob's
summary(df$childid)
summary(med_history$childid)
df <- left_join(df, med_history, by="childid")
table(is.na(df$DOB))
write.csv(ee_full, file = paste0(data_dir, "/wbb-eed-age-sex-master-data.csv"), row.names = FALSE)


# check for compound/dataid 11704
df %>% filter(dataid == 11704)
