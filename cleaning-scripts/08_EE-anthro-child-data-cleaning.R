rm(list=ls())
source(here::here("0-config.R"))

######################
###Load in data
######################

d <- box_read(836289737682)

#### clean anthropometry data ####
# create factor variable with missingness level for growth measurements at year 1 and year 2
growth.var <- c("laz_t1", "waz_t1", "whz_t1", "hcz_t1", "laz_t2", "waz_t2", "whz_t2", "hcz_t2")
for (i in growth.var) {
  cutpoints <- c(-3, -2, -1, -0)
  cuts <- c(min(d[[i]], na.rm = T), cutpoints, max(d[[i]], na.rm = T))
  new_var <- paste(i, "_cat", sep="")
  d[[new_var]] <- cut(d[[i]],
                           cuts,
                           right = FALSE,
                           include.lowest = TRUE)
  d[[new_var]] <- as.factor(d[[new_var]])
  d[[new_var]] <- fct_explicit_na(d[[new_var]], "Missing")
  d[[new_var]] <- factor(d[[new_var]], levels = levels(d[[new_var]]))
}

# check categorical distribution of categorical growth variables
for (i in growth.var){
  print(i)
  print(table(d[[paste(i, "_cat", sep="")]]))
}

cleaned_d <- d %>% select(childid, grep("z_t|delta|velocity|month|days|months|ageday", names(d), value=T))
cleaned_d <- cleaned_d %>% select(!grep("bmiz|acz", names(cleaned_d), value=T)) %>%
  rename(month_at1 = month_t1, month_at2 = month_t2, month_at3 = month_t3)


box_write(
  cleaned_d,
  "bangladesh-cleaned-child-anthropometry-data.RDS",
  dir_id = 140726526642
)
