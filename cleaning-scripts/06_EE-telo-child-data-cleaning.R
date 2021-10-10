rm(list=ls())

source(here::here("0-config.R"))

d<-box_read(830186133564)

# Z-score of telomere measurements
d <- d %>%
  mutate(TS_t2_Z = scale(TS_t2, center=TRUE, scale=TRUE)[,1]) %>%
  mutate(TS_t3_Z = scale(TS_t3, center=TRUE, scale=TRUE)[,1]) %>%
  mutate(delta_TS_Z = scale(delta_TS, center=TRUE, scale=TRUE)[,1])

names(d)
d <- select(d, childid, delta_ts_bp, delta_TS, delta_TS_Z, TS_t2,
            TS_t2_Z, TS_t3, TS_t3_Z, ageday_ht1, ageday_ht2, ageday_ht3,
            month_ht1, month_ht2, month_ht3, monsoon_ht1, monsoon_ht2, monsoon_ht3)

box_write(d, "bangladesh-cleaned-child-telo-data.RDS", 140726526642)
