rm(list=ls())

source(here::here("0-config.R"))

d<-box_read(830186133564)

# Z-score of telomere measurements
d <- d %>%
  mutate(TS_t2_Z = scale(TS_t2, center=TRUE, scale=TRUE)[,1]) %>%
  mutate(TS_t3_Z = scale(TS_t3, center=TRUE, scale=TRUE)[,1]) %>%
  mutate(delta_TS_Z = scale(delta_TS, center=TRUE, scale=TRUE)[,1])

box_write(d, "bangladesh-cleaned-child-telo-growth-covariates.RDS", 140726526642)
