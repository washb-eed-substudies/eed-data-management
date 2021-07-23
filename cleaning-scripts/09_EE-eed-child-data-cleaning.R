rm(list=ls())

source(here::here("0-config.R"))

stool <- box_read(835958132678)
urine <- box_read(835953370668)

d <- full_join(stool, urine, by = c("childid", "dataid")) %>%
  select(-grep(".x|.y", names(.), value=T))

names(d)
d <- select(d, -grep("dataid_|childno|aliq|faid|reason|u[1-9]|samplecoldate", names(d), value=T))
names(d)

box_write(d, "bangladesh-cleaned-child-eed-data.RDS", 140726526642)
