rm(list=ls())
source(here::here("0-config.R"))

######################
###Load in data
######################

anthro <- box_read_csv(845069849870)
diar <- box_read_csv(845049236081)

names(anthro)
names(diar)

get_childid <- function(d){
  as.numeric(paste(as.character(d$dataid), str_extract(d$childid, "[0-9]+"), sep=""))
}

anthro <- anthro %>% mutate(childid_old = childid,
                            childid = get_childid(anthro))
anthro$childid %>% unique() %>% length()
anthro %>% filter(!is.na(motherid)) %>% nrow()
nrow(anthro)
anthro

diar <- diar %>% mutate(childid_old = childid,
                            childid = get_childid(diar)) %>% filter(!is.na(motherid))
