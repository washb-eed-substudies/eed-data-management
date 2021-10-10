rm(list=ls())
source(here::here("0-config.R"))

# create childid
get_childid <- function(v1, v2){
  as.numeric(paste(as.character(v1), as.character(v2), sep=""))
}

cdiy1 <- box_read(783715320649)
names(cdiy1)
cdiy1_select <- cdiy1 %>%
  mutate(childid = get_childid(dataid, tchild)) %>%
  select(childid, dataid, clusterid, block, agedays, month, z_age2mo_cdi_undyr1_all_no4, z_age2mo_cdi_sayyr1_all_no4) %>%
  rename(agedays_cdi_t2 = agedays,
         month_cdi_t2 = month,
         z_cdi_und_t2 = z_age2mo_cdi_undyr1_all_no4,
         z_cdi_say_t2 = z_age2mo_cdi_sayyr1_all_no4)

cdiy2 <- box_read(783719359651)
names(cdiy2)
cdiy2_select <- cdiy2 %>%
  mutate(childid = get_childid(dataid, tchild)) %>%
  select(childid, dataid, clusterid, block, agedays, month, z_age2mo_cdi_undyr2_all_no4, z_age2mo_cdi_sayyr2_all_no4) %>%
  rename(agedays_cdi_t3 = agedays,
         month_cdi_t3 = month,
         z_cdi_und_t3 = z_age2mo_cdi_undyr2_all_no4,
         z_cdi_say_t3 = z_age2mo_cdi_sayyr2_all_no4)

easq <- box_read(783712892843)
names(easq)
easq_select <- easq %>%
  mutate(childid = get_childid(dataid, tchild)) %>%
  select(childid, dataid, clusterid, block, agedays, month, z_age2mo_personal_no4,  z_age2mo_motor_no4,
         z_age2mo_com_no4, z_age2mo_combined_no4) %>%
  rename(agedays_easq = agedays,
         month_easq = month,
         z_personal_easq = z_age2mo_personal_no4,
         z_motor_easq = z_age2mo_motor_no4,
         z_comm_easq = z_age2mo_com_no4,
         z_combined_easq = z_age2mo_combined_no4)

home1 <- box_read(833366932297)%>%
  mutate(childid = substr(childid, 2, 2)) %>%
  mutate(childid = get_childid(dataid, childid)) %>%
  select(childid, dataid, clusterid, block, midline_stimulation) %>%
  rename(fci_t2 = midline_stimulation)

home2 <- box_read(833370783481)%>%
  mutate(childid = substr(childid, 2, 2)) %>%
  mutate(childid = get_childid(dataid, childid)) %>%
  select(childid, dataid, clusterid, block, endline_stimulation) %>%
  rename(fci_t3 = endline_stimulation)

motor <- box_read_csv(833368817893)%>%
  mutate(childid = get_childid(dataid, tchild)) %>%
  select(childid, dataid, clusterid, block, agedays, month, sit_nosupp, crawl_nosupp, stand_supp,
         walk_supp, stand_nosupp, walk_nosupp) %>%
  rename(agedays_motor = agedays,
         month_motor = month,
         who_sit = sit_nosupp, who_crawl = crawl_nosupp,
         who_stand_supp = stand_supp, who_walk_supp = walk_supp,
         who_stand_nosupp = stand_nosupp, who_walk_nosup = walk_nosupp) %>%
  mutate(sum_who = who_stand_supp+who_walk_supp+who_stand_nosupp+who_walk_nosup)

# join separate development datasets
joining <- c("childid")
development <- motor %>% full_join(cdiy1_select, by=joining) %>%
  full_join(cdiy2_select, by=joining) %>% full_join(easq_select, by = joining)

dev_with_fci <- development %>% left_join(home1, joining) %>% left_join(home2, joining)

# save development dataset
box_write(dev_with_fci, "bangladesh-full-development-fci.RDS", 140726526642)
