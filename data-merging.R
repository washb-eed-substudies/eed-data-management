rm(list=ls())

source(here::here("0-config.R"))

mom_sum <- box_read_csv(832562341095)
pregnancy <- box_read(832551885650) %>% select(!c(block, clusterid, tr))

stress <- box_read(832563746109)
telo <- box_read(836452716883)
child_sum <- box_read_csv(831058830481)
eed <- box_read(836810707276)
immune <- box_read(831038135971)
dev <- box_read(841631188155) %>% select(!tr)
anthro <- box_read(836450104872)
symptoms <- box_read(830185406054)
diar <- box_read(830184576350)

dad_pss <- box_read(830182486200)
viol <- box_read(830180549173)
mom_pss <- box_read(830182994077)
cesd <- box_read(830181592480)
hh_wealth <- box_read(831432290888)
enroll <- box_read_csv(833359280029)
tr <- box_read_csv(837263699203)

for (tbl_name in c("mom_sum", "pregnancy", "stress", "immune", "child_sum", "telo", "dev", "anthro", "eed",
              "symptoms", "diar", "dad_pss", "mom_pss", "cesd", "enroll", "hh_wealth", "viol", "tr")) {
  tbl <- get(tbl_name)
  if ("childid" %in% names(tbl)){
    assign(tbl_name, tbl %>% mutate(childid = as.numeric(childid)))
  }
  tbl <- get(tbl_name)
    if ("dataid" %in% names(tbl)){
    assign(tbl_name, tbl %>% mutate(dataid = as.numeric(dataid)))
    }
  tbl <- get(tbl_name)
  if ("clusterid" %in% names(tbl)){
    assign(tbl_name, tbl %>% mutate(clusterid = as.numeric(clusterid)))
  }
  tbl <- get(tbl_name)
  if ("block" %in% names(tbl)){
    assign(tbl_name, tbl %>% mutate(block = as.numeric(block)))
  }
}


#### covariate data ####
child_covariates <- full_join(diar, symptoms, "childid")

household <- full_join(enroll, hh_wealth, by="dataid") %>% full_join(viol, "dataid") %>%
  full_join(mom_pss, "dataid") %>% full_join(dad_pss, "dataid") %>% full_join(cesd, "dataid") %>% full_join(tr, c("block", "clusterid"))


#### combine all child exposure/outcome data ####
# left join with development data because development data includes all arms of the WASH trial - eed subset only contains 4 arms
child <- full_join(immune, child_sum, by="childid") %>% full_join(stress, by=c("childid", "dataid", "clusterid", "block")) %>%
  full_join(telo, c("childid", "dataid", "clusterid", "block")) %>% full_join(eed, c("childid", "dataid", "clusterid", "block")) %>% full_join(anthro, "childid") %>%
  left_join(dev, by=c("childid", "dataid", "clusterid", "block"))
nrow(child)
child$childid %>% unique() %>% length() # make sure there are no duplicates

nodataid <- filter(child, is.na(dataid)) # check to find children without dataids
nodataid$childid
nodataid$dataid <- gsub('.{1}$', '', nodataid$childid)
household %>% filter(dataid %in% nodataid$dataid)
table(child$tr)
# 7004 dataid in sanitation arm - drop
# 204001 has no data - drop
child <- child %>% filter(childid != 70041 | childid != 204001)
# 1805, 6106, 9905, 12805 dataids have enrollment data - drop?
# 99051 childid has only development data, others have no child measurements/data - drop?

child$dataid <- gsub('.{1}$', '', child$childid) %>% as.numeric()

# month_ut2, monsoon_ut2, agemth_ut2 in both the stress (.x) data and eed (.y) data - keep stress
names(child)
sum(child$month_ut2.x != child$month_ut2.y, na.rm=T)
filter(child, is.na(month_ut2.x) & !is.na(month_ut2.y)) %>% nrow()
filter(child, is.na(month_ut2.y) & !is.na(month_ut2.x)) %>% nrow()
child <- child %>% rename(month_ut2 = month_ut2.x, monsoon_ut2 = monsoon_ut2.x,
                          agemth_ut2 = agemth_ut2.x) %>%
  select(!c(month_ut2.y, monsoon_ut2.y, agemth_ut2.y))

names(child)


#### combine all maternal exposure data ####
maternal <- full_join(pregnancy, mom_sum, by="dataid")
names(maternal)


#### combine all datasets ####
# child and maternal exposure/outcome datasets, child and household covariates
dfull <- full_join(child, maternal, "dataid") %>% full_join(child_covariates, "childid") %>% left_join(household, c("dataid")) #left join on only dataid because some children are missing block and clusterid even though we have their household enrollment data
sum(dfull$clusterid.x != dfull$clusterid.y, na.rm=T)
sum(dfull$block != dfull$block, na.rm=T)
dfull <- dfull %>% mutate(clusterid = clusterid.y, block = block.y) %>% select(!c(clusterid.x, block.x))
nrow(dfull)
dfull$childid %>% unique() %>% length()
filter(dfull, is.na(childid)) # have cytokine data for one mom - no enrollment - drop?


#### clean enrollment covariates ####
#set variables as factors/numeric
dfull$sex<-as.factor(dfull$sex)
dfull$sex <- factor(dfull$sex, labels = c("female", "male"))
dfull$birthord<-as.factor(dfull$birthord)
dfull$momage<-as.numeric(dfull$momage)
dfull$momheight<-as.numeric(dfull$momheight)
dfull$momedu<-as.factor(dfull$momedu)
dfull$hfiacat<-as.factor(dfull$hfiacat)
dfull$Nlt18<-as.numeric(dfull$Nlt18)
dfull$Ncomp<-as.numeric(dfull$Ncomp)
dfull$watmin<-as.numeric(dfull$watmin)
dfull$floor<-as.factor(dfull$floor)
dfull$walls<-as.factor(dfull$walls)
dfull$elec<-as.factor(dfull$elec)
dfull$asset_wardrobe<-as.factor(dfull$asset_wardrobe)
dfull$asset_table<-as.factor(dfull$asset_table)
dfull$asset_chair<-as.factor(dfull$asset_chair)
dfull$asset_clock<-as.factor(dfull$asset_clock)
dfull$asset_khat<-as.factor(dfull$asset_khat)
dfull$asset_chouki<-as.factor(dfull$asset_chouki)
dfull$asset_radio<-as.factor(dfull$asset_radio)
dfull$asset_tv<-as.factor(dfull$asset_tv)
dfull$asset_refrig<-as.factor(dfull$asset_refrig)
dfull$asset_bike<-as.factor(dfull$asset_bike)
dfull$asset_moto<-as.factor(dfull$asset_moto)
dfull$asset_sewmach<-as.factor(dfull$asset_sewmach)
dfull$asset_mobile<-as.factor(dfull$asset_mobile)
dfull$n_cattle<-as.numeric(dfull$n_cattle)
dfull$n_goat<-as.numeric(dfull$n_goat)
dfull$n_chicken<-as.numeric(dfull$n_chicken)

dfull$lenhei_med_t2<-as.numeric(dfull$lenhei_med_t2)
dfull$weight_med_t2<-as.numeric(dfull$weight_med_t2)

dfull$monsoon_ht2<-as.factor(dfull$monsoon_ht2)
dfull$monsoon_ht2<-addNA(dfull$monsoon_ht2)
levels(dfull$monsoon_ht2)[length(levels(dfull$monsoon_ht2))]<-"Missing"

dfull$monsoon_ht3<-as.factor(dfull$monsoon_ht3)
dfull$monsoon_ht3<-addNA(dfull$monsoon_ht3)
levels(dfull$monsoon_ht3)[length(levels(dfull$monsoon_ht3))]<-"Missing"

dfull$ageday_ht2<-as.numeric(dfull$ageday_ht2)
dfull$ageday_ht3<-as.numeric(dfull$ageday_ht3)

dfull$anthro_days_btwn_t2_t3<-as.numeric(dfull$anthro_days_btwn_t2_t3)

dfull$tr <- factor(dfull$tr,levels=c("Control","Nutrition + WSH"))

dfull$cesd_sum_t2<-as.numeric(dfull$cesd_sum_t2)
dfull$cesd_sum_ee_t3<-as.numeric(dfull$cesd_sum_ee_t3)
dfull$pss_sum_mom_t3<-as.numeric(dfull$pss_sum_mom_t3)

dfull$diar7d_t2<-as.factor(dfull$diar7d_t2)
dfull$diar7d_t2<-addNA(dfull$diar7d_t2)
levels(dfull$diar7d_t2)[length(levels(dfull$diar7d_t2))]<-"Missing"

dfull$diar7d_t3<-as.factor(dfull$diar7d_t3)
dfull$diar7d_t3<-addNA(dfull$diar7d_t3)
levels(dfull$diar7d_t3)[length(levels(dfull$diar7d_t3))]<-"Missing"

dfull$life_viol_any_t3<-as.factor(dfull$life_viol_any_t3)
dfull$life_viol_any_t3<-addNA(dfull$life_viol_any_t3)
levels(dfull$life_viol_any_t3)[length(levels(dfull$life_viol_any_t3))]<-"Missing"

box_write()
