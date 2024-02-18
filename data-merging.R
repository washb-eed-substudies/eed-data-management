rm(list=ls())

source(here::here("0-config.R"))

mom_sum <- box_read_csv(832562341095)
pregnancy <- box_read(845078269128) %>% select(!c(block, clusterid, tr))

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
animals <- box_read_csv(880183983740)

child_cov <- box_read_csv(870905721225)

for (tbl_name in c("mom_sum", "pregnancy", "stress", "immune", "child_sum", "telo", "dev", "anthro", "eed",
              "symptoms", "diar", "dad_pss", "mom_pss", "cesd", "enroll", "hh_wealth", "viol", "tr", "animals")) {
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

child_cov <- child_cov %>% select(childid, dataid, sex, birthord)

#### covariate data ####
child_covariates <- full_join(diar, symptoms, "childid") %>% full_join(child_cov, by="childid")

household <- full_join(enroll, hh_wealth, by="dataid") %>% full_join(viol, "dataid") %>%
  full_join(mom_pss, "dataid") %>% full_join(dad_pss, "dataid") %>% full_join(cesd, "dataid") %>%
  full_join(tr, c("block", "clusterid")) %>% full_join(animals, "dataid")


#### combine all child exposure/outcome data ####
# left join with development data because development data includes all arms of the WASH trial - eed subset only contains 4 arms
child <- full_join(immune, child_sum, by="childid") %>% full_join(stress, by=c("childid")) %>%
  full_join(telo, c("childid")) %>% full_join(eed, c("childid")) %>% full_join(anthro, "childid") %>%
  left_join(dev, by=c("childid"))
nrow(child)
child$childid %>% unique() %>% length() # make sure there are no duplicates

child$dataid <- gsub('.{1}$', '', child$childid) %>% as.numeric()

no_child_covariates <- child %>% filter(!(childid %in% child_covariates$childid))
no_child_covariates$childid
# all of these show up in the eed dataset
# 204001 has no data (no enrollment data)
# 1805, 6106, 12805, 26807 dataids have enrollment data but no other measurements
# 99051 childid has only development data
# 268071 has anthro data and in stress dataset but no data

eed %>% filter(childid==204001)

child %>% filter(childid==70041)
filter(household, dataid==7004)
# 7004 dataid in sanitation arm - shows up in eed dataset but no data

# month_ut2, monsoon_ut2, agemth_ut2 in both the stress (.x) data and eed (.y) data - keep stress
names(child)
sum(child$ageday_ut2.x != child$ageday_ut2.y, na.rm=T)
filter(child, is.na(ageday_ut2.x) & !is.na(ageday_ut2.y)) %>% nrow()
filter(child, is.na(ageday_ut2.y) & !is.na(ageday_ut2.x)) %>% nrow()
child <- child %>% rename(month_ut2 = month_ut2.x, monsoon_ut2 = monsoon_ut2.x,
                          agemth_ut2 = agemth_ut2.x, ageday_ut2 = ageday_ut2.x, ageyr_ut2 = ageyr_ut2.x) %>%
  select(!c(month_ut2.y, monsoon_ut2.y, agemth_ut2.y, ageday_ut2.y, ageyr_ut2.y))

names(child)


#### combine all maternal exposure data ####
maternal <- full_join(pregnancy, mom_sum, by="dataid")
names(maternal)


#### combine all datasets ####
# child and maternal exposure/outcome datasets, child and household covariates
dfull <- full_join(child, maternal, "dataid") %>% full_join(child_covariates, c("childid", "dataid")) %>% left_join(household, c("dataid")) #left join on only dataid because some children are missing block and clusterid even though we have their household enrollment data
sum(dfull$clusterid.x != dfull$clusterid.y, na.rm=T)
sum(dfull$block.x != dfull$block.y, na.rm=T)
dfull <- dfull %>% mutate(clusterid = clusterid.y, block = block.y) %>% select(!c(clusterid.x, block.x, clusterid.y, block.y))
nrow(dfull)
dfull$childid %>% unique() %>% length()
filter(dfull, is.na(childid)) # have maternal data, cesd, enrollment for one mom dataid 12506 but no child data - also in handwashing arm

dfull <- dfull %>% filter(tr %in% c("Control", "Nutrition", "WSH", "Nutrition + WSH")) %>% filter(!(childid %in% no_child_covariates$childid))


# add variables to turn cesd into binary variables
# classify top 25% of mothers in sample as experiencing high depressive symptoms
cesd_t2_q<-quantile(dfull$cesd_sum_t2, na.rm=T)[4]
cesd_t3_q<-quantile(dfull$cesd_sum_ee_t3, na.rm=T)[4]
dfull$cesd_sum_t2_binary<-ifelse(dfull$cesd_sum_t2 >= cesd_t2_q, 1, 0)
dfull$cesd_sum_ee_t3_binary<-ifelse(dfull$cesd_sum_ee_t3 >= cesd_t3_q, 1, 0)


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

dfull$tr <- factor(dfull$tr,levels=c("Control","Nutrition", "Nutrition + WSH", "WSH"))

dfull$diar7d_t2<-as.factor(dfull$diar7d_t2)
dfull$diar7d_t2<-addNA(dfull$diar7d_t2)
levels(dfull$diar7d_t2)[length(levels(dfull$diar7d_t2))]<-"Missing"

dfull$diar7d_t3<-as.factor(dfull$diar7d_t3)
dfull$diar7d_t3<-addNA(dfull$diar7d_t3)
levels(dfull$diar7d_t3)[length(levels(dfull$diar7d_t3))]<-"Missing"

dfull$ari7d_t2<-as.factor(dfull$ari7d_t2)
dfull$ari7d_t2<-addNA(dfull$ari7d_t2)
levels(dfull$ari7d_t2)[length(levels(dfull$ari7d_t2))]<-"Missing"

dfull$ari7d_t3<-as.factor(dfull$ari7d_t3)
dfull$ari7d_t3<-addNA(dfull$ari7d_t3)
levels(dfull$ari7d_t3)[length(levels(dfull$ari7d_t3))]<-"Missing"

dfull$nose7d_t2<-as.factor(dfull$nose7d_t2)
dfull$nose7d_t2<-addNA(dfull$nose7d_t2)
levels(dfull$nose7d_t2)[length(levels(dfull$nose7d_t2))]<-"Missing"

dfull$nose7d_t3<-as.factor(dfull$nose7d_t3)
dfull$nose7d_t3<-addNA(dfull$nose7d_t3)
levels(dfull$nose7d_t3)[length(levels(dfull$nose7d_t3))]<-"Missing"

dfull$life_viol_any_t3_cat<-as.factor(dfull$life_viol_any_t3)
dfull$life_viol_any_t3_cat<-addNA(dfull$life_viol_any_t3_cat)
levels(dfull$life_viol_any_t3_cat)[length(levels(dfull$life_viol_any_t3_cat))]<-"Missing"

dfull$viol_any_preg_cat<-as.factor(dfull$viol_any_preg)
dfull$viol_any_preg_cat<-addNA(dfull$viol_any_preg_cat)
levels(dfull$viol_any_preg_cat)[length(levels(dfull$viol_any_preg_cat))]<-"Missing"

dfull <- dfull %>%
  mutate(hcflag = ifelse(childid %in% c(001041,
                                        023061,
                                        037071,
                                        050051,
                                        053031,
                                        068051,
                                        069081,
                                        143071,
                                        179061,
                                        184041,
                                        235071,
                                        330041,
                                        271011,
                                        375061), 1, 0),
         lenflag = ifelse(childid %in% c(010081,
                                         013031,
                                         155031), 1, 0),
         weiflag = ifelse(childid %in% c(143071,
                                         101031,
                                         155031,
                                         348071,
                                         404071,
                                         408021), 1, 0),
         whflag = ifelse(lenflag == 1 | weiflag == 1, 1, 0))

box_write(dfull,
          "bangladesh-cleaned-master-data.RDS",
          dir_id = 147779347962)
