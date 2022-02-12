rm(list=ls())

source(here::here("0-config.R"))

master <- box_read(871638120165)

bf_bl <- box_read(911203148027)
bf_ml <- box_read(911203591841)
bf_el <- box_read(911203749044)

bf_bl <- bf_bl %>% select(childId, dataid, c605, )
bf_bl$childid <- as.numeric(paste0(bf_bl$dataid, bf_bl$childId))

bf_ml <- bf_ml %>% select(childId, dataid, c605)
bf_ml$childid <- as.numeric(paste0(bf_ml$dataid, bf_ml$childId))

bf_el <- bf_el %>% select(dataid, childNo, c605)
bf_el$childid <- as.numeric(paste0(bf_el$dataid, bf_el$childNo))

master <- master %>% select(childid, laz_t1, laz_t2, laz_t3,
                            waz_t1, waz_t2, waz_t3,
                            whz_t1, whz_t2, whz_t3,
                            hcz_t1, hcz_t2, hcz_t3)

full <- master %>% inner_join(bf_bl, by=c("childid"="childid"))

full$c605 <- as.factor(full$c605)
full

summary(lm(laz_t1~c605, data=full))
summary(lm(waz_t1~c605, data=full))
summary(lm(whz_t1~c605, data=full))
summary(lm(hcz_t1~c605, data=full))

summary(lm(laz_t2~c605, data=full))
summary(lm(waz_t2~c605, data=full))
summary(lm(whz_t2~c605, data=full))
summary(lm(hcz_t2~c605, data=full))

summary(lm(laz_t3~c605, data=full))
summary(lm(waz_t3~c605, data=full))
summary(lm(whz_t3~c605, data=full))
summary(lm(hcz_t3~c605, data=full))


bf_full <- bf_bl %>% select(childid, c605) %>%
  full_join(bf_ml %>% select(childid, c605), by="childid") %>%
  full_join(bf_el %>% select(childid, c605), by="childid")

nrow(bf_full)

master <- box_read(871638120165)

bf_full <- bf_full %>% rename("t1_bf"="c605.x", "t2_bf"="c605.y", "t3_bf"="c605") %>%
  mutate(t1_bf = t1_bf-1, t2_bf = t2_bf-1, t3_bf = t3_bf-1) %>%
  full_join(master %>% select(childid,agemth_ut1,agemth_ut2,agemth_ut3), by="childid")

long <- bf_full%>%reshape(idvar = "childid", varying = list(c("t1_bf", "t2_bf", "t3_bf"), c("agemth_ut1", "agemth_ut2", "agemth_ut3")),
                  v.names = c("bf", "agemth"), direction = "long")
long <- long %>% mutate(agemth = round(agemth))

grouped <- long %>% group_by(agemth) %>%
  summarise(prop_weaned = mean(bf, na.rm=T)*100, prop_bf = 100-prop_weaned) %>%
  filter(!agemth %>% is.na())
grouped %>%
  # mutate(agemth=factor(agemth)) %>%
  # reshape(idvar="agemth", varying=list(2:3), v.names="prop", times = c("prop_weaned", "prop_bf"), direction = "long") %>%
  ggplot(aes(agemth, prop_weaned)) + geom_bar(stat="identity") + ylab("Percentage weaned")

# bf <- (bf_full %>% apply(2, mean, na.rm=T)*100)[2:4] %>% as.data.frame()
# bf <- rbind(bf, 100-bf)
# names(bf) <- "prop"
# bf %>% mutate(weaned = factor(ifelse(grepl("bf1", rownames(bf)), 0, 1),
#                               levels = c(0,1),
#                               labels = c("Breastfeeding", "Weaned")),
#               time = factor(str_extract(rownames(bf), "[0-9]{1}"))) %>%
#   ggplot(aes(fill=weaned, y=prop, x=time)) +
#   geom_bar(stat="identity") +
#   scale_x_discrete(name=element_blank(), labels=c("3 months", "14 months", "28 months")) +
#   ylab("Percent") +
#   theme(legend.position = "right", legend.title = element_blank())


library(readr)
bl <- read_dta("/Users/sophiatan/Downloads/infant feeding modules/ee_bl_append_ID_clean_infant_20180221.dta")
ml <- read_dta("/Users/sophiatan/Downloads/infant feeding modules/ee_ml_append_ID_clean_infant_20180221.dta")
el <- read_dta("/Users/sophiatan/Downloads/infant feeding modules/ee_el_append_ID_clean_infant_20180221.dta")

dob <- readRDS("/Users/sophiatan/Dropbox/WASH/WBK-EE-analysis/Data/Cleaned/Andrew/WBK-EE-childDOB.rds")
dob <- dob %>% select(childid, DOB)

bf_full <- bl %>% select(childid, c_605, ee_bl_infant_date) %>%
  full_join(ml %>% select(childid, c_605, ee_ml_infant_date), by="childid")

bf_full <- bf_full %>% inner_join(dob, "childid")
bf_full <- bf_full %>% mutate(t1 =c_605.x-1, t2 = c_605.y-1) %>% select(!grep("605", names(.))) %>%
  mutate(age_t1 = lubridate::interval(DOB, ee_bl_infant_date) %/% months(1),
         age_t2 = lubridate::interval(DOB, ee_ml_infant_date) %/% months(1))

long <- data.frame(bf_full)%>%
  select(childid, t1, t2, age_t1, age_t2) %>%
  reshape(idvar = "childid", varying = list(c("t1", "t2"), c("age_t1", "age_t2")),
          v.names = c("bf", "agemth"), direction = "long")
long

grouped <- long %>% group_by(agemth) %>%
  summarise(prop_weaned = mean(bf, na.rm=T)*100, prop_bf = 100-prop_weaned) %>%
  filter(!agemth %>% is.na())
grouped %>%
  # mutate(agemth=factor(agemth)) %>%
  # reshape(idvar="agemth", varying=list(2:3), v.names="prop", times = c("prop_weaned", "prop_bf"), direction = "long") %>%
  ggplot(aes(agemth, prop_weaned)) + geom_bar(stat="identity") + ylab("Percentage weaned")
