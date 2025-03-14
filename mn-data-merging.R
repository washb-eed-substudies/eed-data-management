
rm(list=ls())

library(tidyverse)
library(haven)
mn_bd <- read_dta("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/mn data/washb_mn_CA_20180620.dta")
colnames(mn_bd)
head(mn_bd)

d <- readRDS("C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/bangladesh-cleaned-master-data.RDS")
head(d)

#drop variables in both datasets
#d <- d %>% subset(., select = -c("sex","Nlt18","Ncomp","momage","momheight","momedu","tubewell","watmin","walls","floor"))
d <- d %>% subset(., select = -c(sex,Nlt18,Ncomp,momage,momheight,momedu,tubewell,watmin,walls,floor,union)) %>%
           rename(logRBP_eed=logRBP,
                  logCRP_eed=logCRP,
                  logAGP_eed=logAGP,
                  logRBP_inf_eed=logRBP_inf)



summary(unique(mn_bd$dataid))
summary(unique(d$dataid))

summary(mn_bd$logRBP)
summary(d$logRBP)


mn_bd$childid = mn_bd$dataid * 10 + mn_bd$childid
summary(unique(mn_bd$childid))
summary(unique(d$childid))

#check if this is using private versus public IDs
#or if the EED substudy ID's are different
#ask audrie

#mn_bd$childid <- paste0(mn_bd$dataid, mn_bd$childid)

table(d$Nlt18)
table(mn_bd$Nlt18)


df <- left_join(d, mn_bd, by=c("dataid","childid","clusterid"))
colnames(df)[grepl("\\.x",colnames(df))]

# cor.test(df$logCRP.x, df$logCRP.y, na.rm=T)
# cor.test(df$Nlt18.x, df$Nlt18.y, na.rm=T)

write.csv(df, file = "C:/Users/andre/OneDrive/Documents/washb_substudies/eed-substudy-data/bangladesh-merged-eed-mn-data.csv")
