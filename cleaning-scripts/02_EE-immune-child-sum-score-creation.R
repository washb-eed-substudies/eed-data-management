rm(list=ls())
source(here::here("0-config.R"))
library(viridis)
library(reshape2)
library(caret)
library(RANN)
library(dplyr)
library(ggplot2)
library(GGally)

#read in data
d <- box_read_rds(831038135971)

#-------------------------Year 1-----------------------------#
#select year 1 immune variables
y1.var <- select(d, childid, grep("t2_ln", names(d), value=T))
y1.var <- y1.var[,c(1, 5:17)]
colnames(y1.var)

### deal with missingness
#remove observations with all missing data except childid
y1.var <- y1.var[rowSums(is.na(y1.var)) < 13,]
nrow(y1.var) #remaining obs

#check missingness with children that have at least one measurement
sum(is.na(y1.var)) #number total obs
colSums(is.na(y1.var)) #number by cytokine
missing <- y1.var %>% mutate(missing = rowSums(is.na(y1.var)))
length(which(missing$missing >0)) #number of children with missing data
mean(missing$missing[missing$missing >0]) #average missing per child w/ missing data

#store ids and flag
y1.id <- y1.var[,1]
#remove ids from dataset
y1.var <- y1.var[,-1]

#check correlation prior to imputation
corrplot <- y1.var
colnames(corrplot) <- c("GMC", "IFN-g", "IL-10", "IL-12",
                        "IL-13", "IL-17", "IL-1", "IL-2", "IL-21",
                        "IL-4", "IL-5", "IL-6", "TNF-a")
ggcorr(corrplot, label = TRUE, label_round = 2, label_size = 3)
#all positively correlated

#impute using kNN
set.seed(12345)
missing.model = preProcess(y1.var, "knnImpute")
y1.impute.Z = predict(missing.model, y1.var); sum(is.na(y1.impute.Z))

### create sum score
y1.sumscore <- y1.impute.Z %>%
  mutate(sumscore_t2_Z = scale(rowSums(y1.impute.Z), center = TRUE, scale = TRUE)) %>%
  mutate(sumscore_t2_noil2il21_Z = t2_ln_gmc + t2_ln_ifn + t2_ln_il10 + t2_ln_il12 + t2_ln_il13 + t2_ln_il17 + t2_ln_il1 + t2_ln_il4 + t2_ln_il5 + t2_ln_il6 + t2_ln_tnf) %>%
  mutate(sumscore_t2_noil2il21_Z = scale(sumscore_t2_noil2il21_Z, center = TRUE, scale = TRUE))%>%
  select(sumscore_t2_Z, sumscore_t2_noil2il21_Z)

y1.scoreid <- as.data.frame(cbind(childid = y1.id, y1.sumscore))

#-------------------------Year 2-----------------------------#
#select year 2 immune variables
y2.var <- select(d, childid, grep("t3_ln", names(d), value=T))
y2.var <- y2.var[,c(1, 5:17)]
colnames(y2.var)

### deal with missingness
#remove observations with all missing data except childid
y2.var <- y2.var[rowSums(is.na(y2.var)) < 13,]
nrow(y2.var) #remaining obs

#check missingness with children that have at least one measurement
sum(is.na(y2.var)) #number total obs
colSums(is.na(y2.var)) #number by cytokine
missing <- y2.var %>% mutate(missing = rowSums(is.na(y2.var)))
length(which(missing$missing >0)) #number of children with missing data
mean(missing$missing[missing$missing >0]) #average missing per child

#store ids and flag
y2.id <- y2.var[,1]
#remove ids from PCA dataset
y2.var <- y2.var[,-1]

#check correlation prior to imputation
corrplot <- y2.var
colnames(corrplot) <- c("GMC", "IFN-g", "IL-10", "IL-12",
                        "IL-13", "IL-17", "IL-1", "IL-2", "IL-21",
                        "IL-4", "IL-5", "IL-6", "TNF-a")
ggcorr(corrplot, label = TRUE, label_round = 2, label_size = 3)
#all positively correlated

#impute using kNN
set.seed(12345)
missing.model = preProcess(y2.var, "knnImpute")
y2.impute.Z = predict(missing.model, y2.var); sum(is.na(y2.impute.Z))

### create sum score
y2.sumscore <- y2.impute.Z %>%
  mutate(sumscore_t3_Z = scale(rowSums(y2.impute.Z), center = TRUE, scale = TRUE)) %>%
  mutate(sumscore_t3_noil2il21_Z = t3_ln_gmc + t3_ln_ifn + t3_ln_il10 + t3_ln_il12 + t3_ln_il13 + t3_ln_il17 + t3_ln_il1 + t3_ln_il4 + t3_ln_il5 + t3_ln_il6 + t3_ln_tnf) %>%
  mutate(sumscore_t3_noil2il21_Z = scale(sumscore_t3_noil2il21_Z, center = TRUE, scale = TRUE))%>%
  select(sumscore_t3_Z, sumscore_t3_noil2il21_Z)

y2.scoreid <- as.data.frame(cbind(childid = y2.id, y2.sumscore))

####### write results
#merge years
sumscores <- merge(y1.scoreid, y2.scoreid, all = TRUE)

#childid with no immune data (n=15)
d$childid[!(d$childid %in% sumscores$childid)]

box_write(
  sumscores,
  "bangladesh-cleaned-child-immune-sum-score.csv",
  dir_id = 140726526642
)


