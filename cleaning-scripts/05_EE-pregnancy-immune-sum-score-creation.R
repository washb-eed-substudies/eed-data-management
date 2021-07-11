rm(list=ls())
source(here::here("0-config.R"))
library(viridis)
library(reshape2)
library(caret)
library(RANN)
library(dplyr)
library(ggplot2)
library(GGally)

d<-box_read(830184329206)

#####  Year 1
#select immune variables
mom <- select(d, dataid, grep("mom_t0_ln", names(d), value=T))
colnames(mom)

mom <- mom[rowSums(is.na(mom)) < 13,]
nrow(mom) #remaining obs
mom.cytokine <- mom[,c(2:14)]

#check missingness with children that have at least one measurement
sum(is.na(mom.cytokine)) #number total obs
colSums(is.na(mom.cytokine)) #number by cytokine
missing <- mom.cytokine %>% mutate(missing = rowSums(is.na(mom.cytokine)))
length(which(missing$missing >0)) #number of children with missing data
mean(missing$missing[missing$missing >0]) #average missing per child

#check correlation prior to imputation
corrplot <- mom.cytokine
colnames(corrplot) <- c("GMC", "IFN-g", "IL-10", "IL-12",
                        "IL-13", "IL-17", "IL-1", "IL-2", "IL-21",
                        "IL-4", "IL-5", "IL-6", "TNF-a")
ggcorr(corrplot, label = TRUE, label_round = 2, label_size = 3)

#cormat <- round(cor(mom.cytokine, use="pairwise.complete.obs"),2)
#cormat[lower.tri(cormat)]<- NA
#all positively correlated

#impute using kNN
set.seed(12345)
missing.model = preProcess(mom.cytokine, "knnImpute")
mom.impute.Z = predict(missing.model, mom.cytokine); sum(is.na(mom.impute.Z))

#create sum score
mom.sumscore <- mom.impute.Z %>%
  mutate(sumscore_t0_mom_Z = rowSums(mom.impute.Z)) %>%
  select(sumscore_t0_mom_Z)

mom.sumscore <- scale(mom.sumscore, center = TRUE, scale = TRUE)

mom.sumscore <- as.data.frame(cbind(dataid = mom$dataid, mom.sumscore))

##### export results
box_write(mom.sumscore,
          "bangladesh-maternal-immune-sum-score.csv", dir_id = 140726526642)
