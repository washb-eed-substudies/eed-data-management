rm(list=ls())

source(here::here("0-config.R"))

d<-box_read(830174630871)

#---------------------------------------------------------------------------------------------
# transform outcome distributions
#---------------------------------------------------------------------------------------------
d <- d %>%
  mutate(
    t3_saa_z01_raw=t3_saa_z01,
    t3_saa_z02_raw=t3_saa_z02,
    t3_cort_z01_raw=t3_cort_z01,
    t3_cort_z03_raw=t3_cort_z03,
    t2_f2_8ip_raw=t2_f2_8ip,
    t2_f2_23d_raw=t2_f2_23d,
    t2_f2_VI_raw=t2_f2_VI,
    t2_f2_12i_raw=t2_f2_12i,
    t3_gcr_mean_raw=t3_gcr_mean,
    t3_gcr_cpg12_raw=t3_gcr_cpg12,
    t3_saa_z01=log(t3_saa_z01),
    t3_saa_z02=log(t3_saa_z02),
    t3_cort_z01=log(t3_cort_z01),
    t3_cort_z03=log(t3_cort_z03),
    t2_f2_8ip=log(t2_f2_8ip),
    t2_f2_23d=log(t2_f2_23d),
    t2_f2_VI=log(t2_f2_VI),
    t2_f2_12i=log(t2_f2_12i),
    t3_gcr_mean=gtools::logit(t3_gcr_mean/100),
    t3_gcr_cpg12=gtools::logit(t3_gcr_cpg12/100))


#Clean inf values
d <- do.call(data.frame,lapply(d, function(x) replace(x, is.infinite(x),NA)))



#---------------------------------------------------------------------------------------------
# calc combined iso variable
#---------------------------------------------------------------------------------------------



# Combined exposures: To determine overall oxidative stress, we will combine our four measures of urinary F2-
# isoprostanes: iPF(2a)-III, 2,3-dinor-iPF(2a)-III, iPF(2a)-VI, and 8,12-iso-iPF(2a)-VI that are
# consistent with prior oxidative stress operationalization into a single score.29 We will use
# the first principal component of a principal components analysis of the four measures of
# urinary F2-isoprostanes as the oxidative stress score if all measures are correlated with each other (P-value < 0.2),
# otherwise we will analyze the urinary F2-isoprostanes separately.

#Get correlation of isoprostanes
iso <- d %>% select(c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i"))

cor(iso, use="pairwise.complete.obs")
cor.test(iso[,1], iso[,2])$p.value < 0.2
cor.test(iso[,1], iso[,3])$p.value < 0.2
cor.test(iso[,1], iso[,4])$p.value < 0.2
cor.test(iso[,2], iso[,3])$p.value < 0.2
cor.test(iso[,2], iso[,4])$p.value < 0.2
cor.test(iso[,3], iso[,4])$p.value < 0.2


#isoprostanes are significantly correlated, so calculate 1st principal component

df <-  d %>% select(c("childid","t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i")) %>% as.data.frame()
dim(df)
df <- df[complete.cases(df),]
dim(df)

# #Select assets and seperate out ID
id<-subset(df, select=c("childid")) #drop subjectid
df<-df[,which(!(colnames(df) %in% c("childid")))]

##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(df)


## To get the first principal component in a variable ##
load <- loadings(princ.return)[,1]

pr.cp <- as.matrix(df) %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form.

df$t2_iso_pca <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.

#examine combined score
hist(df$t2_f2_8ip)
hist(df$t2_f2_23d)
hist(df$t2_f2_VI)
hist(df$t2_f2_12i)
hist(df$t2_iso_pca)

#check direction between individual isoprostanes and the combined score
ggplot(df, aes(x=t2_f2_8ip, y=t2_iso_pca)) + geom_point() + geom_smooth()
ggplot(df, aes(x=t2_f2_23d, y=t2_iso_pca)) + geom_point() + geom_smooth()
ggplot(df, aes(x=t2_f2_VI, y=t2_iso_pca)) + geom_point() + geom_smooth()
ggplot(df, aes(x=t2_f2_12i, y=t2_iso_pca)) + geom_point() + geom_smooth()

#merge combined score back into main dataset
df.pca <- data.frame(childid=id, t2_iso_pca=df$t2_iso_pca)

dfull <- left_join(d, df.pca, by="childid")
hist(dfull$t2_iso_pca)


## convert time of day of pre-stressor measurement of cortisol and sAA into continuous variable
time_day <- dfull$t3_col_time_z01
time_split <- str_split(time_day, ":")
cont_time <- function(list_hr_min){
  # takes in list of time
  # first number is hour of the day
  # second number in list is minute of the hour
  num_time <- as.numeric(unlist(list_hr_min))
  num_time[1]+num_time[2]/60
}
dfull$t3_col_time_z01_cont <- sapply(time_split, cont_time)

names(dfull)
dfull <- subset(dfull, select = -c(t3_hr1, t3_hr2, t3_hr3, t3_sysbp1, t3_diasbp1, t3_sysbp2,
                                     t3_diasbp2, t3_sysbp3, t3_diasbp3, t3_sysbp_mean, t3_diasbp_mean,
                                     t3_z01_time, t3_z02_time, t3_z03_time, t3_cort_min_elaps, t3_saa_min_elaps, t3_gcr_stdev))
cleaned_stress <- dfull %>% select(childid, grep("t2|t3", names(dfull), value=T), -c(diar7d_t2, diar7d_t3), -grep("samplecol", names(.), value=T))
names(cleaned_stress)

box_write(cleaned_stress, "bangladesh-cleaned-child-stress-data.RDS", dir_id = 140726526642)

