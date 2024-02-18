rm(list=ls())

source(here::here("0-config.R"))

#Method based on: https://programming-r-pro-bro.blogspot.com/2011/10/principal-component-analysis-use.html

#load libraries
library(tidyverse)
library(caret)

#read in data
data <- box_read_csv(833359280029)

# merge in animal ownership data
animal <- box_read_csv(880183983740)

d <- merge(data, animal, by="dataid")

#Subset to only needed variables for PCA analysis (identifiers plus assets)
colnames(d)

varlist<-c("dataid", "elec", "asset_wardrobe", "asset_table", "asset_chair", "asset_clock",
           "asset_khat", "asset_chouki", "asset_radio", "asset_tv", "asset_refrig",
           "asset_bike", "asset_moto", "asset_sewmach", "asset_mobile", "asset_phone",
           "n_chicken", "n_goat", "n_cattle")

df <- d %>%
  subset(select=c(varlist))

#drop rows with no asset data
df<-df[rowSums(is.na(df[,2:ncol(df)])) != ncol(df)-1,]


#Select assets and seperate out ID
df<-as.data.frame(df)
id<-subset(df, select=c("dataid")) #drop subjectid
df<-df[,which(!(colnames(df) %in% c("dataid")))]




#Drop assets with great missingness
for(i in 1:ncol(df)){
  cat(colnames(df)[i],"\n")
  print(table(is.na(df[,i])))
  print(class((df[,i])))
}

#Drop asset_clock due to ~50% missingness
df <- df %>% subset(., select = -c(asset_clock))
table(is.na(df))

#Scale data.frame
df_scaled <- scale(df, T, T) %>% as.data.frame()

##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(df)

princ.return.scaled <- princomp(df_scaled)


## To get the first principal component in a variable ##
load <- loadings(princ.return)[,1]

pr.cp <- as.matrix(df) %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form.

df$HHwealth_old <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.


load_scaled <- loadings(princ.return.scaled)[,1]

pr.cp.scaled <- as.matrix(df_scaled) %*% load_scaled  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form.

df$HHwealth_scaled <- as.numeric(pr.cp.scaled) ## Gives us the 1st PC in numeric form in pr.

# #Create 4-level household weath index
# quartiles<-quantile(df$HHwealth, probs=seq(0, 1, 0.25))
# print(quartiles)
# df<-as.data.frame(df)
# df$HHwealth_quart<-rep(1, nrow(df))
# df$HHwealth_quart[df$HHwealth>=quartiles[2]]<-2
# df$HHwealth_quart[df$HHwealth>=quartiles[3]]<-3
# df$HHwealth_quart[df$HHwealth>=quartiles[4]]<-4
# table(df$HHwealth_quart)
# df$HHwealth_quart<-factor(df$HHwealth_quart)


#Table assets by pca quartile to identify wealth/poverty level ordering
df<-data.frame(id, df)

# wealth.tab <- d %>% subset(., select=-c(dataid)) %>%
#   group_by(HHwealth_quart) %>%
#   summarise_all(funs(mean)) %>% as.data.frame()
# print(wealth.tab)
#
# #Quartile 1 is low-wealth
# levels(d$HHwealth_quart)<-c("Wealth Q1", "Wealth Q2","Wealth Q3","Wealth Q4")



#Save just the wealth data
pca.wealth<-df %>% subset(select=c(dataid, HHwealth_old,HHwealth_scaled))
head(pca.wealth)

box_write(pca.wealth, "bangladesh-hhwealth.csv", 140725527270)
