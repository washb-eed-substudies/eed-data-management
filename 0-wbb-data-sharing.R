
#install.packages("boxr")
library(boxr)
library(haven)

gps <- read_dta("C:/Users/andre/Downloads/6. WASHB_Baseline_gps.dta")
head(gps)
write.csv(gps,"C:/Users/andre/Downloads/6. WASHB_Baseline_gps.csv")

# #washb SPA
#
# #id
# boxr_id <- "cjm4vlxyeqghaxmb8grx407das8lnr0q"
#
# #secret
# boxr_secret <- "BX6Ailg6PVmYn4jVMhgrlmOn9uhIHrNd"

#Audrie's box
#https://berkeley.app.box.com/folder/140575096710

#ID
boxr_id <- "b176f28aebxv34pp0irdlocofoumpamg"
#Secret:
boxr_secret <- "gT3Kf9xTtlBB3ZxSiHWEV16SsqMr5WCb"


box_auth(client_id = boxr_id, client_secret = boxr_secret)
#It is also important to set the default working directory so that the code can reference the correct folder in box:
#box_setwd("1410")


#Master dataset
box_auth()
d <- box_read("871638120165")
head(d)


library(boxr)
box_auth()
d <- box_read("871638120165")
d <- d %>% filter(.$`pregnancy-immune`==1)
# head(d)
# #usethis::edit_r_environ()
