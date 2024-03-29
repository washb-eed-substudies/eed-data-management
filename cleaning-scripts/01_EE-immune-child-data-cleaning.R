rm(list=ls())
source(here::here("0-config.R"))

######################
###Load in data
######################

d <- box_read(830173814777)

summary(d$t2_ln_il2)
summary(d$t3_ln_il2)
summary(d$t3_ln_il2 - d$t2_ln_il2)
summary(d$il1_t3 - d$il1_t2)
summary(log(d$il1_t3) - log(d$il1_t2))
summary(log(d$il1_t3 - d$il1_t2))
summary(d$d23_il2)


#drop Z-score, sd, and ratio measures
d <- d[,!(grepl("z_",colnames(d)) | grepl("sd_",colnames(d)))]
d <- d %>% subset(., select = -c(t2_ratio_pro_il10,
                                 t2_ratio_th1_il10,
                                 t2_ratio_th2_il10,
                                 t2_ratio_th17_il10,
                                 t2_ratio_th1_th2,
                                 t2_ratio_th1_th17,
                                 t3_ratio_pro_il10,
                                 t3_ratio_th1_il10,
                                 t3_ratio_th2_il10,
                                 t3_ratio_th17_il10,
                                 t3_ratio_th1_th2,
                                 t3_ratio_th1_th17,
                                 d23_ratio_pro_il10,
                                 d23_ratio_th1_il10,
                                 d23_ratio_th2_il10,
                                 d23_ratio_th17_il10,
                                 d23_ratio_th1_th2,
                                 d23_ratio_th1_th17))

x=c("il1_t2", "il6_t2", "tnfa_t2")[1]
summary(as.vector(scale(d[,x], center = FALSE, scale = apply(as.matrix(d[,x]), 2, sd, na.rm = TRUE))))
x=c("il1_t2", "il6_t2", "tnfa_t2")[2]
summary(as.vector(scale(d[,x], center = FALSE, scale = apply(as.matrix(d[,x]), 2, sd, na.rm = TRUE))))
x=c("il1_t2", "il6_t2", "tnfa_t2")[3]
summary(as.vector(scale(d[,x], center = FALSE, scale = apply(as.matrix(d[,x]), 2, sd, na.rm = TRUE))))


#function to create composite score
create_score <- function(d, numerator_vars=c("il1_t2", "il6_t2", "tnfa_t2"), denominator_vars="il10_t2", varname="t2_ratio_pro_il10"){
  for(i in numerator_vars){
    if(i==numerator_vars[1]){
      x = as.vector(scale(d[,i], center = FALSE, scale = apply(as.matrix(d[,i]), 2, sd, na.rm = TRUE)))
    }else{
      x = x + as.vector(scale(d[,i], center = FALSE, scale = apply(as.matrix(d[,i]), 2, sd, na.rm = TRUE)))
    }
  }
  summary(x)


  for(i in denominator_vars){
    if(i==denominator_vars[1]){
      y = as.vector(scale(d[,i], center = FALSE, scale = apply(as.matrix(d[,i]), 2, sd, na.rm = TRUE)))
    }else{
      y = y + as.vector(scale(d[,i], center = FALSE, scale = apply(as.matrix(d[,i]), 2, sd, na.rm = TRUE)))
    }
  }
  summary(y)

  score=log(x/y)
  summary(score)
  d$score <- score
  colnames(d)[ncol(d)] <- varname
  return(d)
}





# *Pro-inflammatory cytokines / IL-10
# *(IL-1 + IL-6 + TNF-a) / IL-10
d <- create_score(d, numerator_vars=c("il1_t2", "il6_t2", "tnfa_t2"), denominator_vars="il10_t2", varname="t2_ratio_pro_il10")
summary(d$t2_ratio_pro_il10)
ggplot(d, aes(x=t2_ratio_pro_il10)) + geom_density()

# *Th1 / IL-10
# *(IL-12 + IFN) / IL-10
# gen t2_ratio_th1_il10 = (il12_t2 + ifng_t2) / il10_t2
d <- create_score(d, numerator_vars=c("il12_t2", "ifng_t2"), denominator_vars="il10_t2", varname="t2_ratio_th1_il10")
summary(d$t2_ratio_th1_il10)
ggplot(d, aes(x=t2_ratio_th1_il10)) + geom_density()

# *Th2 / IL-10
# *(IL-4 + IL-5 + IL-13) / IL-10
# gen t2_ratio_th2_il10 = (il4_t2 + il5_t2 + il13_t2) / il10_t2
d <- create_score(d, numerator_vars=c("il4_t2", "il5_t2", "il13_t2"), denominator_vars="il10_t2", varname="t2_ratio_th2_il10")
summary(d$t2_ratio_th2_il10)
ggplot(d, aes(x=t2_ratio_th2_il10)) + geom_density()


# *Th17 / IL-10
# *(IL-17A + IL-21) / IL-10
# gen t2_ratio_th17_il10 = (il17_t2 + il21_t2) / il10_t2
d <- create_score(d, numerator_vars=c("il17_t2", "il21_t2"), denominator_vars="il10_t2", varname="t2_ratio_th17_il10")
summary(d$t2_ratio_th17_il10)
ggplot(d, aes(x=t2_ratio_th17_il10)) + geom_density()


# *Th1 / Th2
# *(IL-12 + IFN) / (IL-4 + IL-5 + IL-13)
# gen t2_ratio_th1_th2 = (il12_t2 + ifng_t2) / (il4_t2 + il5_t2 + il13_t2)
d <- create_score(d, numerator_vars=c("il12_t2", "ifng_t2"), denominator_vars=c("il4_t2", "il5_t2", "il13_t2"), varname="t2_ratio_th1_th2")
summary(d$t2_ratio_th1_th2)
ggplot(d, aes(x=t2_ratio_th1_th2)) + geom_density()


# *Th1 / Th17
# *(IL-12+IFN) / (IL-17A + IL-21)
# gen t2_ratio_th1_th17 = (il12_t2 + ifng_t2) / (il17_t2 + il21_t2)
d <- create_score(d, numerator_vars=c("il12_t2", "ifng_t2"), denominator_vars=c("il17_t2", "il21_t2"), varname="t2_ratio_th1_th17")
summary(d$t2_ratio_th1_th17)
ggplot(d, aes(x=t2_ratio_th1_th17)) + geom_density()


#
# *Pro-inflammatory cytokines / IL-10
# *(IL-1 + IL-6 + TNF-a) / IL-10
# gen t3_ratio_pro_il10 = (il1_t3 + il6_t3 + tnf_t3) / il10_t3
d <- create_score(d, numerator_vars=c("il1_t3", "il6_t3", "tnfa_t3"), denominator_vars="il10_t3", varname="t3_ratio_pro_il10")
summary(d$t3_ratio_pro_il10)
ggplot(d, aes(x=t3_ratio_pro_il10)) + geom_density()


# *Th1 / IL-10
# *(IL-12 + IFN) / IL-10
# gen t3_ratio_th1_il10 = (il12_t3 + ifn_t3) / il10_t3
d <- create_score(d, numerator_vars=c("il12_t3", "ifng_t3"), denominator_vars="il10_t3", varname="t3_ratio_th1_il10")
summary(d$t3_ratio_th1_il10)
ggplot(d, aes(x=t3_ratio_th1_il10)) + geom_density()


# *Th2 / IL-10
# *(IL-4 + IL-5 + IL-13) / IL-10
# gen t3_ratio_th2_il10 = (il4_t3 + il5_t3 + il13_t3) / il10_t3
d <- create_score(d, numerator_vars=c("il4_t3", "il5_t3", "il13_t3"), denominator_vars="il10_t3", varname="t3_ratio_th2_il10")
summary(d$t3_ratio_th2_il10)
ggplot(d, aes(x=t3_ratio_th2_il10)) + geom_density()


# *Th17 / IL-10
# *(IL-17A + IL-21) / IL-10
# gen t3_ratio_th17_il10 = (il17_t3 + il21_t3) / il10_t3
d <- create_score(d, numerator_vars=c("il17_t3", "il21_t3"), denominator_vars="il10_t3", varname="t3_ratio_th17_il10")
summary(d$t3_ratio_th17_il10)
ggplot(d, aes(x=t3_ratio_th17_il10)) + geom_density()


# *Th1 / Th2
# *(IL-12 + IFN) / (IL-4 + IL-5 + IL-13)
# gen t3_ratio_th1_th2 = (il12_t3 + ifn_t3) / (il4_t3 + il5_t3 + il13_t3)
d <- create_score(d, numerator_vars=c("il12_t3", "ifng_t3"), denominator_vars=c("il4_t3", "il5_t3", "il13_t3"), varname="t3_ratio_th1_th2")
summary(d$t3_ratio_th1_th2)
ggplot(d, aes(x=t3_ratio_th1_th2)) + geom_density()


# *Th1 / Th17
# *(IL-12+IFN) / (IL-17A + IL-21)
# gen t3_ratio_th1_th17 = (il12_t3 + ifn_t3) / (il17_t3 + il21_t3)
d <- create_score(d, numerator_vars=c("il12_t3", "ifng_t3"), denominator_vars=c("il17_t3", "il21_t3"), varname="t3_ratio_th1_th17")
summary(d$t3_ratio_th1_th17)
ggplot(d, aes(x=t3_ratio_th1_th17)) + geom_density()

d <- d %>% mutate(
  d23_ratio_pro_il10 = t3_ratio_pro_il10 - t2_ratio_pro_il10,
  d23_ratio_th1_il10 = t3_ratio_th1_il10 - t2_ratio_th1_il10,
  d23_ratio_th2_il10 = t3_ratio_th2_il10 - t2_ratio_th2_il10,
  d23_ratio_th17_il10 = t3_ratio_th17_il10 - t2_ratio_th17_il10,
  d23_ratio_th1_th2 = t3_ratio_th1_th2 - t2_ratio_th1_th2,
  d23_ratio_th1_th17 = t3_ratio_th1_th17 - t2_ratio_th1_th17)

ggplot(d, aes(x=d23_ratio_pro_il10)) + geom_density()
ggplot(d, aes(x=d23_ratio_th1_il10)) + geom_density()
ggplot(d, aes(x=d23_ratio_th2_il10)) + geom_density()
ggplot(d, aes(x=d23_ratio_th17_il10)) + geom_density()
ggplot(d, aes(x=d23_ratio_th1_th2)) + geom_density()
ggplot(d, aes(x=d23_ratio_th1_th17)) + geom_density()

d <- d %>% select(childid, grep("t2|t3|d23|month", names(d), value=T), -grep("sample|faid|mean", names(d), value=T))

box_write(
  d,
  "bangladesh-cleaned-child-immune-ratio.RDS",
  dir_id = 140726526642
)







