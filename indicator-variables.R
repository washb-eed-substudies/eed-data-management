rm(list=ls())

source(here::here("0-config.R"))

master <- box_read(871638120165)

analyses <- c("eed-dev", "eed-growth", "immune-growth", "immune-dev", "ipv-stress", "ipv-immune", "ipv-telo",
              "pregnancy-stress", "pregnancy-immune", "pregnancy-telo", "stress-dev", "stress-growth",
              "telo-dev")

eed <- list(t1 = c("ln_L_conc_t1", "ln_M_conc_t1", "ln_mpo1", "ln_aat1", "ln_neo1"),
            t2 = c("ln_L_conc_t2", "ln_M_conc_t2", "ln_mpo2", "ln_aat2", "ln_neo2", "ln_reg2"),
            t3 = c("ln_L_conc_t3", "ln_M_conc_t3","ln_mpo3", "ln_aat3", "ln_neo3"))

immune <- list(t2 = c("t2_ln_igf", "t2_ln_crp", "t2_ln_agp", "t2_ln_gmc", "t2_ln_ifn",
                      "t2_ln_il10", "t2_ln_il12", "t2_ln_il13", "t2_ln_il17", "t2_ln_il1",
                      "t2_ln_il2", "t2_ln_il21", "t2_ln_il4", "t2_ln_il5", "t2_ln_il6", "t2_ln_tnf", "sumscore_t2_Z"),
               t3 = c("t3_ln_igf", "t3_ln_crp", "t3_ln_agp", "t3_ln_gmc", "t3_ln_ifn",
                      "t3_ln_il10", "t3_ln_il12", "t3_ln_il13", "t3_ln_il17", "t3_ln_il1",
                      "t3_ln_il2", "t3_ln_il21", "t3_ln_il4", "t3_ln_il5", "t3_ln_il6", "t3_ln_tnf", "sumscore_t3_Z"))

stress <- list(t2 = c("t2_f2_8ip","t2_f2_23d","t2_f2_VI", "t2_f2_12i"),
               t3 = c("t3_map","t3_hr_mean",
                      "t3_saa_z01","t3_saa_z02","t3_cort_z01","t3_cort_z03",
                      "t3_gcr_mean","t3_gcr_cpg12","t3_saa_slope","t3_cort_slope","t3_residual_saa","t3_residual_cort"))

telo <- list(t2 = c("TS_t2"),
             t3 = c("TS_t3"))

pregnancy <- list(t0 = c("vitD_nmol_per_L", "logFERR_inf", "logSTFR_inf", "logRBP_inf",
                          "ln_preg_cort", "logCRP", "logAGP", "ifng_mom_t0", "ln_preg_estri"))

ipv <- list(t2 = c("life_viol_any_t3", "cesd_sum_t2"),
            t3 = c("pss_sum_mom_t3", "pss_sum_dad_t3", "cesd_sum_ee_t3"))

growth <- list(t2 = c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2"),
               t3 = c("laz_t3", "waz_t3", "whz_t3" ,"hcz_t3"))

dev <- list(t2 = c("who_sit", "who_crawl", "who_stand_supp",
                    "who_walk_supp", "who_stand_nosupp", "who_walk_nosup",
                    "sum_who", "z_cdi_und_t2", "z_cdi_say_t2"),
            t3 = c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq",
                    "z_cdi_say_t3", "z_cdi_und_t3"))

filtering <- function(row){
  any(!is.na(row))
}

find_exp_out <- function(name) {
  split <- strsplit(name, "-")
  exp <- split[[1]][1]
  out <- split[[1]][2]
  list(exp, out, get(exp), get(out))
}

for (analysis in analyses) {

  exp_out <- find_exp_out(analysis)
  exp_name <- exp_out[[1]]
  out_name <- exp_out[[2]]
  exp <- exp_out[[3]]
  out <- exp_out[[4]]

  if(exp_name=="pregnancy") {

    t0 <- master[apply(select(master, all_of(exp$t0)), 1, filtering),]
    t0 <- t0[apply(select(t0, all_of(c(out[[1]], out[[2]]))), 1, filtering),]
    total <- t0

  } else if (exp_name=="eed") {

    t1 <- master[apply(select(master, all_of(exp$t1)), 1, filtering),]

    if (out_name=="growth") {
      growth_t1 <- c("laz_t1", "waz_t1", "whz_t1" ,"hcz_t1")
      t1 <- t1[apply(select(t1, all_of(c(growth_t1, out$t2, out$t3))), 1, filtering),]
    } else {
      t1 <- t1[apply(select(t1, all_of(c(out$t2, out$t3))), 1, filtering),]
    }

    t2 <- master[apply(select(master, all_of(exp$t2)), 1, filtering),]
    t2 <- t2[apply(select(t2, all_of(c(out$t2, out$t3))), 1, filtering),]
    t3 <- master[apply(select(master, all_of(exp$t3)), 1, filtering),]
    t3 <- t3[apply(select(t3, all_of(out$t3)), 1, filtering),]
    total <- t1 %>% full_join(t2, by=names(master)) %>% full_join(t3, by=names(master))

  } else {

    t2 <- master[apply(select(master, all_of(exp$t2)), 1, filtering),]
    t2 <- t2[apply(select(t2, all_of(c(out$t2, out$t3))), 1, filtering),]
    t3 <- master[apply(select(master, all_of(exp$t3)), 1, filtering),]
    t3 <- t3[apply(select(t3, all_of(out$t3)), 1, filtering),]
    total <- t2 %>% full_join(t3, by=names(master))

  }
  master[[analysis]] <- ifelse(master$childid %in% total$childid, 1, 0)
  box_write(total, file_name = paste0(analysis, ".RDS"), dir_id = 148798406168)
}

box_write(master,
          "bangladesh-cleaned-master-data.RDS",
          dir_id = 147779347962)
