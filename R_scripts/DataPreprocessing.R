# Packages Required 
require(foreign)
require(tidyr)
require(tidyverse)
require(randomForest)
# data import 

dat_ihoma<-read.spss("./Data/PMNS PREDIABETES DATA_27JUNE2019.sav",
                     to.data.frame = T,use.value.labels = T)
dat_bergplot<-na.omit(dat_ihoma[,c("key","sex","homa_sens_18","homa_beta_18",
                                   "homa_sens_12","homa_beta_12",
                                   "homa_sens_6","homa_beta_6","cgly_st_18yr")]) #n=624
colnames(dat_bergplot)[9]<-"class"
dat_PMNS<-read.spss("./Data/pmns original.sav",to.data.frame = T,use.value.labels = T)
# Data for determinants of HOMA-S and HOMA-B
# Complete cases data for insulin sensenitivity and secretion
dat_model<-na.omit(dat_ihoma[,c("key","sex","b_gage_w",
                                "homa_sens_18","homa_beta_18")]) # n=643
rownames(dat_model)<-dat_model$key
# Data Father PMNS 
dat_PMNS_Father<-readRDS("./Data/PMNS_Father_data.RDS")
rownames(dat_PMNS_Father)<-dat_PMNS_Father$key
dat_model<-merge(dat_model,dat_PMNS_Father,by=0)
var_rm_father<-c("hht","fhcy_i","hbetar","hhct","hplt",
                 "hly_perc","hly_no","hhead","hsplit",
                 "hin2hr","hinfast","hgl2hr","hglfast",
                 "hhb","hproins","hhomar")
dat_model<-dat_model[,-which(colnames(dat_model) %in% var_rm_father)]
rownames(dat_model)<-dat_model$Row.names
dat_model<-dat_model[,-c(1,2,7)]
na_count<-map(dat_model, ~sum(is.na(.)))
dat_model<-na.roughfix(dat_model)
# Data newborn
var_newb<-c("key","wtn","length","hcn","chn","abdn",
            "man","trn","ssn","foot","pondi")
dat_newb<-dat_PMNS[,which(colnames(dat_PMNS) %in% var_newb)]
rownames(dat_newb)<-dat_newb$key
dat_model<-merge(dat_model,dat_newb,by=0)
rownames(dat_model)<-dat_model$Row.names
dat_model<-dat_model[,-c(1,14)]
na_count<-map(dat_model, ~sum(is.na(.)))
dat_model<-na.roughfix(dat_model)
# Data mother 28 wk pregnancy
var_mother<-c("key","wom_edun","gravida","para","age","tobacco","reg_smok","height","head",
              "sya2","dia2","mapa2","fh2","ag2","mwt2","tr2",
              "bi2","ss2","su2","ma2","waist2","hip2","liquor","hb2","hct2","glfast2","gl2hr",
              "fins2","in2hr2","proins2","split2","rcf2","fer2","vitc2","vitac2","chol2",
              "hdlchol2","tg2","urac2","creat2","urcreat2","ur5oxo2","opcr2","alb2","nefa2",
              "wbc2","rbc2","ly_no2","ly_perc2","plt2","malb2","bmi2","whr2","fatmass2",
              "ldlchol2","homar2","betar2","b12_v2","fol_v2","thcy_v2","tcys_v2","cysta_v2",
              "met_v2","mma_v2")
dat_mother_28wk<-dat_PMNS[,colnames(dat_PMNS) %in% var_mother]
rownames(dat_mother_28wk)<-dat_mother_28wk$key
dat_model<-merge(dat_model,dat_mother_28wk,by=0)
rownames(dat_model)<-dat_model$Row.names
dat_model<-dat_model[,-46]
na_count<-map(dat_model, ~sum(is.na(.)))
dat_model<-dat_model[,-c(1,24)]
dat_model$hb2<-as.numeric(as.character(dat_model$hb2))
dat_model[,which(na_count>0 & na_count<60)]<-
  na.roughfix(dat_model[,which(na_count>0 & na_count<60)])
dat_model_wc<-na.omit(dat_model[,-which(na_count>100)]) # n=505,p=66
rownames(dat_model_wc)<-dat_model_wc$Row.names
# data for delta change from 0-6,6-12 
var_delta<-
c("key","b_wt","wt_6","wt_12","b_length","ht_6","ht_12","b_midc","ma_6","ma_12",
  "b_ts", "tr_6","tr_12","b_ss","ss_6","ss_12","sbp_6","sbp_12",
  "dbp_6","dbp_12","waist6","waist12", "hip6", "hip12","glucose_f6","glucose_f12",
  "chol_6","chol_12","hdl_6","hdl_12","tg_6","tg_12","insulin_f6","insuline_f12",
  "tot_bmd_6","tot_bmd_12","tot_bm6","tot_bm12","fat_per_6","fat_per_12",
  "lean_per_6","lean_per_12","tot_fat_6","tot_fat_12","tot_lean_6","tot_lean_12",
   "homa_sens_6","homa_sens_12","homa_beta_6","homa_beta_12")
dat_delta<-dat_ihoma[,which(colnames(dat_ihoma) %in% var_delta)]
# Calculation of delta values
dat_delta$delta_nwt06<-(dat_delta$wt_6-dat_delta$b_wt)
dat_delta$delta_nwt612<-(dat_delta$wt_12-dat_delta$wt_6)
dat_delta$delta_nht06<-(dat_delta$ht_6-dat_delta$b_length)
dat_delta$delta_nht612<-(dat_delta$ht_12-dat_delta$ht_6)
dat_delta$delta_nma06<-(dat_delta$ma_6-dat_delta$b_midc)
dat_delta$delta_nma612<-(dat_delta$ma_12-dat_delta$ma_6)
dat_delta$delta_ntr06<-(dat_delta$tr_6-dat_delta$b_ts)
dat_delta$delta_ntr612<-(dat_delta$tr_12-dat_delta$tr_6)
dat_delta$delta_nss06<-(dat_delta$ss_6-dat_delta$b_ss)
dat_delta$delta_nss612<-(dat_delta$ss_12-dat_delta$ss_6)
dat_delta$delta_sbp612<-(dat_delta$sbp_12-dat_delta$sbp_6)
dat_delta$delta_dbp612<-(dat_delta$dbp_12-dat_delta$dbp_6)
dat_delta$delta_waist612<-(dat_delta$waist12-dat_delta$waist6)
dat_delta$delta_hip612<-(dat_delta$hip12-dat_delta$hip6)
dat_delta$delta_gluf612<-(dat_delta$glucose_f12-dat_delta$glucose_f6)
dat_delta$delta_chol612<-(dat_delta$chol_12-dat_delta$chol_6)
dat_delta$delta_hdl612<-(dat_delta$hdl_12-dat_delta$hdl_6)
dat_delta$delta_tg612<-(dat_delta$tg_12-dat_delta$tg_6)
dat_delta$delta_insulin612<-(dat_delta$insuline_f12-dat_delta$insulin_f6)
dat_delta$delta_totbmd612<-(dat_delta$tot_bmd_12-dat_delta$tot_bmd_6)
dat_delta$delta_totbm612<-(dat_delta$tot_bm12-dat_delta$tot_bm6)
dat_delta$delta_fatper612<-(dat_delta$fat_per_12-dat_delta$fat_per_6)
dat_delta$delta_leanper612<-(dat_delta$lean_per_12-dat_delta$lean_per_6)
dat_delta$delta_totfat612<-(dat_delta$tot_fat_12-dat_delta$tot_fat_6)
dat_delta$delta_totlean612<-(dat_delta$tot_lean_12-dat_delta$tot_lean_6)
dat_delta$delta_homas612<-(dat_delta$homa_sens_12-dat_delta$homa_sens_6)
dat_delta$delta_homab612<-(dat_delta$homa_beta_12-dat_delta$homa_beta_6)
rownames(dat_delta)<-dat_delta$key
which(colnames(dat_delta) %in% var_delta)
dat_delta<-dat_delta[,-c(1:50)]
dat_model_wc<-merge(dat_model_wc,dat_delta,by=0)
dat_model_wc<-na.roughfix(dat_model_wc) # final data for determinant of homa-b and homa-s
dat_model_wc$ratio_homaBS<-dat_model_wc$homa_beta_18/dat_model_wc$homa_sens_18
# discretized variable add 
dat_discretized<-read.csv("Data_homaBS_ratio.csv")
dat_model_wc<-cbind(dat_model_wc,dat_discretized)
dat_model_wc<-dat_model_wc[,-c(95,96)]
