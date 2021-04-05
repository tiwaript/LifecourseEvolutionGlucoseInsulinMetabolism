# Packages Required 
require(foreign)
require(tidyr)
require(tidyverse)
require(randomForest)
# data import 

dat_ihoma<-read.spss("./Data/PMNS PREDIABETES DATA_27JUNE2019.sav",
                     to.data.frame = T,use.value.labels = T)

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
na_count<-map(dat_model, ~sum(is.na(.)))
dat_model[,which(na_count>0 & na_count<60)]<-
  na.roughfix(dat_model[,which(na_count>0 & na_count<60)])
dat_model_wc<-na.omit(dat_model[,-which(na_count>100)]) # n=505
# data for delta change from 0-6,6-12 
