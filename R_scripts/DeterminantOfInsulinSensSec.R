# Modelling : Determinants of Insulin secretion and Insulin sensitivity ratio.
# Boruta Feature Selection : Outcome ratio of homa-B/Homa-S
require(Boruta)
# model 1 : Birth Model homa B and Homa S ratio
BirthModelRatio<-Boruta(ratio_homaBS~.,data=dat_model_wc[,-c(3,4,67:93)],
                        maxRuns=1000,doTrace=2)
confirmed_var<-
  BirthModelRatio$finalDecision[which(BirthModelRatio$finalDecision=="Confirmed")]
par(mar=c(5,3,3,3))
par(oma=c(5,3,3,3))
plot(BirthModelRatio,las=2,xlab="")

# Random Forest model on model1 important variables 
index_training<-sample(1:nrow(dat_model_wc),nrow(dat_model_wc)*0.8,replace=F)

randomForest(ratio_homaBS~.,
             data=dat_model_wc[index_training,c("hwaist","hhip","hbmi","mwt2","su2","fatmass2","ratio_homaBS")],
             ntree=1000)
summary(lm(ratio_homaBS~.,
           data=dat_model_wc[index_training,c("hwaist","hhip","hbmi","mwt2","su2","fatmass2","ratio_homaBS")]))
# Birth Model HOMA-S
BirthModelHOMAS<-Boruta(homa_sens_18~.,data=dat_model_wc[,-c(4,67:94)],
                        maxRuns=1000,doTrace=2)
confirmedVar<-
  BirthModelHOMAS$finalDecision[which(BirthModelHOMAS$finalDecision=="Confirmed")]
par(mar=c(4,3,3,3))
par(oma=c(4,3,3,3))
plot(BirthModelHOMAS,las=2,xlab="")

randomForest(homa_sens_18~.,
data=dat_model_wc[index_training,
                  which(colnames(dat_model_wc[,-c(4,67:94)]) %in% c(getSelectedAttributes(BirthModelHOMAS),"homa_sens_18"))],
             ntree=1000)
summary(lm(homa_sens_18~.,
    data=dat_model_wc[index_training,
    which(colnames(dat_model_wc[,-c(4,67:94)]) %in% c(getSelectedAttributes(BirthModelHOMAS),"homa_sens_18"))]))
# model 2: Birth Model + Delta

BirthDeltaModelRatio<-Boruta(ratio_homaBS~.,data=dat_model_wc[,-c(3,4)],maxRuns=1000,doTrace=2)

# Random Forest model on model2 important variables
randomForest(ratio_homaBS~.,
 data=dat_model_wc[index_training,c("delta_nwt612","delta_nma612","delta_ntr612",
                                    "delta_nss612","delta_waist612","delta_insulin612",
                                    "delta_leanper612","delta_totfat612","delta_totlean612",
                                    "delta_homab612","delta_fatper612", "ratio_homaBS")],ntree=1000)
summary(lm(ratio_homaBS~.,
           data=dat_model_wc[index_training,c("delta_nwt612","delta_nma612","delta_ntr612",
                                              "delta_nss612","delta_waist612","delta_insulin612",
                                              "delta_leanper612","delta_totfat612","delta_totlean612",
                                              "delta_homab612","delta_fatper612", "ratio_homaBS")]))
# wiseR 