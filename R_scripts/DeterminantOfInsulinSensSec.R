# Modelling : Determinants of Insulin secretion and Insulin sensitivity ratio.
# Boruta Feature Selection : Outcome ratio of homa-B/Homa-S
require(Boruta)
# model 1 : Birth Model homa B and Homa S ratio
set.seed(1000)
BirthModelRatio<-Boruta(ratio_homaBS~.,data=dat_model_wc[,-c(3,4,67:93)],
                        maxRuns=1000,doTrace=2)
confirmed_var<-getSelectedAttributes(BirthModelRatio)
par(mar=c(5,2,2,2))
par(oma=c(5,2,2,2))
png("BirthModel_boruta.png",height = 6,width = 12,units = 'in',res=300)
plot(BirthModelRatio,las=2,xlab="",cex.axis=0.5)
dev.off()
# Random Forest model on model1 important variables 
index_training<-sample(1:nrow(dat_model_wc),nrow(dat_model_wc)*0.8,replace=F)

randomForest(ratio_homaBS~.,
             data=dat_model_wc[index_training,c(confirmed_var,"ratio_homaBS")],
             ntree=1000)
summary(lm(ratio_homaBS~.,
           data=dat_model_wc[index_training,c(confirmed_var,"ratio_homaBS")]))
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
set.seed(1000)
BirthDeltaModelRatio<-Boruta(ratio_homaBS~.,data=dat_model_wc[,-c(3,4)],maxRuns=1000,doTrace=2)
confirmed_var<-getSelectedAttributes(BirthDeltaModelRatio)
par(mar=c(5,2,2,2))
par(oma=c(5,2,2,2))
png("BirthdeltaModel_boruta.png",height = 6,width = 12,units = 'in',res=300)
plot(BirthDeltaModelRatio,las=2,xlab="",cex.axis=0.5)
dev.off()
# Random Forest model on model2 important variables
randomForest(ratio_homaBS~.,
 data=dat_model_wc[index_training,c(confirmed_var, "ratio_homaBS")],ntree=1000)
summary(lm(ratio_homaBS~.,
           data=dat_model_wc[index_training,c(confirmed_var, "ratio_homaBS")]))
# discretized homabs ratio: 
set.seed(1000)
BirthModelRatio<-Boruta(Discretized.x~.,data=dat_model_wc[,-c(3,4,67:94)],
                        maxRuns=1000,doTrace=2)
confirmed_var<-getSelectedAttributes(BirthModelRatio)
par(mar=c(5,2,2,2))
par(oma=c(5,2,2,2))
png("BirthModel_boruta_discrete.png",height = 6,width = 12,units = 'in',res=300)
plot(BirthModelRatio,las=2,xlab="",cex.axis=0.5)
dev.off()
randomForest(Discretized.x~.,
             +              data=dat_model_wc[index_training,c(confirmed_var, "Discretized.x")],ntree=1000)
######## birth delta model 
set.seed(1000)
BirthdeltaModelRatio<-Boruta(Discretized.x~.,data=dat_model_wc[,-c(3,4,94)],
                        maxRuns=1000,doTrace=2)
confirmed_var<-getSelectedAttributes(BirthModelRatio)
par(mar=c(5,2,2,2))
par(oma=c(5,2,2,2))
png("BirthdeltaModel_boruta_discrete.png",height = 6,width = 12,units = 'in',res=300)
plot(BirthdeltaModelRatio,las=2,xlab="",cex.axis=0.5)
dev.off()
randomForest(Discretized.x~.,
               data=dat_model_wc[index_training,c(confirmed_var, "Discretized.x")],ntree=1000)
