require(glmnet)
# Birth model of ratio of homa b and homa s 
x<-dat_model_wc[index_training,-c(3,4,67:94)]
x<-model.matrix(~.,x)
x<-x[,-1]
y<-dat_model_wc[index_training,94]
set.seed(1000)
LassoBirthModel<-cv.glmnet(x,y,alpha=0.5,nlambda=100,family="gaussian",nfolds=10,standardize=T)
which(LassoBirthModel$lambda==LassoBirthModel$lambda.min)
Varimp<-
data.frame(LassoBirthModel$glmnet.fit$beta[,which(LassoBirthModel$lambda==LassoBirthModel$lambda.min)]) # replace number by lambda min
colnames(Varimp)[1]<-"Coefficient"
Varimp$var<-rownames(Varimp)
varimp_coeff<-Varimp[which(Varimp$Coefficient!=0),]
varimp_coeff$abscoef<-abs(varimp_coeff$Coefficient)
for(i in 1:nrow(varimp_coeff)){
  if(varimp_coeff[i,1]<0){
    varimp_coeff$direction[i]<-"Negative"
  }
  else{
    varimp_coeff$direction[i]<-"Positive"
  }
}
# Birth model + delta 
x<-dat_model_wc[index_training,-c(3,4,94)]
x<-model.matrix(~.,x)
x<-x[,-1]
y<-dat_model_wc[index_training,94]
#set.seed(1000)
LassodeltaModel<-cv.glmnet(x,y,alpha=0.5,nlambda=100,family="gaussian",nfolds=10,standardize=T)
which(LassodeltaModel$lambda==LassodeltaModel$lambda.min)
Varimp<-data.frame(LassodeltaModel$glmnet.fit$beta[,15]) # replace number by lambda min
colnames(Varimp)[1]<-"Coefficient"
Varimp$var<-rownames(Varimp)
varimp_coeff<-Varimp[which(Varimp$Coefficient!=0),]
varimp_coeff$abscoef<-abs(varimp_coeff$Coefficient)
for(i in 1:nrow(varimp_coeff)){
  if(varimp_coeff[i,1]<0){
    varimp_coeff$direction[i]<-"Negative"
  }
  else{
    varimp_coeff$direction[i]<-"Positive"
  }
  }
# plot 
png("elasticnet_deltamodel_varimp_logplot.png",height = 6,width=6,units = 'in', res=300)
#png("elasticnet_birthmodel_varimp_logplot.png",height = 6,width=6,units = 'in', res=300)
ggplot(varimp_coeff, aes(x=reorder(var,-abscoef), y=-log(abscoef), fill=direction)) +
  geom_bar(stat="identity")+theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab("")
dev.off()