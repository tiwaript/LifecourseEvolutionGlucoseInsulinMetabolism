# confidence interval calculation
dat_18<-na.omit(dat_bergplot[,c("homa_beta_18","homa_sens_18","class")])
dat_18$homa_beta_18<-log(dat_18$homa_beta_18)
dat_18$homa_sens_18<-log(dat_18$homa_sens_18)
model_18<-lm(homa_beta_18~homa_sens_18+class+class*homa_sens_18,
             data=dat_18)

median(dat_18$homa_sens_18,na.rm=T)
new<-data.frame(c(4.36,4.36),c("NGT","Prediabetes"))
colnames(new)<-c("homa_sens_18","class")
predict(model_18,new,interval = "confidence")

##12 years
dat_12<-na.omit(dat_bergplot[,c("homa_beta_12","homa_sens_12","class")])
dat_12$homa_beta_12<-log(dat_12$homa_beta_12)
dat_12$homa_sens_12<-log(dat_12$homa_sens_12)
model_12<-lm(homa_beta_12~homa_sens_12+class+class*homa_sens_12,
             data=dat_12)

median(dat_12$homa_sens_12,na.rm=T)
new<-data.frame(c(5.01,5.01),c("NGT","Prediabetes"))
colnames(new)<-c("homa_sens_12","class")
predict(model_12,new,interval = "confidence")

## 6 years
dat_6<-na.omit(dat_bergplot[,c("homa_beta_6","homa_sens_6","class")])
dat_6$homa_beta_6<-log(dat_6$homa_beta_6)
dat_6$homa_sens_6<-log(dat_6$homa_sens_6)
model_6<-lm(homa_beta_6~homa_sens_6+class+class*homa_sens_6,
             data=dat_6)

median(dat_6$homa_sens_6,na.rm=T)
new<-data.frame(c(5.50,5.50),c("NGT","Prediabetes"))
colnames(new)<-c("homa_sens_6","class")
predict(model_6,new,interval = "confidence")