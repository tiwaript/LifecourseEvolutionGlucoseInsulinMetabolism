# Line plots HOMAB/HOMAS and others 

dat_bergplot_long$ratio_betasens<-dat_bergplot_long$homabeta/dat_bergplot_long$homasens
dat_bergplot_long$prod_betasens<-dat_bergplot_long$homabeta*dat_bergplot_long$homasens
dat_mean<-dat_bergplot_long[1:6,-c(2,5,6)]
dat_mean$time<-c(6,6,12,12,18,18)
dat_mean$class<-c("NGT","Prediabetes","NGT","Prediabetes","NGT","Prediabetes")
dat_mean$ratio_betasens<-c(0.219,0.203,0.54,0.52,1.239,1.41)
dat_mean$key<-c(1,2,1,2,1,2)
dat_mean$prod_betasens<-c(13942.40,12380.76,12142.62,11202.92,8423.06,6536.99)
dat_mean$class<-factor(dat_mean$class,levels = c("Prediabetes","NGT"))
library(ggplot2)
# Basic line plot with points
ggplot(data=na.omit(dat_bergplot_long), aes(x=as.factor(time),alpha=0.1)) +
  geom_line()+
  geom_point()+facet_grid(.~class)
## mean data plot
ggplot(data=dat_mean, aes(x=as.factor(time),y=ratio_betasens,color=class,group=key)) +
  geom_line()+
  geom_point()

ggplot(data=dat_mean, aes(x=as.factor(time),y=log(prod_betasens),color=class,group=key)) +
  geom_line()+
  geom_point()
