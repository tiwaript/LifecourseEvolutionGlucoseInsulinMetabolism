##Code for following plots :: 
#Figure 1, Figure2  and Figure3
## Raw data 
## x, y limit data 
## Gender separated data 
require(ggplot2)

################ Figure1 (18years) ##############
#plot1<-
png("Figure1(18years).png",height = 6,width = 6,units = 'in',res=300)
ggplot(data = na.omit(dat_bergplot[,c("homa_sens_18","homa_beta_18","class")]), 
       aes(x = homa_sens_18,  y = homa_beta_18, color = class)) +
  geom_point(aes(group = class, color = class),shape = 1, size = 2) + 
  scale_color_manual(values = c('chartreuse4', 'red')) +
  geom_smooth(se = T) + 
  ggtitle("18years") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  #      axis.title.x = element_blank(),
  #      axis.title.y = element_blank()) + 
  xlab("Insulin Sensitivity") +ylab("Insulin Secretion") + 
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

  dev.off()
############### 12 years #############
plot2<-

png("Figure2(12years).png",height = 6,width = 6,units = 'in',res=300)
ggplot(data = na.omit(dat_bergplot[,c("homa_sens_12","homa_beta_12","class")]), 
              aes(x = homa_sens_12,  y = homa_beta_12, color = class)) + 
  geom_point(shape=1,size=2)+scale_color_manual(values=c('chartreuse4','red'))+
  geom_smooth(aes(group=class,color=class))+ggtitle("Bergman plot (12 years)")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+
  xlab("Insulin Sensitivity")+ylab("Insulin Secretion")+
  #theme(legend.position="bottom")
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )
  dev.off() 
############## 6 years ##############
plot3<-
png("Figure3 (6 years).png",height = 6,width = 6,units = 'in',res=300)
ggplot(data = na.omit(dat_bergplot[,c("homa_sens_6","homa_beta_6","class")]), 
              aes(x = homa_sens_6,  y = homa_beta_6, color = class)) +
  geom_point(shape=1,size=2)+scale_color_manual(values=c('chartreuse4','red'))+
  geom_smooth(aes(group=class,color=class),alpha=0.2)+ggtitle("Bergman_plot(6 years)")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+
  xlab("Insulin Sensitivity")+ylab("Insulin Secretion")+
  #theme(legend.position="none")
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
dev.off() 

##################################


####################### 18years xlim ylim#################
ggplot(data = dat_bergplot[complete.cases(dat_bergplot),], aes(x = homa_sens_18,  y = homa_beta_18, color = class)) +
  geom_point(shape=1,size=2)+scale_color_manual(values=c('chartreuse4','red'))+
  geom_smooth(aes(group=class,color=class),alpha=0.15)+ggtitle("Bergman_plot(18years)")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+
  xlab("Homa_sensitivity")+ylab("Homa Beta")+xlim(c(0,200))+ylim(c(0,200))
############### 18years gender wise ###
plot1<-ggplot(data = dat_bergplot[complete.cases(dat_bergplot),], 
              aes(x = homa_sens_18,  y = homa_beta_18, color = class)) +facet_grid(.~sex)+
  geom_point(shape=1,size=2)+scale_color_manual(values=c('chartreuse4','red'))+
  geom_smooth(aes(group=class),span=1)+ggtitle("Bergman_plot(18years)")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+
  xlab("Homa_sensitivity")+ylab("Homa Beta")+
  theme(legend.position="none")+xlim(c(0,200))+ylim(c(0,200))
################ 12 years##############
plot2<-ggplot(data = dat_bergplot[complete.cases(dat_bergplot),], aes(x = homa_sens_12,  y = homa_beta_12, color = class)) + 
  geom_point(shape=1,size=2)+scale_color_manual(values=c('chartreuse4','red'))+
  geom_smooth(aes(group=class))+ggtitle("Bergman_plot(12 years)")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+
  xlab("Homa_sensitivity")+ylab("Homa Beta")+theme(legend.position="none")
################12 years gender wise ###########
ggplot(data = dat_bergplot[complete.cases(dat_bergplot),], aes(x = homa_sens_12,  y = homa_beta_12, color = class)) + facet_grid(sex~.)+
  geom_point()+scale_color_manual(values=c('chartreuse4','red'))+
  geom_smooth(aes(group=class),color="black")+ggtitle("Bergman_plot(12 years)")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+xlab("Homa_sensitivity")+ylab("Homa Beta")+theme(legend.position="none")
################ 6 years #############
plot3<-ggplot(data = dat_bergplot[complete.cases(dat_bergplot),], aes(x = homa_sens_6,  y = homa_beta_6, color = class)) +
  geom_point()+scale_color_manual(values=c('chartreuse4','red'))+
  geom_smooth(aes(group=class),color="black")+ggtitle("Bergman_plot(6 years)")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+xlab("Homa_sensitivity")+ylab("Homa Beta")+theme(legend.position="bottom")
################ 6 years gender wise ##############
ggplot(data = dat_bergplot[complete.cases(dat_bergplot),], aes(x = homa_sens_6,  y = homa_beta_6, color = class)) +facet_grid(sex~.)+
  geom_point()+scale_color_manual(values=c('chartreuse4','red'))+
  geom_smooth(aes(group=class),color="black")+ggtitle("Bergman_plot(6 years)")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+xlab("Homa_sensitivity")+ylab("Homa Beta")
################################################
require(gridExtra)
png("BergmanPlotSmooth6_12_18years.png", width = 10, height = 8, units = 'in', res = 300)
grid.arrange(plot1, plot2, plot3,ncol=3)
dev.off()
################################# get legend ########################
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

