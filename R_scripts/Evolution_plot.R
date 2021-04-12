png("InsulinSensitivitySecretionEvolution.png",height = 6,width=8,units = 'in',res=300)
ggplot(data = dat_bergplot_long, aes(x = homasens,  y = homabeta, color = class)) + 
  facet_grid(.~time)+
  geom_point(shape=1,size=2)+scale_color_manual(values=c('chartreuse4','red'))+
  geom_smooth()+
  #ggtitle("Bergman_plot(12 years)")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+
  xlab("Insulin Sensitivity")+ylab("Insulin Secretion")+theme(legend.position="none")+
theme(
  legend.position = c(.6, .99),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)
dev.off()
