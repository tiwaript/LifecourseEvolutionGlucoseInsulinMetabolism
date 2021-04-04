# Packages Required 
require(foreign)
require(tidyr)
# data import 

dat_ihoma<-read.spss("./Data/PMNS PREDIABETES DATA_27JUNE2019.sav",
                     to.data.frame = T,use.value.labels = T)

dat_PMNS<-read.spss("./Data/pmns original.sav",to.data.frame = T,use.value.labels = T)


