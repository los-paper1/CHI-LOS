#Supplementary figures 4a, 4b, 4c, 4d

library(readxl)
library(tibble)
rm(list=ls())

#baseline_data prep
baseline_data = read_excel('Site1_Nutrition_Training.xlsx',sheet = 1)
baseline_data_all = rbind(baseline_data, read_excel('Site2_Nutrition_Training.xlsx',sheet = 1))
baseline_data_all = baseline_data_all[-1,] #row not needed
baseline_data_all = baseline_data_all[-(nrow(baseline_data)),] #removed first blank column which was generated while appending second cehtre's baseline_data
baseline_only_data = baseline_data_all[,1:55] #selected only baseline columns

#GESTATION
baseline_data_all = add_column(baseline_data_all, GESTATION = (baseline_data_all$Gestation.Weeks + baseline_data_all$Gestation.Days/7), .after = 4)
outliers = which(baseline_data_all$GESTATION<26)
baseline_data_all = baseline_data_all[-(outliers),]

gestation_cat1_1 = baseline_data_all[which(baseline_data_all$GESTATION < 32),]
gestation_cat2_1 = baseline_data_all[which(baseline_data_all$GESTATION >= 32 & baseline_data_all$GESTATION <= 34),]
gestation_cat3_1 = baseline_data_all[which(baseline_data_all$GESTATION > 34 & baseline_data_all$GESTATION <= 37),]
gestation_cat4_1 = baseline_data_all[which(baseline_data_all$GESTATION > 37),]

for(j in 1:1){
  LOS=gestation_cat1_1$LOS
  h=hist(LOS,main="Distribution of LOS of <32 gestation")
  xfit<-seq(min(LOS),max(LOS),length=40) 
  yfit<-dnorm(xfit,mean=mean(LOS),sd=sd(LOS)) 
  yfit <- yfit*diff(h$mids[1:2])*length(LOS) 
  lines(xfit, yfit, lwd=2)
}

for(j in 1:1){
  LOS=gestation_cat2_1$LOS
  h=hist(LOS,main="Distribution of LOS of 32-34 gestation")
  xfit<-seq(min(LOS),max(LOS),length=40) 
  yfit<-dnorm(xfit,mean=mean(LOS),sd=sd(LOS)) 
  yfit <- yfit*diff(h$mids[1:2])*length(LOS) 
  lines(xfit, yfit, lwd=2)
}

for(j in 1:1){
  LOS=gestation_cat3_1$LOS
  h=hist(LOS,main="Distribution of LOS of 34-37 gestation")
  xfit<-seq(min(LOS),max(LOS),length=40) 
  yfit<-dnorm(xfit,mean=mean(LOS),sd=sd(LOS)) 
  yfit <- yfit*diff(h$mids[1:2])*length(LOS) 
  lines(xfit, yfit, lwd=2)
}

for(j in 1:1){
  LOS=gestation_cat4_1$LOS
  h=hist(LOS,main="Distribution of LOS of >37 gestation")
  xfit<-seq(min(LOS),max(LOS),length=40) 
  yfit<-dnorm(xfit,mean=mean(LOS),sd=sd(LOS)) 
  yfit <- yfit*diff(h$mids[1:2])*length(LOS) 
  lines(xfit, yfit, lwd=2)
}
