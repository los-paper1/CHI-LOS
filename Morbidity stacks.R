library(tibble)
library(readxl)
library(ggplot2)


morb1 = read_excel('Site1_Neofax_Training.xlsx',sheet = 1)
morb = rbind(morb1, read_excel('Site2_Neofax_Training.xlsx',sheet = 1))
morb = morb[-1,] #row not needed
morb = morb[-(nrow(morb1)),] #removed first blank column which was generated while appending second cehtre's baseline_data
morb = morb[,1:49]
#GESTATION
morb = add_column(morb, GESTATION = (morb$Gestation.Weeks + morb$Gestation.Days/7), .after = 4)
#removing outliers
outliers = which(morb$GESTATION<26)
morb = morb[-(outliers),]


morb[,34:48] <- as.character(unlist(morb[,34:48]))

gest_lessthan_32 = morb[which(morb$GESTATION < 32),]
gest_32_34 = morb[which(morb$GESTATION >= 32 & morb$GESTATION <= 34),]
gest_34_37 = morb[which(morb$GESTATION > 34 & morb$GESTATION <= 37),]
gest_morethan37 = morb[which(morb$GESTATION > 37),]

gest_name = c('gest_lessthan_32', 'gest_32_34', 'gest_34_37', 'gest_morethan37')

### <32 bar plot ####
i=1 #<32 gestation
final_clubbed= c()
for (j in 1:1){
  temp_morb = get(gest_name[i])
  
  sepsis = temp_morb[which(temp_morb$Sepsis == 'TRUE'),]
  sepsis_mean = mean(sepsis$LOS)
  sepsis_count = nrow(sepsis)
  
  Jaundice = temp_morb[which(temp_morb$Jaundice == 'TRUE'),]
  jaundice_mean = mean(Jaundice$LOS)
  jaundice_count = nrow(Jaundice)
  
  rds = temp_morb[which(temp_morb$RDS == 'TRUE'),]
  rds_mean = mean(rds$LOS)
  rds_count = nrow(rds)
  
  ttnb = temp_morb[which(temp_morb$RDS_TTNB == 'TRUE'),]
  ttnb_mean = mean(ttnb$LOS)
  ttnb_count = nrow(ttnb)
  
  mas = temp_morb[which(temp_morb$RDS_MAS == 'TRUE'),]
  mas_mean = mean(mas$LOS)
  mas_count = nrow(mas)
  
  invasive_rds = temp_morb[which(temp_morb$isInvasive == 'TRUE'),]
  invasive_rds_mean = mean(invasive_rds$LOS)
  invasive_rds_count = nrow(invasive_rds)
  
  Pneumothorax = temp_morb[which(temp_morb$Pneumothorax == 'TRUE'),]
  pneumo_mean = mean(Pneumothorax$LOS)
  pneumo_count = nrow(Pneumothorax)
  
  PPHN = temp_morb[which(temp_morb$PPHN == 'TRUE'),]
  pphn_mean = mean(PPHN$LOS)
  pphn_count = nrow(PPHN)
  
  Asphyxia = temp_morb[which(temp_morb$Asphyxia == 'TRUE'),]
  asphyxia_mean = mean(Asphyxia$LOS)
  asphyxia_count = nrow(Asphyxia)
  
  Gestation = rep(gest_name[i], 9)
  Label = c('Sepsis', 'Jaundice', 'RDS', 'TTNB', 'MAS','Invasive RDS', 'Pneumothorax', 'PPHN', 'Asphyxia')
  LOS = c(sepsis_mean, jaundice_mean, rds_mean, ttnb_mean, mas_mean, invasive_rds_mean, pneumo_mean, pphn_mean, asphyxia_mean)
  Count = c(sepsis_count, jaundice_count, rds_count, ttnb_count, mas_count, invasive_rds_count, pneumo_count, pphn_count, asphyxia_count)
  clubbed = cbind(Gestation, Label, LOS, Count)
}

ggplot(data=as.data.frame(final_clubbed), aes(x=Label,y=LOS,fill=Label)) +
  geom_bar(position="dodge",stat="identity") +
  geom_text(aes(label=Count), position=position_dodge(width=0.9), vjust=-0.25, size=5) +
  ggtitle(gest_name[i]) +
  scale_fill_brewer( type = "seq", palette = 4, direction = 1,
                     aesthetics = "fill")+
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    text=element_text(family="serif",size=15,face = 'bold'),
    plot.title=element_text(face="bold",hjust=c(0,0))
  )



### 32-34 bar plot ####
i=2 #32-34 gestation
final_clubbed= c()
for (j in 1:1){
  temp_morb = get(gest_name[i])
  
  sepsis = temp_morb[which(temp_morb$Sepsis == 'TRUE'),]
  sepsis_mean = mean(sepsis$LOS)
  sepsis_count = nrow(sepsis)
  
  Jaundice = temp_morb[which(temp_morb$Jaundice == 'TRUE'),]
  jaundice_mean = mean(Jaundice$LOS)
  jaundice_count = nrow(Jaundice)
  
  rds = temp_morb[which(temp_morb$RDS == 'TRUE'),]
  rds_mean = mean(rds$LOS)
  rds_count = nrow(rds)
  
  ttnb = temp_morb[which(temp_morb$RDS_TTNB == 'TRUE'),]
  ttnb_mean = mean(ttnb$LOS)
  ttnb_count = nrow(ttnb)
  
  mas = temp_morb[which(temp_morb$RDS_MAS == 'TRUE'),]
  mas_mean = mean(mas$LOS)
  mas_count = nrow(mas)
  
  invasive_rds = temp_morb[which(temp_morb$isInvasive == 'TRUE'),]
  invasive_rds_mean = mean(invasive_rds$LOS)
  invasive_rds_count = nrow(invasive_rds)
  
  Pneumothorax = temp_morb[which(temp_morb$Pneumothorax == 'TRUE'),]
  pneumo_mean = mean(Pneumothorax$LOS)
  pneumo_count = nrow(Pneumothorax)
  
  PPHN = temp_morb[which(temp_morb$PPHN == 'TRUE'),]
  pphn_mean = mean(PPHN$LOS)
  pphn_count = nrow(PPHN)
  
  Asphyxia = temp_morb[which(temp_morb$Asphyxia == 'TRUE'),]
  asphyxia_mean = mean(Asphyxia$LOS)
  asphyxia_count = nrow(Asphyxia)
  
  Gestation = rep(gest_name[i], 9)
  Label = c('Sepsis', 'Jaundice', 'RDS', 'TTNB', 'MAS','Invasive RDS', 'Pneumothorax', 'PPHN', 'Asphyxia')
  LOS = c(sepsis_mean, jaundice_mean, rds_mean, ttnb_mean, mas_mean, invasive_rds_mean, pneumo_mean, pphn_mean, asphyxia_mean)
  Count = c(sepsis_count, jaundice_count, rds_count, ttnb_count, mas_count, invasive_rds_count, pneumo_count, pphn_count, asphyxia_count)
  clubbed = cbind(Gestation, Label, LOS, Count)
}



ggplot(data=as.data.frame(final_clubbed), aes(x=Label,y=LOS,fill=Label)) +
  geom_bar(position="dodge",stat="identity") +
  geom_text(aes(label=Count), position=position_dodge(width=0.9), vjust=-0.25, size=5) +
  ggtitle(gest_name[i]) +
  scale_fill_brewer( type = "seq", palette = 4, direction = 1,
                     aesthetics = "fill")+
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    text=element_text(family="serif",size=15,face = 'bold'),
    plot.title=element_text(face="bold",hjust=c(0,0))
  )+ ylim(0,80)


### 34-37 bar plot ####
i=3 #34-37 gestation
final_clubbed= c()
for (j in 1:1){
  temp_morb = get(gest_name[i])
  
  sepsis = temp_morb[which(temp_morb$Sepsis == 'TRUE'),]
  sepsis_mean = mean(sepsis$LOS)
  sepsis_count = nrow(sepsis)
  
  Jaundice = temp_morb[which(temp_morb$Jaundice == 'TRUE'),]
  jaundice_mean = mean(Jaundice$LOS)
  jaundice_count = nrow(Jaundice)
  
  rds = temp_morb[which(temp_morb$RDS == 'TRUE'),]
  rds_mean = mean(rds$LOS)
  rds_count = nrow(rds)
  
  ttnb = temp_morb[which(temp_morb$RDS_TTNB == 'TRUE'),]
  ttnb_mean = mean(ttnb$LOS)
  ttnb_count = nrow(ttnb)
  
  mas = temp_morb[which(temp_morb$RDS_MAS == 'TRUE'),]
  mas_mean = mean(mas$LOS)
  mas_count = nrow(mas)
  
  invasive_rds = temp_morb[which(temp_morb$isInvasive == 'TRUE'),]
  invasive_rds_mean = mean(invasive_rds$LOS)
  invasive_rds_count = nrow(invasive_rds)
  
  Pneumothorax = temp_morb[which(temp_morb$Pneumothorax == 'TRUE'),]
  pneumo_mean = mean(Pneumothorax$LOS)
  pneumo_count = nrow(Pneumothorax)
  
  PPHN = temp_morb[which(temp_morb$PPHN == 'TRUE'),]
  pphn_mean = mean(PPHN$LOS)
  pphn_count = nrow(PPHN)
  
  Asphyxia = temp_morb[which(temp_morb$Asphyxia == 'TRUE'),]
  asphyxia_mean = mean(Asphyxia$LOS)
  asphyxia_count = nrow(Asphyxia)
  
  Gestation = rep(gest_name[i], 9)
  Label = c('Sepsis', 'Jaundice', 'RDS', 'TTNB', 'MAS','Invasive RDS', 'Pneumothorax', 'PPHN', 'Asphyxia')
  LOS = c(sepsis_mean, jaundice_mean, rds_mean, ttnb_mean, mas_mean, invasive_rds_mean, pneumo_mean, pphn_mean, asphyxia_mean)
  Count = c(sepsis_count, jaundice_count, rds_count, ttnb_count, mas_count, invasive_rds_count, pneumo_count, pphn_count, asphyxia_count)
  clubbed = cbind(Gestation, Label, LOS, Count)
}

ggplot(data=as.data.frame(final_clubbed), aes(x=Label,y=LOS,fill=Label)) +
  geom_bar(position="dodge",stat="identity") +
  geom_text(aes(label=Count), position=position_dodge(width=0.9), vjust=-0.25, size=5) +
  ggtitle(gest_name[i]) +
  scale_fill_brewer( type = "seq", palette = 4, direction = 1,
                     aesthetics = "fill")+
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    text=element_text(family="serif",size=15,face = 'bold'),
    plot.title=element_text(face="bold",hjust=c(0,0))
  )+ ylim(0,80)


### >37 bar plot ####
i=4 #>37 gestation
final_clubbed= c()
for (j in 1:1){
  temp_morb = get(gest_name[i])
  
  sepsis = temp_morb[which(temp_morb$Sepsis == 'TRUE'),]
  sepsis_mean = mean(sepsis$LOS)
  sepsis_count = nrow(sepsis)
  
  Jaundice = temp_morb[which(temp_morb$Jaundice == 'TRUE'),]
  jaundice_mean = mean(Jaundice$LOS)
  jaundice_count = nrow(Jaundice)
  
  rds = temp_morb[which(temp_morb$RDS == 'TRUE'),]
  rds_mean = mean(rds$LOS)
  rds_count = nrow(rds)
  
  ttnb = temp_morb[which(temp_morb$RDS_TTNB == 'TRUE'),]
  ttnb_mean = mean(ttnb$LOS)
  ttnb_count = nrow(ttnb)
  
  mas = temp_morb[which(temp_morb$RDS_MAS == 'TRUE'),]
  mas_mean = mean(mas$LOS)
  mas_count = nrow(mas)
  
  invasive_rds = temp_morb[which(temp_morb$isInvasive == 'TRUE'),]
  invasive_rds_mean = mean(invasive_rds$LOS)
  invasive_rds_count = nrow(invasive_rds)
  
  Pneumothorax = temp_morb[which(temp_morb$Pneumothorax == 'TRUE'),]
  pneumo_mean = mean(Pneumothorax$LOS)
  pneumo_count = nrow(Pneumothorax)
  
  PPHN = temp_morb[which(temp_morb$PPHN == 'TRUE'),]
  pphn_mean = mean(PPHN$LOS)
  pphn_count = nrow(PPHN)
  
  Asphyxia = temp_morb[which(temp_morb$Asphyxia == 'TRUE'),]
  asphyxia_mean = mean(Asphyxia$LOS)
  asphyxia_count = nrow(Asphyxia)
  
  Gestation = rep(gest_name[i], 9)
  Label = c('Sepsis', 'Jaundice', 'RDS', 'TTNB', 'MAS','Invasive RDS', 'Pneumothorax', 'PPHN', 'Asphyxia')
  LOS = c(sepsis_mean, jaundice_mean, rds_mean, ttnb_mean, mas_mean, invasive_rds_mean, pneumo_mean, pphn_mean, asphyxia_mean)
  Count = c(sepsis_count, jaundice_count, rds_count, ttnb_count, mas_count, invasive_rds_count, pneumo_count, pphn_count, asphyxia_count)
  clubbed = cbind(Gestation, Label, LOS, Count)
}

ggplot(data=as.data.frame(final_clubbed), aes(x=Label,y=LOS,fill=Label)) +
  geom_bar(position="dodge",stat="identity") +
  geom_text(aes(label=Count), position=position_dodge(width=0.9), vjust=-0.25, size=5) +
  ggtitle(gest_name[i]) +
  scale_fill_brewer( type = "seq", palette = 4, direction = 1,
                     aesthetics = "fill")+
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    text=element_text(family="serif",size=15,face = 'bold'),
    plot.title=element_text(face="bold",hjust=c(0,0))
  )+ ylim(0,80)
