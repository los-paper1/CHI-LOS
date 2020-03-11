library(xlsx)
library(ggplot2)

### Dosage ####
rm(list=ls())
med = read_excel('Site1_Neofax_Training.xlsx',sheet = 1)
dose_raw = rbind(med, read_excel('Site2_Neofax_Training.xlsx',sheet = 1))
dose_raw = dose_raw[-1,] #row not needed
dose_raw = dose_raw[-(nrow(med)),] #removed first blank column which was generated while appending second cehtre's med
#GESTATION
dose_raw = add_column(dose_raw, GESTATION = (dose_raw$Gestation.Weeks + dose_raw$Gestation.Days/7), .after = 4)
dose_raw$Birth.Weight = as.integer(dose_raw$Birth.Weight)
#birth weight wrong entries handled
for (i in 1:length(dose_raw$Birth.Weight)){
  if (dose_raw$Birth.Weight[i]<5){
    dose_raw$Birth.Weight[i] = dose_raw$Birth.Weight[i]*1000
  }
  if (dose_raw$Birth.Weight[i]<50 & dose_raw$Birth.Weight[i]>=5){
    dose_raw$Birth.Weight[i] = dose_raw$Birth.Weight[i]*100
  }
}

outliers = which(dose_raw$GESTATION<26)
dose_raw = dose_raw[-(outliers),]
dose_raw$UHID = as.character(dose_raw$UHID)

#changing "Recommended" col name to "NeoFax recom"
names(dose_raw)[names(dose_raw) == "Recommended...50"] <- "NeoFax recom"
count=50
for (j in 1:ncol(dose_raw)){
  z= paste("Recommended", "...", count, sep = "", collapse = NULL)
  if(z %in% colnames(dose_raw))
  {
    names(dose_raw)[names(dose_raw) == z] <- "NeoFax recom"
  }
  count=count+9
}

dose_to_be_altered = dose_raw
dose_to_be_altered = add_column(dose_to_be_altered, dose_final_verdict = "", .after = 1)

dose_raw$GESTATION = as.numeric(dose_raw$GESTATION)
gestation_med_cat1 = dose_raw[which(dose_raw$GESTATION < 32),]
gestation_med_cat2 = dose_raw[which(dose_raw$GESTATION >= 32 & dose_raw$GESTATION <= 34),]
gestation_med_cat3 = dose_raw[which(dose_raw$GESTATION > 34 & dose_raw$GESTATION <= 37),]
gestation_med_cat4 = dose_raw[which(dose_raw$GESTATION > 37),]

med_name = c('gestation_med_cat1', 'gestation_med_cat2', 'gestation_med_cat3', 'gestation_med_cat4')

for (ges_med in 1:length(med_name)){
  dose = get(med_name[ges_med])
  diff_index = grep('Diff', colnames(dose))
  #Changed class to numeric!
  dose[,diff_index] <- as.numeric(as.character(unlist(dose[,diff_index])))
  
  numerator = 0
  given_value = as.numeric(unlist(dose[,diff_index[1]]))
  dummy = given_value
  numerator = numerator + dummy
  for (i in 2:250) {
    #unlist so that "values" is generated and not "med file"
    given_value = as.numeric(unlist(dose[,diff_index[i]]))
    dummy = given_value
    dummy[which(is.na(dummy))] = 0
    numerator = numerator + dummy
  }
  
  # med not given #
  temp_med = dose[which(is.na(numerator)),]
  
  if (nrow(temp_med) > 0){
    for (j in 1:nrow(dose_to_be_altered)){
      for (i in 1:length(temp_med$UHID)){
        if(temp_med$UHID[i] == dose_to_be_altered$UHID[j])  {
          dose_to_be_altered$dose_final_verdict[j] = "Medication not required"
        }
      }
    }
  }
  
  # no deviation #
  temp_med = dose[which(numerator == 0),]
  
  if (nrow(temp_med) > 0){
    for (j in 1:nrow(dose_to_be_altered)){
      for (i in 1:length(temp_med$UHID)){
        if(temp_med$UHID[i] == dose_to_be_altered$UHID[j])  {
          dose_to_be_altered$dose_final_verdict[j] = "No deviation"
        }
      }
    }
  }
  
  # negative #
  temp_med = dose[which(numerator < 0),]
  
  if (nrow(temp_med) > 0){
    for (j in 1:nrow(dose_to_be_altered)){
      for (i in 1:length(temp_med$UHID)){
        if(temp_med$UHID[i] == dose_to_be_altered$UHID[j])  {
          dose_to_be_altered$dose_final_verdict[j] = "Deviation"
        }
      }
    }
  }
  
  # positive #
  temp_med = dose[which(numerator > 0),]
  
  if (nrow(temp_med) > 0){
    for (j in 1:nrow(dose_to_be_altered)){
      for (i in 1:length(temp_med$UHID)){
        if(temp_med$UHID[i] == dose_to_be_altered$UHID[j])  {
          dose_to_be_altered$dose_final_verdict[j] = "Deviation"
        }
      }
    }
  }
}  

medication_stack = dose_to_be_altered[,1:10]
a = medication_stack[which(medication_stack$GESTATION < 32),]
b = medication_stack[which(medication_stack$GESTATION >= 32 & medication_stack$GESTATION <= 34),]
c = medication_stack[which(medication_stack$GESTATION > 34 & medication_stack$GESTATION <= 37),]
d = medication_stack[which(medication_stack$GESTATION > 37),]

gest_name = c('a', 'b', 'c', 'd')

final_clubbed= c()
for (i in 1:length(gest_name)){
  temp_med = get(gest_name[i])
  
  med_not_given = temp_med[which(temp_med$dose_final_verdict == 'Medication not required'),]
  med_not_given_mean = mean(med_not_given$LOS)
  med_not_given_count = nrow(med_not_given)
  
  no_deviation = temp_med[which(temp_med$dose_final_verdict == 'No deviation'),]
  no_deviation_mean = mean(no_deviation$LOS)
  no_deviation_count = nrow(no_deviation)
  
  neg_dev = temp_med[which(temp_med$dose_final_verdict == 'Deviation'),]
  neg_dev_mean = mean(neg_dev$LOS)
  neg_dev_count = nrow(neg_dev)
 
  Gestation = rep(gest_name[i], 4)
  Label = c('Medication not required', 'No deviation', 'Deviation')
  LOS = c(med_not_given_mean, no_deviation_mean, neg_dev_mean)
  Count = c(med_not_given_count, no_deviation_count, neg_dev_count)
  clubbed = cbind(Gestation, Label, LOS, Count)
  final_clubbed = rbind(final_clubbed, clubbed)
}
data = as.data.frame(final_clubbed)
data[,1:2] = as.character(unlist(data[,1:2]))
data[,3:4] = as.numeric(as.character(unlist(data[,3:4])))

tiff("Dosage_Medication.tiff", units="in", width=15, height=7, res=300)
# insert ggplot code
ggplot(data=data, aes(x=Gestation,y=LOS,fill=Label)) +
  geom_bar(position="dodge",stat="identity") +
  geom_text(aes(label=Count), position=position_dodge(width=0.9), vjust=-0.25, size=5) +
  ggtitle('Medication Dosage stats') +
  scale_fill_brewer( type = "seq", palette = 4, direction = 1,
                     aesthetics = "fill")+
  scale_x_discrete(
    labels=c("31.9 and less", "32-34", "34-37", "37.1 and more")
  )  +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    text=element_text(family="serif",size=20,face = 'bold'),
    plot.title=element_text(face="bold",hjust=c(0,0))
  )

dev.off()

### Frequency ####
rm(list=ls())
med = read_excel('Site1_Neofax_Training.xlsx',sheet = 2)
freq_raw = rbind(med, read_excel('Site2_Neofax_Training.xlsx',sheet = 2))
freq_raw = freq_raw[-1,] #row not needed
freq_raw = freq_raw[-(nrow(med)),] #removed first blank column which was generated while appending second cehtre's med
#GESTATION
freq_raw = add_column(freq_raw, GESTATION = (freq_raw$Gestation.Weeks + freq_raw$Gestation.Days/7), .after = 4)
freq_raw$Birth.Weight = as.integer(freq_raw$Birth.Weight)
#birth weight wrong entries handled
for (i in 1:length(freq_raw$Birth.Weight)){
  if (freq_raw$Birth.Weight[i]<5){
    freq_raw$Birth.Weight[i] = freq_raw$Birth.Weight[i]*1000
  }
  if (freq_raw$Birth.Weight[i]<50 & freq_raw$Birth.Weight[i]>=5){
    freq_raw$Birth.Weight[i] = freq_raw$Birth.Weight[i]*100
  }
}

outliers = which(freq_raw$GESTATION<26)
freq_raw = freq_raw[-(outliers),]
freq_raw$UHID = as.character(freq_raw$UHID)

#changing "Recommended" col name to "NeoFax recom"
names(freq_raw)[names(freq_raw) == "Recommended...50"] <- "NeoFax recom"
count=50
for (j in 1:ncol(freq_raw)){
  z= paste("Recommended", "...", count, sep = "", collapse = NULL)
  if(z %in% colnames(freq_raw))
  {
    names(freq_raw)[names(freq_raw) == z] <- "NeoFax recom"
  }
  count=count+9
}

freq_to_be_altered = freq_raw
freq_to_be_altered = add_column(freq_to_be_altered, freq_final_verdict = "", .after = 1)

freq_raw$GESTATION = as.numeric(freq_raw$GESTATION)
gestation_med_cat1 = freq_raw[which(freq_raw$GESTATION < 32),]
gestation_med_cat2 = freq_raw[which(freq_raw$GESTATION >= 32 & freq_raw$GESTATION <= 34),]
gestation_med_cat3 = freq_raw[which(freq_raw$GESTATION > 34 & freq_raw$GESTATION <= 37),]
gestation_med_cat4 = freq_raw[which(freq_raw$GESTATION > 37),]

med_name = c('gestation_med_cat1', 'gestation_med_cat2', 'gestation_med_cat3', 'gestation_med_cat4')

for (ges_med in 1:length(med_name)){
  freq = get(med_name[ges_med])
  diff_index = grep('Diff', colnames(freq))
  #Changed class to numeric!
  freq[,diff_index] <- as.numeric(as.character(unlist(freq[,diff_index])))
  
  numerator = 0
  given_value = as.numeric(unlist(freq[,diff_index[1]]))
  dummy = given_value
  numerator = numerator + dummy
  for (i in 2:250) {
    #unlist so that "values" is generated and not "med file"
    given_value = as.numeric(unlist(freq[,diff_index[i]]))
    dummy = given_value
    dummy[which(is.na(dummy))] = 0
    numerator = numerator + dummy
  }
  
  # med not given #
  temp_med = freq[which(is.na(numerator)),]
  
  if (nrow(temp_med) > 0){
    for (j in 1:nrow(freq_to_be_altered)){
      for (i in 1:length(temp_med$UHID)){
        if(temp_med$UHID[i] == freq_to_be_altered$UHID[j])  {
          freq_to_be_altered$freq_final_verdict[j] = "Medication not required"
        }
      }
    }
  }
  
  # no deviation #
  temp_med = freq[which(numerator == 0),]
  
  if (nrow(temp_med) > 0){
    for (j in 1:nrow(freq_to_be_altered)){
      for (i in 1:length(temp_med$UHID)){
        if(temp_med$UHID[i] == freq_to_be_altered$UHID[j])  {
          freq_to_be_altered$freq_final_verdict[j] = "No deviation"
        }
      }
    }
  }
  
  # negative #
  temp_med = freq[which(numerator < 0),]
  
  if (nrow(temp_med) > 0){
    for (j in 1:nrow(freq_to_be_altered)){
      for (i in 1:length(temp_med$UHID)){
        if(temp_med$UHID[i] == freq_to_be_altered$UHID[j])  {
          freq_to_be_altered$freq_final_verdict[j] = "Deviation"
        }
      }
    }
  }
  
  # positive #
  temp_med = freq[which(numerator > 0),]
  
  if (nrow(temp_med) > 0){
    for (j in 1:nrow(freq_to_be_altered)){
      for (i in 1:length(temp_med$UHID)){
        if(temp_med$UHID[i] == freq_to_be_altered$UHID[j])  {
          freq_to_be_altered$freq_final_verdict[j] = "Deviation"
        }
      }
    }
  }
}  

medication_stack = freq_to_be_altered[,1:10]
a = medication_stack[which(medication_stack$GESTATION < 32),]
b = medication_stack[which(medication_stack$GESTATION >= 32 & medication_stack$GESTATION <= 34),]
c = medication_stack[which(medication_stack$GESTATION > 34 & medication_stack$GESTATION <= 37),]
d = medication_stack[which(medication_stack$GESTATION > 37),]

gest_name = c('a', 'b', 'c', 'd')

final_clubbed= c()
for (i in 1:length(gest_name)){
  temp_med = get(gest_name[i])
  
  med_not_given = temp_med[which(temp_med$freq_final_verdict == 'Medication not required'),]
  med_not_given_mean = mean(med_not_given$LOS)
  med_not_given_count = nrow(med_not_given)
  
  no_deviation = temp_med[which(temp_med$freq_final_verdict == 'No deviation'),]
  no_deviation_mean = mean(no_deviation$LOS)
  no_deviation_count = nrow(no_deviation)
  
  neg_dev = temp_med[which(temp_med$freq_final_verdict == 'Deviation'),]
  neg_dev_mean = mean(neg_dev$LOS)
  neg_dev_count = nrow(neg_dev)
  
  Gestation = rep(gest_name[i], 4)
  Label = c('Medication not required', 'No deviation', 'Deviation')
  LOS = c(med_not_given_mean, no_deviation_mean, neg_dev_mean)
  Count = c(med_not_given_count, no_deviation_count, neg_dev_count)
  clubbed = cbind(Gestation, Label, LOS, Count)
  final_clubbed = rbind(final_clubbed, clubbed)
}
data = as.data.frame(final_clubbed)
data[,1:2] = as.character(unlist(data[,1:2]))
data[,3:4] = as.numeric(as.character(unlist(data[,3:4])))

tiff("Frequency_Medication.tiff", units="in", width=15, height=7, res=300)
# insert ggplot code
ggplot(data=data, aes(x=Gestation,y=LOS,fill=Label)) +
  geom_bar(position="dodge",stat="identity") +
  geom_text(aes(label=Count), position=position_dodge(width=0.9), vjust=-0.25, size=5) +
  ggtitle('Medication frequency stats') +
  scale_fill_brewer( type = "seq", palette = 4, direction = 1,
                     aesthetics = "fill")+
  scale_x_discrete(
    labels=c("31.9 and less", "32-34", "34-37", "37.1 and more")
  )  +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    text=element_text(family="serif",size=20,face = 'bold'),
    plot.title=element_text(face="bold",hjust=c(0,0))
  )
dev.off()

