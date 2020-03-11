# Table 2a,2b and supplementary 3a,3b
# medicine wise table - gest wise and med wise
library(tibble)
library(readxl)

########## Dose #########
rm(list=ls())
dose1 = read_excel('Site1_Neofax_Training.xlsx',sheet = 1)
dose = rbind(dose1, read_excel('Site2_Neofax_Training.xlsx',sheet = 1))
dose = dose[-1,] #row not needed
dose = dose[-(nrow(dose1)),] #removed first blank column which was generated while appending second cehtre's baseline_data
write.csv(dose, 'freq.csv')
dose = read.csv('freq.csv')

dose = add_column(dose, GESTATION = (dose$Gestation.Weeks + dose$Gestation.Days/7), .after = 4)
dose$GESTATION = as.character(dose$GESTATION)

#removing outliers
outliers = which(dose$GESTATION<26)
dose = dose[-(outliers),]

gestation_cat1 = dose[which(dose$GESTATION < 32),]
gestation_cat2 = dose[which(dose$GESTATION >= 32 & dose$GESTATION <= 34),]
gestation_cat3 = dose[which(dose$GESTATION > 34 & dose$GESTATION <= 37),]
gestation_cat4 = dose[which(dose$GESTATION > 37),]
data_name = c('gestation_cat1', 'gestation_cat2', 'gestation_cat3', 'gestation_cat4')

column_index = grep('Medicine', colnames(dose))
antibiotics = c()


for (i in 1:250) {
  antibiotics = c(antibiotics, unique(as.character(dose[,column_index[i]])))
}
antibiotics = unique(antibiotics)
antibiotics = antibiotics[!is.na(antibiotics)]
temp_table = c()
allcolsofgest = c()

for (gest_category in 1:length(data_name)){
  temp_table =c()
  temp_data = get(data_name[gest_category])
  dose_error = 0
  no_dose_error = 0
  total_doses = 0
  patients_with_error = 0
  patients_without_error = 0
  dose_error_percentage = 0
  
  for (i in 1:length(antibiotics)) {
    dose_error[i] = 0
    no_dose_error[i] = 0
    total_doses[i] = 0
    patients_with_error[i] = 0
    patients_without_error[i] = 0
    for (j in 1:250) {
      error = 0
      no_error = 0
      patients = 0
      patients_no_error = 0
      filter_data = temp_data[which(temp_data[,column_index[j]] == antibiotics[i]),]
      error = sum(filter_data[which(filter_data[,column_index[j]+5] == 'Deviation'),column_index[j]+7])
      no_error = sum(filter_data[which(filter_data[,column_index[j]+5] == 'No Error'),column_index[j]+7])
      patients = nrow(filter_data[which(filter_data[,column_index[j]+5] == 'Deviation'),])
      patients_no_error = nrow(filter_data[which(filter_data[,column_index[j]+5] == 'No Error'),])
      dose_error[i] = dose_error[i] + error
      no_dose_error[i] = no_dose_error[i] + no_error
      total_doses[i] = dose_error[i] + no_dose_error[i]
      patients_with_error[i] = patients_with_error[i] + patients 
      patients_without_error[i] = patients_without_error[i] + patients_no_error
      dose_error_percentage[i] = round(((dose_error[i] / (dose_error[i] + no_dose_error[i]))*100),digits=2)
    }
        something = cbind(dose_error[i], total_doses[i])
        temp_table = rbind(temp_table, something)
      }
      allcolsofgest = cbind(allcolsofgest, temp_table)
}

# temp_result = cbind(antibiotics, dose_error, no_dose_error, dose_error_percentage, patients_with_error, patients_without_error)
# dose_result = cbind(antibiotics, dose_error, total_doses)

tabletemp1 = cbind(antibiotics, allcolsofgest)
tabletemp2 = c("", "Dose deviations", "Total doses given", "Dose deviations", "Total doses given", "Dose deviations", "Total doses given", "Dose deviations", "Total doses given")
tabletemp3 = c("Gestations", "<32", "<32", "32-34","32-34", "34-37", "34-37", ">37", ">37")

final_table_med = rbind(tabletemp3, tabletemp2, tabletemp1)
view(final_table_med)


#### Frequency ####
rm(list=ls())
freq1 = read_excel('Site1_Neofax_Training.xlsx',sheet = 2)
freq = rbind(freq1, read_excel('Site2_Neofax_Training.xlsx',sheet = 2))
freq = freq[-1,] #row not needed
freq = freq[-(nrow(freq1)),] #removed first blank column which was generated while appending second cehtre's baseline_data
write.csv(freq, 'freq.csv')
freq = read.csv('freq.csv')

freq = add_column(freq, GESTATION = (freq$Gestation.Weeks + freq$Gestation.Days/7), .after = 4)
freq$GESTATION = as.character(freq$GESTATION)

#removing outliers
outliers = which(freq$GESTATION<26)
freq = freq[-(outliers),]

gestation_cat1 = freq[which(freq$GESTATION < 32),]
gestation_cat2 = freq[which(freq$GESTATION >= 32 & freq$GESTATION <= 34),]
gestation_cat3 = freq[which(freq$GESTATION > 34 & freq$GESTATION <= 37),]
gestation_cat4 = freq[which(freq$GESTATION > 37),]
data_name = c('gestation_cat1', 'gestation_cat2', 'gestation_cat3', 'gestation_cat4')

column_index = grep('Medicine', colnames(freq))
antibiotics = c()

for (i in 1:250) {
  antibiotics = c(antibiotics, unique(as.character(freq[,column_index[i]])))
}
antibiotics = unique(antibiotics)
antibiotics = antibiotics[!is.na(antibiotics)]
temp_table = c()
allcolsofgest = c()


for (gest_category in 1:length(data_name)){
  temp_table =c()
  temp_data = get(data_name[gest_category])
  freq_error = 0
  no_freq_error = 0
  total_freqs = 0
  patients_with_error = 0
  patients_without_error = 0
  freq_error_percentage = 0
  
  for (i in 1:length(antibiotics)) {
    freq_error[i] = 0
    no_freq_error[i] = 0
    total_freqs[i] = 0
    patients_with_error[i] = 0
    patients_without_error[i] = 0
    for (j in 1:250) {
      error = 0
      no_error = 0
      patients = 0
      patients_no_error = 0
      filter_data = temp_data[which(temp_data[,column_index[j]] == antibiotics[i]),]
      error = sum(filter_data[which(filter_data[,column_index[j]+5] == 'Deviation'),column_index[j]+7])
      no_error = sum(filter_data[which(filter_data[,column_index[j]+5] == 'No Error'),column_index[j]+7])
      patients = nrow(filter_data[which(filter_data[,column_index[j]+5] == 'Deviation'),])
      patients_no_error = nrow(filter_data[which(filter_data[,column_index[j]+5] == 'No Error'),])
      
      freq_error[i] = freq_error[i] + error
      no_freq_error[i] = no_freq_error[i] + no_error
      total_freqs[i] = freq_error[i] + no_freq_error[i]
      patients_with_error[i] = patients_with_error[i] + patients 
      patients_without_error[i] = patients_without_error[i] + patients_no_error
      freq_error_percentage[i] = round(((freq_error[i] / (freq_error[i] + no_freq_error[i]))*100),digits=2)
    }
    something = cbind(freq_error[i], total_freqs[i])
    temp_table = rbind(temp_table, something)
  }
  allcolsofgest = cbind(allcolsofgest, temp_table)
}

# temp_result = cbind(antibiotics, freq_error, no_freq_error, freq_error_percentage, patients_with_error, patients_without_error)
# freq_result = cbind(antibiotics, freq_error, total_freqs)

tabletemp1 = cbind(antibiotics, allcolsofgest)
tabletemp2 = c("", "freq deviations", "Total freqs given", "freq deviations", "Total freqs given", "freq deviations", "Total freqs given", "freq deviations", "Total freqs given")
tabletemp3 = c("Gestations", "<32", "<32", "32-34","32-34", "34-37", "34-37", ">37", ">37")

final_table_med = rbind(tabletemp3, tabletemp2, tabletemp1)
view(final_table_med)
