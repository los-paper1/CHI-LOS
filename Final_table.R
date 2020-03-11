#install xquartz
# library(qpcR)
library(tibble)
library(lsmeans)
library(dplyr)
library(relimp)
library(rstudioapi)
library(readxl)
library(multcomp)
library(emmeans)

options(scipen=999) #switches off the scientific notations
rm(list=ls())

#data prep
data = read_excel('Site1_Nutrition_Training.xlsx',sheet = 1)
data_all = rbind(data, read_excel('Site2_Nutrition_Training.xlsx',sheet = 1))
data_all = data_all[-1,] #row not needed
data_all = data_all[-(nrow(data)),] #removed first blank column which was generated while appending second cehtre's data
data_all = data_all[,1:47] #selected only baseline columns

data_all$Birth.Weight = as.integer(data_all$Birth.Weight)
#birth weight wrong entries handled
for (i in 1:length(data_all$Birth.Weight)){
  if (data_all$Birth.Weight[i]<5){
    data_all$Birth.Weight[i] = data_all$Birth.Weight[i]*1000
  }
  if (data_all$Birth.Weight[i]<50 & data_all$Birth.Weight[i]>=5){
    data_all$Birth.Weight[i] = data_all$Birth.Weight[i]*100
  }
}

## Adding all columns that have to be put in model ##
#GESTATION
data_all = add_column(data_all, GESTATION = (data_all$Gestation.Weeks + data_all$Gestation.Days/7), .after = 4)

#Mode of delivery 
for (i in 1:nrow(data_all)){
  if(data_all$Mode.of.delivery[i] == "Vaccum" | data_all$Mode.of.delivery[i] == "Forceps"){
    data_all$Mode.of.delivery[i] = 'NVD'
  }
}

#Single/multiple
for (i in 1:nrow(data_all)){
  if(data_all$Single.Multiple[i] == "Twins" | data_all$Single.Multiple[i] == "Triplets"){
    data_all$Single.Multiple[i] = 'Multiple'
  }
}

#APGAR
data_all = add_column(data_all, APGAR = "", .after = 14)
for (i in 1:nrow(data_all)){
  if (is.na(data_all$APGAR.ONE[i]) & is.na(data_all$APGAR.FIVE[i])){
    data_all$APGAR[i] = 'Not available' }
  if ((!is.na(data_all$APGAR.ONE[i]) & data_all$APGAR.ONE[i] <=5) | (!is.na(data_all$APGAR.FIVE[i]) & data_all$APGAR.FIVE[i] <=5)){
    data_all$APGAR[i] = 'Less than 5'
  }
  if ((!is.na(data_all$APGAR.ONE[i]) & data_all$APGAR.ONE[i] >5) | (!is.na(data_all$APGAR.FIVE[i]) &data_all$APGAR.FIVE[i] >5)){
    data_all$APGAR[i] = 'Greater than 5' 
  }
}

#Maternal diseases
data_all = add_column(data_all, Maternal_diseases = "", .after = 23)
for (i in 1:nrow(data_all)){
  if (data_all$Hypertension[i] == 'TRUE' | data_all$Gestational.Hypertension[i] == 'TRUE' | data_all$Diabetes[i] == 'TRUE' |
      data_all$Gestational.Diabetes[i] == 'TRUE' | data_all$CHRONIC.KIDNEY.DISEASE[i] == 'TRUE' | data_all$Hypothyroidism[i] == 'TRUE'
      | data_all$Hyperthyroidism[i] == 'TRUE'){
    data_all$Maternal_diseases[i] = 'TRUE'
  }
  else{
    data_all$Maternal_diseases[i] = 'FALSE'
  }
}

#Maternal infections
data_all = add_column(data_all, Maternal_infections = "", .after = 28)
for (i in 1:nrow(data_all)){
  if (data_all$Fever[i] == 'TRUE' | data_all$UTI[i] == 'TRUE' | data_all$History.Of.Infections[i] == 'TRUE'){
    data_all$Maternal_infections[i] = 'TRUE'
  }
  else{
    data_all$Maternal_infections[i] = 'FALSE'
  }
}

#Maternal risk factors
data_all = add_column(data_all, Maternal_risk_factors = "", .after = 36)
for (i in 1:nrow(data_all)){
  if (data_all$PROM[i] == 'TRUE' | data_all$PPROM[i] == 'TRUE' | 
      data_all$Chorioamniotis[i] == 'TRUE' | data_all$Oligohydraminos[i] == 'TRUE' | data_all$Polyhydraminos[i] == 'TRUE'){
    data_all$Maternal_risk_factors[i] = 'TRUE'
  }
  else{
    data_all$Maternal_risk_factors[i] = 'FALSE'
  }
}

#Need for resuscitation
data_all = add_column(data_all, Need.for.resuscitation = "", .after = 53)
for (i in 1:nrow(data_all)){
  if (data_all$Initial.Steps[i] == 'TRUE'){
    data_all$Need.for.resuscitation[i] = 'Initial Steps'
  }
  if (data_all$O2[i] == 'TRUE'){
    data_all$Need.for.resuscitation[i] = 'O2'
  }
  if (data_all$Chest.Compression[i] == 'TRUE'){
    data_all$Need.for.resuscitation[i] = 'Chest compression'
  }
  if (data_all$PPV[i] == 'TRUE'){
    data_all$Need.for.resuscitation[i] = 'PPV'
  }
}
for (i in 1:nrow(data_all)){
  if (data_all$Need.for.resuscitation[i] == ""){
    data_all$Need.for.resuscitation[i] = 'FALSE'
  }
}

#Umbilical doppler
data_all$Umbilical.Doppler[is.na(data_all$Umbilical.Doppler)] = 'Not available'

# #removing outliers
outliers = which(data_all$GESTATION<26)
data_all = data_all[-(outliers),]



#### Starting all nutrition files ####
sheet_names = c('Energy', 'Protein', 'Vitamina', 'Vitamind','Calcium', 'Phosphorus', 'Iron')

for (i in 1:length(sheet_names)){
  assign(sheet_names[i], read_excel('Site1_Nutrition_Training.xlsx',sheet = sheet_names[i]))
  assign(sheet_names[i],rbind(get(sheet_names[i]), read_excel('Site2_Nutrition_Training.xlsx',sheet = sheet_names[i])))
  assign(sheet_names[i], get(sheet_names[i])[-1,])
  #removed first row of Site 2 after rbind
  assign(sheet_names[i], get(sheet_names[i])[-(nrow(data)),])
}

binaries = c('binaryenergy', 'binaryprotein', 'binaryvitamina', 'binaryvitamind','binarycalcium', 'binaryphosphorus', 'binaryiron')

for (i in 1:length(sheet_names)){
  temp_data = get(sheet_names[i])
  temp_data$UHID = as.character(temp_data$UHID)
  temp_data = add_column(temp_data, Binary = "", .after = 1)
  temp_data = add_column(temp_data, GESTATION = (temp_data$Gestation.Weeks + temp_data$Gestation.Days/7), .after = 4)
  outliers = which(temp_data$GESTATION<26)
  temp_data = temp_data[-(outliers),]
  
  
  #changing "Recommended" col name to "ASPEN recom"
  names(temp_data)[names(temp_data) == "Recommended...51"] <- "ASPEN recom"
  count=51
  for (j in 1:ncol(temp_data)){
    z= paste("Recommended", "...", count, sep = "", collapse = NULL)
    if(z %in% colnames(temp_data))
    {
      names(temp_data)[names(temp_data) == z] <- "ASPEN recom"
    }
    count=count+7
  }
  
  gestation_cat1_1 = temp_data[which(temp_data$GESTATION < 32),]
  gestation_cat2_1 = temp_data[which(temp_data$GESTATION >= 32 & temp_data$GESTATION <= 34),]
  gestation_cat3_1 = temp_data[which(temp_data$GESTATION > 34 & temp_data$GESTATION <= 37),]
  gestation_cat4_1 = temp_data[which(temp_data$GESTATION > 37),]
  
  data_name_1 = c('gestation_cat1_1', 'gestation_cat2_1', 'gestation_cat3_1', 'gestation_cat4_1')

  
  #run the below line only once
  this_file = temp_data
  
  
  for (gest_cat in 1:length(data_name_1)){
    category_file = get(data_name_1[gest_cat])
    given_index = grep('Given', colnames(category_file))
    recommend_index = grep('Recommend', colnames(category_file))
    los_index = grep('LOS', colnames(category_file))
    los=0
    los = as.numeric(unlist(category_file[,los_index[1]]))
    recommended_final = 105
    if(sheet_names[i] == "Energy"){
      recommended_final = 105
    }
    if(sheet_names[i] == "Protein"){
      recommended_final = 3
    }
    num = 0
    denominator = 0
    for (k in 1:199) {
      given_value = as.numeric(unlist(category_file[,given_index[k]]))
      given_value[is.na(given_value)] = 0
      recommend_value = as.numeric(unlist(category_file[,recommend_index[k]]))
      recommend_value[is.na(recommend_value)] = 0
      
      dummy = (recommend_value - given_value)
      dummy[which(is.na(dummy))] = 0
      dummy = dummy*dummy
      num = num + dummy
      recomm_dummy = recommend_value
      recomm_dummy[which(is.na(recomm_dummy))] = 0
      denominator = denominator + recomm_dummy
    }
    numerator = sqrt(num)
    numerator <- numerator[!is.na(numerator)]
    
    uhids_morethan75 = category_file$UHID[which(numerator > quantile(numerator)[2])]
    uhids_lessthan75 = category_file$UHID[which(numerator <= quantile(numerator)[2])]
    
    ## 4th quartile vs rest
    for (l in 1:nrow(this_file)){
      for (zz in 1:length(uhids_morethan75)){
        if(uhids_morethan75[zz] == this_file$UHID[l])  {
          this_file$Binary[l] = "1"
        }
      }
    }
    
    for (l in 1:nrow(this_file)){
      for (zz in 1:length(uhids_lessthan75)){
        if(uhids_lessthan75[zz] == this_file$UHID[l])  {
          this_file$Binary[l] = "0"
        }
      }
    }
    
  }
  assign(binaries[i], this_file$Binary)
}
#### Nutrition ends ####

#### Starting medication files ####
#med_data prep
med = read_excel('Site1_Neofax_Training_withoutcaffeine.xlsx',sheet = 1)
dose_raw = rbind(med, read_excel('Site2_Neofax_Training_withoutcaffeine.xlsx',sheet = 1))
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

med_name = c('gestation_med_cat1','gestation_med_cat2','gestation_med_cat3','gestation_med_cat4')

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
          dose_to_be_altered$dose_final_verdict[j] = "Med not given"
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
}  
dose_deviation = dose_to_be_altered$dose_final_verdict
#### Medication ends ####

# input for model
data_to_be_put_in_model = cbind(data_all, binaryenergy, binaryprotein, binaryvitamina, binaryvitamind, binarycalcium, binaryphosphorus, binaryiron, dose_deviation)

##### Starting model and lsmeans #####
data_to_be_put_in_model = add_column(data_to_be_put_in_model, GES_CATEGORY = "", .after = 5)
colnames(data_to_be_put_in_model) = paste(colnames(data_to_be_put_in_model), 'zzz', sep = '')
for (i in 1:ncol(data_to_be_put_in_model)) {
  if(class(data_to_be_put_in_model[,i]) == 'logical'){
    data_to_be_put_in_model[,i] = as.factor(data_to_be_put_in_model[,i])
  }
}

for (i in 60:ncol(data_to_be_put_in_model)) {
  if(class(data_to_be_put_in_model[,i]) == 'integer'){
    data_to_be_put_in_model[,i] = as.factor(data_to_be_put_in_model[,i])
  }
}

gestation_cat1 = data_to_be_put_in_model[which(data_to_be_put_in_model$GESTATIONzzz < 32),]
#2 represents 25% (index starts from 1 with 0,25,50,75 and 100%)
gestation_cat1$GES_CATEGORYzzz[which(gestation_cat1$GESTATIONzzz > quantile(gestation_cat1$GESTATIONzzz)[2])] = 'Category1'
gestation_cat1$GES_CATEGORYzzz[which(gestation_cat1$GESTATIONzzz <= quantile(gestation_cat1$GESTATIONzzz)[2])] = 'Category2'

gestation_cat2 = data_to_be_put_in_model[which(data_to_be_put_in_model$GESTATIONzzz >= 32 & data_to_be_put_in_model$GESTATIONzzz <= 34),]
gestation_cat2$GES_CATEGORYzzz[which(gestation_cat2$GESTATIONzzz > quantile(gestation_cat2$GESTATIONzzz)[2])] = 'Category1'
gestation_cat2$GES_CATEGORYzzz[which(gestation_cat2$GESTATIONzzz <= quantile(gestation_cat2$GESTATIONzzz)[2])] = 'Category2'

gestation_cat3 = data_to_be_put_in_model[which(data_to_be_put_in_model$GESTATIONzzz > 34 & data_to_be_put_in_model$GESTATIONzzz <= 37),]
gestation_cat3$GES_CATEGORYzzz[which(gestation_cat3$GESTATIONzzz > quantile(gestation_cat3$GESTATIONzzz)[2])] = 'Category1'
gestation_cat3$GES_CATEGORYzzz[which(gestation_cat3$GESTATIONzzz <= quantile(gestation_cat3$GESTATIONzzz)[2])] = 'Category2'

gestation_cat4 = data_to_be_put_in_model[which(data_to_be_put_in_model$GESTATIONzzz > 37),]
gestation_cat4$GES_CATEGORYzzz[which(gestation_cat4$GESTATIONzzz > quantile(gestation_cat4$GESTATIONzzz)[2])] = 'Category1'
gestation_cat4$GES_CATEGORYzzz[which(gestation_cat4$GESTATIONzzz <= quantile(gestation_cat4$GESTATIONzzz)[2])] = 'Category2'

data_name = c('gestation_cat1', 'gestation_cat2','gestation_cat3','gestation_cat4')

final_result = cbind(NA,NA,NA,NA,NA,NA)
for (i in 1:length(data_name)) {
  finalcolumn = c()
  value = c()
  response = c()
  temp_data = get(data_name[i])
  temp_data = temp_data[,c(2,6,9,10,11,12,13,25,30,38,39,40,41,42,43,44,45,46,47,51,55,56,62)]
  #temp_data = temp_data[,c(2,6,9,42,43)]
  
  temp_data[] <- lapply(temp_data, function(x) if(is.factor(x)) factor(x) else x)
  
  this_is_the_data = 0
  this_is_the_data = as.data.frame(this_is_the_data)
  
  #Only those columns will be taken that have 2 or more factors. issue of model- error that variable has only 1 factor, resolved.
  for (col_name in colnames(temp_data)){
    if ((class(temp_data[,col_name])) == "factor"){
      if (length(levels(temp_data[,col_name])) >1){
        this_is_the_data = cbind(this_is_the_data,temp_data[,col_name])
        colnames(this_is_the_data)[colnames(this_is_the_data) == "temp_data[, col_name]"] = col_name
      }
    }
    else{
      this_is_the_data = cbind(this_is_the_data,temp_data[,col_name])
      colnames(this_is_the_data)[colnames(this_is_the_data) == "temp_data[, col_name]"] = col_name
    }
  }
  this_is_the_data = this_is_the_data[,-1]
  
  model = lm(log(LOSzzz) ~ ., data = this_is_the_data)
  summary(model)
  #ref_grid(model)
  
  dummy = as.data.frame(which(round(summary(model)[["coefficients"]][,4],2) <= 0.050))
  column_lsmeans = c()
  for (j in 1:nrow(dummy)) {
    column_lsmeans = c(column_lsmeans,strsplit(rownames(dummy),split = 'zzz')[[j]][1])
  }
  if (column_lsmeans[1]=='(Intercept)'){
    column_lsmeans = column_lsmeans[-(column_lsmeans == '(Intercept)')]  
  }
  column_lsmeans = unique(column_lsmeans)
  column_lsmeans = paste(column_lsmeans,'zzz',sep = '')
  
  final_data = select(this_is_the_data, LOSzzz,column_lsmeans)
  model = lm(log(LOSzzz) ~ ., data = final_data)
  summary(model)
  predict(model)
  for (column in column_lsmeans) {
    lsmean_Result = as.data.frame(lsmeans(model, column,type='response',weights="cells"))[,1:2]
    finalcolumn = c(finalcolumn,rep(column,nrow(lsmean_Result)))
    value = c(value,as.character(lsmean_Result[,1]))
    response = c(response, round(lsmean_Result[,2]) - round(median(this_is_the_data$LOSzzz)))
    
    #if(column == "dose_deviationzzz"){
    #  los_no_med = which(temp_data$dose_deviationzzz=='Deviation')
    #  los_no_med_final = append(los_no_med,which(temp_data$dose_deviationzzz=='No deviation'))
    #  median_medication_given = median(temp_data$LOSzzz[los_no_med_final])
    #  response = c(response, round(lsmean_Result[,2]) - round(median_medication_given))
    #}else{
    #  response = c(response, round(lsmean_Result[,2]) - round(median(this_is_the_data$LOSzzz)))
    #}
  }
  for (k in 1:length(finalcolumn)) {
    if(finalcolumn[k] %in% final_result[,1]){
      index = which(final_result[,1] == finalcolumn[k])
      index = index[which(final_result[index,2] == value[k])]
      if(length(index) != 0){
        final_result[index, i+2] = response[k]
      } else {
        param = c(finalcolumn[k], value[k])
        param[i+2] = response[k]
        final_result = qpcR:::rbind.na(final_result, param)
      }
    } else {
      param = c(finalcolumn[k], value[k])
      param[i+2] = response[k]
      final_result = qpcR:::rbind.na(final_result, param)
    }
  }
}

final_result = final_result[-1,]
row1 = c("Number of patients", "", nrow(gestation_cat1), nrow(gestation_cat2), nrow(gestation_cat3),nrow(gestation_cat4))
row2 = c("Median LOS", "", median(gestation_cat1$LOSzzz), median(gestation_cat2$LOSzzz), median(gestation_cat3$LOSzzz),median(gestation_cat4$LOSzzz))
row3 = c("IQR of LOS", "", IQR(gestation_cat1$LOSzzz), IQR(gestation_cat2$LOSzzz), IQR(gestation_cat3$LOSzzz),IQR(gestation_cat4$LOSzzz))

final_result[,1] = gsub('zzz','',final_result[,1])
colnames(final_result) = c('Parameter','Factors','Gest <32','Gest 32-34','Gest 34-37','Gest >37')                               
final_result = rbind(row1,row2,row3,final_result)

view(final_result)


