#BASELINE TABLE - Table 1
#Final baseline table is "baselinetable" object
library(readxl)
library(tibble)

#baseline_data prep
baseline_data = read_excel('Site1_Nutrition_Training.xlsx',sheet = 1)
baseline_data_all = rbind(baseline_data, read_excel('Site2_Nutrition_Training.xlsx',sheet = 1))
baseline_data_all = baseline_data_all[-1,] #row not needed
baseline_data_all = baseline_data_all[-(nrow(baseline_data)),] #removed first blank column which was generated while appending second cehtre's baseline_data
baseline_data_all$Birth.Weight = as.integer(baseline_data_all$Birth.Weight)
#birth weight wrong entries handled
for (i in 1:length(baseline_data_all$Birth.Weight)){
  if (baseline_data_all$Birth.Weight[i]<5){
    baseline_data_all$Birth.Weight[i] = baseline_data_all$Birth.Weight[i]*1000
  }
  if (baseline_data_all$Birth.Weight[i]<50 & baseline_data_all$Birth.Weight[i]>=5){
    baseline_data_all$Birth.Weight[i] = baseline_data_all$Birth.Weight[i]*100
  }
}

#GESTATION
baseline_data_all = add_column(baseline_data_all, GESTATION = (baseline_data_all$Gestation.Weeks + baseline_data_all$Gestation.Days/7), .after = 4)

outliers = which(baseline_data_all$GESTATION<26)
baseline_data_all = baseline_data_all[-(outliers),]

#Mode of delivery 
for (i in 1:nrow(baseline_data_all)){
  if(baseline_data_all$Mode.of.delivery[i] == "Vaccum" | baseline_data_all$Mode.of.delivery[i] == "Forceps"){
      baseline_data_all$Mode.of.delivery[i] = 'NVD'
  }
}

#Single/multiple
for (i in 1:nrow(baseline_data_all)){
  if(baseline_data_all$Single.Multiple[i] == "Twins" | baseline_data_all$Single.Multiple[i] == "Triplets"){
    baseline_data_all$Single.Multiple[i] = 'Multiple'
  }
}

#APGAR
baseline_data_all = add_column(baseline_data_all, APGAR = "", .after = 14)
for (i in 1:nrow(baseline_data_all)){
  if (is.na(baseline_data_all$APGAR.ONE[i]) & is.na(baseline_data_all$APGAR.FIVE[i])){
    baseline_data_all$APGAR[i] = 'Not available' }
  if ((!is.na(baseline_data_all$APGAR.ONE[i]) & baseline_data_all$APGAR.ONE[i] <=5) | (!is.na(baseline_data_all$APGAR.FIVE[i]) & baseline_data_all$APGAR.FIVE[i] <=5)){
    baseline_data_all$APGAR[i] = 'Less than 5'
  }
  if ((!is.na(baseline_data_all$APGAR.ONE[i]) & baseline_data_all$APGAR.ONE[i] >5) | (!is.na(baseline_data_all$APGAR.FIVE[i]) &baseline_data_all$APGAR.FIVE[i] >5)){
    baseline_data_all$APGAR[i] = 'Greater than 5' 
  }
}

#Maternal diseases
baseline_data_all = add_column(baseline_data_all, Maternal_diseases = "", .after = 23)
for (i in 1:nrow(baseline_data_all)){
  if (baseline_data_all$Hypertension[i] == 'TRUE' | baseline_data_all$Gestational.Hypertension[i] == 'TRUE' | baseline_data_all$Diabetes[i] == 'TRUE' |
      baseline_data_all$Gestational.Diabetes[i] == 'TRUE' | baseline_data_all$CHRONIC.KIDNEY.DISEASE[i] == 'TRUE' | baseline_data_all$Hypothyroidism[i] == 'TRUE'
      | baseline_data_all$Hyperthyroidism[i] == 'TRUE'){
    baseline_data_all$Maternal_diseases[i] = 'TRUE'
  }
  else{
    baseline_data_all$Maternal_diseases[i] = 'FALSE'
  }
}

#Maternal infections
baseline_data_all = add_column(baseline_data_all, Maternal_infections = "", .after = 28)
for (i in 1:nrow(baseline_data_all)){
  if (baseline_data_all$Fever[i] == 'TRUE' | baseline_data_all$UTI[i] == 'TRUE' | baseline_data_all$History.Of.Infections[i] == 'TRUE'){
    baseline_data_all$Maternal_infections[i] = 'TRUE'
  }
  else{
    baseline_data_all$Maternal_infections[i] = 'FALSE'
  }
}

#Maternal risk factors
baseline_data_all = add_column(baseline_data_all, Maternal_risk_factors = "", .after = 36)
for (i in 1:nrow(baseline_data_all)){
  if (baseline_data_all$PROM[i] == 'TRUE' | baseline_data_all$PPROM[i] == 'TRUE' |
      baseline_data_all$Chorioamniotis[i] == 'TRUE' | baseline_data_all$Oligohydraminos[i] == 'TRUE' | baseline_data_all$Polyhydraminos[i] == 'TRUE'){
    baseline_data_all$Maternal_risk_factors[i] = 'TRUE'
  }
  else{
    baseline_data_all$Maternal_risk_factors[i] = 'FALSE'
  }
}

#Need for resuscitation
baseline_data_all = add_column(baseline_data_all, Need.for.resuscitation = "", .after = 51)
for (i in 1:nrow(baseline_data_all)){
  if (baseline_data_all$Initial.Steps[i] == 'TRUE'){
    baseline_data_all$Need.for.resuscitation[i] = 'Initial Steps'
  }
  if (baseline_data_all$O2[i] == 'TRUE'){
    baseline_data_all$Need.for.resuscitation[i] = 'O2'
  }
  if (baseline_data_all$Chest.Compression[i] == 'TRUE'){
    baseline_data_all$Need.for.resuscitation[i] = 'Chest compression'
  }
  if (baseline_data_all$PPV[i] == 'TRUE'){
    baseline_data_all$Need.for.resuscitation[i] = 'PPV'
  }
}
for (i in 1:nrow(baseline_data_all)){
  if (baseline_data_all$Need.for.resuscitation[i] == ""){
    baseline_data_all$Need.for.resuscitation[i] = 'FALSE'
  }
}

#Umbilical doppler
baseline_data_all$Umbilical.Doppler[is.na(baseline_data_all$Umbilical.Doppler)] = 'Not available'

baseline_only_data = baseline_data_all[,1:55] #selected only baseline columns

cat = c(2,5,6,8,9,10,11,12,15,24,29,37,38,39,40,41,42,43,44,45,46,52,53)
categorical = baseline_only_data[,cat]
for (m in 4:length(categorical)){
  categorical[,m] <- as.factor(unlist(categorical[,m]))
}

gestation_cat1_1 = categorical[which(categorical$GESTATION < 32),]
gestation_cat2_1 = categorical[which(categorical$GESTATION >= 32 & categorical$GESTATION <= 34),]
gestation_cat3_1 = categorical[which(categorical$GESTATION > 34 & categorical$GESTATION <= 37),]
gestation_cat4_1 = categorical[which(categorical$GESTATION > 37),]

#los, gestation and birth weight indices in new files
indices = c(1,2,3)
#removed los, gestation and BW columns - kept only categorical ones
ges_cat1_2 = gestation_cat1_1[,-indices]
ges_cat2_2 = gestation_cat2_1[,-indices]
ges_cat3_2 = gestation_cat3_1[,-indices]
ges_cat4_2 = gestation_cat4_1[,-indices]

data_name_1 = c('gestation_cat1_1', 'gestation_cat2_1', 'gestation_cat3_1', 'gestation_cat4_1')

ges_lessthan_32 = as.data.frame(summary(ges_cat1_2), is.na=TRUE)
ges_lessthan_32 = na.omit(ges_lessthan_32)
ges_32_34 = as.data.frame(summary(ges_cat2_2), is.na=TRUE)
ges_32_34 = na.omit(ges_32_34)
ges_34_37 = as.data.frame(summary(ges_cat3_2), is.na=TRUE)
ges_34_37 = na.omit(ges_34_37)
ges_morethan_37 = as.data.frame(summary(ges_cat4_2), is.na=TRUE)
ges_morethan_37 = na.omit(ges_morethan_37)

see = cbind(ges_lessthan_32, ges_32_34[,3], ges_34_37[,3], ges_morethan_37[,3],stringsAsFactors = FALSE)
for (m in 1:ncol(see)){
    see[,m] <- as.character(unlist(see[,m]))
}

intro = c("", "Gestation categories", "Gest <32", "Gest 32-34", "Gest 34-37", "Gest >37")
num_patients = c("", "Number of patients", nrow(gestation_cat1_1), nrow(gestation_cat2_1), nrow(gestation_cat3_1), nrow(gestation_cat4_1))
median_los = c("","Median LOS", median(gestation_cat1_1$LOS), median(gestation_cat2_1$LOS), median(gestation_cat3_1$LOS), median(gestation_cat4_1$LOS))
iqr_los = c("", "IQR LOS", IQR(gestation_cat1_1$LOS), IQR(gestation_cat2_1$LOS), IQR(gestation_cat3_1$LOS), IQR(gestation_cat4_1$LOS))
gest_mean = c("", "Gestation age (mean)", round(mean(gestation_cat1_1$GESTATION), digits=1), round(mean(gestation_cat2_1$GESTATION), digits=1), round(mean(gestation_cat3_1$GESTATION), digits=1), round(mean(gestation_cat4_1$GESTATION), digits=1))
sd_gest = c("", "Gestation age (SD)", round(sd(gestation_cat1_1$GESTATION), digits=1), round(sd(gestation_cat2_1$GESTATION), digits=1), round(sd(gestation_cat3_1$GESTATION), digits=1), round(sd(gestation_cat4_1$GESTATION), digits=1))
bw_mean = c("", "Birth weight (mean)", round(mean(gestation_cat1_1$Birth.Weight), digits=1), round(mean(gestation_cat2_1$Birth.Weight), digits=1), round(mean(gestation_cat3_1$Birth.Weight), digits=1), round(mean(gestation_cat4_1$Birth.Weight), digits=1))
sd_bw = c("", "Birth weight (SD)", round(sd(gestation_cat1_1$Birth.Weight), digits=1), round(sd(gestation_cat2_1$Birth.Weight), digits=1), round(sd(gestation_cat3_1$Birth.Weight), digits=1), round(sd(gestation_cat4_1$Birth.Weight), digits=1))

see = rbind(intro, num_patients, median_los, iqr_los, gest_mean, sd_gest, bw_mean, sd_bw, see)

#### Nutrition mean LOS values now #####
sheet_names = c('Energy', 'Protein')

for (i in 1:length(sheet_names)){
  assign(sheet_names[i], read_excel('Site1_Nutrition_Training.xlsx',sheet = sheet_names[i]))
  assign(sheet_names[i],rbind(get(sheet_names[i]), read_excel('Site2_Nutrition_Training.xlsx',sheet = sheet_names[i])))
  assign(sheet_names[i], get(sheet_names[i])[-1,])
  #removed first row of kalawati after rbind
  assign(sheet_names[i], get(sheet_names[i])[-nrow(baseline_data),])
}

for (i in 1:length(sheet_names)){
  temp_data = get(sheet_names[i])
  temp_data$UHID = as.character(temp_data$UHID)
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
  
  gestation_cat1 = temp_data[which(temp_data$GESTATION < 32),]
  gestation_cat2 = temp_data[which(temp_data$GESTATION >= 32 & temp_data$GESTATION <= 34),]
  gestation_cat3 = temp_data[which(temp_data$GESTATION > 34 & temp_data$GESTATION <= 37),]
  gestation_cat4 = temp_data[which(temp_data$GESTATION > 37),]
  
  data_name = c('gestation_cat1', 'gestation_cat2', 'gestation_cat3', 'gestation_cat4')
  
  col1 = c(sheet_names[i],sheet_names[i])
  col2 = c('4th quartile', 'Remaining')
  # col2 = c('1st quartile', 'Remaining')
  all_cols = cbind(col1, col2)
  
  for (gest_cat in 1:length(data_name)){
    category_file = get(data_name[gest_cat])
    
    given_index = grep('Given', colnames(category_file))
    recommend_index = grep('Recommend', colnames(category_file))
    diff_index = grep('Diff', colnames(category_file))
    
    los_index = grep('LOS', colnames(category_file))
    los = 0
    num = 0
    denominator = 0
    recomm_dummy = 0
    recommended_final = 105
    if(sheet_names[i] == "Energy"){
      recommended_final = 105
    }
    if(sheet_names[i] == "Protein"){
      recommended_final = 3
    }
    los = as.numeric(unlist(category_file[,los_index[1]]))
    counter = 0
    for (k in 1:199) {
      diff_value = as.numeric(unlist(category_file[,diff_index[k]]))
      diff_value[is.na(diff_value)] = 0
      
      given_value = as.numeric(unlist(category_file[,given_index[k]]))
      given_value[is.na(given_value)] = 0
      recommend_value = as.numeric(unlist(category_file[,recommend_index[k]]))
      recommend_value[is.na(recommend_value)] = 0
      dummy = (recommend_value - given_value)
      dummy[which(is.na(dummy))] = 0
      diff_value = diff_value*diff_value
      num = num + diff_value
      recomm_dummy = recomm_dummy + recommend_value
      recomm_dummy[which(is.na(recomm_dummy))] = 0
      counter = counter + 1
      
    }
    numerator = sqrt(num) 
    numerator <- numerator[!is.na(numerator)]
    #fourth quartile
    #fourth = round(median(category_file$LOS[which(numerator <= quantile(numerator)[5] & numerator > quantile(numerator)[4])]), digits=1)
    fourth = round(IQR(numerator[which(numerator >= quantile(numerator)[4])]), digits=1)
    total_fourth = numerator[which(numerator >= quantile(numerator)[4])]
    
    quartile_1_fourth = round(median(total_fourth[which(total_fourth <= quantile(total_fourth)[2])]), digits=1)
    quartile_3_fourth = round(median(total_fourth[which(total_fourth <= quantile(total_fourth)[4] & total_fourth > quantile(total_fourth)[3])]), digits=1)
    
    net_fourth = paste(quartile_1_fourth, quartile_3_fourth)
    #gestation_cat1$GES_CATEGORYzzz[which(gestation_cat1$GESTATIONzzz > quantile(gestation_cat1$GESTATIONzzz)[2])] = 'Category1'
    
    fourth_deviation = median(numerator[which(numerator >= quantile(numerator)[4])], digits =1 )
    dum1 = capture.output(cat(fourth_deviation,"(", net_fourth,")"))
    
    # rest quartiles
    rest = round(IQR(numerator[which(numerator < quantile(numerator)[4])]), digits=1)
    
    total_rest = numerator[which(numerator < quantile(numerator)[4])]
    
    quartile_1_rest = round(median(total_rest[which(total_rest <= quantile(total_rest)[2])]), digits=1)
    quartile_3_rest = round(median(total_rest[which(total_rest <= quantile(total_rest)[4] & total_rest > quantile(total_rest)[3])]), digits=1)
    
    net_rest = paste(quartile_1_rest, quartile_3_rest)
    
    rest_deviation = median(numerator[which(numerator < quantile(numerator)[4])], digits = 1)
    #gestation_cat1$GES_CATEGORYzzz[which(gestation_cat1$GESTATIONzzz <= quantile(gestation_cat1$GESTATIONzzz)[2])] = 'Category2'
    
    dum2 = capture.output(cat(rest_deviation,"(", net_rest,")"))
    
    # #first quartile
    # first = round(mean(category_file$LOS[which(numerator <= quantile(numerator)[2])]), digits=1)
    # # rest quartiles
    # rest = round(mean(category_file$LOS[which(numerator > quantile(numerator)[2])]), digits=1)
    
    col3 = c(dum1,dum2)
    # col3 = c(first,rest)
    all_cols = cbind(all_cols, col3)
  }
  all_cols = as.data.frame(all_cols)
  names(all_cols) <- names(see)
  see = rbind(see, all_cols)
}
#### Nutrition ends #####

#### Medication mean LOS values now #####
#med_data prep
med_data = read_excel('Site1_Neofax_Training.xlsx',sheet = 1)
med_data_all = rbind(med_data, read_excel('Site2_Neofax_Training.xlsx',sheet = 1))
med_data_all = med_data_all[-1,] #row not needed
med_data_all = med_data_all[-(nrow(med_data)),] #removed first blank column which was generated while appending second cehtre's med_data
#GESTATION
med_data_all = add_column(med_data_all, GESTATION = (med_data_all$Gestation.Weeks + med_data_all$Gestation.Days/7), .after = 4)
med_data_all$Birth.Weight = as.integer(med_data_all$Birth.Weight)
#birth weight wrong entries handled
for (i in 1:length(med_data_all$Birth.Weight)){
  if (med_data_all$Birth.Weight[i]<5){
    med_data_all$Birth.Weight[i] = med_data_all$Birth.Weight[i]*1000
  }
  if (med_data_all$Birth.Weight[i]<50 & med_data_all$Birth.Weight[i]>=5){
    med_data_all$Birth.Weight[i] = med_data_all$Birth.Weight[i]*100
  }
}

outliers = which(med_data_all$GESTATION<26)
med_data_all = med_data_all[-(outliers),]
dose_raw = med_data_all

dose_raw$UHID = as.character(dose_raw$UHID)

#changing "Recommended" col name to "Neofax recom"
names(dose_raw)[names(dose_raw) == "Recommended...52"] <- "NeoFax recom"
count=52
for (j in 1:ncol(dose_raw)){
  z= paste("Recommended", "...", count, sep = "", collapse = NULL)
  if(z %in% colnames(dose_raw))
  {
    names(dose_raw)[names(dose_raw) == z] <- "NeoFax recom"
  }
  count=count+9
}

dose_raw$GESTATION = as.numeric(dose_raw$GESTATION)
gestation_med_cat1 = dose_raw[which(dose_raw$GESTATION < 32),]
gestation_med_cat2 = dose_raw[which(dose_raw$GESTATION >= 32 & dose_raw$GESTATION <= 34),]
gestation_med_cat3 = dose_raw[which(dose_raw$GESTATION > 34 & dose_raw$GESTATION <= 37),]
gestation_med_cat4 = dose_raw[which(dose_raw$GESTATION > 37),]

med_data_name = c('gestation_med_cat1', 'gestation_med_cat2', 'gestation_med_cat3', 'gestation_med_cat4')

med_col1 = c("Dose","Dose","Dose")
med_col2 = c('No deviation', 'Negative deviation', 'Positive deviation')
med_all_cols = cbind(med_col1, med_col2)

for (ges_med in 1:length(med_data_name)){
  dum0=0
  dum1=0
  dum2=0
  dum3=0
  dose = get(med_data_name[ges_med])
  diff_index = grep('Diff', colnames(dose))
  #Changed class to numeric!
  dose[,diff_index] <- as.numeric(as.character(unlist(dose[,diff_index])))
  
  numerator = 0
  given_value = as.numeric(unlist(dose[,diff_index[1]]))
  dummy = given_value
  numerator = numerator + dummy
  for (i in 2:250) {
    #unlist so that "values" is generated and not "med_data file"
    given_value = as.numeric(unlist(dose[,diff_index[i]]))
    dummy = given_value
    dummy[which(is.na(dummy))] = 0
    numerator = numerator + dummy
  }
  
  # med not given #
  temp_med_data = dose[which(is.na(numerator)),]
  if (nrow(temp_med_data)>0){
    dum0 = capture.output(cat(round(mean(temp_med_data$LOS), digits=1),"(", nrow(temp_med_data),")"))
  }
  
  # no deviation #
  temp_med_data = dose[which(numerator == 0),]
  if (nrow(temp_med_data)>0){
    dum1 = capture.output(cat(round(mean(temp_med_data$LOS), digits=1),"(", nrow(temp_med_data),")"))
  }
  
  # negative #
  temp_med_data = dose[which(numerator < 0),]
  if (nrow(temp_med_data)>0){
    dum2 = capture.output(cat(round(mean(temp_med_data$LOS), digits=1),"(", nrow(temp_med_data),")"))
  }
  
  # positive #
  temp_med_data = dose[which(numerator > 0),]
  if (nrow(temp_med_data)>0){
    dum3 = capture.output(cat(round(mean(temp_med_data$LOS), digits=1),"(", nrow(temp_med_data),")"))
  }
    med_col3 = c(dum1,dum2,dum3)
    med_all_cols = cbind(med_all_cols, med_col3)
  }
med_all_cols = as.data.frame(med_all_cols)
names(med_all_cols) <- names(see)
see = rbind(see, med_all_cols)

baselinetable = see
view(baselinetable)

