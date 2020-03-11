#install xquartz
#Sequentially the following bubble plots will be generated
#i = 1 ---> Energy bubble plot
#i = 2 ---> Protein bubble plot
#i = 3 ---> Vitamin A bubble plot
#i = 4 ---> Vitamin D bubble plot
#i = 5 ---> Calcium bubble plot
#i = 6 ---> Phosphorus bubble plot
#i = 7 ---> Iron bubble plot

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(readxl)
library(tidyverse)

data = read_excel('Site1_Nutrition_Validation.xlsx',sheet = 1)

sheet_names = c('Energy', 'Protein', 'Vitamina', 'Vitamind','Calcium', 'Phosphorus', 'Iron')

for (sheet in 1:length(sheet_names)){
  assign(sheet_names[sheet], read_excel('Site1_Nutrition_Validation.xlsx',sheet = sheet_names[sheet]))
  assign(sheet_names[sheet],rbind(get(sheet_names[sheet]), read_excel('Site2_Nutrition_Validation.xlsx',sheet = sheet_names[sheet])))
  assign(sheet_names[sheet], get(sheet_names[sheet])[-1,])
  #removed first row of kalawati after rbind
  assign(sheet_names[sheet], get(sheet_names[sheet])[-(nrow(data)),])
}

data_name = c('ges_lessthan_32', 'ges_32_34', 'ges_34_37', 'ges_morethan_37')
file_names = c('first_32', 'second_32_34', 'third_34_37', 'fourth_37')

### Energy bubble plot ####
i=1 #Energy
for (j in 1:1){
  plot_data = get(sheet_names[i])
  plot_data$Birth.Weight = as.integer(plot_data$Birth.Weight)
  #birth weight wrong entries handled
  for (var in 1:length(plot_data$Birth.Weight)){
    if (plot_data$Birth.Weight[var]<5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*1000
    }
    if (plot_data$Birth.Weight[var]<50 & plot_data$Birth.Weight[var]>=5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*100
    }
  }
  
  ## Adding all columns that have to be put in model ##
  #GESTATION
  plot_data = add_column(plot_data, GESTATION = (plot_data$Gestation.Weeks + plot_data$Gestation.Days/7), .after = 4)
  
  #removing outliers
  
  
  ges_lessthan_32 = plot_data[which(plot_data$GESTATION < 32),]
  ges_32_34 = plot_data[which(plot_data$GESTATION >= 32 & plot_data$GESTATION <= 34),]
  ges_34_37 = plot_data[which(plot_data$GESTATION > 34 & plot_data$GESTATION <= 37),]
  ges_morethan_37 = plot_data[which(plot_data$GESTATION > 37),]  

  arr_of_means = c()
  arr_of_lengths = c()
  for (j in 1:length(data_name)){
    final_data = get(data_name[j])
    give_index = grep('Given', colnames(final_data))
  
    given = 0
    give_value = as.numeric(unlist(final_data[,give_index[1]]))
    give_value = na.exclude(give_value)
    arr_of_means[1] = mean(give_value)
    arr_of_lengths[1] = length(give_value)
    mean_of_cat = 0
    len = 0
    for (values in 2:199) {
      give_value = as.numeric(unlist(final_data[,give_index[values]]))
      give_value = na.exclude(give_value)
      mean_of_cat = mean(give_value)
      arr_of_means[values] = mean_of_cat
      len = length(give_value)
      arr_of_lengths[values] = len
    }
    Order.given = arr_of_means
    Number.of.patients = arr_of_lengths
    Gestation = rep(file_names[j], 199)
    DOL = c(1:199)
    assign(file_names[j], cbind(Gestation, DOL, Order.given, Number.of.patients))
  }
  input_for_bubble = c()
  input_for_bubble = rbind(first_32, second_32_34, third_34_37, fourth_37)
  input_for_bubble = as.data.frame(input_for_bubble)
  input_for_bubble$Gestation = as.character(input_for_bubble$Gestation)
  input_for_bubble$DOL = as.numeric(as.character(input_for_bubble$DOL))
  input_for_bubble$Order.given = as.numeric(as.character(input_for_bubble$Order.given))
  input_for_bubble$Number.of.patients = as.numeric(as.character(input_for_bubble$Number.of.patients))
}

  input_for_bubble %>%
    arrange(desc(Number.of.patients)) %>%

    ggplot(aes(x=DOL, y=Order.given, size=Number.of.patients, color=Gestation,fill=Gestation)) +
    geom_point(alpha=0.8,shape=21, color="black") +
    theme_bw() + theme_ipsum() + ggtitle(sheet_names[i]) + 
    #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="I") +
    scale_size(range = c(.1, 24), name="Patients count") +
    theme(text=element_text(family="serif",size=20,face = 'bold'))+
    xlim(0, 80) + ylim(0,160)
### Protein bubble plot ####
i=2 #Protein
for (j in 1:1){
  plot_data = get(sheet_names[i])
  plot_data$Birth.Weight = as.integer(plot_data$Birth.Weight)
  #birth weight wrong entries handled
  for (var in 1:length(plot_data$Birth.Weight)){
    if (plot_data$Birth.Weight[var]<5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*1000
    }
    if (plot_data$Birth.Weight[var]<50 & plot_data$Birth.Weight[var]>=5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*100
    }
  }
  
  ## Adding all columns that have to be put in model ##
  #GESTATION
  plot_data = add_column(plot_data, GESTATION = (plot_data$Gestation.Weeks + plot_data$Gestation.Days/7), .after = 4)
  
  #removing outliers
  
  ges_lessthan_32 = plot_data[which(plot_data$GESTATION < 32),]
  ges_32_34 = plot_data[which(plot_data$GESTATION >= 32 & plot_data$GESTATION <= 34),]
  ges_34_37 = plot_data[which(plot_data$GESTATION > 34 & plot_data$GESTATION <= 37),]
  ges_morethan_37 = plot_data[which(plot_data$GESTATION > 37),]  
  
  arr_of_means = c()
  arr_of_lengths = c()
  for (j in 1:length(data_name)){
    final_data = get(data_name[j])
    give_index = grep('Given', colnames(final_data))
    
    given = 0
    give_value = as.numeric(unlist(final_data[,give_index[1]]))
    give_value = na.exclude(give_value)
    arr_of_means[1] = mean(give_value)
    arr_of_lengths[1] = length(give_value)
    mean_of_cat = 0
    len = 0
    for (values in 2:199) {
      give_value = as.numeric(unlist(final_data[,give_index[values]]))
      give_value = na.exclude(give_value)
      mean_of_cat = mean(give_value)
      arr_of_means[values] = mean_of_cat
      len = length(give_value)
      arr_of_lengths[values] = len
    }
    Order.given = arr_of_means
    Number.of.patients = arr_of_lengths
    Gestation = rep(file_names[j], 199)
    DOL = c(1:199)
    assign(file_names[j], cbind(Gestation, DOL, Order.given, Number.of.patients))
  }
  input_for_bubble = c()
  input_for_bubble = rbind(first_32, second_32_34, third_34_37, fourth_37)
  input_for_bubble = as.data.frame(input_for_bubble)
  input_for_bubble$Gestation = as.character(input_for_bubble$Gestation)
  input_for_bubble$DOL = as.numeric(as.character(input_for_bubble$DOL))
  input_for_bubble$Order.given = as.numeric(as.character(input_for_bubble$Order.given))
  input_for_bubble$Number.of.patients = as.numeric(as.character(input_for_bubble$Number.of.patients))
}

  input_for_bubble %>%
  arrange(desc(Number.of.patients)) %>%

  ggplot(aes(x=DOL, y=Order.given, size=Number.of.patients, color=Gestation,fill=Gestation)) +
  geom_point(alpha=0.8,shape=21, color="black") +
  theme_bw() + theme_ipsum() + ggtitle(sheet_names[i]) + 
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="I") +
  scale_size(range = c(.1, 24), name="Patients count") +
  theme(text=element_text(family="serif",size=20,face = 'bold'))+
  xlim(0, 80) + ylim(0,4)

### Vitamin A bubble plot ####
i=3 #Vitamin A
for (j in 1:1){
  plot_data = get(sheet_names[i])
  plot_data$Birth.Weight = as.integer(plot_data$Birth.Weight)
  #birth weight wrong entries handled
  for (var in 1:length(plot_data$Birth.Weight)){
    if (plot_data$Birth.Weight[var]<5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*1000
    }
    if (plot_data$Birth.Weight[var]<50 & plot_data$Birth.Weight[var]>=5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*100
    }
  }
  
  ## Adding all columns that have to be put in model ##
  #GESTATION
  plot_data = add_column(plot_data, GESTATION = (plot_data$Gestation.Weeks + plot_data$Gestation.Days/7), .after = 4)
  
  #removing outliers
  
  ges_lessthan_32 = plot_data[which(plot_data$GESTATION < 32),]
  ges_32_34 = plot_data[which(plot_data$GESTATION >= 32 & plot_data$GESTATION <= 34),]
  ges_34_37 = plot_data[which(plot_data$GESTATION > 34 & plot_data$GESTATION <= 37),]
  ges_morethan_37 = plot_data[which(plot_data$GESTATION > 37),]  
  
  arr_of_means = c()
  arr_of_lengths = c()
  for (j in 1:length(data_name)){
    final_data = get(data_name[j])
    give_index = grep('Given', colnames(final_data))
    
    given = 0
    give_value = as.numeric(unlist(final_data[,give_index[1]]))
    give_value = na.exclude(give_value)
    arr_of_means[1] = mean(give_value)
    arr_of_lengths[1] = length(give_value)
    mean_of_cat = 0
    len = 0
    for (values in 2:199) {
      give_value = as.numeric(unlist(final_data[,give_index[values]]))
      give_value = na.exclude(give_value)
      mean_of_cat = mean(give_value)
      arr_of_means[values] = mean_of_cat
      len = length(give_value)
      arr_of_lengths[values] = len
    }
    Order.given = arr_of_means
    Number.of.patients = arr_of_lengths
    Gestation = rep(file_names[j], 199)
    DOL = c(1:199)
    assign(file_names[j], cbind(Gestation, DOL, Order.given, Number.of.patients))
  }
  input_for_bubble = c()
  input_for_bubble = rbind(first_32, second_32_34, third_34_37, fourth_37)
  input_for_bubble = as.data.frame(input_for_bubble)
  input_for_bubble$Gestation = as.character(input_for_bubble$Gestation)
  input_for_bubble$DOL = as.numeric(as.character(input_for_bubble$DOL))
  input_for_bubble$Order.given = as.numeric(as.character(input_for_bubble$Order.given))
  input_for_bubble$Number.of.patients = as.numeric(as.character(input_for_bubble$Number.of.patients))
}

input_for_bubble %>%
  arrange(desc(Number.of.patients)) %>%
  ggplot(aes(x=DOL, y=Order.given, size=Number.of.patients, color=Gestation,fill=Gestation)) +
  geom_point(alpha=0.8,shape=21, color="black") +
  theme_bw() + theme_ipsum() + ggtitle(sheet_names[i]) + 
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="I") +
  scale_size(range = c(.1, 24), name="Patients count") +
  theme(text=element_text(family="serif",size=20,face = 'bold'))+
  xlim(0, 80) + ylim(0,2333)


### Vitamin D bubble plot ####
i=4 #Vitamin D
for (j in 1:1){
  plot_data = get(sheet_names[i])
  plot_data$Birth.Weight = as.integer(plot_data$Birth.Weight)
  #birth weight wrong entries handled
  for (var in 1:length(plot_data$Birth.Weight)){
    if (plot_data$Birth.Weight[var]<5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*1000
    }
    if (plot_data$Birth.Weight[var]<50 & plot_data$Birth.Weight[var]>=5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*100
    }
  }
  
  ## Adding all columns that have to be put in model ##
  #GESTATION
  plot_data = add_column(plot_data, GESTATION = (plot_data$Gestation.Weeks + plot_data$Gestation.Days/7), .after = 4)
  
  #removing outliers
  
  ges_lessthan_32 = plot_data[which(plot_data$GESTATION < 32),]
  ges_32_34 = plot_data[which(plot_data$GESTATION >= 32 & plot_data$GESTATION <= 34),]
  ges_34_37 = plot_data[which(plot_data$GESTATION > 34 & plot_data$GESTATION <= 37),]
  ges_morethan_37 = plot_data[which(plot_data$GESTATION > 37),]  
  
  arr_of_means = c()
  arr_of_lengths = c()
  for (j in 1:length(data_name)){
    final_data = get(data_name[j])
    give_index = grep('Given', colnames(final_data))
    
    given = 0
    give_value = as.numeric(unlist(final_data[,give_index[1]]))
    give_value = na.exclude(give_value)
    arr_of_means[1] = mean(give_value)
    arr_of_lengths[1] = length(give_value)
    mean_of_cat = 0
    len = 0
    for (values in 2:199) {
      give_value = as.numeric(unlist(final_data[,give_index[values]]))
      give_value = na.exclude(give_value)
      mean_of_cat = mean(give_value)
      arr_of_means[values] = mean_of_cat
      len = length(give_value)
      arr_of_lengths[values] = len
    }
    Order.given = arr_of_means
    Number.of.patients = arr_of_lengths
    Gestation = rep(file_names[j], 199)
    DOL = c(1:199)
    assign(file_names[j], cbind(Gestation, DOL, Order.given, Number.of.patients))
  }
  input_for_bubble = c()
  input_for_bubble = rbind(first_32, second_32_34, third_34_37, fourth_37)
  input_for_bubble = as.data.frame(input_for_bubble)
  input_for_bubble$Gestation = as.character(input_for_bubble$Gestation)
  input_for_bubble$DOL = as.numeric(as.character(input_for_bubble$DOL))
  input_for_bubble$Order.given = as.numeric(as.character(input_for_bubble$Order.given))
  input_for_bubble$Number.of.patients = as.numeric(as.character(input_for_bubble$Number.of.patients))
}

input_for_bubble %>%
  arrange(desc(Number.of.patients)) %>%
  ggplot(aes(x=DOL, y=Order.given, size=Number.of.patients, color=Gestation,fill=Gestation)) +
  geom_point(alpha=0.8,shape=21, color="black") +
  theme_bw() + theme_ipsum() + ggtitle(sheet_names[i]) + 
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="I") +
  scale_size(range = c(.1, 24), name="Patients count") +
  theme(text=element_text(family="serif",size=20,face = 'bold'))+
  xlim(0, 80) + ylim(0,800)


### Calcium bubble plot ####
i=5 #Calcium
for (j in 1:1){
  plot_data = get(sheet_names[i])
  plot_data$Birth.Weight = as.integer(plot_data$Birth.Weight)
  #birth weight wrong entries handled
  for (var in 1:length(plot_data$Birth.Weight)){
    if (plot_data$Birth.Weight[var]<5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*1000
    }
    if (plot_data$Birth.Weight[var]<50 & plot_data$Birth.Weight[var]>=5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*100
    }
  }
  
  ## Adding all columns that have to be put in model ##
  #GESTATION
  plot_data = add_column(plot_data, GESTATION = (plot_data$Gestation.Weeks + plot_data$Gestation.Days/7), .after = 4)
  
  #removing outliers
  
  ges_lessthan_32 = plot_data[which(plot_data$GESTATION < 32),]
  ges_32_34 = plot_data[which(plot_data$GESTATION >= 32 & plot_data$GESTATION <= 34),]
  ges_34_37 = plot_data[which(plot_data$GESTATION > 34 & plot_data$GESTATION <= 37),]
  ges_morethan_37 = plot_data[which(plot_data$GESTATION > 37),]  
  
  arr_of_means = c()
  arr_of_lengths = c()
  for (j in 1:length(data_name)){
    final_data = get(data_name[j])
    give_index = grep('Given', colnames(final_data))
    
    given = 0
    give_value = as.numeric(unlist(final_data[,give_index[1]]))
    give_value = na.exclude(give_value)
    arr_of_means[1] = mean(give_value)
    arr_of_lengths[1] = length(give_value)
    mean_of_cat = 0
    len = 0
    for (values in 2:199) {
      give_value = as.numeric(unlist(final_data[,give_index[values]]))
      give_value = na.exclude(give_value)
      mean_of_cat = mean(give_value)
      arr_of_means[values] = mean_of_cat
      len = length(give_value)
      arr_of_lengths[values] = len
    }
    Order.given = arr_of_means
    Number.of.patients = arr_of_lengths
    Gestation = rep(file_names[j], 199)
    DOL = c(1:199)
    assign(file_names[j], cbind(Gestation, DOL, Order.given, Number.of.patients))
  }
  input_for_bubble = c()
  input_for_bubble = rbind(first_32, second_32_34, third_34_37, fourth_37)
  input_for_bubble = as.data.frame(input_for_bubble)
  input_for_bubble$Gestation = as.character(input_for_bubble$Gestation)
  input_for_bubble$DOL = as.numeric(as.character(input_for_bubble$DOL))
  input_for_bubble$Order.given = as.numeric(as.character(input_for_bubble$Order.given))
  input_for_bubble$Number.of.patients = as.numeric(as.character(input_for_bubble$Number.of.patients))
}

input_for_bubble %>%
  arrange(desc(Number.of.patients)) %>%
  ggplot(aes(x=DOL, y=Order.given, size=Number.of.patients, color=Gestation,fill=Gestation)) +
  geom_point(alpha=0.8,shape=21, color="black") +
  theme_bw() + theme_ipsum() + ggtitle(sheet_names[i]) + 
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="I") +
  scale_size(range = c(.1, 24), name="Patients count") +
  theme(text=element_text(family="serif",size=20,face = 'bold'))+
  xlim(0, 80) + ylim(0,310)


### Phosphorus bubble plot ####
i=6 #Phosphorus
for (j in 1:1){
  plot_data = get(sheet_names[i])
  plot_data$Birth.Weight = as.integer(plot_data$Birth.Weight)
  #birth weight wrong entries handled
  for (var in 1:length(plot_data$Birth.Weight)){
    if (plot_data$Birth.Weight[var]<5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*1000
    }
    if (plot_data$Birth.Weight[var]<50 & plot_data$Birth.Weight[var]>=5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*100
    }
  }
  
  ## Adding all columns that have to be put in model ##
  #GESTATION
  plot_data = add_column(plot_data, GESTATION = (plot_data$Gestation.Weeks + plot_data$Gestation.Days/7), .after = 4)
  
  #removing outliers
  
  ges_lessthan_32 = plot_data[which(plot_data$GESTATION < 32),]
  ges_32_34 = plot_data[which(plot_data$GESTATION >= 32 & plot_data$GESTATION <= 34),]
  ges_34_37 = plot_data[which(plot_data$GESTATION > 34 & plot_data$GESTATION <= 37),]
  ges_morethan_37 = plot_data[which(plot_data$GESTATION > 37),]  
  
  arr_of_means = c()
  arr_of_lengths = c()
  for (j in 1:length(data_name)){
    final_data = get(data_name[j])
    give_index = grep('Given', colnames(final_data))
    
    given = 0
    give_value = as.numeric(unlist(final_data[,give_index[1]]))
    give_value = na.exclude(give_value)
    arr_of_means[1] = mean(give_value)
    arr_of_lengths[1] = length(give_value)
    mean_of_cat = 0
    len = 0
    for (values in 2:199) {
      give_value = as.numeric(unlist(final_data[,give_index[values]]))
      give_value = na.exclude(give_value)
      mean_of_cat = mean(give_value)
      arr_of_means[values] = mean_of_cat
      len = length(give_value)
      arr_of_lengths[values] = len
    }
    Order.given = arr_of_means
    Number.of.patients = arr_of_lengths
    Gestation = rep(file_names[j], 199)
    DOL = c(1:199)
    assign(file_names[j], cbind(Gestation, DOL, Order.given, Number.of.patients))
  }
  input_for_bubble = c()
  input_for_bubble = rbind(first_32, second_32_34, third_34_37, fourth_37)
  input_for_bubble = as.data.frame(input_for_bubble)
  input_for_bubble$Gestation = as.character(input_for_bubble$Gestation)
  input_for_bubble$DOL = as.numeric(as.character(input_for_bubble$DOL))
  input_for_bubble$Order.given = as.numeric(as.character(input_for_bubble$Order.given))
  input_for_bubble$Number.of.patients = as.numeric(as.character(input_for_bubble$Number.of.patients))
}

input_for_bubble %>%
  arrange(desc(Number.of.patients)) %>%
  ggplot(aes(x=DOL, y=Order.given, size=Number.of.patients, color=Gestation,fill=Gestation)) +
  geom_point(alpha=0.8,shape=21, color="black") +
  theme_bw() + theme_ipsum() + ggtitle(sheet_names[i]) + 
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="I") +
  scale_size(range = c(.1, 24), name="Patients count") +
  theme(text=element_text(family="serif",size=20,face = 'bold'))+
  xlim(0, 80) + ylim(0,160)

### Iron bubble plot ####
i=7 #iron
for (j in 1:1){
  plot_data = get(sheet_names[i])
  plot_data$Birth.Weight = as.integer(plot_data$Birth.Weight)
  #birth weight wrong entries handled
  for (var in 1:length(plot_data$Birth.Weight)){
    if (plot_data$Birth.Weight[var]<5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*1000
    }
    if (plot_data$Birth.Weight[var]<50 & plot_data$Birth.Weight[var]>=5){
      plot_data$Birth.Weight[var] = plot_data$Birth.Weight[var]*100
    }
  }
  
  ## Adding all columns that have to be put in model ##
  #GESTATION
  plot_data = add_column(plot_data, GESTATION = (plot_data$Gestation.Weeks + plot_data$Gestation.Days/7), .after = 4)
  
  #removing outliers
  
  ges_lessthan_32 = plot_data[which(plot_data$GESTATION < 32),]
  ges_32_34 = plot_data[which(plot_data$GESTATION >= 32 & plot_data$GESTATION <= 34),]
  ges_34_37 = plot_data[which(plot_data$GESTATION > 34 & plot_data$GESTATION <= 37),]
  ges_morethan_37 = plot_data[which(plot_data$GESTATION > 37),]  
  
  arr_of_means = c()
  arr_of_lengths = c()
  for (j in 1:length(data_name)){
    final_data = get(data_name[j])
    give_index = grep('Given', colnames(final_data))
    
    given = 0
    give_value = as.numeric(unlist(final_data[,give_index[1]]))
    give_value = na.exclude(give_value)
    arr_of_means[1] = mean(give_value)
    arr_of_lengths[1] = length(give_value)
    mean_of_cat = 0
    len = 0
    for (values in 2:199) {
      give_value = as.numeric(unlist(final_data[,give_index[values]]))
      give_value = na.exclude(give_value)
      mean_of_cat = mean(give_value)
      arr_of_means[values] = mean_of_cat
      len = length(give_value)
      arr_of_lengths[values] = len
    }
    Order.given = arr_of_means
    Number.of.patients = arr_of_lengths
    Gestation = rep(file_names[j], 199)
    DOL = c(1:199)
    assign(file_names[j], cbind(Gestation, DOL, Order.given, Number.of.patients))
  }
  input_for_bubble = c()
  input_for_bubble = rbind(first_32, second_32_34, third_34_37, fourth_37)
  input_for_bubble = as.data.frame(input_for_bubble)
  input_for_bubble$Gestation = as.character(input_for_bubble$Gestation)
  input_for_bubble$DOL = as.numeric(as.character(input_for_bubble$DOL))
  input_for_bubble$Order.given = as.numeric(as.character(input_for_bubble$Order.given))
  input_for_bubble$Number.of.patients = as.numeric(as.character(input_for_bubble$Number.of.patients))
}

input_for_bubble %>%
  arrange(desc(Number.of.patients)) %>%
  ggplot(aes(x=DOL, y=Order.given, size=Number.of.patients, color=Gestation,fill=Gestation)) +
  geom_point(alpha=0.8,shape=21, color="black") +
  theme_bw() + theme_ipsum() + ggtitle(sheet_names[i]) + 
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="I") +
  scale_size(range = c(.1, 24), name="Patients count") +
  theme(text=element_text(family="serif",size=20,face = 'bold'))+
  xlim(0, 80) + ylim(0,6)

