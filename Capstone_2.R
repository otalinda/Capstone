library(plyr)
library(tidyverse)
library(caret)
library(randomForest)
library(ggplot2)
library(viridis)
library(ggthemes)
library(ggrepel)
library(stringr)
library(reshape)
library(rockchalk)
library(RColorBrewer)
library(kableExtra)
library(reshape2)
library(devtools)
library(scales)
library(gridExtra)
library(cowplot)
library(gtable)

############################################################################################
#Import data, assign column names, remove missing values#
#NOTE: Data is already partitionned into training and test sets to be downloaded separately#
############################################################################################

#TRAINING SET#
##############

#1)Import
raw_training <- read.table("/Users/grettadigbeu/Desktop/R Files/adult.data", header=FALSE, sep=",", na.strings = "?", strip.white=TRUE)

#2) Assign column names
colnames(raw_training) <- c("age", 
                            "workclass", 
                            "final_weight", 
                            "education_level", 
                            "education_num", 
                            "marital_status", 
                            "occupation", 
                            "relationship", 
                            "race", 
                            "sex", "capital_gain", 
                            "capital_loss", 
                            "hours_per_week", 
                            "native_country", 
                            "income_category")

#3) Remove NAs

training <-na.omit(raw_training)

head(training)
str(training)

#VALIDATION SET#
################

#1)Import
raw_test <- read.table("/Users/grettadigbeu/Desktop/R Files/adult.test", skip = 1, header=FALSE, sep=",", na.strings = "?", strip.white=TRUE)
head(raw_test)

#2)Assign column names
colnames(raw_test) <- c("age", 
                       "workclass", 
                       "final_weight", 
                       "education_level", 
                       "education_num", 
                       "marital_status", 
                       "occupation", 
                       "relationship", 
                       "race", 
                       "sex", 
                       "capital_gain", 
                       "capital_loss", 
                       "hours_per_week", 
                       "native_country", 
                       "income_category")

#3) Remove NAs
testing <- na.omit(raw_test)
str(testing)

length(is.na(testing))

#4) Fix income bracket levels so they're the same as in training set
#Access Gist function to display str output in tidy dataframe 
devtools::source_gist('4a0a5ab9fe7e1cf3be0e')
str1 <- strtable(training)
str2 <- strtable(testing)

levels(testing$income_category)[1] <- "<=50K"
levels(testing$income_category)[2] <- ">50K"

rm(raw_test, raw_training)

#5) Examine relationship between education_level and education_num variables

    t1 <- training %>% distinct(education_level, education_num) %>% arrange(education_num)
    t2 <- testing %>% distinct(education_level, education_num) %>% arrange(education_num)
    
    #Drop education_num feature from both sets
    
    training <- training %>% select(-education_num)
    testing <- testing %>% select(-education_num)

############################################################################################
#Explore the data#
#Identify trends and patterns that may require transforming the attributes#
#NOTE: USING ENTIRE DATASET# 
############################################################################################

#Bind rows to combine testing and training data
data_all <- rbind(training, testing)
str(data_all)


#Income Class Overall# 
#####################

share <- data_all %>%
  group_by(income_category) %>%
  tally() %>%
  mutate(pct = n / sum(n), percentage = paste0(round(pct*100, 1), "%"))

share %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

######################
#CATEGORICAL FEATURES#  
######################  

#1 - Workclass#
###############
  summary(data_all$workclass)

  #Remove unused level "never_worked" 
  data_all$workclass <- droplevels(data_all$workclass)
  summary(data_all$workclass)


  #Make the bar graph of frequencies 
  proportion0 <- data_all %>%
    group_by(workclass) %>%
    tally() %>%
    mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
  

  p0 <- proportion0 %>% 
    ggplot(aes(x = reorder(workclass, -pct), y = pct)) +
    geom_bar(stat = "identity", position = "dodge", fill = "lightgoldenrod1", color = "black", alpha=.9) +
    geom_text(aes(label = y_label), position = position_dodge(width=.9), vjust = -.25) +
    scale_y_continuous(labels = percent_format(accuracy = 1))+
    xlab("")+
    ylab("")+
    theme_clean()+
    ggtitle("Types of Employment") 
    

  #Make stacked bars
  proportion1 <- data_all %>%
    group_by(workclass, income_category) %>%
    tally() %>%
    group_by(workclass) %>%
    mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
  
  
  p1<- proportion1 %>% 
    ggplot(aes(y = pct, x = workclass, fill = income_category))+
    geom_bar(position = "fill", stat = "identity", alpha = .8) +
    scale_fill_viridis(discrete=TRUE, option = "viridis")+
    geom_text(aes(label = y_label), position = position_stack(vjust = .5), color = "black") +
    xlab("")+
    ylab("")+
    scale_y_continuous(labels = percent_format(accuracy = 1))+
    ggtitle("Breakdown of Income Class by Class of Worker") + 
    theme_clean()
  

  
#2 - Education Level#
#####################

  summary(data_all$education_level)
  
  
  #Make the bar graph of frequencies  
  p2 <- ggplot(data=data_all, aes(fct_infreq(factor(education_level)))) +
    geom_bar(color = "black", fill = "lightgoldenrod1", alpha=.9) +
    theme_clean()+
    labs(x = "", y = "Count") +
    ggtitle("Educational Attainment") + 
    scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                   scientific = FALSE))+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 5))

  #Lump educational attaiments of 12th grade and below into a single factor level
  
  data_all$education_level <- combineLevels(data_all$education_level, c('Preschool', '1st-4th', '5th-6th', '7th-8th', '9th', '10th', '11th', '12th'), newLabel = "12th and below")
                
  levels(data_all$education_level)
  
  #Make bar graph of frequencies with updated levels
 
  proportion3 <- data_all %>%
    group_by(education_level) %>%
    tally() %>%
    mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
  
  
  p3 <- proportion3 %>% 
    ggplot(aes(x = reorder(education_level, -pct), y = pct)) +
    geom_bar(stat = "identity", position = "dodge", fill = "lightgoldenrod1", color = "black", alpha=.9) +
    geom_text(aes(label = y_label), position = position_dodge(width=.9), vjust = -.25) +
    scale_y_continuous(labels = percent_format(accuracy = 1))+
    xlab("")+
    ylab("")+
    theme_clean()+
    ggtitle("Educational Attainment") 

  
  #Make stacked bars
  proportion4 <- data_all %>%
    group_by(education_level, income_category) %>%
    tally() %>%
    group_by(education_level) %>%
    mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
  
  
  p4 <- proportion4 %>% 
    ggplot(aes(y = pct, x = education_level, fill = income_category))+
    geom_bar(position = "fill", stat = "identity", alpha = .75) +
    scale_fill_viridis(discrete=TRUE, option = "viridis")+
    geom_text(aes(label = y_label), position = position_stack(vjust = .5), color = "black") +
    xlab("")+
    ylab("")+
    scale_y_continuous(labels = percent_format(accuracy = 1))+
    ggtitle("Breakdown of Income Class by Education Attainment") + 
    theme_clean()
  
  
  # 3 - Marital Status#
  #####################
  
  summary(data_all$marital_status)
  
  #Make bar chart
  proportion5 <- data_all %>%
    group_by(marital_status) %>%
    tally() %>%
    mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
  
  
  p5 <- proportion5 %>% 
    ggplot(aes(x = reorder(marital_status, -pct), y = pct)) +
    geom_bar(stat = "identity", position = "dodge", fill = "lightgoldenrod1", color = "black", alpha=.9) +
    geom_text(aes(label = y_label), position = position_dodge(width=.9), vjust = -.25) +
    scale_y_continuous(labels = percent_format(accuracy = 1))+
    xlab("")+
    ylab("")+
    theme_clean()+
    ggtitle("Marital Status") 
  
  
  #Make stacked bars
  proportion6 <- data_all %>%
    group_by(marital_status, income_category) %>%
    tally() %>%
    group_by(marital_status) %>%
    mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
  
  
  p6 <- proportion6 %>% 
    ggplot(aes(y = pct, x = marital_status, fill = income_category))+
    geom_bar(position = "fill", stat = "identity", alpha = .75) +
    scale_fill_viridis(discrete=TRUE, option = "viridis")+
    geom_text(aes(label = y_label), position = position_stack(vjust = .5), color = "black") +
    xlab("")+
    ylab("")+
    ggtitle("Breakdown of Income Class by Marital Status") + 
    scale_y_continuous(labels = percent_format(accuracy = 1))+
    theme_clean()
  

  # 4 - Occupation#
  #################
  summary(data_all$occupation)
  
  #Make bar chart
  proportion7 <- data_all %>%
    group_by(occupation) %>%
    tally() %>%
    mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
  
  
  p7 <- proportion7 %>% 
    ggplot(aes(x = reorder(occupation, -pct), y = pct)) +
    geom_bar(stat = "identity", position = "dodge", fill = "lightgoldenrod1", color = "black", alpha=.9) +
    geom_text(aes(label = y_label), position = position_dodge(width=.9), vjust = -.25) +
    scale_y_continuous(labels = percent_format(accuracy = 1))+
    xlab("")+
    ylab("")+
    theme_clean()+
    ggtitle("Type of Occupation") 
  
  
  #Make stacked bars
  proportion8 <- data_all %>%
    group_by(occupation, income_category) %>%
    tally() %>%
    group_by(occupation) %>%
    mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
  
  
  p8 <- proportion8 %>% 
    ggplot(aes(y = pct, x = occupation, fill = income_category))+
    geom_bar(position = "fill", stat = "identity", alpha = .75) +
    scale_fill_viridis(discrete=TRUE, option = "viridis")+
    geom_text(aes(label = y_label), position = position_stack(vjust = .5), color = "black") +
    xlab("")+
    ylab("")+
    ggtitle("Breakdown of Income Class by Type of Occupation") + 
    scale_y_continuous(labels = percent_format(accuracy = 1))+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 4))
    theme_clean()
  
    
    #5 - Sex#
    #########
    
    summary(data_all$sex)
    
    #Make bar chart
    proportion9 <- data_all %>%
      group_by(sex) %>%
      tally() %>%
      mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
    
    
    p9 <- proportion9 %>% 
      ggplot(aes(x = reorder(sex, -pct), y = pct)) +
      geom_bar(stat = "identity", position = "dodge", fill = "lightgoldenrod1", color = "black", alpha=.9) +
      geom_text(aes(label = y_label), position = position_dodge(width=.9), vjust = -.25) +
      scale_y_continuous(labels = percent_format(accuracy = 1))+
      xlab("")+
      ylab("")+
      theme_clean()+
      ggtitle("Sex") 
    
    #Make stacked bars
    proportion10 <- data_all %>%
      group_by(sex, income_category) %>%
      tally() %>%
      group_by(sex) %>%
      mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
    
    
    p10 <- proportion10%>% 
      ggplot(aes(y = pct, x = sex, fill = income_category))+
      geom_bar(position = "fill", stat = "identity", alpha = .75) +
      scale_fill_viridis(discrete=TRUE, option = "viridis")+
      geom_text(aes(label = y_label), position = position_stack(vjust = .5), color = "black") +
      xlab("")+
      ylab("")+
      ggtitle("Breakdown of Income Class by Sex") + 
      scale_y_continuous(labels = percent_format(accuracy = 1))+
      theme_clean()
    
    
  # 6 - Relationship#
  ###################
    summary(data_all$relationship) %>%
      kable() %>% 
      kable_styling(bootstrap_options = c("striped", "hover"))
  
  
  #7 - Race#
  #########
  
  summary(data_all$race)
  
  #Make bar chart
  proportion11 <- data_all %>%
    group_by(race) %>%
    tally() %>%
    mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
  
  
  p11 <- proportion11 %>% 
    ggplot(aes(x = reorder(race, -pct), y = pct)) +
    geom_bar(stat = "identity", position = "dodge", fill = "lightgoldenrod1", color = "black", alpha=.9) +
    geom_text(aes(label = y_label), position = position_dodge(width=.9), vjust = -.25) +
    scale_y_continuous(labels = percent_format(accuracy = 1))+
    xlab("")+
    ylab("")+
    theme_clean()+
    ggtitle("Race/Ethnic Origin") 
  
  
  #Make stacked bars
  proportion12 <- data_all %>%
    group_by(race, income_category) %>%
    tally() %>%
    group_by(race) %>%
    mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
  
  
  p12 <- proportion12 %>% 
    ggplot(aes(y = pct, x = race, fill = income_category))+
    geom_bar(position = "fill", stat = "identity", alpha = .75) +
    scale_fill_viridis(discrete=TRUE, option = "viridis")+
    geom_text(aes(label = y_label), position = position_stack(vjust = .5), color = "black") +
    xlab("")+
    ylab("")+
    ggtitle("Breakdown of Income Class by Race") + 
    scale_y_continuous(labels = percent_format(accuracy = 1))+
    theme_clean()
  
  
  #8 - Native Country#
  ###################
  
  summary(data_all$native_country)
  levels(data_all$native_country)
  
    #Collapse native countries into regions and assign them to new variable native_region
    
    Asia <- c("Cambodia", "India", "Laos", "Thailand", "Vietnam", "Hong", "Iran", "China", "Japan", "Philippines", "Taiwan")
      
    Europe <- c("France", "Italy", "Poland", "Scotland", "Germany", "Portugal", "Yugoslavia", "England", "Greece", "Holand-Netherlands", "Hungary", "Ireland")
    
    North_America <- c("Outlying-US(Guam-USVI-etc)", "Canada", "United-States", "Puerto-Rico")
    
    Latin_America_Carrib <- c("Columbia", "Ecuador", "Guatemala", "Honduras", "Cuba", "El-Salvador", "Haiti", "Jamaica", "Mexico", "Peru", "Trinadad&Tobago", "Dominican-Republic", "Nicaragua")
    
    Unknown <- c("South")
    
    data_all <- data_all %>% mutate(native_region = case_when(native_country %in% Asia ~ "Asia",
                                   native_country %in% Europe ~ "Europe", 
                                   native_country %in% North_America ~ "North_America",
                                   native_country %in% Latin_America_Carrib ~ "Latin_America_Carrib",
                                   native_country %in% Unknown ~ "Unknown"))
    
    #Check that all levels are assigned a region
    
    length(which(data_all$native_region == ""))
    
    #Convert to factor
    
    data_all$native_region <- as.factor(data_all$native_region)
    
    levels(data_all$native_region)
    
    
    #Make bar chart
    proportion13 <- data_all %>%
      group_by(native_region) %>%
      tally() %>%
      mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 0), "%"))
    
    
    p13 <- proportion13 %>% 
      ggplot(aes(x = reorder(native_region, -pct), y = pct)) +
      geom_bar(stat = "identity", position = "dodge", fill = "lightgoldenrod1", color = "black", alpha=.9) +
      geom_text(aes(label = y_label), position = position_dodge(width=.9), vjust = -.25) +
      scale_y_continuous(labels = percent_format(accuracy = 1))+
      xlab("")+
      ylab("")+
      theme_clean()+
      ggtitle("Native Region") 

   
    #Make stacked bars
    proportion14 <- data_all %>%
      group_by(native_region, income_category) %>%
      tally() %>%
      group_by(native_region) %>%
      mutate(pct = n / sum(n), y_label = paste0(round(pct*100, 1), "%"))
    
    
    p14 <- proportion14 %>% 
      ggplot(aes(y = pct, x = native_region, fill = income_category))+
      geom_bar(position = "fill", stat = "identity", alpha = .75) +
      scale_fill_viridis(discrete=TRUE, option = "viridis")+
      geom_text(aes(label = y_label), position = position_stack(vjust = .5), color = "black") +
      xlab("")+
      ylab("")+
      ggtitle("Breakdown of Income Class by Native Region") + 
      scale_y_continuous(labels = percent_format(accuracy = 1))+
      theme_clean()
    
  
#####################
#CONTINUOUS FEATURES#  
#####################  
 
   #Age#
  #####
  
  summary(data_all$age)
  
  #Make the histogram
  p13 <- data_all %>% ggplot(aes(age)) +
    geom_histogram(binwidth = 1, color = "black", fill = "purple4", alpha=.95) +
    theme_clean()+
    labs(x = "", y = "Count") +
    ggtitle("Age Distribution") 
  
  
  #Make box plots
  p14 <- data_all %>% ggplot(aes(income_category, age, fill = income_category)) +
    geom_boxplot(alpha = .8) +
    xlab("")+
    ylab("Age")+
    ggtitle("Age Distribution by Income Category")+
    scale_fill_viridis(discrete=TRUE, option = "viridis")+
    theme_clean()  
  
  #Make density plot
  p15 <- data_all %>% ggplot(aes(x = age, fill = income_category)) +
    geom_density(position = "identity", alpha = .6) +
    ggtitle("Age Density by Income Category")+
    scale_fill_viridis(discrete=TRUE, option = "viridis")+
    xlab("")+
    ylab("Density")+
    theme_clean()


#Capital Gain & Loss#
#################### 
  
  summary(data_all$capital_loss)
  summary(data_all$capital_gain)
  
  p16 <- data_all %>% ggplot(aes(x = capital_loss)) +
    geom_density(fill = "gold", alpha=.95) +
    theme_clean()+
    labs(x = "", y = "Density") +
    ggtitle("Distribution of Capital Loss") 
  
  
  p17 <- data_all %>% ggplot(aes(x = capital_gain)) +
    geom_density(fill = "gold", alpha=.95) +
    theme_clean()+
    labs(x = "", y = "Density") +
    ggtitle("Distribution of Capital Gain") 
  
    #Compute proportion of values equal to zero
  
    data_all %>% filter(capital_gain == 0) %>% summarise(value = 0, prevalence = n()/45222*100)
    data_all %>% filter(capital_loss == 0) %>% summarise(value = 0, prevalence = n()/45222*100)
    
    #Look at distribution of non-zero values
    
    q_gain <- quantile(x = subset(data_all$capital_gain, data_all$capital_gain != 0), probs = seq(0, 1, .25))
    q_loss <- quantile(x = subset(data_all$capital_loss, data_all$capital_loss != 0), probs = seq(0, 1, .25))
    
    quantiles <- data.frame(Capital_Gain = q_gain, Capital_Loss = q_loss)
    
    IQR(q_gain)
    IQR(q_loss)
    
    pos_gain <- subset(data_all, capital_gain != 0)
    neg_loss <- subset(data_all, capital_loss != 0)
    
    p18 <- pos_gain %>% ggplot(aes(capital_gain)) +
      geom_histogram(binwidth = 2500, color = "black", fill = "purple4", alpha=.95) +
      theme_clean()+
      labs(x = "", y = "Count") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(breaks = seq(0,1000,50)) +
      ggtitle("Histogram of Non-Zero Capital Gain") 
    
    p19 <- neg_loss %>% ggplot(aes(capital_loss)) +
      geom_histogram(binwidth = 200, color = "black", fill = "purple4", alpha=.95) +
      theme_clean()+
      labs(x = "", y = "Count") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(breaks = seq(0,1000,50)) +
      ggtitle("Histogram of Non-Zero Capital Loss") 
    
   p18
   p19  
   
   p20 <- pos_gain %>% ggplot(aes(x = factor(0), y = capital_gain)) +
     geom_boxplot(alpha = .8, fill = "gold") +
     xlab("")+
     ylab("Capital Gain")+
     ggtitle("Boxplot of Non-Zero Capital Gain") +
     scale_y_continuous(breaks = seq(0, 100000, 5000)) +
     stat_summary(fun.y = mean, 
                  geom = 'point', 
                  shape = 19,
                  color = "red",
                  cex = 2) +
     scale_fill_viridis()+
     theme_clean()  
   
   p21 <- neg_loss %>% ggplot(aes(x = factor(0), y = capital_loss)) +
     geom_boxplot(alpha = .8, fill = "darkorchid4") +
     xlab("")+
     ylab("Capital Loss") +
     scale_y_continuous(breaks = seq(0, 5000, 500)) +
     stat_summary(fun.y = mean, 
                  geom = 'point', 
                  shape = 19,
                  color = "red",
                  cex = 2) + 
     ggtitle("Boxplot of Non-Zero Capital Loss") +
     theme_clean()  
   
    p20
    p21
    #Create bins of values for capital_loss and capital_gain attributes 
    
    
    data_all <- data_all %>% 
      mutate(cap_gain = ifelse(capital_gain <= 3464, " Low",
                               ifelse(capital_gain > 3464 & capital_gain < 14084, " Medium", " High")))
    
    
    data_all$cap_gain <- factor(data_all$cap_gain,
                                ordered = TRUE,
                                levels = c(" Low", " Medium", " High"))
    
    data_all <- data_all %>% 
      mutate(cap_loss = ifelse(capital_loss <= 1672, " Low",
                               ifelse(capital_gain > 1672 & capital_gain < 1977, " Medium", " High")))
    
    
    data_all$cap_loss <- factor(data_all$cap_loss,
                                ordered = TRUE,
                                levels = c(" Low", " Medium", " High"))
    
    
  
  #####################################
  #Explore Associations Among Features#
  #####################################
  
  #1)Continuous Variables
  
  #Remove the final_weight, and outcome variable from the dataset
  cor_vars <- subset(data_all, select = -c(final_weight,
                                           income_category))
  
  #Convert all factor levels to numeric
  cor_vars$workclass <- as.numeric(cor_vars$workclass)
  cor_vars$education_level <- as.numeric(cor_vars$education_level)
  cor_vars$marital_status <- as.numeric(cor_vars$marital_status)
  cor_vars$occupation <- as.numeric(cor_vars$occupation)
  cor_vars$relationship <- as.numeric(cor_vars$relationship)
  cor_vars$race <- as.numeric(cor_vars$race)
  cor_vars$sex <- as.numeric(cor_vars$sex)
  cor_vars$native_country <- as.numeric(cor_vars$native_country)
  
  
  #Compute the correlations and create the correlation matrix, then heatmap
  cordata <- round(cor(cor_vars),2)
  melted_cordata <- melt(cordata)
  head(melted_cordata)
  
  melted_cordata %>% ggplot(aes(x=X1, y=X2, fill=value, label=value)) +
    geom_tile() +
    scale_fill_viridis() +
    xlab("") +
    ylab("") + 
    geom_text(color = "white") +
    ggtitle("Correlation Matrix of Features - Excluding Weight Variable")
  
  
  