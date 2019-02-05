# analysis.R file

# load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(caTools)
library(rpart)
library(caret)
library(car)

# read data
data <- read.csv("data/diabetic_data_original.csv", header=TRUE, 
                 na.strings = c("NA","na",""," ","?"), stringsAsFactors = FALSE)

# View the dataset
# View(data)


## Checking structure of data
str(data)
#  observations of  variables
summary(data)


# Check for NA
sapply(data, function(x) sum(is.na(x))) 
# NAs noted in some variables; weight 98569, race 2273, payer code 40256, medical speciality 49949, diag_1 21, diag_1 358, diag_3 1423 

# View the column names
colnames(data)

# Check variable
# encounter_id
length(unique(data$encounter_id))
# 101766 variable, no duplicate
 #  remove encounterid
data <- subset(data, select=-c(encounter_id))

# patient_nbr
table(data$patient_nbr)
# lots of entries and few repeats as patient comes more than once for different aliments. No good contribution to the study.
 #  remove unwanted variable
Patient_Id <- data$patient_nbr
data <- subset(data, select=-c(patient_nbr))

# race
table(data$race)
# AfricanAmerican: 19210, Asian: 641, Caucasian: 76099, Hispanic: 2037, Other: 1506 
class(data$race)
data$race <- as.factor(data$race)
class(data$race)


# gender
table(data$gender)
#Female : 54708, Male: 47055, Unknown/Invalid: 3
class(data$gender)
data$gender <- replace(data$gender, data$gender == "Unknown/Invalid", NA)
table(data$gender)
#convert gender to as.numeric
summary(factor(data$gender))
# convert to factor
data$gender <- as.factor(data$gender)
# give values 1 and 0 to the factors
levels(data$gender)<-c(1,0)
#convert to as.numeric
data$gender <- as.numeric(levels(data$gender))[data$gender]
summary(data$gender)

# Age
table(data$age)
# [0-10):161,[10-20):691,[20-30):1657,[30-40):9685,[40-50):9685,[50-60):17256,[60-70):22483,[70-80):26068,[80-90):17197,[90-100):2793 
data$age <- replace(data$age, data$age == "[0-10)", "1")
data$age <- replace(data$age, data$age == "[10-20)", "2")
data$age <- replace(data$age, data$age == "[20-30)","3")
data$age <- replace(data$age, data$age == "[30-40)","3")
data$age <- replace(data$age, data$age == "[40-50)", "4")
data$age <- replace(data$age, data$age == "[50-60)", "4")
data$age <- replace(data$age, data$age == "[60-70)", "5")
data$age <- replace(data$age, data$age == "[70-80)", "6")
data$age <- replace(data$age, data$age == "[80-90)", "7")
data$age <- replace(data$age, data$age == "[90-100)", "7")
table(data$age)
# agegroup1:161, agegroup2:691, agegroup3:5432, agegroup4:26941, agegroup5:22483, agegroup6:26068, agegroup7:19990
ggplot(data, aes(data$age)) + geom_bar()
ggsave("figs/age_barplot.png")
data$age <- as.factor(data$age)


# weight
table(data$weight)
# [0-25):48,[100-125):625,[125-150):145,[150-175):35,[175-200):11,[25-50):97,[50-75):897,[75-100):1336,>200:3 
summary(data$weight)
# length: 101766
sum(is.na(data$weight))
# 98569/101766: 0.9685848

# Checking if the data is unbalanced.
ggplot(data, aes(data$weight)) + geom_bar()
ggsave("figs/weight.png")
# It is unbalanced
#  remove unwanted variable
data <- subset(data, select=-c(weight))

#admission_type_id
table(data$admission_type_id)
#1:-53990,2:-18480,3:-18869,4:-10,5:-4785,6:-5291,7:-21,8:-320
data$admission_type_id <- replace(data$admission_type_id, data$admission_type_id == 6, "5")
data$admission_type_id <- replace(data$admission_type_id, data$admission_type_id == 8, "5")
data$admission_type_id <- replace(data$admission_type_id, data$admission_type_id == 2, "1")
table(data$admission_type_id)
# 1:-72470,3:-18869,4:-10,5:-10396,7:-21
data$admission_type_id <- as.factor(data$admission_type_id)


# discharge_disposition_id 1 8 16 17 27 30 
table(data$discharge_disposition_id)
# 1:-60234,2:-2128,3:-13954,4:-815,5:-1184,6:-12902,7:-623,8:-108,9:-21,10:-6,11:-1642,12:-3,13:-399,14:-372,15:-63,16:-11,17:-14,18:-3691,19:-8,20:-2,
#22:-1993,23:-412,24:-48,25:-989,27:-5,28:-139
# Few are grouped together
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 1,"group1")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 8,"group1")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 16,"group1")
data$discharge_disposition_id <- replace(data$discharge_disposition_id,data$discharge_disposition_id == 17,"group1")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 27,"group1")
data$discharge_disposition_id <- replace(data$discharge_disposition_id,data$discharge_disposition_id == 30, "group1")

# 2 3 4 5 6 9 10 13 14 15 22 23 24 28 29 # Few are grouped together  
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 2, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 3, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 4, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 5, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 6, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 9, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 10, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 13, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 14, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 15, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 22, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 23, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 24, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 28, "group2")
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 29, "group2")

   
   
# 7 12 18 25 26 # Few are grouped together reducing the factor levels 
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 7, "group3") 
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 12, "group3") 
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 18, "group3") 
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 25, "group3") 
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 26, "group3")   

# 11 19 20 21# Few are grouped together
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 11, "group4")  
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 19, "group4")  
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 20, "group4")  
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == 21, "group4")  

table(data$discharge_disposition_id)

# group1;-60372, group2:-34436, group3:-5306, group4:-1652  # Few are grouped together
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == "group1", "1") 
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == "group2", "2") 
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == "group3", "3") 
data$discharge_disposition_id <- replace(data$discharge_disposition_id, data$discharge_disposition_id == "group4", "4") 
table(data$discharge_disposition_id)
#1=60372, 2=34436,  3=5306,  4=1652 records 
summary(data$discharge_disposition_id)
data$discharge_disposition_id <- as.factor(data$discharge_disposition_id)


# admission_source_id
table(data$admission_source_id)
#1:-29565,2:-1104,3:-187,4:3187,5:-855,6:-2264,7:-57494,8:-16,9:-125,10:-8,11:-2,13:-1,14:-2,17:-6781,20:-161,22:-12,25:-2
ggplot(data, aes(data$admission_source_id)) + geom_bar()
# 1,2,3,8 # Few are grouped together
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 1, "group1")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 2, "group1")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 3, "group1")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 8, "group1")

# 4, 5, 6, 7, 10,18, 19 ,22, 25, 26 # Few are grouped together
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 4, "group2")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 5, "group2")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 6, "group2")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 7, "group2")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 10, "group2")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 18, "group2")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 19, "group2")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 22, "group2")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 25, "group2")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 26, "group2")

   
   
# 9, 15, 17, 20, 21 # Few are grouped together
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 9, "group3")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 15, "group3")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 17, "group3")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 20, "group3")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 21, "group3")

 
# 11, 12, 13, 14, 23, 24 # Few are grouped together
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 11, "group4")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 12, "group4")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 13, "group4")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 14, "group4")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 23, "group4")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == 24, "group4")
table(data$admission_source_id)
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == "group1", "1")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == "group2", "2")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == "group3", "3")
data$admission_source_id <- replace(data$admission_source_id, data$admission_source_id == "group4", "4")
table(data$admission_source_id)
#1:-30872, 2:-63822,  3:-7067,  4:-5
data$admission_source_id <- as.factor(data$admission_source_id)

# time_in_hospital
table(data$time_in_hospital)
ggplot(data, aes(data$time_in_hospital)) + geom_bar()
ggsave("figs/timeinhospital.png")
str(data$time_in_hospital)


# Payercode
ggplot(data, aes(data$payer_code)) + geom_bar()
 #  remove unwanted variable, not helpful as not patient disease info
data <- subset(data, select=-c(payer_code))

# medical_specialty
table(data$medical_specialty)
ggplot(data, aes(data$medical_specialty)) + geom_bar()
ggsave("figs/medspeciality.png")
#  remove unwanted variable
data <- subset(data, select=-c(medical_specialty))

ggplot(data, aes(data$num_lab_procedures)) + geom_bar()
ggsave("figs/labnormal.png")
quantile(data$num_lab_procedures, seq(0,1, 0.01))
data$num_lab_procedures[data$num_lab_procedures > quantile(data$num_lab_procedures, 0.99)] <- 
  quantile(data$num_lab_procedures, 0.99)
ggplot(data, aes(data$num_lab_procedures)) + geom_bar()
ggsave("figs/lab_OutlierTreated.png")

ggplot(data, aes(data$num_procedures)) + geom_bar()
ggsave("figs/procedure.png")

ggplot(data, aes(data$num_medications)) + geom_bar()
ggsave("figs/Medication.png")
quantile(data$num_medications, seq(0,1, 0.01))
data$num_medications[data$num_medications > quantile(data$num_medications, 0.99)] <- 
  quantile(data$num_medications, 0.99)
ggplot(data, aes(data$num_medications)) + geom_bar()
ggsave("figs/medication_OutlierTreated.png")

ggplot(data, aes(data$number_outpatient)) + geom_bar()
ggsave("figs/Outpatient.png")

ggplot(data, aes(data$number_emergency)) + geom_bar()
ggsave("figs/emergency.png")
ggplot(data, aes(data$number_inpatient)) + geom_bar()
ggsave("figs/inpatient.png")

ggplot(data, aes(data$number_diagnoses)) + geom_bar()
ggsave("figs/diagnosis.png")

ggplot(data, aes(data$max_glu_serum)) + geom_bar()
ggsave("figs/glu_serum.png")

# A1Cresult compensate for this column
#  remove unwanted variable
data <- subset(data, select=-c(max_glu_serum))
#A1Cresult
ggplot(data, aes(data$A1Cresult)) + geom_bar()
ggsave("figs/A1C.png")
data$A1Cresult <- as.factor(data$A1Cresult)

#  remove unwanted variable

data <- subset(data, select=-c(metformin,repaglinide,nateglinide,chlorpropamide,glimepiride,acetohexamide,
                               glipizide,glyburide,tolbutamide,pioglitazone,
                               rosiglitazone,acarbose,miglitol,troglitazone,tolazamide,examide,citoglipton,
                               glyburide.metformin,glipizide.metformin,glimepiride.pioglitazone,
                               metformin.rosiglitazone,metformin.pioglitazone))

#View(data)

#change
ggplot(data, aes(data$change)) + geom_bar()
ggsave("figs/change.png")
data$change <- replace(data$change, data$change == "Ch", "1")
data$change <- replace(data$change, data$change == "No", "0")
# convert to factor
summary(factor(data$change))
data$change <- as.numeric(data$change)
str(data$change)

#diabetesMed
ggplot(data, aes(data$diabetesMed)) + geom_bar()
ggsave("figs/diaMed.png")
data$diabetesMed <- replace(data$diabetesMed, data$diabetesMed == "Yes", "1")
data$diabetesMed <- replace(data$diabetesMed, data$diabetesMed == "No", "0")
#convert gender to as.numeric
summary(factor(data$diabetesMed))
data$diabetesMed <- as.numeric(data$diabetesMed)
str(data$diabetesMed)

#readmitted
ggplot(data, aes(data$readmitted)) + geom_bar()
ggsave("figs/readmitted.png")
#data$readmitted <- replace(data$readmitted, data$readmitted == "NO", "0")
data$readmitted <- replace(data$readmitted, data$readmitted == "<30", "YES")
data$readmitted <- replace(data$readmitted, data$readmitted == ">30", "YES")
ggplot(data, aes(data$readmitted)) + geom_bar()
ggsave("figs/readmitted1.png")
#convert gender to as.numeric
summary(factor(data$readmitted))
#data$readmitted <- as.numeric(data$readmitted)
str(data$readmitted)
#View(data)


# feature engineering -------------------------------------------------------------------
## diab_code <- 250.xx
## circulatory_code <- 390-459, 785

diagnosis <- data.frame(diag1 = as.character(data$diag_1),
                        diag2 = as.character(data$diag_2),
                        diag3 = as.character(data$diag_3),
                        stringsAsFactors = F)
str(diagnosis)
diagnosis[is.na(diagnosis) == T] <- 0

make_comorbidity <- function(x){
  
  comorbidity <- character(length = nrow(data))
  diabetes_code <- "^[2][5][0]"
  circulatory_code <- "^[3][9][0-9]|^[4][0-5][0-9]"
  
  if((str_detect(x[1], diabetes_code) == F & (str_detect(x[2], circulatory_code) == F & str_detect(x[3], circulatory_code) == F)) |
     (str_detect(x[2], diabetes_code) == F & (str_detect(x[1], circulatory_code) == F & str_detect(x[3], circulatory_code) == F)) |
     (str_detect(x[3], diabetes_code) == F & (str_detect(x[1], circulatory_code) == F & str_detect(x[2], circulatory_code) == F))){
    comorbidity <- 0
  }
  
  if((str_detect(x[1], diabetes_code) == T & (str_detect(x[2], circulatory_code) == F & str_detect(x[3], circulatory_code) == F)) |
     (str_detect(x[2], diabetes_code) == T & (str_detect(x[1], circulatory_code) == F & str_detect(x[3], circulatory_code) == F)) |
     (str_detect(x[3], diabetes_code) == T & (str_detect(x[1], circulatory_code) == F & str_detect(x[2], circulatory_code) == F))){
    comorbidity <- 1
  }
  
  if((str_detect(x[1], circulatory_code) == T & (str_detect(x[2], diabetes_code) == F & str_detect(x[3], diabetes_code) == F)) |
     (str_detect(x[2], circulatory_code) == T & (str_detect(x[1], diabetes_code) == F & str_detect(x[3], diabetes_code) == F)) |
     (str_detect(x[3], circulatory_code) == T & (str_detect(x[1], diabetes_code) == F & str_detect(x[2], diabetes_code) == F))){
    comorbidity <- 2
  }
  
  if((str_detect(x[1], diabetes_code) == T & (str_detect(x[2], circulatory_code) == T | str_detect(x[3], circulatory_code) == T)) |
     (str_detect(x[2], diabetes_code) == T & (str_detect(x[1], circulatory_code) == T | str_detect(x[3], circulatory_code) == T)) |
     (str_detect(x[3], diabetes_code) == T & (str_detect(x[1], circulatory_code) == T | str_detect(x[2], circulatory_code) == T))){
    comorbidity <- 3
  }
  comorbidity <- ordered(comorbidity, levels = c(0,1,2,3))
  return(comorbidity)
}
data$comorbidity <- apply(diagnosis, 1, make_comorbidity)

# remove diagnosis -----------------------------------------------------------------
data$diag_1 <- NULL
data$diag_2 <- NULL
data$diag_3 <- NULL

# comorbidity and readmission --------------------------------------------------------
prop.table(table(data$readmitted, data$comorbidity), margin = 2)

# analyse comorbidity
ggplot(data, aes(x=comorbidity, y=number_inpatient)) + geom_bar(stat = "identity")
ggsave("figs/comorbidity_inpatient.png")

ggplot(data, aes(x=comorbidity, y=number_outpatient)) + geom_bar(stat = "identity")
ggsave("figs/comorbidity_Outpatient.png")

ggplot(data, aes(x=comorbidity, y=number_emergency)) + geom_bar(stat = "identity")
ggsave("figs/comorbidity_emergency.png")

tapply(data$number_inpatient, data$comorbidity, mean)
tapply(data$number_emergency, data$comorbidity, mean)
tapply(data$number_outpatient, data$comorbidity, mean)

tapply(data$time_in_hospital, data$comorbidity, mean)
tapply(data$number_diagnoses, data$comorbidity, mean)
tapply(data$num_lab_procedures, data$comorbidity, mean)
tapply(data$num_procedures, data$comorbidity, mean)
tapply(data$num_medications, data$comorbidity, mean)

# Analyse A1Cresult
ggplot(data, aes(x=A1Cresult, y=number_inpatient)) + geom_bar(stat = "identity")
ggsave("figs/A1C_inpatient.png")
ggplot(data, aes(x=A1Cresult, y=number_outpatient)) + geom_bar(stat = "identity")
ggsave("figs/A1C_outpatient.png")
ggplot(data, aes(x=A1Cresult, y=number_emergency)) + geom_bar(stat = "identity")
ggsave("figs/A1C_emergency.png")

tapply(data$number_inpatient, data$A1Cresult, mean)
tapply(data$number_outpatient, data$A1Cresult, mean)
tapply(data$number_emergency, data$A1Cresult, mean)

#Missing values
sapply(data, function(x) sum(is.na(x)))

#Percentage of missing values
colMeans(is.na(data))

# Removing NAs
# before removing NAs patient ID is joined to the data
data <- cbind(Patient_Id, data)
data<-na.omit(data)
ID <- data$Patient_Id
data <- data[, 2:21]

# race
#creating dummy variable for race variable    
dummy_race <- model.matrix(~race - 1,data=data)
# Removing the 1st dummy variable
dummy_race <- dummy_race[,-1]
# Adding dummy variable of race to data and removing race column
data <- cbind(data[,-1], dummy_race)

#age
#creating dummy variable for age variable    
dummy_age <- model.matrix(~age - 1,data=data)
# Removing the 1st dummy variable
dummy_age <- dummy_age[,-1]
# Adding dummy variable of age to data and removing age column
data <- cbind(data[,-2], dummy_age)

#admission_type_id
#creating dummy variable for admission_type_id variable    
dummy_admission_type_id <- model.matrix(~admission_type_id - 1,data=data)
# Removing the 1st dummy variable
dummy_admission_type_id <- dummy_admission_type_id[,-1]
# Adding dummy variable of admission_type_id to data and removing admission_type_id column
data <- cbind(data[,-2], dummy_admission_type_id)

#discharge_disposition_id
#creating dummy variable for discharge_disposition_id variable    
dummy_discharge_disposition_id <- model.matrix(~discharge_disposition_id - 1,data=data)
# Removing the 1st dummy variable
dummy_discharge_disposition_id <- dummy_discharge_disposition_id[,-1]
# Adding dummy variable of discharge_disposition_id to data and removing discharge_disposition_id column
data <- cbind(data[,-2], dummy_discharge_disposition_id)

#admission_source_id
#creating dummy variable for admission_source_id variable    
dummy_admission_source_id <- model.matrix(~admission_source_id - 1,data=data)
# Removing the 1st dummy variable
dummy_admission_source_id <- dummy_admission_source_id[,-1]
# Adding dummy variable of admission_source_id to data and removing admission_source_id column
data <- cbind(data[,-2], dummy_admission_source_id)

#A1Cresult
#creating dummy variable for A1Cresult variable    
dummy_A1Cresult <- model.matrix(~A1Cresult - 1,data=data)
# Removing the 1st dummy variable
dummy_A1Cresult <- dummy_A1Cresult[,-1]
# Adding dummy variable of A1Cresult to data and removing A1Cresult column
data <- cbind(data[,-10], dummy_A1Cresult)

#insulin
#creating dummy variable for insulin variable    
dummy_insulin <- model.matrix(~insulin - 1,data=data)
# Removing the 1st dummy variable
dummy_insulin <- dummy_insulin[,-1]
# Adding dummy variable of insulin to data and removing insulin column
data <- cbind(data[,-10], dummy_insulin)

#comorbidity
#creating dummy variable for comorbidity variable    
dummy_comorbidity <- model.matrix(~comorbidity - 1,data=data)
# Removing the 1st dummy variable
dummy_comorbidity <- dummy_comorbidity[,-1]
# Adding dummy variable of race to data and removing race column
data <- cbind(data[,-13], dummy_comorbidity)

# Feature standardisation

# Normalising continuous features, scaling

data$time_in_hospital <- scale(data$time_in_hospital)
data$num_lab_procedures <- scale(data$num_lab_procedures)
data$num_procedures <- scale(data$num_procedures)
data$num_medications <- scale(data$num_medications)
data$number_outpatient <- scale(data$number_outpatient)
data$number_emergency <- scale(data$number_emergency)
data$number_inpatient <- scale(data$number_inpatient)
data$number_diagnoses <- scale(data$number_diagnoses)

#View(data)
  
# Readmitted is separated and added to the first column in the dataset and removed the older column
readmitted <- data$readmitted
data <- cbind(readmitted, data)
data <- data[,-13]

str(data)
# All are numerical variable. Data is cleaned to arrive at 41 variables.

# join the ID of patients
data <- cbind(ID, data)


# write the cleaned data
write.csv(data, "data/data_cleaned.csv", row.names = FALSE, quote= FALSE)

########################################################################

# Load the cleaned data
#load("rdas/data_cleaned.rda")
#data <- data_cleaned
library(MASS)

data <- read.csv("data/data_cleaned.csv", header=TRUE, 
                 na.strings = c("NA","na",""," ","?"), stringsAsFactors = FALSE)

# save the ID of patients
ID <- data$ID

# Remove the ID before analysis
data <- data[2:42]

#data$readmitted[data$readmitted == "YES"] <- "1"
#data$readmitted[data$readmitted == "NO"] <- "0"
table(data$readmitted)
data$readmitted <- as.factor(data$readmitted)

# splitting the data between train and test
library(caTools)
set.seed(100)

indices = sample.split(data$readmitted, SplitRatio = 0.7)

train = data[indices,]

test = data[!(indices),]
########################################################################
# Logistic Regression: 

#Initial model
model_1 <- glm(readmitted ~ ., data = train, family = "binomial")
summary(model_1)

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
library(car)
sort(vif(model_2))

 # remove admission source id 4
# removing  discharge_disposition_id3
                
model_16 <- glm(formula = readmitted ~ gender + time_in_hospital + num_lab_procedures + 
                  num_procedures + number_outpatient + number_emergency + number_inpatient + 
                  number_diagnoses + diabetesMed +
                  raceOther + age5 + age6 + admission_type_id3 + 
                  admission_type_id5 +
                  admission_source_id3 + 
                  A1CresultNone + insulinSteady + 
                  comorbidity1 + comorbidity2 + comorbidity3, family = "binomial", 
                data = train)
               
summary(model_16)
sort(vif(model_16))


########################################################################
 # Final Model With only significant variables in the model
 
 final_model <- model_16
 ########################################################################
 
 #######################################################################
 
 ### Model Evaluation
 
 ### Test Data ####
 
 #predicted probabilities of readmitted for test data
 
 test_pred = predict(final_model, type = "response", 
                     newdata = test)
 
 
 # Let's see the summary 
 
 summary(test_pred)
 
 # Let's use the probability cutoff of 50%.
 
 test_pred_readmitted <- factor(ifelse(test_pred >= 0.50, "YES", "NO"))
 test_actual_readmitted <- factor(test$readmitted)
 
 test_conf <- confusionMatrix(test_pred_readmitted, test_actual_readmitted, positive = "YES")
 test_conf
 #########################################################################################
 # Sensitivity is very low. So let's choose a different cutoff value
 
 # Let's find out the optimal probalility cutoff 
 # First let's create a function to find the accuracy, sensitivity and specificity
 # for a given cutoff
 
 perform_fn <- function(cutoff) 
 {
   predicted_readmitted <- factor(ifelse(test_pred >= cutoff, "YES", "NO"))
   conf <- confusionMatrix(predicted_readmitted, test_actual_readmitted, positive = "YES")
   acc <- conf$overall[1]
   sens <- conf$byClass[1]
   spec <- conf$byClass[2]
   out <- t(as.matrix(c(sens, spec, acc))) 
   colnames(out) <- c("sensitivity", "specificity", "accuracy")
   return(out)
 }
 
 # Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.
 
 # Summary of test probability
 
 summary(test_pred)
 
 s = seq(.01,.80,length=100)
 
 OUT = matrix(0,100,3)
 
 
 for(i in 1:100)
 {
   OUT[i,] = perform_fn(s[i])
 } 
 
 
 plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
 axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
 axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
 lines(s,OUT[,2],col="darkgreen",lwd=2)
 lines(s,OUT[,3],col=4,lwd=2)
 box()
 legend("topright",col=c(2,"darkgreen",4,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"), cex=0.4)
 
 ggsave("figs/cutoff1.png")
 
 cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
 
 cutoff
 
 # Let's choose a cutoff value of 0.42 for final model
 
 test_cutoff_readmitted <- factor(ifelse(test_pred >= cutoff, "YES", "NO"))
 
 conf_final <- confusionMatrix(test_cutoff_readmitted, test_actual_readmitted, positive = "YES")
 
 acc <- conf_final$overall[1]
 
 sens <- conf_final$byClass[1]
 
 spec <- conf_final$byClass[2]
 
 acc
# Accuracy: 0.5993701  
 sens
# Sensitivity:  0.6780481  
 spec
# Specificity: 0.5312285  
 
 test_cutoff_readmitted <- ifelse(test_cutoff_readmitted=="YES",1,0)
 test_actual_readmitted <- ifelse(test_actual_readmitted=="YES",1,0)
 
 ##################################################################################################
 ### Exporting Data for Excel Analysis (KS, Gain, Lift etc.) ######
 
 myeval <- matrix(nrow = length(test_pred),ncol = 2)
 myeval[,1] <- test_pred
 myeval[,2] <- test_actual_readmitted
 colnames(myeval) <- c("Predicted_Prob","Actual_Labels")
 write.csv(myeval,"data/myeval.csv")
 
 ##################################################################################################
 ### KS -statistic - Test Data ######
 
 library(ROCR)
 #on testing  data
 pred_object_test<- prediction(test_cutoff_readmitted, test_actual_readmitted)
 
 performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
 
 ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
   (attr(performance_measures_test, "x.values")[[1]])
 
 max(ks_table_test)
 # 0.2092766
 
 ####################################################################
 # Lift & Gain Chart 
 
 # Loading dplyr package 
 library(dplyr)
 
 lift <- function(labels , predicted_prob,groups=10) {
   
   if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
   if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
   helper = data.frame(cbind(labels , predicted_prob))
   helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
   gaintable = helper %>% group_by(bucket)  %>%
     summarise_at(vars(labels ), funs(total = n(),
                                      totalresp=sum(., na.rm = TRUE))) %>%
     
     mutate(Cumresp = cumsum(totalresp),
            Gain=Cumresp/sum(totalresp)*100,
            Cumlift=Gain/(bucket*(100/groups))) 
   return(gaintable)
 }
 
 readmitted_decile = lift(test_actual_readmitted, test_pred, groups = 10)
 readmitted_decile
 
 # A tibble: 10 x 6
 #bucket total totalresp Cumresp      Gain  Cumlift
 #<int> <int>     <dbl>   <dbl>     <dbl>    <dbl>
 #1      1  2985      2177    2177  15.71501 1.571501
 #2      2  2985      1858    4035  29.12726 1.456363
 #3      3  2985      1671    5706  41.18963 1.372988
 #4      4  2985      1468    7174  51.78662 1.294665
 #5      5  2984      1359    8533  61.59677 1.231935
 #6      6  2985      1260    9793  70.69227 1.178204
 #7      7  2985      1196   10989  79.32578 1.133225
 #8      8  2985      1120   12109  87.41067 1.092633
 #9      9  2985       985   13094  94.52104 1.050234
 #10     10  2984       759   13853 100.00000 1.000000
 
 Gain <- c(0,readmitted_decile$Gain)
 Deciles <- c(0,readmitted_decile$bucket)
 plot(y=Gain,x=Deciles,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

 Random_Gain <- seq(from=0,to=100,by=10)
 lines(y=Random_Gain,x=Deciles,type ="l",lwd = 2, col="red")
 
 Perfect_Gain <- vector(mode = "numeric", length = 11)
 for (i in 2:11){Perfect_Gain[i] <- 100*min(1,129*(i-1)/209)}
 lines(y=Perfect_Gain,x=Deciles,type ="l",lwd = 2, col="darkgreen")
 
 legend("bottomright",col=c("darkgreen","black","red"),lwd =c(2,2,2,2),c("Perfect Model","Actual Model","Random Model"), cex = 0.4)
 ggsave("figs/Gain_Random_model_perfectmodel.png")
 # plotting the lift chart
 Lift <- Gain/Random_Gain
 Random_Lift <- Random_Gain/Random_Gain
 
 plot(y=Lift,x=Deciles,type ="l",ylim=c(0,3.5),lwd = 2,xlab="Bucket",ylab="Lift",main = "Lift Chart",ylim<-c())
 lines(y=Random_Lift,x=Deciles,type ="l",lwd = 2, col="red")
 
 legend("topright",col=c("black","red"),lwd =c(2,2,2),c("Actual Model","Random Model"), cex = 0.45)
 ggsave("figs/Lift.png")
 
################################################################################################################################
 # Load the cleaned data
 data <- read.csv("data/data_cleaned.csv", header=TRUE, 
                  na.strings = c("NA","na",""," ","?"), stringsAsFactors = FALSE)
 
 # save the ID of patients
 ID <- data$ID
 
 # Remove the ID before analysis
 data <- data[2:42]
 
 data$readmitted[data$readmitted == "YES"] <- "1"
 data$readmitted[data$readmitted == "NO"] <- "0"
 table(data$readmitted)
 data$readmitted <- as.factor(data$readmitted)
 
  
# sampling down the data
set.seed(123)
data_svm <- data[sample(nrow(data), nrow(data)*0.25, replace = F),]

# split data into train and test in 0.75:0.25 ratio, respectively
set.seed(123)

indices <- sample(2, nrow(data_svm), replace = T, prob = c(0.75, 0.25))
train <- data_svm[indices == 1, ]
test <- data_svm[indices == 2, ]

# First let's try SVM
library("kernlab")

svm.linear <- ksvm(readmitted~., data=train, scale =FALSE, kernel="vanilladot")
svm.predict <- predict(svm.linear, test[,-1])
library(caret)
confusionMatrix(svm.predict, test$readmitted)

################################################################################################

# Now, let's try random forest

# Load the cleaned data
data <- read.csv("data/data_cleaned.csv", header=TRUE, 
                 na.strings = c("NA","na",""," ","?"), stringsAsFactors = FALSE)

# save the ID of patients
ID <- data$ID

# Remove the ID before analysis
data <- data[2:42]


table(data$readmitted)
data$readmitted <- as.factor(data$readmitted)

# sampling down the data
set.seed(123)
data_rf <- data[sample(nrow(data), nrow(data)*0.25, replace = F),]

set.seed(123)

indices <- sample(2, nrow(data_rf), replace = T, prob = c(0.75, 0.25))
train <- data_rf[indices == 1, ]
test <- data_rf[indices == 2, ]



library(randomForest)
library(caret)
set.seed(123)
rf.model <- randomForest(readmitted ~ ., data = train, do.trace = T)
rf.predict <- predict(rf.model, test[,-1], type = "class")
confusionMatrix(rf.predict, test$readmitted)

# Random forest gives us better accuracy. Moving ahead with the forest model.

# So, Let's find out the optimal probability cutoff.
# First, let's create a function to find the accuracy, sensitivity and specificity
# for a given cutoff.
rf_predict <- data.frame(predict(rf.model, test[,-1], type = "prob"))
#colnames(rf.predict) <- c("NO", "YES")
predicted_readmission <- factor(ifelse(rf_predict$YES >= 0.5, "YES", "NO"))
#predicted_readmission <- as.data.frame(predicted_readmission)
#test_true_readmitted <- factor(ifelse(test$readmitted==1,"Yes","No"))

perform_fn <- function(cutoff) 
{
  predicted_readmission <- factor(ifelse(rf_predict$YES >= cutoff, "YES", "NO"))
  conf <- confusionMatrix(predicted_readmission, test$readmitted, positive = "YES")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(rf_predict$YES)
summary(rf_predict$NO)

# Creating cutoff values from 0.01 to 0.95 for plotting and initiallizing a matrix of 100 X 3.

s = seq(.01,.95,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",
     lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("topright",col=c(2,"darkgreen",4),lwd=c(1,1,1),
       c("Sensitivity","Specificity","Accuracy"), cex=0.4)


cutoff <- s[which.min(abs(OUT[,1]-OUT[,2]))]

cutoff

#For this ideal cutoff value, let's see what the confusion matrix looks like

test_cutoff_readmission <- factor(ifelse(rf_predict$YES >= cutoff, "YES", "NO"))

library(caret)
conf_final <- confusionMatrix(test_cutoff_readmission, test$readmitted, positive = "YES")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#Checking important variables
varImpPlot(rf.model)

#Checking distribution of yeses and nos
Id_index <- as.numeric(row.names(test))
PATIENT_ID <- ID[Id_index]

results <- data.frame(PATIENT_ID, rf_predict)
                      
head(results)
hist(results$NO)
hist(results$YES)

# stratification
low_threshold <- 0.3
high_threshold <- 0.7

results$risk_bucket <- character(length = nrow(results))
results$risk_bucket <- apply(results, 1, function(x){
  if(x[3] < low_threshold)
    x[4] <- "LOW"
  else if(x[3] > high_threshold)
    x[4] <- "HIGH"
  else
    x[4] <- "MEDIUM"
})
head(results)
ggplot(data = results, aes(risk_bucket, fill=risk_bucket)) + 
  geom_bar() +
  theme(legend.position = "none") +
  labs(x="Risk bucket", y="Number of patients")

prop.table(table(results$risk_bucket))
library(dplyr)
# low risk patients
low_risk <- subset(results, results$risk_bucket == "LOW")
low_risk <- low_risk %>% arrange(YES)
head(low_risk)

# high risk patients
high_risk <- subset(results, results$risk_bucket == "HIGH")
high_risk <- high_risk %>% arrange(desc(YES))
head(high_risk)

# medium risk
medium_risk <- subset(results, results$risk_bucket == "MEDIUM")
medium_risk <- medium_risk %>% arrange(desc(YES))
head(medium_risk)

