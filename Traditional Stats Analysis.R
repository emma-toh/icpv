#Traditional Statistics Analysis for ICPV Data
library(tableone)
library(dplyr)

data <- read.csv("df_dynamic_cohort.csv")
static <- read.csv("static_info.csv")
comorb <- read.csv("eicu_comorbidities.csv")

left<- left_join(static,data,by="patientunitstayid")
big_df<- left_join(left,comorb,by="patientunitstayid")
dim(big_df)
cat_feat = c('gender','ethnicity','unitdischargestatus','label',"hypertension")
big_df[cat_feat] <- lapply(big_df[cat_feat], factor)
shapiro.test(bobo$age) #test shaprio-wilk for each continuous variable
bigdf$unitdischargestatus <- ifelse(bigdf$unitdischargestatus=='Alive','0','1')
bigdf$unitdischargestatus <- as.factor(bigdf$unitdischargestatus)

table1 <- CreateTableOne(data=bobo,strata='label') # strata='anysurgery'
manuwhitney <- c("BMI","GCS","age","icp","Non.Invasive.BP...Non.Invasive.BP.Systolic_merged3","Non.Invasive.BP...Non.Invasive.BP.Diastolic_merged3","glucose","temperature_merged","heartrate_merged")
print(table1, nonnormal = manuwhitney, quote=TRUE, noSpaces=TRUE)

#To create multivariate analysis
Def1 <- glm(label ~ icpv_mean_def1_45+gender+age+hypertension+Invasive.and.Non.Invasive.BP.Systolic.merged+temperature_merged+glucose+GCS, data = big_df,
            family = binomial)
Def2 <- glm(label ~ icpv_mean_def2_45+gender+age+hypertension+Invasive.and.Non.Invasive.BP.Systolic.merged+temperature_merged+glucose+GCS, data = big_df,
            family = binomial)
summary(Def1)
summary(Def2)

#subgroup with At least one episode of intracranial hypertension
htn <- subset(bigdf, label=='1')

Def1 <- glm(unitdischargestatus ~ icpv_mean_def1_45+gender+age+hypertension+Invasive.and.Non.Invasive.BP.Systolic.merged+temperature_merged+glucose+GCS, data = htn,
            family = binomial)

Def2 <- glm(unitdischargestatus ~ icpv_mean_def2_45+gender+age+hypertension+Invasive.and.Non.Invasive.BP.Systolic.merged+temperature_merged+glucose+GCS, data = htn,
            family = binomial)
summary(Def1)
summary(Def2)

#subgroup with no episode of intracranial hypertension
nohtn <- subset(bigdf, label=='0')
Def1 <- glm(unitdischargestatus ~ icpv_mean_def1_45+gender+age+hypertension+Invasive.and.Non.Invasive.BP.Systolic.merged+temperature_merged+glucose+GCS, data = nohtn,
            family = binomial)
Def2 <- glm(unitdischargestatus ~ icpv_mean_def2_45+gender+age+hypertension+Invasive.and.Non.Invasive.BP.Systolic.merged+temperature_merged+glucose+GCS, data = nohtn,
            family = binomial)
summary(Def1)
summary(Def2)



