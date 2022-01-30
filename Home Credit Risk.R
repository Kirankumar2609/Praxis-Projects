#setting working directory
setwd('D:\\DS\\Python\\Praxis\\FMO\\home-credit-default-risk')

#read train and test data
train <- read.csv('application_train.csv')
test <- read.csv('application_test.csv')


#make a copy of the dataframe
df_train <- data.frame(train)
df_test <- data.frame(test)

str(df_train)
#====================================================================================================================

#converting train categorical columns to factor types
cols <- c('NAME_CONTRACT_TYPE','CODE_GENDER','FLAG_OWN_CAR',
          'FLAG_OWN_REALTY','NAME_TYPE_SUITE','NAME_INCOME_TYPE',
          'NAME_EDUCATION_TYPE','NAME_FAMILY_STATUS','NAME_HOUSING_TYPE',
          'FLAG_MOBIL','FLAG_EMP_PHONE','FLAG_WORK_PHONE','FLAG_CONT_MOBILE',
          'FLAG_PHONE','FLAG_EMAIL','OCCUPATION_TYPE','WEEKDAY_APPR_PROCESS_START',
          'REG_REGION_NOT_LIVE_REGION','REG_REGION_NOT_WORK_REGION','LIVE_REGION_NOT_WORK_REGION',
          'REG_CITY_NOT_LIVE_CITY','REG_CITY_NOT_WORK_CITY','LIVE_CITY_NOT_WORK_CITY',
          'ORGANIZATION_TYPE','FONDKAPREMONT_MODE','HOUSETYPE_MODE','WALLSMATERIAL_MODE',
          'EMERGENCYSTATE_MODE','REGION_RATING_CLIENT','REGION_RATING_CLIENT_W_CITY')

df_train[cols] <- lapply(df_train[cols],factor)
str(df_train)


#Chi-square test
source('D:\\DS\\Praxis\\Study Material\\Term-2\\I2R\\Exercises\\Functions.R')
c <- ChiTest(df_train,target = 'TARGET')
c
dim(c)
subset(c,c[,'p-value'] < 0.05)
dim(subset(c,c[,'p-value'] < 0.05))
row.names(subset(c,c[,'p-value'] >= 0.05))


#T-test
t <- TTest(df_train,target = 'TARGET')
t
dim(t)
subset(t,t[,'p-value'] < 0.05)
dim(subset(t,t[,'p-value'] < 0.05))
row.names(subset(t,t[,'p-value'] >= 0.05))

#======================================================================================================================

#Correcting missing value in 'OWN_CAR_AGE' column
df_train$OWN_CAR_AGE[df_train$OWN_CAR_AGE==0] = 1 #Assigning min. value as 1 for car age
summary(df_train$OWN_CAR_AGE)
tapply(df_train$OWN_CAR_AGE,df_train$FLAG_OWN_CAR,FUN = median,na.rm=T)
df_train$OWN_CAR_AGE[is.na(df_train$OWN_CAR_AGE)==T & df_train$FLAG_OWN_CAR=='Y'] = 9
df_train$OWN_CAR_AGE[is.na(df_train$OWN_CAR_AGE)==T & df_train$FLAG_OWN_CAR=='N'] = 0
tapply(df_train$OWN_CAR_AGE,df_train$FLAG_OWN_CAR,FUN = median)

#======================================================================================================================

#dropping the insignificant features (p>=0.05 from t-test & chi-sq test)
drop <- c("FLAG_MOBIL","FLAG_CONT_MOBILE","FLAG_EMAIL","LIVE_REGION_NOT_WORK_REGION",
          "SK_ID_CURR","AMT_INCOME_TOTAL","NONLIVINGAPARTMENTS_AVG","NONLIVINGAPARTMENTS_MODE",
          "NONLIVINGAPARTMENTS_MEDI","FLAG_DOCUMENT_2","FLAG_DOCUMENT_5","FLAG_DOCUMENT_7",
          "FLAG_DOCUMENT_12","FLAG_DOCUMENT_19","FLAG_DOCUMENT_20","FLAG_DOCUMENT_21",
          "AMT_REQ_CREDIT_BUREAU_HOUR","AMT_REQ_CREDIT_BUREAU_DAY","AMT_REQ_CREDIT_BUREAU_WEEK",
          "AMT_REQ_CREDIT_BUREAU_QRT")

df_train <- df_train[,!(names(df_train) %in% drop)]

#======================================================================================================================

#Information Value
install.packages('Information')
library(Information)

Iv <- create_infotables(data=df_train,y='TARGET',bins=10,parallel=F)
IV_value = data.frame(Iv$Summary)
subset(IV_value,IV_value[,'IV'] < 0.03) #filtering those columns whose IV is < 0.03
dim(subset(IV_value,IV_value[,'IV'] < 0.03))
subset(IV_value,IV_value[,'IV'] < 0.03,select=Variable) #filtering those columns whose IV is < 0.03


#Dropping those columns whose IV is < 0.03

df_train <- subset(df_train,select=-c(YEARS_BEGINEXPLUATATION_MODE,YEARS_BEGINEXPLUATATION_MEDI,YEARS_BEGINEXPLUATATION_AVG,
                                      FLAG_DOCUMENT_3,ENTRANCES_MEDI,WALLSMATERIAL_MODE,ENTRANCES_AVG,DAYS_REGISTRATION,
                                      REGION_POPULATION_RELATIVE,ENTRANCES_MODE,AMT_ANNUITY,EMERGENCYSTATE_MODE,NONLIVINGAREA_AVG,
                                      NONLIVINGAREA_MEDI,NONLIVINGAREA_MODE,HOUSETYPE_MODE,BASEMENTAREA_AVG,REG_CITY_NOT_LIVE_CITY,
                                      BASEMENTAREA_MEDI,NAME_FAMILY_STATUS,BASEMENTAREA_MODE,OWN_CAR_AGE,FLOORSMIN_AVG,FLOORSMIN_MODE,
                                      FLOORSMIN_MEDI,AMT_REQ_CREDIT_BUREAU_YEAR,LIVINGAPARTMENTS_MEDI,LIVINGAPARTMENTS_AVG,YEARS_BUILD_MODE,
                                      YEARS_BUILD_MEDI,YEARS_BUILD_AVG,LANDAREA_AVG,LANDAREA_MEDI,LANDAREA_MODE,LIVINGAPARTMENTS_MODE,
                                      NAME_HOUSING_TYPE,NAME_CONTRACT_TYPE,COMMONAREA_AVG,COMMONAREA_MEDI,DEF_30_CNT_SOCIAL_CIRCLE,
                                      AMT_REQ_CREDIT_BUREAU_MON,LIVE_CITY_NOT_WORK_CITY,COMMONAREA_MODE,FONDKAPREMONT_MODE,DEF_60_CNT_SOCIAL_CIRCLE,
                                      FLAG_DOCUMENT_6,FLAG_WORK_PHONE,HOUR_APPR_PROCESS_START,FLAG_PHONE,FLAG_OWN_CAR,CNT_CHILDREN,CNT_FAM_MEMBERS,
                                      OBS_60_CNT_SOCIAL_CIRCLE,OBS_30_CNT_SOCIAL_CIRCLE,FLAG_DOCUMENT_13,FLAG_DOCUMENT_16,NAME_TYPE_SUITE,
                                      FLAG_DOCUMENT_14,FLAG_DOCUMENT_18,FLAG_DOCUMENT_8,FLAG_DOCUMENT_15,WEEKDAY_APPR_PROCESS_START,REG_REGION_NOT_WORK_REGION,
                                      FLAG_OWN_REALTY,REG_REGION_NOT_LIVE_REGION,FLAG_DOCUMENT_9,FLAG_DOCUMENT_11,FLAG_DOCUMENT_17,FLAG_DOCUMENT_4,
                                      FLAG_DOCUMENT_10))
summary(df_train)

#======================================================================================================================================================================================


#Missing Value Imputation

df_train$DAYS_LAST_PHONE_CHANGE[is.na(df_train$DAYS_LAST_PHONE_CHANGE)==T] = 0

df_train$EXT_SOURCE_1[is.na(df_train$EXT_SOURCE_1)==T] = median(df_train$EXT_SOURCE_1,na.rm=T)

df_train$EXT_SOURCE_2[is.na(df_train$EXT_SOURCE_2)==T] = median(df_train$EXT_SOURCE_2,na.rm=T)

df_train$EXT_SOURCE_3[is.na(df_train$EXT_SOURCE_3)==T] = median(df_train$EXT_SOURCE_3,na.rm=T)

df_train$APARTMENTS_AVG[is.na(df_train$APARTMENTS_AVG)==T] = median(df_train$APARTMENTS_AVG,na.rm=T)

df_train$ELEVATORS_AVG[is.na(df_train$ELEVATORS_AVG)==T] = median(df_train$ELEVATORS_AVG,na.rm=T)

df_train$FLOORSMAX_AVG[is.na(df_train$FLOORSMAX_AVG)==T] = median(df_train$FLOORSMAX_AVG,na.rm=T)

df_train$LIVINGAREA_AVG[is.na(df_train$LIVINGAREA_AVG)==T] = median(df_train$LIVINGAREA_AVG,na.rm=T)

df_train$APARTMENTS_MODE[is.na(df_train$APARTMENTS_MODE)==T] = median(df_train$APARTMENTS_MODE,na.rm=T)

df_train$ELEVATORS_MODE[is.na(df_train$ELEVATORS_MODE)==T] = median(df_train$ELEVATORS_MODE,na.rm=T)

df_train$FLOORSMAX_MODE[is.na(df_train$FLOORSMAX_MODE)==T] = median(df_train$FLOORSMAX_MODE,na.rm=T)

df_train$LIVINGAREA_MODE[is.na(df_train$LIVINGAREA_MODE)==T] = median(df_train$LIVINGAREA_MODE,na.rm=T)

df_train$APARTMENTS_MEDI[is.na(df_train$APARTMENTS_MEDI)==T] = median(df_train$APARTMENTS_MEDI,na.rm=T)

df_train$ELEVATORS_MEDI[is.na(df_train$ELEVATORS_MEDI)==T] = median(df_train$ELEVATORS_MEDI,na.rm=T)

df_train$FLOORSMAX_MEDI[is.na(df_train$FLOORSMAX_MEDI)==T] = median(df_train$FLOORSMAX_MEDI,na.rm=T)

df_train$LIVINGAREA_MEDI[is.na(df_train$LIVINGAREA_MEDI)==T] = median(df_train$LIVINGAREA_MEDI,na.rm=T)

df_train$TOTALAREA_MODE[is.na(df_train$TOTALAREA_MODE)==T] = median(df_train$TOTALAREA_MODE,na.rm=T)

summary(df_train)

#missing value - AMT_GOODS_PRICE column - knn imputation

install.packages('VIM')
library(VIM)

df_train = kNN(df_train, variable = c('AMT_GOODS_PRICE'), k = 49, imp_var = F)

View(df_train)

summary(df_train)
str(df_train)


#missing value - OCCUPATION_TYPE column - knn model fit (R-took a long time. Stopped in middle)

'''
knn_train <- df_train[!is.na(df_train$OCCUPATION_TYPE),]
knn_test <- df_train[is.na(df_train$OCCUPATION_TYPE),]
X_train <- subset(knn_train,select=-c(CODE_GENDER,NAME_INCOME_TYPE,
                                      NAME_EDUCATION_TYPE,FLAG_EMP_PHONE,
                                      OCCUPATION_TYPE,REG_CITY_NOT_WORK_CITY,
                                      ORGANIZATION_TYPE))
y_train <- knn_train$OCCUPATION_TYPE
X_test <- subset(knn_test,select=-c(CODE_GENDER,NAME_INCOME_TYPE,
                                      NAME_EDUCATION_TYPE,FLAG_EMP_PHONE,
                                      OCCUPATION_TYPE,REG_CITY_NOT_WORK_CITY,
                                      ORGANIZATION_TYPE))
y_test <- knn_test$OCCUPATION_TYPE


X_train <- data.frame(scale(X_train))
X_test <- data.frame(scale(X_test))

library(class)
model <- knn(train=X_train, test = X_test, cl = y_train, k=21)
model

'''

#OCCUPATION_TYPE MISSING VALUES IMPUTED USING PYTHON

OCCUPATION_TYPE <- read.csv('OCCUPATION_TYPE.csv')
str(OCCUPATION_TYPE)

df_train$OCCUPATION_TYPE <- OCCUPATION_TYPE$OCCUPATION_TYPE
df_train$OCCUPATION_TYPE <- as.factor(df_train$OCCUPATION_TYPE)

summary(df_train)
str(df_train)
sum(is.na(df_train))

#======================================================================================================================

#Plotting Graphs
Graph_v1(df_train,dir='D:\\DS\\Python\\Praxis\\FMO\\home-credit-default-risk\\R-Graphs')

#======================================================================================================================

#Outlier/Anamoly Correction

summary(df_train)

df_train$DAYS_BIRTH <- (df_train$DAYS_BIRTH/-365) #converting DAYS_BIRTH from days to years
summary(df_train$DAYS_BIRTH)

#correcting anamoly in DAYS_EMPLOYED column
df_train$DAYS_EMPLOYED[df_train$DAYS_EMPLOYED==365243] = median(df_train$DAYS_EMPLOYED[df_train$DAYS_EMPLOYED != 365243])
df_train$DAYS_EMPLOYED <- (df_train$DAYS_EMPLOYED/-365) #converting DAYS_EMPLOYED from days to years
summary(df_train$DAYS_EMPLOYED)

#======================================================================================================================

#Multicollinearity Check

str(df_train)
names(df_train)

vif_train <- subset(df_train,select=-c(CODE_GENDER,NAME_INCOME_TYPE,NAME_EDUCATION_TYPE,
                                       FLAG_EMP_PHONE,OCCUPATION_TYPE,REGION_RATING_CLIENT,
                                       REGION_RATING_CLIENT_W_CITY,REG_CITY_NOT_WORK_CITY,
                                       ORGANIZATION_TYPE))
vif_train <- data.frame(df_train)

#fit the logistic regression model
model <- glm(formula = TARGET ~., data = vif_train, family = 'binomial')
summary(model)
library(car)
vif(model)

#selecting final variables after VIF check
final <- subset(df_train,select=c(TARGET,AMT_CREDIT,DAYS_BIRTH,DAYS_EMPLOYED,DAYS_ID_PUBLISH,
                                  EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3,TOTALAREA_MODE,
                                  DAYS_LAST_PHONE_CHANGE,CODE_GENDER,NAME_INCOME_TYPE,
                                  NAME_EDUCATION_TYPE,OCCUPATION_TYPE,REGION_RATING_CLIENT,
                                  REG_CITY_NOT_WORK_CITY,ORGANIZATION_TYPE))

final$TARGET <- as.factor(final$TARGET)
str(final)
#======================================================================================================================

#Fitting logistic regression model with train-valid split

#stratified splitting
install.packages('splitTools')
library(splitTools)
library(ranger)
inds <- partition(final$TARGET, p=c(train=0.55,valid=0.45))
str(inds)

tra <- final[inds$train,]
val <- final[inds$valid,]

length(tra$TARGET[tra$TARGET==1])
length(val$TARGET[val$TARGET==1])

#X_train <- subset(tra,select=-c(TARGET))
#X_valid <- subset(val,select=-c(TARGET))
#y_train <- tra[,'TARGET']
#y_test <- val[,'TARGET']

#====================================================================================================

#1. Direct Logistic regression

model <- glm(formula = TARGET ~., data = tra, family = 'binomial') #fitting the model
summary(model)
pred <- predict(model,val, type='response') #predicting probability

#finding the optimal cut-off
install.packages('InformationValue')
library(InformationValue)
optCutOff <- optimalCutoff(val$TARGET, pred)[1]
optCutOff

#finding the optimal cut-off using loop
score <- c()
for(optCutOff in seq(0.1,0.9,0.1))
{
  f1 <- 2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))
  score <- c(score,f1)
  print(paste(optCutOff,f1,sep='-'))
}
print(score)
optCutOff <- 0.1

confusionMatrix(val$TARGET, pred, threshold=optCutOff) # The columns are actual, while rows are predicted.

#Transposed
t(confusionMatrix(val$TARGET, pred, threshold=optCutOff))

misClassError(val$TARGET,pred,threshold=optCutOff) #1-accuracy
sensitivity(val$TARGET, pred, threshold=optCutOff) #recall
specificity(val$TARGET, pred, threshold=optCutOff)
precision(val$TARGET, pred, threshold=optCutOff)

2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))#FI_score

plotROC(val$TARGET,pred)
install.packages('pROC')
library(pROC)
auc(val$TARGET,pred)

#===================================================================================================

#2. Logistic regression with log transformation

final <- subset(df_train,select=c(TARGET,AMT_CREDIT,DAYS_BIRTH,DAYS_EMPLOYED,DAYS_ID_PUBLISH,
                                  EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3,TOTALAREA_MODE,
                                  DAYS_LAST_PHONE_CHANGE,CODE_GENDER,NAME_INCOME_TYPE,
                                  NAME_EDUCATION_TYPE,OCCUPATION_TYPE,REGION_RATING_CLIENT,
                                  REG_CITY_NOT_WORK_CITY,ORGANIZATION_TYPE))

final$TARGET <- as.factor(final$TARGET)


#memory.limit(16000)
#memory.limit()

names(final)
final['AMT_CREDIT'] = log(final['AMT_CREDIT'])
final['DAYS_EMPLOYED'] = log(final['DAYS_EMPLOYED']+1)
final['DAYS_LAST_PHONE_CHANGE'] = (final['DAYS_LAST_PHONE_CHANGE']*-1)/365
final['DAYS_LAST_PHONE_CHANGE'] = log(final['DAYS_LAST_PHONE_CHANGE']+1)
final['TOTALAREA_MODE'] = log(final['TOTALAREA_MODE']+1)
final['EXT_SOURCE_1'] = log(final['EXT_SOURCE_1'])
final['EXT_SOURCE_3'] = log(final['EXT_SOURCE_3'])

summary(final)
str(final)

library(splitTools)
library(ranger)
inds <- partition(final$TARGET, p=c(train=0.55,valid=0.45))

tra <- final[inds$train,]
val <- final[inds$valid,]

model <- glm(formula = TARGET ~., data = tra, family = 'binomial') #fitting the model
summary(model)
pred <- predict(model,val, type='response') #predicting probability


#finding the optimal cut-off
library(InformationValue)
optCutOff <- optimalCutoff(val$TARGET, pred)[1]
optCutOff

#finding the optimal cut-off using loop
score <- c()
for(optCutOff in seq(0.1,0.9,0.1))
{
  f1 <- 2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))
  score <- c(score,f1)
  print(paste(optCutOff,f1,sep='-'))
}
print(score)
optCutOff <- 0.1

confusionMatrix(val$TARGET, pred, threshold=optCutOff) # The columns are actual, while rows are predicted.

#Transposed
t(confusionMatrix(val$TARGET, pred, threshold=optCutOff))

misClassError(val$TARGET,pred,threshold=optCutOff) #1-accuracy
sensitivity(val$TARGET, pred, threshold=optCutOff) #recall
specificity(val$TARGET, pred, threshold=optCutOff)
precision(val$TARGET, pred, threshold=optCutOff)

2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))#FI_score

plotROC(val$TARGET,pred)
library(pROC)
auc(val$TARGET,pred)

#==========================================================================================================================

#3. Logistic regression with log transformation and assigning weights similar to python

final <- subset(df_train,select=c(TARGET,AMT_CREDIT,DAYS_BIRTH,DAYS_EMPLOYED,DAYS_ID_PUBLISH,
                                  EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3,TOTALAREA_MODE,
                                  DAYS_LAST_PHONE_CHANGE,CODE_GENDER,NAME_INCOME_TYPE,
                                  NAME_EDUCATION_TYPE,OCCUPATION_TYPE,REGION_RATING_CLIENT,
                                  REG_CITY_NOT_WORK_CITY,ORGANIZATION_TYPE))


final$TARGET <- as.factor(final$TARGET)


final['AMT_CREDIT'] = log(final['AMT_CREDIT'])
final['DAYS_EMPLOYED'] = log(final['DAYS_EMPLOYED']+1)
final['DAYS_LAST_PHONE_CHANGE'] = (final['DAYS_LAST_PHONE_CHANGE']*-1)/365
final['DAYS_LAST_PHONE_CHANGE'] = log(final['DAYS_LAST_PHONE_CHANGE']+1)
final['TOTALAREA_MODE'] = log(final['TOTALAREA_MODE']+1)
final['EXT_SOURCE_1'] = log(final['EXT_SOURCE_1'])
final['EXT_SOURCE_3'] = log(final['EXT_SOURCE_3'])

library(splitTools)
library(ranger)
inds <- partition(final$TARGET, p=c(train=0.55,valid=0.45))

tra <- final[inds$train,]

prop.table(table(tra$TARGET))

dim(tra)[1]/(2*table(tra$TARGET)[1])
dim(tra)[1]/(2*table(tra$TARGET)[2])

#assigning weights to each observations
model_weights <- ifelse(tra$TARGET == '0',
                        1/(table(tra$TARGET)[1])*0.5,
                        1/(table(tra$TARGET)[2])*0.5)

val <- final[inds$valid,]

model <- glm(formula = TARGET ~., data = tra, family = 'binomial', weights = model_weights) #fitting the model
summary(model)
pred <- predict(model,val, type='response') #predicting probability

#finding the optimal cut-off
library(InformationValue)
optCutOff <- optimalCutoff(val$TARGET, pred)[1]
optCutOff

#finding the optimal cut-off using loop
score <- c()
for(optCutOff in seq(0.05,0.95,0.05))
{
  f1 <- 2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))
  score <- c(score,f1)
  print(paste(optCutOff,f1,sep='-'))
}
print(score)
optCutOff <- 0.65

confusionMatrix(val$TARGET, pred, threshold=optCutOff) # The columns are actual, while rows are predicted.

#Transposed
t(confusionMatrix(val$TARGET, pred, threshold=optCutOff))

misClassError(val$TARGET,pred,threshold=optCutOff) #1-accuracy
sensitivity(val$TARGET, pred, threshold=optCutOff) #recall
specificity(val$TARGET, pred, threshold=optCutOff)
precision(val$TARGET, pred, threshold=optCutOff)

2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))#FI_score

plotROC(val$TARGET,pred)
library(pROC)
auc(val$TARGET,pred)

#=====================================================================================================

#4. Logistic regression with log transformation, business intuitive variable and assigning weights

final <- subset(df_train,select=c(TARGET,AMT_CREDIT,DAYS_BIRTH,DAYS_EMPLOYED,DAYS_ID_PUBLISH,
                                  EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3,TOTALAREA_MODE,
                                  DAYS_LAST_PHONE_CHANGE,CODE_GENDER,NAME_INCOME_TYPE,
                                  NAME_EDUCATION_TYPE,OCCUPATION_TYPE,REGION_RATING_CLIENT,
                                  REG_CITY_NOT_WORK_CITY,ORGANIZATION_TYPE))


final$TARGET <- as.factor(final$TARGET)
final$DAYS_EMPLOYED_PERCENT <- final$DAYS_EMPLOYED/final$DAYS_BIRTH


final['AMT_CREDIT'] = log(final['AMT_CREDIT'])
final['DAYS_EMPLOYED'] = log(final['DAYS_EMPLOYED']+1)
final['DAYS_LAST_PHONE_CHANGE'] = (final['DAYS_LAST_PHONE_CHANGE']*-1)/365
final['DAYS_LAST_PHONE_CHANGE'] = log(final['DAYS_LAST_PHONE_CHANGE']+1)
final['TOTALAREA_MODE'] = log(final['TOTALAREA_MODE']+1)
final['EXT_SOURCE_1'] = log(final['EXT_SOURCE_1'])
final['EXT_SOURCE_3'] = log(final['EXT_SOURCE_3'])

library(splitTools)
library(ranger)
inds <- partition(final$TARGET, p=c(train=0.55,valid=0.45))

tra <- final[inds$train,]

prop.table(table(tra$TARGET))

dim(tra)[1]/(2*table(tra$TARGET)[1])
dim(tra)[1]/(2*table(tra$TARGET)[2])

#assigning weights to each observations
model_weights <- ifelse(tra$TARGET == '0',
                        1/(table(tra$TARGET)[1])*0.5,
                        1/(table(tra$TARGET)[2])*0.5)

val <- final[inds$valid,]

model <- glm(formula = TARGET ~., data = tra, family = 'binomial', weights = model_weights) #fitting the model
summary(model)
pred <- predict(model,val, type='response') #predicting probability

#finding the optimal cut-off using loop
score <- c()
for(optCutOff in seq(0.05,0.95,0.05))
{
  f1 <- 2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))
  score <- c(score,f1)
  print(paste(optCutOff,f1,sep='-'))
}

optCutOff <- 0.65

confusionMatrix(val$TARGET, pred, threshold=optCutOff) # The columns are actual, while rows are predicted.

#Transposed
t(confusionMatrix(val$TARGET, pred, threshold=optCutOff))

misClassError(val$TARGET,pred,threshold=optCutOff) #1-accuracy
sensitivity(val$TARGET, pred, threshold=optCutOff) #recall
specificity(val$TARGET, pred, threshold=optCutOff)
precision(val$TARGET, pred, threshold=optCutOff)

2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))#FI_score

plotROC(val$TARGET,pred)
library(pROC)
auc(val$TARGET,pred)

#======================================================================================================================

#5. Logistic regression with log transformation, business intuitive variable, assigning weights and interaction term for EXT_SOURCE

final <- subset(df_train,select=c(TARGET,AMT_CREDIT,DAYS_BIRTH,DAYS_EMPLOYED,DAYS_ID_PUBLISH,
                                  EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3,TOTALAREA_MODE,
                                  DAYS_LAST_PHONE_CHANGE,CODE_GENDER,NAME_INCOME_TYPE,
                                  NAME_EDUCATION_TYPE,OCCUPATION_TYPE,REGION_RATING_CLIENT,
                                  REG_CITY_NOT_WORK_CITY,ORGANIZATION_TYPE))

do.call(poly, c(lapply(6:8, function(x) final[,x]), degree=3, raw=T))
inter <- data.frame(do.call(poly, c(lapply(6:8, function(x) final[,x]), degree=3, raw=T)))
inter <- log(inter)

final <- subset(final,select=-c(EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3))
final <- cbind(final,inter)

final$TARGET <- as.factor(final$TARGET)
final$DAYS_EMPLOYED_PERCENT <- final$DAYS_EMPLOYED/final$DAYS_BIRTH


final['AMT_CREDIT'] = log(final['AMT_CREDIT'])
final['DAYS_EMPLOYED'] = log(final['DAYS_EMPLOYED']+1)
final['DAYS_LAST_PHONE_CHANGE'] = (final['DAYS_LAST_PHONE_CHANGE']*-1)/365
final['DAYS_LAST_PHONE_CHANGE'] = log(final['DAYS_LAST_PHONE_CHANGE']+1)
final['TOTALAREA_MODE'] = log(final['TOTALAREA_MODE']+1)
#final['EXT_SOURCE_1'] = log(final['EXT_SOURCE_1'])
#final['EXT_SOURCE_3'] = log(final['EXT_SOURCE_3'])

library(splitTools)
library(ranger)
inds <- partition(final$TARGET, p=c(train=0.55,valid=0.45))

tra <- final[inds$train,]

prop.table(table(tra$TARGET))

dim(tra)[1]/(2*table(tra$TARGET)[1])
dim(tra)[1]/(2*table(tra$TARGET)[2])

#assigning weights to each observations
model_weights <- ifelse(tra$TARGET == '0',
                        1/(table(tra$TARGET)[1])*0.5,
                        1/(table(tra$TARGET)[2])*0.5)

val <- final[inds$valid,]

model <- glm(formula = TARGET ~., data = tra, family = 'binomial', weights = model_weights) #fitting the model
summary(model)
pred <- predict(model,val, type='response') #predicting probability

#finding the optimal cut-off using loop
score <- c()
for(optCutOff in seq(0.05,0.95,0.05))
{
  f1 <- 2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))
  score <- c(score,f1)
  print(paste(optCutOff,f1,sep='-'))
}

optCutOff <- 0.6

confusionMatrix(val$TARGET, pred, threshold=optCutOff) # The columns are actual, while rows are predicted.

#Transposed
t(confusionMatrix(val$TARGET, pred, threshold=optCutOff))

misClassError(val$TARGET,pred,threshold=optCutOff) #1-accuracy
sensitivity(val$TARGET, pred, threshold=optCutOff) #recall
specificity(val$TARGET, pred, threshold=optCutOff)
precision(val$TARGET, pred, threshold=optCutOff)

2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))#FI_score

plotROC(val$TARGET,pred)
library(pROC)
auc(val$TARGET,pred)


#======================================================================================================================

#FINAL MODEL

#Logistic regression with log transformation, business intuitive variable had best roc_auc score after considering features significance

final <- subset(df_train,select=c(TARGET,AMT_CREDIT,DAYS_BIRTH,DAYS_EMPLOYED,DAYS_ID_PUBLISH,
                                  EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3,TOTALAREA_MODE,
                                  DAYS_LAST_PHONE_CHANGE,
                                  REGION_RATING_CLIENT,
                                  REG_CITY_NOT_WORK_CITY))


final$TARGET <- as.factor(final$TARGET)

final$DAYS_EMPLOYED_PERCENT <- final$DAYS_EMPLOYED/final$DAYS_BIRTH #business intuitive variable


final['AMT_CREDIT'] = log(final['AMT_CREDIT'])
final['DAYS_EMPLOYED'] = log(final['DAYS_EMPLOYED']+1)
final['DAYS_LAST_PHONE_CHANGE'] = (final['DAYS_LAST_PHONE_CHANGE']*-1)/365
final['DAYS_LAST_PHONE_CHANGE'] = log(final['DAYS_LAST_PHONE_CHANGE']+1)
final['TOTALAREA_MODE'] = log(final['TOTALAREA_MODE']+1)
final['EXT_SOURCE_1'] = log(final['EXT_SOURCE_1'])
final['EXT_SOURCE_3'] = log(final['EXT_SOURCE_3'])

library(splitTools)
library(ranger)
inds <- partition(final$TARGET, p=c(train=0.55,valid=0.45))

tra <- final[inds$train,]

val <- final[inds$valid,]


model <- glm(formula = TARGET ~., data = tra, family = 'binomial') #fitting the model
summary(model)
pred <- predict(model,val, type='response') #predicting probability

#finding the optimal cut-off using loop
score <- c()
for(optCutOff in seq(0.05,0.95,0.05))
{
  f1 <- 2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))
  score <- c(score,f1)
  print(paste(optCutOff,f1,sep='-'))
}

optCutOff <- 0.1

confusionMatrix(val$TARGET, pred, threshold=optCutOff) # The columns are actual, while rows are predicted.

#Transposed
t(confusionMatrix(val$TARGET, pred, threshold=optCutOff))

misClassError(val$TARGET,pred,threshold=optCutOff) #1-accuracy
sensitivity(val$TARGET, pred, threshold=optCutOff) #recall
specificity(val$TARGET, pred, threshold=optCutOff)
precision(val$TARGET, pred, threshold=optCutOff)

2*(precision(val$TARGET, pred, threshold=optCutOff))*(sensitivity(val$TARGET, pred, threshold=optCutOff))/((precision(val$TARGET, pred, threshold=optCutOff))+(sensitivity(val$TARGET, pred, threshold=optCutOff)))#FI_score

plotROC(val$TARGET,pred)
library(pROC)
auc(val$TARGET,pred)

pred[pred>0.1] <- 1
pred[pred<0.1] <- 0
pred
Concordance(val$TARGET, pred)

#==================================================================================================

#STEPWISE/FORWARD/BACKWARD SELECTION

#STEPWISE REGRESSION

library(MASS)
step.model <- stepAIC(model, direction = "both", trace = FALSE)
summary(step.model)


#FORWARD SELECTION

step.model <- stepAIC(model, direction = "forward", trace = FALSE)
summary(step.model)


#BACKWARD ELIMINATION

step.model <- stepAIC(model, direction = "backward", trace = FALSE)
summary(step.model)


#======================================================================================================================

#PREDICTING FOR TEST DATA

#FINAL MODEL - Logistic regression with log transformation, business intuitive variable 
#              had best roc_auc score with significant features

summary(df_test)

#converting test categorical columns to factor types
cols <- c('NAME_CONTRACT_TYPE','CODE_GENDER','FLAG_OWN_CAR',
          'FLAG_OWN_REALTY','NAME_TYPE_SUITE','NAME_INCOME_TYPE',
          'NAME_EDUCATION_TYPE','NAME_FAMILY_STATUS','NAME_HOUSING_TYPE',
          'FLAG_MOBIL','FLAG_EMP_PHONE','FLAG_WORK_PHONE','FLAG_CONT_MOBILE',
          'FLAG_PHONE','FLAG_EMAIL','OCCUPATION_TYPE','WEEKDAY_APPR_PROCESS_START',
          'REG_REGION_NOT_LIVE_REGION','REG_REGION_NOT_WORK_REGION','LIVE_REGION_NOT_WORK_REGION',
          'REG_CITY_NOT_LIVE_CITY','REG_CITY_NOT_WORK_CITY','LIVE_CITY_NOT_WORK_CITY',
          'ORGANIZATION_TYPE','FONDKAPREMONT_MODE','HOUSETYPE_MODE','WALLSMATERIAL_MODE',
          'EMERGENCYSTATE_MODE','REGION_RATING_CLIENT','REGION_RATING_CLIENT_W_CITY')
df_test[cols] <- lapply(df_test[cols],factor)

#dropping the insignificant features (p>=0.05 from t-test & chi-sq test)
drop <- c("FLAG_MOBIL","FLAG_CONT_MOBILE","FLAG_EMAIL","LIVE_REGION_NOT_WORK_REGION",
          "SK_ID_CURR","AMT_INCOME_TOTAL","NONLIVINGAPARTMENTS_AVG","NONLIVINGAPARTMENTS_MODE",
          "NONLIVINGAPARTMENTS_MEDI","FLAG_DOCUMENT_2","FLAG_DOCUMENT_5","FLAG_DOCUMENT_7",
          "FLAG_DOCUMENT_12","FLAG_DOCUMENT_19","FLAG_DOCUMENT_20","FLAG_DOCUMENT_21",
          "AMT_REQ_CREDIT_BUREAU_HOUR","AMT_REQ_CREDIT_BUREAU_DAY","AMT_REQ_CREDIT_BUREAU_WEEK",
          "AMT_REQ_CREDIT_BUREAU_QRT")

df_test <- df_test[,!(names(df_test) %in% drop)]

#Dropping those columns based on IV

df_test <- subset(df_test,select=-c(YEARS_BEGINEXPLUATATION_MODE,YEARS_BEGINEXPLUATATION_MEDI,YEARS_BEGINEXPLUATATION_AVG,
                                      FLAG_DOCUMENT_3,ENTRANCES_MEDI,WALLSMATERIAL_MODE,ENTRANCES_AVG,DAYS_REGISTRATION,
                                      REGION_POPULATION_RELATIVE,ENTRANCES_MODE,AMT_ANNUITY,EMERGENCYSTATE_MODE,NONLIVINGAREA_AVG,
                                      NONLIVINGAREA_MEDI,NONLIVINGAREA_MODE,HOUSETYPE_MODE,BASEMENTAREA_AVG,REG_CITY_NOT_LIVE_CITY,
                                      BASEMENTAREA_MEDI,NAME_FAMILY_STATUS,BASEMENTAREA_MODE,OWN_CAR_AGE,FLOORSMIN_AVG,FLOORSMIN_MODE,
                                      FLOORSMIN_MEDI,AMT_REQ_CREDIT_BUREAU_YEAR,LIVINGAPARTMENTS_MEDI,LIVINGAPARTMENTS_AVG,YEARS_BUILD_MODE,
                                      YEARS_BUILD_MEDI,YEARS_BUILD_AVG,LANDAREA_AVG,LANDAREA_MEDI,LANDAREA_MODE,LIVINGAPARTMENTS_MODE,
                                      NAME_HOUSING_TYPE,NAME_CONTRACT_TYPE,COMMONAREA_AVG,COMMONAREA_MEDI,DEF_30_CNT_SOCIAL_CIRCLE,
                                      AMT_REQ_CREDIT_BUREAU_MON,LIVE_CITY_NOT_WORK_CITY,COMMONAREA_MODE,FONDKAPREMONT_MODE,DEF_60_CNT_SOCIAL_CIRCLE,
                                      FLAG_DOCUMENT_6,FLAG_WORK_PHONE,HOUR_APPR_PROCESS_START,FLAG_PHONE,FLAG_OWN_CAR,CNT_CHILDREN,CNT_FAM_MEMBERS,
                                      OBS_60_CNT_SOCIAL_CIRCLE,OBS_30_CNT_SOCIAL_CIRCLE,FLAG_DOCUMENT_13,FLAG_DOCUMENT_16,NAME_TYPE_SUITE,
                                      FLAG_DOCUMENT_14,FLAG_DOCUMENT_18,FLAG_DOCUMENT_8,FLAG_DOCUMENT_15,WEEKDAY_APPR_PROCESS_START,REG_REGION_NOT_WORK_REGION,
                                      FLAG_OWN_REALTY,REG_REGION_NOT_LIVE_REGION,FLAG_DOCUMENT_9,FLAG_DOCUMENT_11,FLAG_DOCUMENT_17,FLAG_DOCUMENT_4,
                                      FLAG_DOCUMENT_10))

#Selecting final_test data based on VIF, Area Under ROC curve, F1-score, accuracy and features significance.
final_test <- subset(df_test,select=c(AMT_CREDIT,DAYS_BIRTH,DAYS_EMPLOYED,DAYS_ID_PUBLISH,
                                  EXT_SOURCE_1,EXT_SOURCE_2,EXT_SOURCE_3,TOTALAREA_MODE,
                                  DAYS_LAST_PHONE_CHANGE,
                                  REGION_RATING_CLIENT,
                                  REG_CITY_NOT_WORK_CITY))

#imputing missing values using the method adopted for training data

summary(final_test)

final_test$EXT_SOURCE_1[is.na(final_test$EXT_SOURCE_1)==T] = median(final_test$EXT_SOURCE_1,na.rm=T)

final_test$EXT_SOURCE_2[is.na(final_test$EXT_SOURCE_2)==T] = median(final_test$EXT_SOURCE_2,na.rm=T)

final_test$EXT_SOURCE_3[is.na(final_test$EXT_SOURCE_3)==T] = median(final_test$EXT_SOURCE_3,na.rm=T)

final_test$TOTALAREA_MODE[is.na(final_test$TOTALAREA_MODE)==T] = median(final_test$TOTALAREA_MODE,na.rm=T)

summary(final_test)


#Feature Engineering

final_test$DAYS_BIRTH <- (final_test$DAYS_BIRTH/-365) #converting DAYS_BIRTH from days to years

#correcting anamoly in DAYS_EMPLOYED column
final_test$DAYS_EMPLOYED[final_test$DAYS_EMPLOYED==365243] = median(final_test$DAYS_EMPLOYED[final_test$DAYS_EMPLOYED != 365243])
final_test$DAYS_EMPLOYED <- (final_test$DAYS_EMPLOYED/-365) #converting DAYS_EMPLOYED from days to years

#business intuitive variable
final_test$DAYS_EMPLOYED_PERCENT <- final_test$DAYS_EMPLOYED/final_test$DAYS_BIRTH 

#Log Transformation
final_test['AMT_CREDIT'] = log(final_test['AMT_CREDIT'])
final_test['DAYS_EMPLOYED'] = log(final_test['DAYS_EMPLOYED']+1)
final_test['DAYS_LAST_PHONE_CHANGE'] = (final_test['DAYS_LAST_PHONE_CHANGE']*-1)/365
final_test['DAYS_LAST_PHONE_CHANGE'] = log(final_test['DAYS_LAST_PHONE_CHANGE']+1)
final_test['TOTALAREA_MODE'] = log(final_test['TOTALAREA_MODE']+1)
final_test['EXT_SOURCE_1'] = log(final_test['EXT_SOURCE_1'])
final_test['EXT_SOURCE_3'] = log(final_test['EXT_SOURCE_3'])

summary(final_test)

#Final chosen model
summary(model)

#Predicting target variable for test data
pred_test <- predict(model,final_test, type='response') #predicting probability

#Predicted target variable
pred_test

length(pred_test)
length(pred_test[pred_test>=0.1])

length(pred_test[pred_test>=0.1])/length(pred_test)*100
