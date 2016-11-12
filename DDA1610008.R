library(car)
library(Hmisc)
library(ROCR)
library(caret)
library(ggplot2)
library(dplyr)

# Download the data set as german_credit
german_credit <- read.csv("german.csv", stringsAsFactors = F)

str(german_credit)

#-------------------------------------------------
# Data prpeapartion and feature transformation
#------------------------------------------------
  
#1000 observations with 21 variables.
#Converting the relevant columns to factor as per data dictionary
#Will also convert categorical variables into dummy variables

german_credit$Status.of.existing.checking.account <- as.factor(german_credit$Status.of.existing.checking.account)
summary(german_credit$Status.of.existing.checking.account)
dummy_1 <- data.frame(model.matrix(~ german_credit$Status.of.existing.checking.account))
#Since we need only n-1 dummary variables, we remove one
dummy_1 <- dummy_1[,-1]
colnames(dummy_1) <- c("A12","A13","A14")

german_credit$Credit.history <- as.factor(german_credit$Credit.history)
summary(german_credit$Credit.history)
dummy_2 <- data.frame(model.matrix(~german_credit$Credit.history ))
#Since we need only n-1 dummary variables, we remove one
dummy_2 <-dummy_2[,-1]
colnames(dummy_2) <- c("A31","A32","A33","A34")

german_credit$Purpose <- as.factor(german_credit$Purpose)
summary(german_credit$Purpose)
dummy_3 <- data.frame(model.matrix(~german_credit$Purpose ))
#Since we need only n-1 dummary variables, we remove one
dummy_3 <- dummy_3[,-1]
colnames(dummy_3) <- c("A41","A42","A43","A44","A45","A46","A48","A49","A410")


german_credit$Savings.account.bonds <- as.factor(german_credit$Savings.account.bonds)
summary(german_credit$Savings.account.bonds)
dummy_4 <- data.frame(model.matrix(~german_credit$Savings.account.bonds))
#Since we need only n-1 dummary variables, we remove one
dummy_4 <- dummy_4[,-1]
colnames(dummy_4) <- c("A62","A63","A64","A65")


german_credit$Present.employment.since. <- as.factor(german_credit$Present.employment.since.)
summary(german_credit$Present.employment.since.)
dummy_5 <- data.frame(model.matrix(~german_credit$Present.employment.since.))
#Since we need only n-1 dummary variables, we remove one
dummy_5 <- dummy_5[,-1]
colnames(dummy_5) <- c("A72","A73","A74","A75")


german_credit$Personal.status.and.sex <- as.factor(german_credit$Personal.status.and.sex)
summary(german_credit$Personal.status.and.sex)
dummy_6 <- data.frame(model.matrix(~german_credit$Personal.status.and.sex ))
#Since we need only n-1 dummary variables, we remove one
dummy_6 <- dummy_6[,-1]
colnames(dummy_6) <- c("A92","A93","A94")

german_credit$Other.debtors...guarantors <- as.factor(german_credit$Other.debtors...guarantors)
summary(german_credit$Other.debtors...guarantors)
dummy_7 <- data.frame(model.matrix(~german_credit$Other.debtors...guarantors))
#Since we need only n-1 dummary variables, we remove one
dummy_7 <- dummy_7[,-1]
colnames(dummy_7) <- c("A102","A103")

german_credit$Property <- as.factor(german_credit$Property)
summary(german_credit$Property)
dummy_8 <- data.frame(model.matrix(~german_credit$Property))
#Since we need only n-1 dummary variables, we remove one
dummy_8 <- dummy_8[,-1]
colnames(dummy_8) <- c("A122","A123","A124")


german_credit$Other.installment.plans <- as.factor(german_credit$Other.installment.plans)
summary(german_credit$Other.installment.plans)
dummy_9 <- data.frame(model.matrix(~german_credit$Other.installment.plans))
#Since we need only n-1 dummary variables, we remove one
dummy_9 <- dummy_9[,-1]
colnames(dummy_9) <- c("A142","A143")


german_credit$Housing. <- as.factor(german_credit$Housing.)
summary(german_credit$Housing.)
dummy_10 <- data.frame(model.matrix(~german_credit$Housing.))
#Since we need only n-1 dummary variables, we remove one
dummy_10 <- dummy_10[,-1]
colnames(dummy_10) <- c("A152","A153")


german_credit$Job_status <- as.factor(german_credit$Job_status)
summary(german_credit$Job_status)
dummy_11 <- data.frame(model.matrix(~german_credit$Job_status))
#Since we need only n-1 dummary variables, we remove one
dummy_11 <- dummy_11[,-1]
colnames(dummy_11) <- c("A172","A173","A174")

german_credit$Telephone.<- as.factor(german_credit$Telephone.)
summary(german_credit$Telephone.)
dummy_12 <- data.frame(model.matrix(~german_credit$Telephone.))
#Since we need only n-1 dummary variables, we remove one
colnames(dummy_12)<- c("A191","A192")
dummy_12  <- dummy_12[,-1]

german_credit$foreign.worker <- as.factor(german_credit$foreign.worker)
summary(german_credit$foreign.worker)
dummy_13 <- data.frame(model.matrix(~german_credit$foreign.worker))
#Since we need only n-1 dummary variables, we remove one
colnames(dummy_13) <- c("A201","A202")
dummy_13 <- dummy_13[,-1]

#check if there are any missing values
sum(is.na(german_credit))

str(german_credit)

#We now test for outliers. We will begin with numerical variables

#Duration in months
#---------------------
boxplot(german_credit$Duration.in.month)
quantile(german_credit$Duration.in.month, seq(0,1,0.01))

#there's a massive jump from 98% quantile. Hence, we can cap the higher values to 48
german_credit$Duration.in.month[german_credit$Duration.in.month > 48] <- 48

summary(german_credit$Duration.in.month)

#Credit amount
-----------------------
summary(german_credit$Credit.amount)  
boxplot(german_credit$Credit.amount)
quantile(german_credit$Credit.amount, seq(0,1,0.01))

nrow(german_credit[german_credit$Credit.amount>10961,])

#massive jump from 97%. since only 30 values above it, we cap all these values
german_credit$Credit.amount[german_credit$Credit.amount >10961] <- 10961

#Installment rate
------------------------------
boxplot(german_credit$Installment.rate.in.percentage.of.disposable.income)
#No visible outliers
#convert to numeric
german_credit$Installment.rate.in.percentage.of.disposable.income <- as.numeric(german_credit$Installment.rate.in.percentage.of.disposable.income)
summary(german_credit$Installment.rate.in.percentage.of.disposable.income)

#Present Residence since
---------------------------
boxplot(german_credit$Present.residence.since)  
summary(german_credit$Present.residence.since)

#No visible outliers

#Age
#--------------------------
boxplot(german_credit$Age.in.Years)  
summary(german_credit$Age.in.Years)
quantile(german_credit$Age.in.Years, seq(0,1,0.01))

#age range is quite reasonable (19-75) for spenders. Therefore we won't perform outlier treatment.
summary(german_credit$Age.in.Years)

#Number of existing credits in the bank
#---------------------------------------
  boxplot(german_credit$Number.of.existing.credits.at.this.bank.)
summary(german_credit$Number.of.existing.credits.at.this.bank.)
quantile(german_credit$Number.of.existing.credits.at.this.bank., seq(0,1,0.01))
#no visible outliers except for one. hence capping upper value to 3
german_credit$Number.of.existing.credits.at.this.bank.[german_credit$Number.of.existing.credits.at.this.bank. >3] <- 3

summary(german_credit$Number.of.existing.credits.at.this.bank.)

#Number of people liable
#--------------------------------
boxplot(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)  
#Just one outlier.
summary(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)


#combine all the dummy variables and numeric variables. We exclude the original factor columns
#for which we had created the dummies.
german_credit_1 <- cbind(german_credit[,c(2,5,8,11,13,16,18,21)], dummy_1,dummy_2,
                         dummy_3,dummy_4, dummy_5,dummy_6,dummy_7,dummy_8,dummy_9,
                         dummy_10,dummy_11,dummy_12,dummy_13)


#check the ratio of 0 and 1
table(german_credit_1$Default_status)

#we need to split the data in a similar ratio. train and test data is also 70 30 ratio
library(caTools)
set.seed(100)
german_credit_1_split <- sample.split(german_credit_1$Default_status, SplitRatio = 0.7)
german_credit_train <- german_credit_1[german_credit_1_split,]
german_credit_test <- german_credit_1[!german_credit_1_split,]

table(german_credit_train$Default_status)

#create the initial model.
initial_model <- glm(Default_status~.,data = german_credit_train,family = "binomial")


summary(initial_model)

#we use step function to eliminate unneccessary variables.
best_model = step(initial_model,direction = "both")
summary(best_model)
vif(best_model)

# Only one variable shows VIF>3 but it is significant.
# So we will eliminate A46 since it has highest P value and observe AIC 

model_1 <-  glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                  Age.in.Years + A13 + A14 + A32 + A33 + A34 + A41 + A42 + 
                  A43 + A44 + A410 + A64 + A65 + A74 + A93 + A103 + A143 + 
                  A152, family = "binomial", data = german_credit_train)
 
summary(model_1)

# AIC hasn't changed much. now observe vif to select variable to remove

vif(model_1)

# Keeping high VIF since it is significant. Eliminating high p value instead (A42)

model_2 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + A13 + A14 + A32 + A33 + A34 + A41 + 
                 A43 + A44 + A410 + A64 + A65 + A74 + A93 + A103 + A143 + 
                 A152, family = "binomial", data = german_credit_train)
summary(model_2)

#check for insignificant variables with vif >3
vif(model_2)

#a410 has a high p value and a32 is significant
model_3 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + A13 + A14 + A32 + A33 + A34 + A41 + A43 + 
                 A44 +  A64 + A65 + A74 + A93 + A103 + A143 + A152, 
               family = "binomial", data = german_credit_train)
summary(model_3)

#check vif >3
vif(model_3)

#since a32 is significant, we remove a103 based on p value
model_4 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + A13 + A14 + A32 + A33 + A34 + A41 + A43 + 
                 A44 + A64 + A65 + A74 + A93 + A143 + A152, family = "binomial", 
               data = german_credit_train)

summary(model_4)                              

#check vif >3
vif(model_4)

#since a32 is significant, we will remove a43 based on p value
model_5 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + A13 + A14 + A32 + A33 + A34 + A41 + 
                 A44 + A64 + A65 + A74 + A93 + A143 + A152, family = "binomial", 
               data = german_credit_train)

summary(model_5)

#check vif > 3
vif(model_5)

#since a32 is significant, we remove  a44  based on p value
model_6 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 Age.in.Years + A13 + A14 + A32 + A33 + A34 + A41 + 
                 A64 + A65 + A74 + A93 + A143 + A152, family = "binomial", 
               data = german_credit_train)

summary(model_6)

vif(model_6)

#since a32 is significant, we remove  age  based on p value
model_7 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                  A13 + A14 + A32 + A33 + A34 + A41 + A64 + 
                 A65 + A74 + A93 + A143 + A152, family = "binomial", data = german_credit_train)
summary(model_7)
vif(model_7)

#since a32 is significant, we remove  a65  based on p value
model_8 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 A13 + A14 + A32 + A33 + A34 + A41 + A64 + A74 + A93 + 
                 A143 + A152, family = "binomial", data = german_credit_train)

summary(model_8)
vif(model_8)

#since a32 is significant, we remove  a64  based on p value
model_9 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                 A13 + A14 + A32 + A33 + A34 + A41 + A74 + A93 + A143 + 
                 A152, family = "binomial", data = german_credit_train)
summary(model_9)
vif(model_9)

#since a32 is significant, we remove  a93   based on p value
model_10 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                  A13 + A14 + A32 + A33 + A34 + A41 + A74 + A143 + A152, 
                family = "binomial", data = german_credit_train)
summary(model_10)
vif(model_10)

#since a32 is significant, we remove  a143  based on p value
model_11<-glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                A13 + A14 + A32 + A33 + A34 + A41 + A74 + A152, family = "binomial", 
              data = german_credit_train)

summary(model_11)
vif(model_11)

# we remove  a152  based on p value
model_12 <- glm(formula = Default_status ~ Installment.rate.in.percentage.of.disposable.income + 
                  Age.in.Years + A13 + A14 + A32 + A33 + A34 + A74 + A143 + 
                  dummy_14.duration_bucket_number3 + dummy_14.duration_bucket_number4 + 
                  dummy_14.duration_bucket_number6 + dummy_14.duration_bucket_number8, 
                family = "binomial", data = german_credit_train)
summary(model_12)
vif(model_12)

#we remove  a143  based on p value
model_13 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                  A13 + A14 + A32 + A33 + A34 + A41 + A74, family = "binomial", 
                data = german_credit_train)
summary(model_13)

#we remove  a41  based on p value
model_14 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                  A13 + A14 + A32 + A33 + A34 + A74, family = "binomial", 
                data = german_credit_train)
summary(model_14)

#we remove  a74  based on p value
model_15 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                  A13 + A14 + A32 + A33 + A34, family = "binomial", data = german_credit_train)
summary(model_15)

#we remove  a13  based on p value
model_16 <- glm(formula = Default_status ~ Duration.in.month + Installment.rate.in.percentage.of.disposable.income + 
                  A14 + A32 + A33 + A34, family = "binomial", data = german_credit_train)
summary(model_16)


#we remove  installment rate  based on p value
model_18 <- glm(formula = Default_status ~ Duration.in.month +  
                  A14 + A32 + A33 + A34, family = "binomial", data = german_credit_train)
summary(model_18)

#we remove  a33  based on p value
model_19 <- glm(formula = Default_status ~ Duration.in.month + A14 + A32 + 
                  A34, family = "binomial", data = german_credit_train)
summary(model_19)

#we remove  a32  based on p value
model_20 <- glm(formula = Default_status ~ Duration.in.month + A14 + 
                  A34, family = "binomial", data = german_credit_train)
summary(model_20)
vif(model_20)



#Since all variables now are significant, we'll choose model 20 as the final model
# AIC 728.65
final_model <- model_20

# Model Evaluation
#----------------------------
german_credit_train$predict_prob <- predict(final_model, type = "response")
rcorr.cens(german_credit_train$predict_prob, german_credit_train$Default_status)

#C-test for training is 76%

#Measure the c- statistic
german_credit_test$predict_prob <- predict(final_model, newdata = german_credit_test, type = "response")
rcorr.cens(german_credit_test$predict_prob, german_credit_test$Default_status)
#Ctest for test data set is 71.9%

#Measure the k statistic
model_score <- prediction(german_credit_train$predict_prob,german_credit_train$Default_status)
model_perf <- performance(model_score, "tpr", "fpr")

plot(model_perf)

ks_table <- attr(model_perf, "y.values")[[1]] - attr(model_perf,"x.values")[[1]]

ks = max(ks_table)

which(ks_table == ks)
39/700 #first decile

#K stat for training is 39

model_score_test <-prediction(german_credit_test$predict_prob,german_credit_test$Default_status)

model_perf_test <- performance(model_score_test, "tpr", "fpr")

ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])
ks_test = max(ks_table_test)
which(ks_table_test == ks_test)
23/300
#kstat for test data set is 23

#lies in the first decile

# Selecting Threshold value
#-----------------------------
#Since we're interested in knowing if customer will default, 1 will indicate positive identification.
#So we're interested in accurately prediciting 1. 
confusionMatrix(as.numeric(german_credit_train$predict_prob >0.25), german_credit_train$Default_status, positive = "1")

#For training data 
# Sensitivity : 84.76%
# Specificity:  56.94%
# Accuracy :    65.29%

confusionMatrix(as.numeric(german_credit_test$predict_prob >0.25), german_credit_test$Default_status, positive = "1")

#For testing data 
# Sensitivity : 75.56%
# Specificity:  55.71%
# Accuracy :    61.67%


#with the current model, we observe that only certain factors of a categorical variable are included.
# this doesn't make too much business sense. Let us try adding the remaining factors of the variable
# while keeping the most significant ones to check if the model performs better

model_21 <- glm(formula = Default_status ~ Duration.in.month + A14 + A12 + A13 +  A31 + A32 + A33+
                    A34, family = "binomial", data = german_credit_train)
summary(model_21)
vif(model_21)

#AIC of 711.37 (lower and hence better than previous of 728.65)

# we will retain high vif factors in order to keep the model realistic.
#now we perform other checks to observe model behaviour.
final_model_2 <- model_21

# Model Evaluation
#----------------------------
german_credit_train$predict_prob <- predict(final_model_2, type = "response")
rcorr.cens(german_credit_train$predict_prob, german_credit_train$Default_status)

#C-stat : 79.16% for training data (better than previous model score of 76%)

#Measure the c- statistic
german_credit_test$predict_prob <- predict(final_model_2, newdata = german_credit_test, type = "response")
rcorr.cens(german_credit_test$predict_prob, german_credit_test$Default_status)

#C-stat : 72.8% for test data (better than previous model of 71.9%)

#Measure the k statistic
model_score <- prediction(german_credit_train$predict_prob,german_credit_train$Default_status)
model_perf <- performance(model_score, "tpr", "fpr")

plot(model_perf)

ks_table <- attr(model_perf, "y.values")[[1]] - attr(model_perf,"x.values")[[1]]

ks = max(ks_table)

which(ks_table == ks)
102/700 #first decile (better than previoius model of 39)


model_score_test <-prediction(german_credit_test$predict_prob,german_credit_test$Default_status)

model_perf_test <- performance(model_score_test, "tpr", "fpr")

ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])
ks_test = max(ks_table_test)
which(ks_table_test == ks_test)
55/300 

#lies in the first decile (better than previous model score of 23)

# Selecting Threshold value
#-----------------------------
#Since we're interested in knowing if customer will default, 1 will indicate positive identification.
#So we're interested in accurately prediciting 1. 
confusionMatrix(as.numeric(german_credit_train$predict_prob >0.25), german_credit_train$Default_status, positive = "1")

#For training data 
# Sensitivity : 82.38%
# Specificity:  60.41%
# Accuracy :    67%

confusionMatrix(as.numeric(german_credit_test$predict_prob >0.25), german_credit_test$Default_status, positive = "1")

#For testing data 
# Sensitivity : 76.67% (higher than previous model)
# Specificity:  57.14% (higher than previous model)
# Accuracy :    63%    (higher than previous model)

#Thus we take final_model_2 as our final model. All evaluative scores show a fairly accurate model
