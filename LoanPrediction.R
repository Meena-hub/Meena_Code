###########################Import and Audit the Data################
setwd('F:\\Sessions\\DSP04ONL\\LoanPred')
train <- read.csv('train_ctrUa4K.csv',header = TRUE, na.strings = c(""))
test <- read.csv('test_lAUu6dG.csv', header = TRUE, na.strings = c(""))

str(train)

#######################Data Prep and Data Transformation################

#change 3+ in dependents to 3

train$Dependents <- as.character(train$Dependents)
train$Dependents <- ifelse(train$Dependents=='3+','3',train$Dependents)
train$Dependents <- as.factor(train$Dependents)
table(train$Dependents)
sum(is.na(train))

train$Credit_History <- as.factor(train$Credit_History)

names(train)
#Missing values
set.seed(123)
install.packages('Amelia')
library(Amelia)
newimpute <- amelia(train, m=5,
                    idvars = c('Loan_ID', 'Education','ApplicantIncome','CoapplicantIncome','Property_Area','Loan_Status'),
                    noms = c('Gender','Married', 'Self_Employed','Credit_History'),
                    ords = c('Dependents'))

write.amelia(newimpute, file.stem = 'imputed_data_set')

####################Import the imputed data set#####################

train_imp <- read.csv('imputed_data_set1.csv', na.strings = c(''))
sum(is.na(train_imp))

str(train_imp)
train_imp$Gender <- as.factor(train_imp$Gender)
train_imp$Married <- as.factor(train_imp$Married)
train_imp$Education <- as.factor(train_imp$Education)
train_imp$Self_Employed <- as.factor(train_imp$Self_Employed)
train_imp$Credit_History <- as.factor(train_imp$Credit_History)
train_imp$Property_Area <- as.factor(train_imp$Property_Area)
train_imp$Loan_Status <- as.factor(train_imp$Loan_Status)
train_imp$Dependents <- as.factor(train_imp$Dependents)
sum(is.na(train_imp$Credit_History))

summary(train_imp)

library(e1071)
skewness(train_imp$ApplicantIncome)
skewness(train_imp$CoapplicantIncome)
skewness(train_imp$LoanAmount)
skewness(train_imp$Loan_Amount_Term)

#Boxplot

boxplot(train_imp$ApplicantIncome)
boxplot(train_imp$CoapplicantIncome)
boxplot(train_imp$LoanAmount)
boxplot(train_imp$Loan_Amount_Term)


#########Bi Variate Analysis######
library(sqldf)
cont_vars <- sqldf('select ApplicantIncome,CoapplicantIncome,LoanAmount,
                  Loan_Amount_Term from train_imp')

continuous_vars <- subset(train_imp, 
                          select = c('ApplicantIncome',
                                     'CoapplicantIncome',
                                     'LoanAmount','Loan_Amount_Term'))

library(Hmisc)

corr <- rcorr(as.matrix(cont_vars))
corr$r
#There is no variable which is correlated, all are independent

#Chi Square test between Gender and Loan_Status
#Chi Square Test between Married and Loan_Status

tbl_1 <- table(train_imp$Gender, train_imp$Loan_Status)
tbl_2 <- table(train_imp$Married, train_imp$Loan_Status)
tbl_1
tbl_2

library(MASS)
chisq.test(tbl_1)
#this test shows there is no dependency btw gender and loan status
#rejected alternate hypothesis p value is 0.7 which is high
#null hypothesis - because inpedenpent
chisq.test(tbl_2)
#this test shows there is a dependency btw married and loan status

#Write and SQL query to find the average income of loan status Yes and No
#Write and SQL query to find the average loan_amt of loan status Yes and No
avg_income <- sqldf("select avg(ApplicantIncome),Loan_Status from train_imp
                    group by Loan_Status")
avg_loanAmt <- sqldf("select avg(LoanAmount),Loan_Status from train_imp
                    group by Loan_Status")

###########Multi-Variate Model##################

#Logistic Regression
train_imp <- train_imp[,-1]
train_imp <- train_imp[,-1]
names(train_imp)
model1_LR <- glm(Loan_Status~., data = train_imp, family='binomial')
summary(model1_LR)

#Predicted values
train_imp$Pred_loanStatus <- model1_LR$fitted.values
train_imp$Predicted <- ifelse(train_imp$Pred_loanStatus>0.5,1,0)

#Confusion matrix
conf1 <- table(train_imp$Loan_Status, train_imp$Predicted)
conf1
#85+107+15+407 -> 614
# (85+407)/614 --> 80.13%

#scale
train_imp_scale <- train_imp
train_imp_scale$ApplicantIncome <- scale(train_imp$ApplicantIncome,center=TRUE,scale=TRUE)
train_imp_scale$CoapplicantIncome <- scale(train_imp$CoapplicantIncome,center=TRUE,scale=TRUE)
train_imp_scale$LoanAmount <- scale(train_imp$LoanAmount,center=TRUE,scale=TRUE)
train_imp_scale$Loan_Amount_Term <- scale(train_imp$Loan_Amount_Term,center=TRUE,scale=TRUE)
names(train_imp_scale)
model1_LR_scale <- glm(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+
                         ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+
                         Credit_History+Property_Area, data = train_imp_scale, family='binomial')
summary(model1_LR_scale)
#Predicted values
train_imp_scale$Pred_loanStatus <- model1_LR_scale$fitted.values
train_imp_scale$Predicted <- ifelse(train_imp_scale$Pred_loanStatus>0.5,1,0)

#Confusion matrix
conf1_scale <- table(train_imp_scale$Loan_Status, train_imp_scale$Predicted)
conf1_scale
#85+107+15+407 -> 614
# (85+407)/614 --> 80.13%

##Test data
summary(test)
str(test)
str(train_imp_scale)
test$Gender <- as.factor(test$Gender)
test$Married <- as.factor(test$Married)
test$Education <- as.factor(test$Education)
test$Self_Employed <- as.factor(test$Self_Employed)
test$Credit_History <- as.factor(test$Credit_History)
test$Property_Area <- as.factor(test$Property_Area)
test$Dependents <- as.factor(test$Dependents)
test$Dependents <- as.character(test$Dependents)
test$Dependents <- ifelse(test$Dependents=='3+','3',test$Dependents)
test$Dependents <- as.factor(test$Dependents)
test$Loan_Amount_Term <- as.matrix(test$Loan_Amount_Term)
test$LoanAmount <- as.matrix(test$LoanAmount)
test$CoapplicantIncome <- as.matrix(test$CoapplicantIncome)
test$ApplicantIncome <- as.matrix(test$ApplicantIncome)
test <- test[,-1]
test$Preds <- predict.glm(model1_LR_scale, test, type = 'response')
test$LoanStatus <- ifelse(test$Preds>0.5,1,0)

#####################Decision Tree##################
library(rpart)
library(rpart.plot)
dtree <- rpart(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+
                 CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area,
               data=train_imp)
rpart.plot(dtree)
train_imp$preds_tree <- predict(dtree, train_imp, type = 'class')
cm_tree <- table(train_imp$Loan_Status, train_imp$preds_tree)
cm_tree
##79.9% accuracy
###################RandomForest##################################
library(randomForest)
rf <- randomForest(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+
                     CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area,
                   data=train_imp, ntree = 500)
rf$confusion
##78.9% accuracy
