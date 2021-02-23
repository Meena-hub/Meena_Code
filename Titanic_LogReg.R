#Import the data
train_data <- read.csv('Titanic_train.csv', header = TRUE, sep = ',', na.strings=c(""))
#Audit the data
str(train_data)
#PassID,Name,ticket,cabin are not necessary
#Check Missing values
sum(is.na(train_data))
sum(is.na(train_data$Age))
train_data$Age<-ifelse(is.na(train_data$Age),median(train_data$Age,na.rm=TRUE),
                           train_data$Age)
sum(is.na(train_data$Embarked))
table(train_data$Embarked)
train_data$Embarked <- ifelse(is.na(train_data$Embarked), 'S', train_data$Embarked)
sum(is.na(train_data$Embarked))
train_data$Embarked <- as.factor(train_data$Embarked)
sum(is.na(train_data))
#To check inconsistency in categorical variables
table(train_data$Pclass)
table(train_data$Sex)
table(train_data$SibSp)
table(train_data$Parch)
#Transform the variables
train_data$Survived <- as.factor(train_data$Survived)
train_data$Pclass <- as.factor(train_data$Pclass)
train_data$Sex <- as.factor(train_data$Sex)
train_data$SibSp <- as.factor(train_data$SibSp)
train_data$Parch <- as.factor(train_data$Parch)
#Check data distribution
library(e1071)
skewness(train_data$Age)
boxplot(train_data$Age)
skewness(train_data$Fare)
boxplot(train_data$Fare)
train_data$Fare <- scale(train_data$Fare,center=TRUE,scale=TRUE)
####Bi-Variate Analysis
library(Hmisc)
names(train_data)
contvar <- subset(train_data, select = c('Age','SibSp','Parch','Fare'))
#rcorr accepts only numeric input so we convert dataset into matrix
correlation_matrix <- rcorr(as.matrix(contvar))
#the o/p of rcorr is a list
is.data.frame(correlation_matrix)
is.list(correlation_matrix)
#Categorical varibale bi-variate analysis
#Check relation between Gender and Survival
tbl <- table(train_data$Survived,train_data$Sex)
tbl
#Null hypothesis : Survived and Sex are independent
#Alternate hypothesis : Survived and Sex are dependent
library(MASS)
chisq.test(tbl)
#this shows Survived is dependent on Sex because p value is almost 0
chisq.test(table(train_data$Survived,train_data$Pclass))
chisq.test(table(train_data$Pclass,train_data$Survived))
#Making the model ----- Logistic REg--- 
names(train_data)
set.seed(123)
model_1ogReg <- glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, 
                    data = train_data, family='binomial')
summary(model_1ogReg)
##Predict - Model Scoring
train_data$Pred_Survived <- model_1ogReg$fitted.values
train_data$Predicted <- ifelse(train_data$Pred_Survived>0.5,1,0)
cm_logistic <- table(train_data$Survived,train_data$Predicted)
#### Test Data ####
str(test)
test$Pclass <- as.factor(test$Pclass)
test$SibSp <- as.factor(test$SibSp)
test$Parch <- as.factor(test$Parch)
#Checking missing values
sum(is.na(test))
sum(is.na(test$Embarked))
table(test$Parch)
test$Parch <- ifelse(test$Parch==7,1,test$Parch)
test$Age <- ifelse(is.na(test$Age), 
                   median(test$Age, na.rm = TRUE),
                   test$Age)
test$Fare <- ifelse(is.na(test$Fare), median(test$Fare, na.rm = TRUE),
                    test$Fare)
test$Fare <- as.matrix(test$Fare)
#Predict on the test data
test$Preds_probs <- predict.glm(model_1ogReg, test, type = 'response')
test$Survived <- ifelse(test$Preds_probs>0.5,1,0)
getwd()
write.csv(test, 'scored_test.csv')
Submission <- subset(test, select = c('PassengerId','Survived'))
print(Submission)

######################Decision Tree#####################################################
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
##png(file = "decisiontree.bmp")

output_tree <- rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train_data)
summary(output_tree)
rpart.plot(output_tree)
treeP
train_data$preds_tree <- predict(output_tree, train_data, type = 'class')
table(train_data$Survived, train_data$preds_tree)

#################Random Forest######################################################
install.packages("randomForest")
library(randomForest)
set.seed(123)

rf <- randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, ntree = 1000, data = train_data)
summary(rf)
rf$confusion
rf$importance
