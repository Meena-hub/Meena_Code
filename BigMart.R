#Load the data
trainDF <- read.csv('train_v9rqX0R.csv', header = TRUE, sep = ',',
                    na.strings=c("NA","NAN"," "))
testDF <- read.csv('test_AbJTz2l.csv', header = TRUE, sep = ',',
                   na.strings=c("NA","NAN"," "))
str(trainDF)
trainDF$Outlet_Size <- as.factor(trainDF$Outlet_Size)
trainDF$Item_Type <- as.factor(trainDF$Item_Type)
trainDF$Outlet_Type <- as.factor(trainDF$Outlet_Type)
trainDF$Outlet_Location_Type <- as.factor(trainDF$Outlet_Location_Type)
trainDF$Item_Fat_Content <- as.character(trainDF$Item_Fat_Content)
sum(is.na(trainDF$Item_Weight))
#Missing value
trainDF$Item_Weight<-ifelse(is.na(trainDF$Item_Weight),median(trainDF$Item_Weight,na.rm=TRUE),
                            trainDF$Item_Weight)
#inconsistency with fat content
table(trainDF$Item_Fat_Content)
trainDF$Item_Fat_Content <- ifelse(trainDF$Item_Fat_Content=="LF","Low Fat",
                                   trainDF$Item_Fat_Content)
trainDF$Item_Fat_Content <- ifelse(trainDF$Item_Fat_Content=="low fat","Low Fat",
                            ifelse(trainDF$Item_Fat_Content=="reg","Regular",
                                   trainDF$Item_Fat_Content))
trainDF$Item_Fat_Content <- as.factor(trainDF$Item_Fat_Content)
#Check missing values in outlet size
sum(is.na(trainDF$Outlet_Size))
table(trainDF$Outlet_Size)
trainDF$Outlet_Size <- as.character(trainDF$Outlet_Size)
trainDF$Outlet_Size<-ifelse(trainDF$Outlet_Size=='','Medium',trainDF$Outlet_Size)
trainDF$Outlet_Size <- as.factor(trainDF$Outlet_Size)
#summary
summary(trainDF)
#Derive a new variable - Years of business
trainDF$YOB <- 2021-trainDF$Outlet_Establishment_Year
summary(trainDF$YOB)

##############Exploratory Data Analysis###################
library(e1071)
library(Hmisc)
describe(trainDF)
skewness(trainDF$Item_Weight)
boxplot(trainDF$Item_Weight)

skewness(trainDF$Item_Visibility)
boxplot(trainDF$Item_Visibility)

skewness(trainDF$Item_MRP)
boxplot(trainDF$Item_MRP)

skewness(trainDF$Item_Outlet_Sales)
boxplot(trainDF$Item_Outlet_Sales)

skewness(trainDF$YOB)
boxplot(trainDF$YOB)

names(trainDF)
#Correlation
continuous_vars <- trainDF[c('Item_Weight','Item_Visibility','YOB','Item_Outlet_Sales'
                           ,'Item_MRP')]

corr <- rcorr(as.matrix(continuous_vars))
write.csv(corr$r, 'cormatrix.csv')
corr$r

#Categorical Variable Bi-Variate
chisq_tab <- table(trainDF$Outlet_Size, trainDF$Outlet_Location_Type)
#Chi Squared Test
#Hypothesis: Null - Outlet_Location_Type and Outlet_Size are independent
#Hypothesis: Alternate -  Outlet_Location_Type and Outlet_Size are dependent   
install.packages('MASS')
library(MASS)
chisq.test(chisq_tab)

chisq_tab_2 <- table(trainDF$Outlet_Location_Type, trainDF$Outlet_Type)
#Hypothesis: Null - Outlet_Location_Type and Outlet_Type are independent
#            Alternate - Outlet_Location_Type and Outlet_Type are dependent
chisq.test(chisq_tab_2)
chisq_tab_2
chisq_tab
##########################Multi-Variate Model##########################
names(trainDF)h
train_1 <-trainDF[c("Item_Weight","Item_Fat_Content","Item_Visibility",
                  "Item_Type","Item_MRP","Outlet_Size","Outlet_Location_Type",
                  "Outlet_Type","Item_Outlet_Sales","YOB")]
model_try <- lm(Item_Outlet_Sales~., data = trainDF)
summary(model_try)
model_1 <- lm(Item_Outlet_Sales~., data = train_1)
summary(model_1)
train_1$predict_model_1 <- predict(model_1)
rmse_model_1 <- sqrt(mean((train_1$Item_Outlet_Sales - train_1$predict_model_1)^2))
rmse_model_1

############################Preparation of Test Data#######################
#Impute Missing Values for Item_Weight
sum(is.na(testDF$Item_Weight))
testDF$Item_Weight <- ifelse(is.na(testDF$Item_Weight),
                           median(testDF$Item_Weight, na.rm = TRUE),
                           testDF$Item_Weight)
#Item Fat Content Inconsistency
table(testDF$Item_Fat_Content)
testDF$Item_Fat_Content <- as.character(testDF$Item_Fat_Content)
str(testDF$Item_Fat_Content)
testDF$Item_Fat_Content <- ifelse(testDF$Item_Fat_Content=='LF', 
                                'Low Fat', testDF$Item_Fat_Content)
testDF$Item_Fat_Content <- ifelse(testDF$Item_Fat_Content=='low fat', 
                                'Low Fat', testDF$Item_Fat_Content)
testDF$Item_Fat_Content <- ifelse(testDF$Item_Fat_Content=='reg', 
                                'Regular', testDF$Item_Fat_Content)
testDF$Item_Fat_Content <- as.factor(testDF$Item_Fat_Content)
#Handle values for Item_outlet_size
table(testDF$Outlet_Size)
testDF$Outlet_Size <- as.character(testDF$Outlet_Size)
testDF$Outlet_Size <- ifelse(testDF$Outlet_Size=='', 'Medium', testDF$Outlet_Size)
testDF$Outlet_Size <- as.factor(testDF$Outlet_Size)
str(testDF)
str(trainDF)
#Generate variable YOB - Years of Business
testDF$YOB <- 2021 - testDF$Outlet_Establishment_Year

#Retain only important variables
test_1 <- testDF[c("Item_Weight","Item_Fat_Content","Item_Visibility",
                 "Item_Type","Item_MRP","Outlet_Size","Outlet_Location_Type",
                 "Outlet_Type","YOB")]
#Predicted Item_Outlet_Sales for the test dataset
test_1$Pred_Item_Outlet_Sales <- predict(model_1, test_1)
write.csv(test_1, 'final_preds.csv')

###################Decision Tree######################
library(rpart)
library(rpart.plot)
names(trainDF)
output_tree <- rpart(Item_Outlet_Sales~., data = train_1)
summary(output_tree)
rpart.plot(output_tree)
train_1$Preds_sales_dtree <- predict(output_tree, train_1)

rmse_dtree <- sqrt(mean((train_1$Item_Outlet_Sales - train_1$Preds_sales_dtree)^2))
rmse_dtree

##################Random Forest######################################
library(randomForest)
set.seed(123)
train_1 <-trainDF[c("Item_Weight","Item_Fat_Content","Item_Visibility",
                  "Item_Type","Item_MRP","Outlet_Size","Outlet_Location_Type",
                  "Outlet_Type","Item_Outlet_Sales","YOB")]
rf <- randomForest(Item_Outlet_Sales~., ntree = 100, data = train_1)
train_1$preds_rf <- predict(rf,train_1)
rmse_rf <- sqrt(mean((train_1$Item_Outlet_Sales - train_1$preds_rf)^2))
rmse_rf

test_1$Pred_Item_Outlet_Sales <- predict(rf, test_1)
test_1$preds_rf <- predict(rf,test_1)
rmse_rft <- sqrt(mean((test_1$Pred_Item_Outlet_Sales - test_1$preds_rf)^2))
rmse_rft
write.csv(test_1, 'final_preds.csv')
