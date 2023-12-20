# to use Random Forest C4.5 and C5.0 algorithm for comparison
# optimal no. mtry:https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

rm(list = ls()) # to clear the environment on R console

library(ggplot2) # for data visualisation
library(tidyverse) # for data manipulation
library(dplyr) # for mutate function, delete columns
library(corrplot) # plot correlation matrix

# to load dataset
df <- read.csv('C:/Users/miche/Desktop/RDataAnalysis/Online Shoppers Purchasing Intention/online_shoppers_intention.csv')

# to display preview of dataset
head(df)

# to see variable types, no. of obs and column
glimpse(df)   #contains 12,330 rows and 18 columns

# to check for missing values 
anyNA(df)
sum(is.na(df)) # there are no missing values

# to generate descriptive statistics of dataset
summary(df)

# to check if dataset contains any duplicated values
duplicated(df)
sum(duplicated(df)) # there are 125 (1.01%) duplicated values in csv file

# to delete duplicated values in R using dplyr's distinct() function
df2 <- df %>%
  distinct()

# to view new dimension of data frame
dim(df2) # contains 12,205 rows and 18 columns after deleting duplicates
glimpse(df2)

# to double check if data frame still contain any duplicated values
duplicated(df2)
sum(duplicated(df2)) #after deletion, no more duplicated values

# to convert Month from <chr> to factor, factor is known as a categorical value that stores both string and integer values
df2$Month <- as.character(df2$Month)
# to change from June to Jun, without changing June, R will recognise it as "NA"
df2$Month[df2$Month == "June"] <- "Jun"
df2$Month <- as.factor(df2$Month)
df2$Month = factor(df2$Month, levels = month.abb)

# to check that month <chr> is converted to  a factor for further processing
glimpse (df2)

# Exploratory Analysis: any distinct difference between shoppers (True) and non-shoppers(False)
# Using "Revenue" as target variable for all independent variables
ggplot(df2, aes(Administrative, fill= Revenue)) + geom_bar() + labs(x = "Administrative") + labs(y = " ")

# Computing the Contingency Table (Administrative)
table(df2$Revenue, df2$Administrative)

ggplot(df2, aes(Administrative_Duration, fill = Revenue, colour = Revenue)) +
  geom_density (alpha = 0.6) 

# Computing the Contingency Table (Administrative_Duration)
table(df2$Revenue, df2$Administrative_Duration)

ggplot(df2, aes(Informational, fill=Revenue)) + geom_bar() + labs(x = "Informational") + labs(y = " ")

# Computing the Contingency Table (Informational)
table(df2$Revenue, df2$Informational)

ggplot(df2, aes(Informational_Duration, fill = Revenue, colour = Revenue)) +
  geom_density (alpha = 0.6) 

ggplot(df2, aes(ProductRelated, fill = Revenue, colour = Revenue)) +
  geom_density (alpha = 0.6) 

ggplot(df2, aes(ProductRelated_Duration, fill = Revenue, colour = Revenue)) +
  geom_density (alpha = 0.6) 

ggplot(df2, aes(BounceRates, fill = Revenue, colour = Revenue)) +
  geom_density (alpha = 0.6) 

ggplot(df2, aes(ExitRates, fill = Revenue, colour = Revenue)) +
  geom_density (alpha = 0.6) 

ggplot(df2, aes(PageValues, fill = Revenue, colour = Revenue)) +
  geom_density (alpha = 0.6)

ggplot(df2, aes(SpecialDay, fill=Revenue)) + geom_bar() + labs(x = "Special Day") + labs(y = " ")

# Computing the Contingency Table (SpecialDay)
table(df2$Revenue, df2$SpecialDay)

ggplot(df2, aes(Month, fill=Revenue)) + geom_bar() + labs(x = "Month") + labs(y = " ")

# Computing the Contingency Table (Month)
table(df2$Revenue, df2$Month)

ggplot(df2, aes(OperatingSystems, fill=Revenue)) + geom_bar() + labs(x = "OperatingSystems") + labs(y = " ")

# Computing the Contingency Table (OperatingSystems)
table(df2$Revenue, df2$OperatingSystems)

ggplot(df2, aes(Browser, fill=Revenue)) + geom_bar() + labs(x = "Browser") + labs(y = " ")

# Computing the Contingency Table (Browser)
table(df2$Revenue, df2$Browser)

ggplot(df2, aes(Region, fill=Revenue)) + geom_bar() + labs(x = "Region") + labs(y = " ")

# Computing the Contingency Table (Region)
table(df2$Revenue, df2$Region)

ggplot(df2, aes(TrafficType, fill=Revenue)) + geom_bar() + labs(x = "TrafficType") + labs(y = " ")

# Computing the Contingency Table (TrafficType)
table(df2$Revenue, df2$TrafficType)

ggplot(df2, aes(VisitorType, fill=Revenue)) + geom_bar() + labs(x = "VisitorType") + labs(y = " ")

# Computing the Contingency Table (VisitorType)
table(df2$Revenue, df2$VisitorType)

ggplot(df2, aes(Weekend, fill=Revenue)) + geom_bar() + labs(x = "Weekend") + labs(y = " ")

# Computing the Contingency Table (Weekend)
table(df2$Revenue, df2$Weekend)

# to see count of customers who shop/do not shop online
table(df2$Revenue)

# to generate probabilities of outcome variable 'Revenue'
prop.table(table(df$Revenue))

# there is huge class imbalance on target variable,
ggplot(df2, aes(x=Revenue, fill = Revenue)) +
  geom_bar() + labs(title='"Revenue" target variable distribution')

# to build a correlation map to understand r/s between only Numeric variables
corrplot(cor(df2[, 1:10]), type = 'upper', method = 'number', tl.cex = 0.7)

# to convert categorical variables to Ordinal Factors
#df2$OperatingSystems <- factor(df2$OperatingSystems, order = TRUE, levels = c(1,2,3,4,5,6,7,8))

#df2$Browser <- factor(df2$Browser, order = TRUE, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13))

#df2$Region <- factor(df2$Region, order = TRUE, levels = c(1,2,3,4,5,6,7,8,9))

#df2$TrafficType <- factor(df2$TrafficType, order = TRUE, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
#library(plyr)       

#df2$Month <- factor(df2$Month, order = TRUE, levels =c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'))
#df2$Month_Numeric <-mapvalues(df2$Month, from = c('Feb', 'Mar', 'May', 'June','Jul', 'Aug', 'Sep','Oct', 'Nov','Dec'), to = c(2,3,5,6,7,8,9,10,11,12))

#df2$VisitorType <- factor(df2$VisitorType, order = TRUE, levels = c('Returning_Visitor', 'Other', 'New_Visitor'))
#df2$VisitorType_Numeric <-mapvalues(df2$VisitorType, from = c("Returning_Visitor", "Other", "New_Visitor"), to = c(1,2,3))

#library(rpart) # for Decision tree
#library(rpart.plot) # to plot decision tree

# to build a decision tree model
#dt <- rpart(Revenue~., data = trainData, method = 'class')

# to visualize the decision tree with rpart.plot
#rpart.plot(dt)

# to generate confusion table and accuracy
#treePred <- predict(dt, testData, type = 'class')

# to generate a confusion matrix
#table(treePred, testData$Revenue)
# Accuracy of Decision Tree generated
#mean(treePred==testData$Revenue)


# to change <chr> variable to factor then to dummy variables as machine learnin model can only take numeric data, not Ordinal data
df2 <- df2 %>% 
  mutate(OperatingSystems = as.numeric(OperatingSystems),
         Browser = as.numeric(Browser),
          Region = as.numeric(Region),
         VisitorType = as.factor(VisitorType),
          Month = as.factor(Month),
          Weekend = as.integer(Weekend))

# to convert 'Revenue' to factor variable, as it is a 'response' variable Y in predictive modelling 
df2$Revenue <- as.factor(df2$Revenue)  

# to create Dummy Variables for "Month" and "VisitorType" variable
#if Month = Feb, return 1, otherwise return 0
  df2 <- df2 %>%
        mutate(Month_Feb = case_when(Month == "Feb" ~ 1,
                                  Month == "Mar" ~ 0,
                                  Month == "May" ~ 0,
                                  Month == "Jun" ~ 0,
                                  Month == "Jul" ~ 0,
                                  Month == "Aug" ~ 0, 
                                  Month == "Sep" ~ 0,
                                  Month == "Oct" ~ 0,
                                  Month == "Nov" ~ 0,
                                  Month == "Dec" ~ 0))
  

#if Month = Mar, return 1, otherwise return 0
 df2 <- df2 %>%
       mutate(Month_Mar = case_when(Month == "Feb" ~ 0,
                                Month == "Mar" ~ 1,
                                Month == "May" ~ 0,
                                Month == "Jun" ~ 0,
                                Month == "Jul" ~ 0,
                                Month == "Aug" ~ 0, 
                                Month == "Sep" ~ 0,
                                Month == "Oct" ~ 0,
                                Month == "Nov" ~ 0,
                                Month == "Dec" ~ 0))

#if Month = May, return 1, otherwise return 0
 df2 <- df2 %>%
      mutate(Month_May = case_when(Month == "Feb" ~ 0,
                            Month == "Mar" ~ 0,
                            Month == "May" ~ 1,
                            Month == "Jun" ~ 0,
                            Month == "Jul" ~ 0,
                            Month == "Aug" ~ 0, 
                            Month == "Sep" ~ 0,
                            Month == "Oct" ~ 0,
                            Month == "Nov" ~ 0,
                            Month == "Dec" ~ 0))

#if Month = Jun, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(Month_Jun = case_when(Month == "Feb" ~ 0,
                               Month == "Mar" ~ 0,
                               Month == "May" ~ 0,
                               Month == "Jun" ~ 1,
                               Month == "Jul" ~ 0,
                               Month == "Aug" ~ 0, 
                               Month == "Sep" ~ 0,
                               Month == "Oct" ~ 0,
                               Month == "Nov" ~ 0,
                               Month == "Dec" ~ 0))

#If Month = Jul, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(Month_Jul = case_when(Month == "Feb" ~ 0,
                               Month == "Mar" ~ 0,
                               Month == "May" ~ 0,
                               Month == "Jun" ~ 0,
                               Month == "Jul" ~ 1,
                               Month == "Aug" ~ 0, 
                               Month == "Sep" ~ 0,
                               Month == "Oct" ~ 0,
                               Month == "Nov" ~ 0,
                               Month == "Dec" ~ 0))

# If Month= Aug, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(Month_Aug = case_when(Month == "Feb" ~ 0,
                               Month == "Mar" ~ 0,
                               Month == "May" ~ 0,
                               Month == "Jun" ~ 0,
                               Month == "Jul" ~ 0,
                               Month == "Aug" ~ 1,
                               Month == "Sep" ~ 0,
                               Month == "Oct" ~ 0,
                               Month == "Nov" ~ 0,
                               Month == "Dec" ~ 0))

#If Month = Sep, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(Month_Sep = case_when(Month == "Feb" ~ 0,
                               Month == "Mar" ~ 0,
                               Month == "May" ~ 0,
                               Month == "Jun" ~ 0,
                               Month == "Jul" ~ 0,
                               Month == "Aug" ~ 0,
                               Month == "Sep" ~ 1,
                               Month == "Oct" ~ 0,
                               Month == "Nov" ~ 0,
                               Month == "Dec" ~ 0))

# if Month = Oct, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(Month_Oct = case_when(Month == "Feb" ~ 0,
                               Month == "Mar" ~ 0,
                               Month == "May" ~ 0,
                               Month == "Jun" ~ 0,
                               Month == "Jul" ~ 0,
                               Month == "Aug" ~ 0,
                               Month == "Sep" ~ 0,
                               Month == "Oct" ~ 1,
                               Month == "Nov" ~ 0,
                               Month == "Dec" ~ 0))

# if Month = Nov, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(Month_Nov = case_when(Month == "Feb" ~ 0,
                               Month == "Mar" ~ 0,
                               Month == "May" ~ 0,
                               Month == "Jun" ~ 0,
                               Month == "Jul" ~ 0,
                               Month == "Aug" ~ 0,
                               Month == "Sep" ~ 0,
                               Month == "Oct" ~ 0,
                               Month == "Nov" ~ 1,
                               Month == "Dec" ~ 0))

# if Month = Dec, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(Month_Dec = case_when(Month == "Feb" ~ 0,
                               Month == "Mar" ~ 0,
                               Month == "May" ~ 0,
                               Month == "Jun" ~ 0,
                               Month == "Jul" ~ 0,
                               Month == "Aug" ~ 0,
                               Month == "Sep" ~ 0,
                               Month == "Oct" ~ 0,
                               Month == "Nov" ~ 0,
                               Month == "Dec" ~ 1))

# if VistorType = Returning_Visitor, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(VisitorType_Returning_Visitor = case_when(VisitorType == "Returning_Visitor" ~ 1,
                                                   VisitorType == "Other" ~ 0,
                                                   VisitorType == "New_Visitor" ~ 0))

# if VistorType = Other, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(VisitorType_Other = case_when(VisitorType == "Returning_Visitor" ~ 0,
                                       VisitorType == "Other" ~ 1,
                                       VisitorType == "New_Visitor" ~ 0))

# if VistorType = New_Visitor, return 1, otherwise return 0
df2 <- df2 %>%
  mutate(VisitorType_New_Visitor = case_when(VisitorType == "Returning_Visitor" ~ 0,
                                             VisitorType == "Other" ~ 0,
                                             VisitorType == "New_Visitor" ~ 1))

# to check if data types for variables been changed for Dummy variables
glimpse(df2)  

# to drop Month and VisitorType (dplyr) before building logistic regression model
df2 <- select(df2, -c(Month, VisitorType))

# to check that Categorical variables: Month & VisitorType is removed from data frame
glimpse(df2)

#to create Training and Test data using createDataPartition from 'Caret' Package
library(caret)
# To Prep Training (70%) and Test (30%) data.
set.seed(100)
trainDataIndex <- createDataPartition(df2$Revenue, p=0.7, list = FALSE)  # 70% training data
trainData <- df2[trainDataIndex, ]
testData <- df2[-trainDataIndex, ]

#(DMwR) Functions and data with 'Data Mining with R and SMOTE algorithm
# to install DMwR from the CRAN github mirror
install.packages("devtools")

# if there is any error installing DMwR package, disable AVAST for 1 hr
library(devtools)
install_github("cran/DMwR")

# to load DMwR library installed from github
library(DMwR)

# to oversample "Revenue" target variable using SMOTE
# to set perc.over=100 to double quantity of positive cases
# set perc.under=200 to keep half of what was created as neg cases
trainData$Revenue <- as.factor(trainData$Revenue)
trainData <- SMOTE(Revenue ~ ., trainData, perc.over = 100, perc.under=200)

# to check if the 'Reveue' TRUE and FALSE  are in equal ratios
table(trainData$Revenue)

# to load machine learning algorithm, a Classification prediction
library(randomForest)
# a set of tools to help explain which variables are most important in RF
library(randomForestExplainer)

x <-trainData

# to create initial Random Forest model, random forest seems to encounter problems with Ordinal Factors
#set.seed(100)
#rfmodel <- randomForest (Revenue ~., data=trainData, importance= TRUE)
#rfmodel

# Create model with default paramters, estimated accuracy is 91.18%
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Revenue~., data=trainData, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

# Random Search for mtry using CARET package
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(Revenue~., data=trainData, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

# to find the optimal number of mtry and build better random forest model
# mtry: No. of variables randomly sampled as candidates at each split
#classifier = train(form =  Revenue~ ., data = trainData, method = 'rf')
#classifier
#classifier$bestTune # the optimal mtry = 15

# to build the random forest model again using best mtry value = 7
set.seed(100)
rf <-randomForest(Revenue~.,data=trainData, mtry=7, importance=TRUE,ntree=500)
rf

# to display the cross-validation error rate against the number of trees
plot(rf)

# to examine the variable importance of predictors in Random Forest model
importance(rf)
varImpPlot(rf, sort=TRUE, main = 'Features Importance by RF')

# to generate a multi-way importance plot
#importance_frame <- measure_importance(rf)
#plot_multi_way_importance(importance_frame)

# prediction and confusion matrix for training data
p1 <- predict(rf, trainData)
# to generated a confusion matrix on the training data
confusionMatrix(p1, trainData$ Revenue) #accuracy rate = 100%

# prediction and confusion matrix for testing data
p2 <- predict (rf,testData)
# to generated a confusion matrix on the testing data
confusionMatrix(p2, testData$ Revenue) #accuracy rate = 86.81%

# measure of predictive capability of a categorical x variable to
# accurately predict the goods and bads
install.packages("InformationValue")

library(InformationValue)

plotROC(y_act, p2)

AUROC(y_act, p2)

# to run C5.0 algorithm Decision Tree
install.packages("C50")
install.packages("printr")

# C5.0 decision tree
library(C50)
#The printr package is a companion to knitr
library(printr)

# to generate a C5.0 decision tree model using training data
dtmodel <- C5.0(Revenue ~., data=trainData)
dtmodel

# to predict the results on the testing data
results <- predict(object=dtmodel, newdata=testData, type="class")

# to generate a Confusion matrix
table(results, testData$Revenue) 

# C5.0 decision tree
plot(dtmodel)

# to generate a confusion matrix for C5.0 decision tree test data
C5.0 <- confusionMatrix(results, factor(testData$Revenue))
C5.0

