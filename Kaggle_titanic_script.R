

#Predicting Titanic


rm(list = ls(all.names=TRUE))
library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(tree)
library(leaflet)
library(forcats)
library(caret)
library(pROC)


head(train)

summary(train)

str(train)


colSums(is.na(train))


#Removing useless variables

### Kan de faktisk bruges ved at sammenligne ticket og navne? 
#Det virker til at familier købte 
#M¨ske prøv noget med last name (før komma), kan være det kan spille sammen
#Med alder og anden survival rate (børn af en kvinde med stor sandsynlighed for)
#at overleve foreksempel

train_data <- select(train, -Name, -Ticket)



#reformatting cabin variable to a dummy for cabin label or not

levels(train_data$Cabin)

train_data$Cabin <- as.factor(train_data$Cabin)

levels(train_data$Cabin)

train_data$Cabin2 <- ifelse(train_data$Cabin == "", 0, 1)


train_data2 <- train_data

#Cleaning Sex and Embarked

train_data2$Embarked <- as.factor(train_data2$Embarked)

train_data2$Sex <- as.factor(train_data2$Sex)

levels(train_data2$Sex)


train_data2 <- select(train_data, -Cabin)




train_data2$Survived <- as.factor(train_data2$Survived)

train_data2$Sex <- as.factor(train_data2$Sex)

train_data2$PClass <- as.factor(train_data2$PClass)

train_data2$Embarked <- as.factor(train_data2$Embarked)

train_data2$Cabin2 <- as.factor(train_data2$Cabin2)


str(train_data2)

summary(train_data2)



train_data2 <- train_data2  %>% 
  mutate(Survived = factor(Survived, 
                        labels = make.names(c("No","Yes"))))

train_data2 <- train_data2  %>% 
  mutate(Cabin2 = factor(Cabin2, 
                        labels = make.names(c("No","Yes"))))


#Cleaning test set



test_data <- select(test, -Name, -Ticket)



#reformatting cabin variable to a dummy for cabin label or not

levels(test_data$Cabin)

test_data$Cabin <- as.factor(test_data$Cabin)

levels(test_data$Cabin)

test_data$Cabin2 <- ifelse(test_data$Cabin == "", 0, 1)



test_data2 <- test_data

#Cleaning Sex and Embarked

test_data2$Embarked <- as.factor(test_data2$Embarked)

test_data2$Sex <- as.factor(test_data2$Sex)

levels(test_data2$Sex)


test_data2 <- select(test_data, -Cabin)




test_data2$Sex <- as.factor(test_data2$Sex)

test_data2$PClass <- as.factor(test_data2$PClass)

test_data2$Embarked <- as.factor(test_data2$Embarked)

test_data2$Cabin2 <- as.factor(test_data2$Cabin2)


str(test_data2)

summary(test_data2)




test_data2 <- test_data2  %>% 
  mutate(Cabin2 = factor(Cabin2, 
                         labels = make.names(c("No","Yes"))))

######

titanic.formula <- as.formula(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Cabin2)

######

model_glm <- glm(titanic.formula, family="binomial", train_data2)


summary(model_glm)


train_data3<- na.omit(train_data2)

myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)
myGrid <- expand.grid(
  alpha = 0:1,
  lambda = seq(0.0001, 1, length = 20))
memory.limit(1000000)
LM_train <- train(
  titanic.formula,
  train_data3,
  method = "glmnet",
  tuneGrid = myGrid,
  trControl = myControl,
  preProcess = c("center", "scale", "zv"),
  metric = "ROC",
  importance = "impurity"
)  

Pred_Lm<- predict(object = LM_train,
                  newdata = train_data3,
                  type = "prob")

Lm_roc <- roc(train_data3$Survived, Pred_Lm$"Yes")

plot(Lm_roc)

coords(Lm_roc, "best","threshold")

y_or_n_LM <- ifelse(Pred_Lm$"Yes" > 0.4493496, "Yes", "No")

# Convert to factor: p_class
p_class_LM <- factor(y_or_n_LM, levels = levels(train_data3[["Survived"]]))

# Create confusion matrix
confusionMatrix(p_class_LM, train_data3[["Survived"]], positive = "Yes")



##Random Forest models

#Random Forest
myGrid2<- data.frame(
  .mtry = c(2,3,4,5,6,7,8),
  .splitrule = "gini",
  .min.node.size = 5
)


RF_train <- train(
  titanic.formula,
  train_data3,
  method = "ranger",
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final",
  ),
  tuneGrid = myGrid2,
  importance = "impurity",
  respect.unordered.factors= TRUE
)


Pred_RF<- predict(object = RF_train,
                   newdata = train_data3,
                   type = "prob")

RF_roc <- roc(train_data3$Survived, Pred_RF$"Yes")

plot(RF_roc)

coords(RF_roc, "best","threshold")

y_or_n_RF <- ifelse(Pred_RF$"Yes" > 0.4142018, "Yes", "No")

# Convert to factor: p_class
p_class_RF <- factor(y_or_n_RF, levels = levels(train_data3[["Survived"]]))

# Create confusion matrix
confusionMatrix(p_class_RF, train_data3[["Survived"]], positive = "Yes")




####PREDICTION########

#Using RF singe accuracy of 89.7%, sensivity of 87,9% and specifity of 91.0%

Pred_RF_test<- predict(object = RF_train,
                  newdata = test_data2,
                  type = "prob")


y_or_n_RF_test <- ifelse(Pred_RF_test$"Yes" > 0.41420181, "Yes", "No")

# Convert to factor: p_class
p_class_RF_test <- factor(y_or_n_RF_test, levels = levels(train_data3[["Survived"]]))



