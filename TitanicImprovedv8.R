###################### Start of Kaggle Titanic Prediction #######################

# Reading the files from the set directory 
setwd("C:\\Users\\madhurima\\Desktop\\Kaggle")
tit.train<-read.csv("train.csv")
tit.test<-read.csv("test.csv")
str(tit.train)
str(tit.test)

# Adding an extra column to both train and test set that would help differentitate the Passenger Id in the merged dataset##
tit.train$BelongsTrain<-TRUE
tit.test$BelongsTrain<-FALSE

# Adding Survived column in test dataset
tit.test$Survived<-NA
names(tit.test)

# Combining training and test set
titanic.full<-rbind(tit.train, tit.test)

# Steps to ensure whether the combine worked O.K 
nrow(titanic.full)
table(titanic.full$BelongsTrain)

# Categorical casting of some int variables
str(titanic.full)
titanic.full$Pclass<-as.factor(titanic.full$Pclass)

# Missing values treatment 
colSums(is.na(titanic.full))

# Get all the records for missing values of Age
titanic.full[is.na(titanic.full$Age), "Age"]

# Building linear regression model for age to predict the missing values of age
# Before building the model, lets filter the outliers of age, so that model can be built on non outlier values of age
boxplot(titanic.full$Age)
boxplot.stats(titanic.full$Age)
topwhisker.Age<-boxplot.stats(titanic.full$Age)$stats[5]
nonoutliers.Age<-titanic.full$Age < topwhisker.Age
# titanic.full[nonoutliers.Age,]  #This is my data without outlier values of age

Age.mod<-lm(Age~Pclass + Sex + SibSp + Parch + Fare + Embarked, data = titanic.full[nonoutliers.Age,])

# Apply the model results to predict the missing values of age in titanic.full dataset 
Age.rows<-titanic.full[is.na(titanic.full$Age), c("Pclass","Sex","SibSp","Parch","Fare", "Embarked")]
missingAge.predict<-predict(Age.mod, newdata = Age.rows)

# Impute the  missing Age values in data set with predicted values 
titanic.full[is.na(titanic.full$Age), "Age"]<-missingAge.predict

sum(is.na(titanic.full$Age))

# Build linear reg model to predict the missing values of Fare
# Before building the model, lets filter the outliers of Fare, so that model can be built on non outlier values of Fare
boxplot(titanic.full$Fare)
boxplot.stats(titanic.full$Fare)
topwhisker.Fare<-boxplot.stats(titanic.full$Fare)$stats[5]
nonoutliers.Fare<-titanic.full$Fare < topwhisker.Fare
# titanic.full[nonoutliers.Fare,]  #This is my data without outlier values of Fare

Fare.mod<-lm(Fare~Pclass + Sex + SibSp + Parch + Age + Embarked, data = titanic.full[nonoutliers.Fare,])

# Apply the model results to predict the missing values of Fare
Fare.rows<-titanic.full[is.na(titanic.full$Fare), c("Pclass","Sex","SibSp","Parch","Age", "Embarked")]
missingFare.predict<-predict(Fare.mod, newdata = Fare.rows)

# Impute the  missing Fare values in data set with predicted values 
titanic.full[is.na(titanic.full$Fare), "Fare"]<-missingFare.predict 

## Feature Engineering 1 ##

# Convert names column to characters 
titanic.full$Name<-as.character(titanic.full$Name)

# Extract the titles out from the names
titanic.full$Title<-strsplit(titanic.full$Name, split = '[,.]')[[1]][2]
titanic.full$Title<-sapply(titanic.full$Name, FUN = function(x){strsplit(x, split='[,.]')[[1]][2]})
titanic.full$Title<-sub(' ', '', titanic.full$Title)

  # Combining less frequently occuring titles together 
titanic.full$Title[titanic.full$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
titanic.full$Title[titanic.full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
titanic.full$Title[titanic.full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

titanic.full$Title<-factor(titanic.full$Title)  #Creation of title variable


# Feature Engineering 2
# Creating family size variable from sibsp and parch variables 
titanic.full$FamilySize<- titanic.full$SibSp + titanic.full$Parch + 1

# Feature Engineering 3- Creating family id variable 
# Combining surname with family size #
titanic.full$Surname <- sapply(titanic.full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

titanic.full$FamilyID <- paste(as.character(titanic.full$FamilySize), titanic.full$Surname, sep="")
titanic.full$FamilyID[titanic.full$FamilySize <= 2] <- 'Small'

table(titanic.full$FamilyID)

familyIDs <- data.frame(table(titanic.full$FamilyID))
familyIDs <- familyIDs[familyIDs$Freq <= 2,]
titanic.full$FamilyID[titanic.full$FamilyID %in% familyIDs$Var1] <- 'Small'
titanic.full$FamilyID <- factor(titanic.full$FamilyID)

# Reducing the no. of levels in Family ID to less than 32 levels as random forest can't handle more than this size #
titanic.full$FamilyID1 <- titanic.full$FamilyID
titanic.full$FamilyID1 <- as.character(titanic.full$FamilyID1)
titanic.full$FamilyID1[titanic.full$FamilySize <= 3] <- 'Small'
titanic.full$FamilyID1 <- factor(titanic.full$FamilyID1)


# Performing feature engineering 4 
titanic.full$Deck <- substr(titanic.full$Cabin, start =1, stop=1)
titanic.full$CabinRoom <- substr(titanic.full$Cabin, start = 2, stop = 12)
titanic.full$CabinRoom <- substr(titanic.full$CabinRoom, start = 1, stop = 2)

index<-titanic.full$CabinRoom[grepl("E", titanic.full$CabinRoom)]   #Checking occurences of E
titanic.full$CabinRoom<-gsub(" E", "", titanic.full$CabinRoom)

testing<-titanic.full$CabinRoom[grepl("G", titanic.full$CabinRoom)] 
titanic.full$CabinRoom<-gsub(" G", "", titanic.full$CabinRoom)

titanic.full$CabinRoom<-as.factor(titanic.full$CabinRoom)
#levels(titanic.full$CabinRoom)

titanic.full$Deck<-as.factor(titanic.full$Deck)
#str(titanic.full$Deck)

#Replacing empty string with a non-missing values
levels(titanic.full$Deck)[levels(titanic.full$Deck)== ""] <- "Unknown"                    
levels(titanic.full$CabinRoom)[levels(titanic.full$CabinRoom)== ""] <- 0
# Making cabin room an integer variable
titanic.full$CabinRoom<-as.integer(as.character(titanic.full$CabinRoom))


# Splitting the full titanic set to train and test for model building
titanic.train<-titanic.full[titanic.full$BelongsTrain==TRUE,]
titanic.test<-titanic.full[titanic.full$BelongsTrain==FALSE,]

nrow(titanic.train)
nrow(titanic.test)

# Categorical casting of Survived in training set
titanic.train$Survived<-as.factor(titanic.train$Survived)

# Model Building using Decision Tree
# build an equation and cast it as a formula
survived.equation<- "Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FamilySize+FamilyID+Deck+CabinRoom"
survived.formula<-as.formula(survived.equation)

#build the model
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
titanic.model <- rpart(survived.equation,data=titanic.train, method="class")
fancyRpartPlot(titanic.model)

# Prediction of titanic.model on test data set
Survived<-predict(titanic.model, newdata = titanic.test, type = "class")

#Exporting PassengerID and Survived columns for test set to a new dataset for Kaggle submission#
PassengerId<-titanic.test$PassengerId
results.df<-as.data.frame(PassengerId)
results.df$Survived<-Survived
write.csv(results.df, "C:\\Users\\madhurima\\Desktop\\Kaggle\\kaggleresultsv13.csv", row.names = FALSE)


# Building Regression Model using randomForest 

#build an equation of Survived and cast it as a formula
survived.equation<- "Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FamilySize+FamilyID1+Deck+CabinRoom"
survived.formula<-as.formula(survived.equation)

#build the model
library(randomForest)
titanic.model<-randomForest(formula= survived.formula, data = titanic.train, ntree = 500, mtry= 3, nodeszize = 0.01*nrow(titanic.train))

#Checking the variable importance
varImpPlot(titanic.model)

# Prediction of titanic.model on test data set
Survived<-predict(titanic.model, newdata = titanic.test)

#Exporting PassengerID and Survived columns for test set to a new dataset for Kaggle submission#
PassengerId<-titanic.test$PassengerId
results.df<-as.data.frame(PassengerId)
results.df$Survived<-Survived
write.csv(results.df, "C:\\Users\\madhurima\\Desktop\\Kaggle\\kaggleresultsv14.csv", row.names = FALSE)




## Building Regression Model using cForest ##

#build an equation for Survived and cast it a formula#
survived.equation<- "Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FamilySize+FamilyID+Deck+CabinRoom"
survived.formula<-as.formula(survived.equation)

#Model building
library(party)
set.seed(415)
titanic.model <- cforest(formula = survived.formula,
                 data = titanic.train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))

# Prediction of titanic.model on test data set
Survived <- predict(titanic.model, titanic.test, OOB=TRUE, type = "response")

#Exporting PassengerID and Survived columns for test set to a new dataset for Kaggle submission#
PassengerId<-titanic.test$PassengerId
results.df<-as.data.frame(PassengerId)
results.df$Survived<-Survived
write.csv(results.df, "C:\\Users\\madhurima\\Desktop\\Kaggle\\kaggleresultsv15.csv", row.names = FALSE)




