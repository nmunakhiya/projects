library(tidyverse)      # collection of best packages
library(caret)          # machine learning functions
library(MLmetrics)      # machine learning metrics
library(car)            # VIF calculation
library(rpart)          # decision tree
library(class)
train <- read_csv("titanic/train.csv")
test <- read_csv("titanic/test.csv")
dim(train)
#checking duplicate data
anyDuplicated(train)
# Checking Missing values
colSums(is.na(train))
colSums(is.na(test))
#in cabin replacing Na with new categories

train$Cabin <- replace_na(train$Cabin, 'X0')
test$Cabin <- replace_na(test$Cabin, 'X0')
#splittiting the name
train$Surname <- sapply(str_split(train$Name, ','), `[`, 1) %>% str_trim()
temp <- sapply(str_split(train$Name, ','), `[`, 2)
train$Title <- sapply(str_split(temp, '\\.'), `[`, 1) %>% str_trim()
train <- train %>% select(-Name)

test$Surname <- sapply(str_split(test$Name, ','), `[`, 1) %>% str_trim()
temp <- sapply(str_split(test$Name, ','), `[`, 2)
test$Title <- sapply(str_split(temp, '\\.'), `[`, 1) %>% str_trim()
test <- test %>% select(-Name)
unique(train$Title)
unique(test$Title)
# As we can see there is Dona in test but not in train we transform that into Mrs
test[test$Title =='Dona', 'Title'] = 'Mrs'
#The strategy is to group Age by Title then take the median of each group.
#After that, assign the median to Age's missing values as per the corresponding Title.
age_by_title <- train %>% 
  group_by(Title) %>% 
  summarise(median = median(Age, na.rm = TRUE))

train <- merge(train, age_by_title)
train[is.na(train$Age), 'Age'] <- train[is.na(train$Age), 'median']
train <- train %>% select(-median)

test <- merge(test, age_by_title)
test[is.na(test$Age), 'Age'] <- test[is.na(test$Age), 'median']
test <- test %>% select(-median)
table(train$Embarked)
#As emabrked is categorical we replace missing value with mode that is S
train$Embarked <- replace_na(train$Embarked, 'S')
test$Embarked <- replace_na(test$Embarked, 'S')
#Passenger Fare should correlate with Pclass: the higher the class, the higher the fare.
#Just like imputing Age, the strategy is to group Fare by Pclass then take the 
#median of each group. After that, assign the median to Fare's missing values as per the corresponding Pclass
fare_by_pclass <- train %>% 
  group_by(Pclass) %>% 
  summarise(median = median(Fare, na.rm = TRUE))

train <- merge(train, fare_by_pclass)
train[is.na(train$Fare), 'Fare'] <- train[is.na(train$Fare), 'median']
train <- train %>% select(-median)

test <- merge(test, fare_by_pclass)
test[is.na(test$Fare), 'Fare'] <- test[is.na(test$Fare), 'median']
test <- test %>% select(-median)
#Checking missing values again
colSums(is.na(train))
colSums(is.na(test))
# No missing values 
train <- train %>% 
  mutate_at(vars(Pclass, Title, Survived, Sex, Cabin, Embarked), as.factor)

test <- test %>% 
  mutate_at(vars(Pclass, Title, Sex, Cabin, Embarked), as.factor)
summary(train$Age)
Ageglimpse(train)
View(train$Age)

# Changin survived 
train$SurvivedFac <- ifelse(train$Survived=="1","yes","no")
train$SurvivedFac <- factor(train$Survived, levels=0:1, labels=c("no","yes"))
train$SurvivedNum <- as.numeric(train$Survived) 
train$Survived <- NULL # drop original
train$SurvivedFac <- ifelse(train$Survived=="1","yes","no")
train$SurvivedFac <- factor(train$Survived, levels=0:1, labels=c("no","yes"))
train$SurvivedNum <- as.numeric(train$Survived) 
train$Survived <- NULL # drop original
View(train)
# Now class
# Pclass
train$PclassFac <- ifelse(train$Pclass==1, "1st Class", ifelse(train$Pclass==2, "2nd Class", "3rd Class"))
train$PclassFac <- factor(train$PclassFac)
train$PclassNum <- as.integer(train$Pclass)
train$Pclass <- NULL
#aGE
# create AgeFac for Age Categories
train$AgeFac <- ifelse(train$Age > 0 & train$Age < 13, "Child",
                       ifelse(train$Age > 12 & train$Age < 20, "Teen",
                              ifelse(train$Age > 19 & train$Age < 36, "YoungAdult", 
                                     ifelse(train$Age > 35 & train$Age < 56, "MiddleAged", "Elderly"))))
train$AgeFac <- factor(train$AgeFac, levels=c("Child","Teen","YoungAdult","MiddleAged","Elderly"))
train$AgeNum <- as.integer(train$Age)
train$Age <- NULL
# Change SibSp and Parch to factor
train$SiblingSpouse <- factor(train$SibSp)
train$ParentChildren <- factor(train$Parch)
train$NumRelatives <- train$SibSp + train$Parch
train$NumRelatives <- factor(train$NumRelatives)
train$SibSp <- NULL
train$Parch <- NULL
# embarked
train$Embarked <- ifelse(train$Embarked=="C","Cherbourg",
                         ifelse(train$Embarked=="Q","Queensland","Southhampton"))
train$Embarked[is.na(train$Embarked)] <- "Southhampton"
train$Embarked <- factor(train$Embarked) # re-factoring
df<-data.frame(train$Sex,train$Embarked,train$SurvivedFac,train$PclassFac,train$AgeFac) 


library(arules)


rule <- apriori(df, 
                # min support & confidence
                parameter=list(minlen=2,support = 0.003, confidence = 0.8),  
                appearance = list(default = "lhs", rhs=c("train.SurvivedFac=yes","train.SurvivedFac=no")))
library(arulesViz)

inspectDT(rule[1:10]) 
sort.rule <- sort(rule, by="lift")
plot(sort.rule[1:10], method="graph", control=list(nodeCol="red", edgeCol="blue"))
rule1 <- apriori(df, 
                # min support & confidence
                parameter=list(minlen=2,support = 0.003, confidence = 0.8),  
                appearance = list(default = "lhs", rhs="train.SurvivedFac=yes"))
library(arulesViz)

inspectDT(rule1[1:10]) 
sort.rule1 <- sort(rule1, by="lift")
plot(sort.rule1[1:10], method="graph", control=list(nodeCol="red", edgeCol="blue"))
