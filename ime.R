library(readr)

library(dplyr)
library(caTools)#for splitting
library(CatEncoders) # label & one hot encoding
library(lmtest) 
library(e1071)# For svr model
library(Metrics)
library(caret)
library(car)
library(dummies)
library(superml)
library(car)
dat <- read_csv("census.csv")
head(dat,10)
summary(dat)

View(dat)
data.frame(dat)
##Checking for duplicated data 
sum(duplicated(dat))
# we have around 3462 duplicate data . we remove it 

dat<- distinct(dat)

## checking missing values
missing_data <- dat %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "#E7B800", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()
## No missing values 
## DATA CLEANING 
table(dat$workclass)

#Combined 'Never-worked' and 'Without-pay' into Unemployed
unemployed <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}
dat$workclass <- sapply(dat$workclass,unemployed)
table(dat$workclass)
#Combine State and Local gov jobs into a category called SL-gov and combine self-employed jobs into a category called self-emp
group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}
dat$workclass <- sapply(dat$workclass,group_emp)
table(dat$workclass)

table(dat$maritalstatus)
#No need of transforming martial status  
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

dat$nativecountry <- sapply(dat$nativecountry,group_country)
table(dat$nativecountry)
#Factorize the newly created column
dat$workclass <- sapply(dat$workclass,factor)
dat$nativecountry <- sapply(dat$`nativecountry`,factor)
dat$maritalstatus <- sapply(dat$`maritalstatus`,factor)
dat$occupation <- sapply(dat$occupation,factor)
dat$relationship <- sapply(dat$relationship,factor)
dat$over50k <- sapply(dat$over50k,factor)
dat$education<- sapply(dat$education,factor)
dat$sex<- sapply(dat$sex,factor)
dat$race<- sapply(dat$race,factor)
View(dat)
#----------------------------------------------------------------------------#
boxplot(train)
boxplot(dat$capitalgain)
#outliers
Q <- quantile(dat$capitalgain,probs = c(.25,.75),na.rm = TRUE)
iqr <- IQR(dat$capitalgain)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
eliminated<- subset(dat,dat$capitalgain > (Q[1] - 1.5*iqr) & dat$capitalgain < (Q[2]+1.5*iqr))                                                                                     
eliminated
#no outliers present
#corrletaion
M <- cor(dat[,c(9:11)])

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
)

#EDA
income<-  dat$over50k
xtab <- lapply(dat[,c(1:12)], function(x) xtabs(~ x+income))
xtab

library(CGPfunctions)
# plotting and checking independence usingc chi.square test 
PlotXTabs(dat,nativecountry,over50k,plottype = "side")
#we see in both the incomes most data of people are from north america 
PlotXTabs2(dat,nativecountry,over50k)

PlotXTabs(dat,workclass,over50k,plottype = "side")
PlotXTabs2(dat,workclass,over50k)


one <- as.data.frame(table(dat$workclass))
two <- (table(dat$over50k))
two
sex1 <- as.data.frame(table(dat$sex))
sex1
perc <- (sex1$Freq/(length(sex1$Freq)))

library(ggplot2)
#simple male female plot
ggplot(data =sex1,aes(x=Var1,y=Freq,fill=Var1))+
  geom_bar(stat="identity")+geom_text(label=sex1$Freq) 


df <- data.frame(table(dat$sex,dat$race,dat$over50k))
#check independency for race and salary
table <-xtabs(~dat$race+dat$over50k,dat) 
as.data.frame(table)
#H0: Two variable are independant
#H1; two var are dependent
chisq.test(table)
#as we see Chi calculated is less than Chi tabulated we reject the null hypothesis.

ggplot(df, aes(x =Var1, y =Freq,fill=Var2))+geom_bar(stat = "identity")+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Var3), vjust=-0.3, size=3.5)+
  theme_minimal()+geom_bar(aes(fill =Var2), stat="identity")+facet_wrap(~Var3)+
  xlab("gender")+ylab("frequency")

#workclass and salary
df1 <- data.frame(table(dat$workclass,dat$sex,dat$over50k))
df1
table1 <- xtabs(~dat$workclass+dat$over50k)
chisq.test(table1)
# 2.167
#again reject the null hypothesis.
ggplot(df1, aes( y=Var1, x =Freq,fill=Var2))+geom_bar(stat = "identity")+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq), size=3.5)+
  theme_minimal()+geom_bar(aes(fill =Var2), stat="identity")+facet_wrap(~Var3)
#Mostly people works in Private sector
#education and salary
df2 <- data.frame((table(dat$education,dat$over50k)))
df2
table2 <- xtabs(~dat$education+dat$over50k,dat)
chisq.test(table2)
#again reject null hypothesis.
ggplot(df2, aes( y=Var1, x =Freq,fill=Freq))+geom_bar(stat = "identity")+
  geom_bar(stat="identity", fill="steelblue")+
  facet_wrap(~Var2)+geom_text(aes(label=Freq))
#Hs graduates are the highest earners that earns less than 50K
#In more than 50K Bachelors earns most
#Relationship and salary
df3 <- data.frame(table(dat$relationship,dat$sex,dat$over50k))
df3
perc <- (df3$Freq/length(df3$Freq))
perc
table3 <- xtabs(~dat$relationship+dat$over50k,dat)
chisq.test(table3)
#again reject null hypothesis
ggplot(df3, aes( x=Var1, y=Freq,fill=Var2))+geom_bar(stat = "identity")+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Freq),vjust=-.25, size=3.5)+
  theme_minimal()+geom_bar(aes(fill =Var2), stat="identity")+facet_wrap(~Var3)

#Mostly Husband are the one earning. there is female husband that earns less than 50K
df4 <- data.frame(table(dat$sex,dat$over50k))
table4 <- xtabs(~dat$sex+dat$over50k,dat)
chisq.test(table4)
#again reject
ggplot(df4, aes( y=Var1, x =Freq,fill=Freq))+geom_bar(stat = "identity")+
  geom_bar(stat="identity", fill="steelblue")+
  facet_wrap(~Var2)+geom_text(aes(label=Freq))
#Male dominant
#race& salary
df5 <- data.frame(table(dat$race,dat$over50k))
table5 <- xtabs(~dat$race+dat$over50k,dat)
chisq.test(table5)
ggplot(df5, aes( y=Var1, x =Freq,fill=Freq))+geom_bar(stat = "identity")+
  geom_bar(stat="identity", fill="steelblue")+
  facet_wrap(~Var2)+geom_text(aes(label=Freq))
#Mostly people earning are white
#age  Plot
df6 <- data.frame(table(dat$age))
ggplot(df6,aes(x=Var1,y=Freq,fill=Freq))+geom_bar(stat="identity")+geom_text(aes(label=Freq),vjust=-1.8)
df6
df7 <- data.frame(table(dat$age,dat$over50k))
table6 <- xtabs(~dat$age+dat$over50k,dat)
table6
chisq.test(table6)
ggplot(df7, aes( y=Var1, x =Freq,fill=Freq))+geom_bar(stat = "identity")+
  geom_bar(stat="identity", fill="steelblue")+
  facet_wrap(~Var2)+geom_text(aes(label=Freq))

## Ending the EDA part
## splitting data into train and test 
split <- sample.split(dat$over50k,.90)
train <- subset(dat,split==TRUE)
test <- subset(dat,split==FALSE)
dim(train)
dim(test)
na_cols <- colSums(is.na(train)) / nrow(train)
na_cols <- names(na_cols[which(na_cols > 0.9)])

cat_cols <- names(train)[sapply(train, is.character)]

for(c in cat_cols){
  lbl <- LabelEncoder$new()
  lbl$fit(c(train[[c]], test[[c]]))
  train[[c]] <- lbl$transform(train[[c]])
  test[[c]] <- lbl$transform(test[[c]])
}
glm_model = glm(over50k ~ ., family = binomial(logit), data = train)
test$predicted.income = predict(glm_model, newdata=test)
set.seed(1)
test$income_class <- ifelse(test$predicted.income > 0.5, ">50K","<=50K")
glm_con <-confusionMatrix(as.factor( test$income_class),as.factor( test$over50k))
glm_con
# decision tree
tree_model <- rpart(over50k ~ ., train, method = "class")

all_probs <- predict(tree_model, test, type = "prob")
rpart.plot(tree_model,  box.palette="RdBu", shadow.col="gray")
test$income_class <- ifelse(all_probs[,1]>0.5,"<=50K",">50K")
dt_con <- confusionMatrix(as.factor(test$income_class),test$over50k)
dt_con 
library(MASS)
stepAIC(glm_model,direction = "both")
