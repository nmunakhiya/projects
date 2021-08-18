library(readr)
library(caTools)
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(car)
library(caret)
library(psych)
library(superml)

telco <- read_csv("Telco-Customer-Churn.csv")
View(telco)
dim(telco)
summary(telco)

ls(telco)

missing_data <- telco %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "#E7B800", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()

telco <- data.frame(lapply(telco, function(x) {
  gsub("No internet service", "No", x)}))

telco <- data.frame(lapply(telco, function(x) {
  gsub("No phone service", "No", x)}))
#after removing missing value 11
telco <- na.omit(telco)


options(repr.plot.width = 6, repr.plot.height = 4)
missing_data <- telco %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing")
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "#E7B800", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip()+ 
  theme_bw()
#Different plot
tab1 <- as.data.frame(table(telco$gender,telco$Churn))
p1 <- ggplot(tab1, aes(x=Var1,y=Freq,fill=Var2))+ geom_bar(stat="identity")+geom_text(aes(label=Freq))
#the male female ration are almost same.The amount of people thats gonna churn is also almost same.
# or we can say females are more than 

p2 <- ggplot(telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')
#churn rate is higher in fibre optics
tab2 <- as.data.frame((table(telco$SeniorCitizen,telco$Churn)))
p3 <- ggplot(tab2, aes(x=Var1,y=Freq,fill=Var2))+ geom_bar(stat="identity")+geom_text(aes(label=Freq),vjust=1.5)+xlab("senior citizen")+ylab("Frequency")
#Younger generation are more prone to churn than old ones 
p4 <- ggplot(telco, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')
p5 <- ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')
p6 <- ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')
plot_grid(p1,p2,p3,p4,p5,p6)
plot_grid(p4,p5,p6)
#checking correlation for continous var

telco_cor <- round(cor(telco[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(telco_cor, method="color", col=col(200),  
                   type="upper", order="hclust", 
                   addCoef.col = "black", # Add coefficient of correlation
                   tl.col="black", tl.srt=45, #Text label color and rotation
)
#Data standardisation
num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
telco[num_columns] <- sapply(telco[num_columns], as.numeric)


telco_int <- telco[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))                   
telco <- mutate(telco, tenure_bin = tenure)

telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 year'
telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 years'
telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 years'
telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 years'
telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 years'
telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 years'

telco$tenure_bin <- as.factor(telco$tenure_bin)                   
ggplot(telco, aes(tenure_bin, fill = tenure_bin)) + geom_bar()

#One Label encoding
telco_cat <- telco[,-c(1,6,19,20)]
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))

date<- data.frame(predict(dmy, newdata = telco))
dat <- cbind(telco_int,dummy)

split <- sample.split(dat$Churn,.70)
train <- subset(dat,split==TRUE)
test <- subset(dat,split==FALSE)
xt <- lm(train$Churn~.,data=train)
plot(xt)
View(dat)
library(superml)
library(Metrics)

lf <- LMTrainer$new(family="binomial")
lf$fit(X = train, y = "Churn")
summary(lf$model)
predictions <- lf$predict(df = test)
rmse(actual = test$Churn, predicted = predictions)

library(rpart)
library(rpart.plot)
fit <- rpart(Churn~., data = train, method = 'class')
rpart.plot(fit, extra = 106)
predict <-predict(fit,test)
rmse(actual = test$Churn, predicted = predict)

model_1 = glm(Churn ~ ., data = train, family = "binomial")
summary(model_1)
model_2<- stepAIC(model_1, direction="both")
vif(model_2)
#removing deviceprotection.
model_3 <-glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
                Partner + InternetService.xFiber.optic + InternetService.xNo + 
                OnlineSecurity + OnlineBackup + TechSupport + 
                StreamingTV + Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                PaymentMethod.xElectronic.check + tenure_bin.x1.2.years + 
                tenure_bin.x5.6.years, family = "binomial", data = train)
summary(model_3)
vif(model_3)
#Removing StreamingTV  as it has high p-value 

model_4 <- glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + 
                 Partner + InternetService.xFiber.optic + InternetService.xNo + 
                 OnlineSecurity + OnlineBackup + TechSupport +  
                 Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                 PaymentMethod.xElectronic.check + tenure_bin.x1.2.years + 
                 tenure_bin.x5.6.years, family = "binomial", data = train)

summary(model_4)
vif(model_4)
#here we see model 3 has low AIC so we use that.
final_model <- model_3
pred <- predict(final_model, type = "response", newdata = dat[,-24])
summary(pred)
dat$prob <- pred

# Using probability cutoff of 50%.

pred_churn <- factor(ifelse(pred >= 0.50, "Yes", "No"))
actual_churn <- factor(ifelse(dat$Churn==1,"Yes","No"))
table(actual_churn,pred_churn)

cutoff_churn <- factor(ifelse(pred >=0.50, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity
perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, actual_churn, positive = "Yes")
  accuray <- conf$overall[1]
  sensitivity <- conf$byClass[1]
  specificity <- conf$byClass[2]
  out <- t(as.matrix(c(sensitivity, specificity, accuray))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}
options(repr.plot.width =8, repr.plot.height =6)
summary(pred)
s = seq(0.01,0.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),
     type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottom",col=c(2,"darkgreen",4,"darkred"),text.font =3,inset = 0.02,
       box.lty=0,cex = 0.8, 
       lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
abline(v = 0.32, col="red", lwd=1, lty=2)
axis(1, at = seq(0.1, 1, by = 0.1))
#choose cutoff at .32

cutoff_churn <- factor(ifelse(pred >=0.32, "Yes", "No"))
conf_final <- confusionMatrix(cutoff_churn, actual_churn, positive = "Yes")
accuracy <- conf_final$overall[1]
sensitivity <- conf_final$byClass[1]
specificity <- conf_final$byClass[2]
accuracy
sensitivity
specificity

