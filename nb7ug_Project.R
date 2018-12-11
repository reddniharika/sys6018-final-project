#Systems Project
#Computing id : nb7ug
#Name: Niharika Reddy

#Load basic packages
install.packages("cowplot")
library(cowplot)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(tidyr)

data <- read.csv("Telecom_churn.csv")

#Bird's eye view of the dataset
str(data)
summary(data)
data$Churn<-factor(ifelse((data$Churn=="Yes"),1,0))

#Identiying number of missing values
missing<- data %>% summarize_all(funs(sum(is.na(.))))

#Visualizing the missing values

missing %>% gather(key ="variables", value ="missing_values") %>%
  ggplot(aes(x=variables, y=missing_values)) +geom_bar(stat="identity", fill = "blue") + 
  coord_flip() + ggtitle("Missing values per variable")

#There are 11 missing values in the dataset, and all of them are from one variable - "Total Charges".
#Out of 7043 records, 11 records are missing and for the purpose of this EDA, those NA's will be removed.


row_index<- which(is.na(data$TotalCharges))
row_index

newdata<- data[-row_index,]

#Checking how many rows were deleted
nrow(data) - nrow(newdata)

#Converting each variable to its respective class
newdata$SeniorCitizen <- as.factor(newdata$SeniorCitizen)

#Checking levels of some of the services
levels(newdata$MultipleLines)
levels(newdata$PhoneService)
levels(newdata$InternetService)
levels(newdata$OnlineSecurity)
levels(newdata$DeviceProtection)
levels(newdata$TechSupport)
levels(newdata$StreamingTV)
levels(newdata$StreamingMovies)


#Proportion of Churn customers
options(repr.plot.width = 4, repr.plot.height = 6)
newdata %>% group_by(Churn)  %>% dplyr::summarize(Percentage = 100*n()/nrow(newdata)) %>%
  ggplot(aes(Churn, Percentage)) + geom_bar(stat="identity", fill=c("blue", "navyblue")) +
  ggtitle("Churn Percentage")

#Roughly 1 out of 4 customer is churning out. Let's deep-dive further into customer profile and understand which customers contribute more towards churn.


options(repr.plot.width =4, repr.plot.height = 4)
newdata %>% ggplot(aes(y=MonthlyCharges, x="", fill= Churn)) + geom_boxplot()
#Monthly charges for customers who have churned is much higher at 75 compared to around 60 for customers who have not churned


options(repr.plot.width =4, repr.plot.height = 4)
newdata %>% ggplot(aes(y=tenure, x="", fill= Churn)) + geom_boxplot()
#Customers with less tenure are more like to churn.
#Few outliers do exist.

options(repr.plot.width =4, repr.plot.height = 4)
newdata %>% ggplot(aes(y=TotalCharges, x="", fill= Churn)) + geom_boxplot()
#Median of total charges for churned customer is less. A lot of outliers are present.

#Let's visualize categorical variables and see if we can find anything.
#Preparing a grid of plots

options(repr.plot.width = 8, repr.plot.height = 12)
plot_grid(ggplot(newdata, aes(x=PaymentMethod,fill=Churn))+ geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)),
          ggplot(newdata, aes(x=PaperlessBilling,fill=Churn))+ geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)),
          ggplot(newdata, aes(x=Contract,fill=Churn))+ geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)),
          ggplot(newdata, aes(x=Partner,fill=Churn))+ geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)),
          ggplot(newdata, aes(x=PhoneService,fill=Churn))+ geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)),
          ggplot(newdata, aes(x=MultipleLines,fill=Churn))+ geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)),
          ggplot(newdata, aes(x=InternetService,fill=Churn))+ geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)),
          ggplot(newdata, aes(x=OnlineSecurity,fill=Churn))+ geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)),
          ggplot(newdata, aes(x=StreamingTV,fill=Churn))+ geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 8)))
#Lots of interesting information:
#Customers with month-to-month contract churned the most in their category.
#Customers with FiberOptic internet service churned quite a lot as well.
#Customers that didn't opt for internet security churned way more than others.
#Let's drill down one step further on these.

#Learning more about the month-to-month customers

#Were they senior citizen who switched to another telecom
newdata %>% filter(Contract=="Month-to-month") %>% group_by(Churn, SeniorCitizen) %>% dplyr::summarize(count=n()) %>% 
  mutate(percent= 100*count/sum(count))
#26% of all Senior citizens with month-to-month contract have churned 

newdata %>% filter(Contract=="Month-to-month") %>% group_by(Churn, InternetService) %>% dplyr::summarize(count=n()) %>% 
  mutate(percent= 100*count/sum(count)) %>%
  ggplot(aes(x=InternetService, y=percent, fill=Churn)) +geom_bar(stat="identity")
#Again we see a similar statistic that we saw earlier where customer with Fiberoptic connections were churning the most as compared to others. Here as well, of all the customers having month-to-month contract, it is the customers with fiberoptic that are switching to other telecos.

newdata %>% filter(tenure<=12) %>% group_by(Churn, StreamingTV) %>% dplyr::summarize(count=n()) %>% 
  mutate(percent= 100*count/sum(count)) %>% 
  ggplot(aes(x=StreamingTV, y=percent, fill=Churn)) +geom_bar(stat="identity")
#Customers that didn't have tv streaming services churned a lot. ABout 60% of customers who did not have streaming TV have churned


#Split train and test data
# Set seed
set.seed(123)
library(caret)
# Split data, 75% distribution of churn for training
train.index <- createDataPartition(
  y = newdata$Churn, p = 0.75, list = FALSE
)

train <- newdata[train.index,]
test <- newdata[-train.index,]


# Building a decision tree
library(tree)
tree.churn=tree(Churn~.-customerID,train )

summary(tree.churn)
#Variables actually used in tree construction:
#"Contract"    "MonthlyCharges"    "InternetService" "tenure" 
#Misclassifcation error rate is high at 21%
plot(tree.churn)
text(tree.churn,pretty=0)

tree.pred=predict(tree.churn,test ,type ="class")
table(tree.pred ,test$Churn)
#A single decison tree gave about 77.2% accuracy

#Pruning the tree
cv.churn=cv.tree(tree.churn,FUN=prune.misclass)
cv.churn


par(mfrow=c(1,2))
plot(cv.churn$size,cv.churn$dev,type="b")
plot(cv.churn$k,cv.churn$dev,type="b")

#Predicting using the pruned tree
prune.churn=prune.misclass(tree.churn,best=4)
plot(prune.churn)
text(prune.churn,pretty=0)

tree.pred=predict(prune.churn,test ,type="class")
table(tree.pred ,test$Churn)
# Accuracy did not improve after pruning the tree


#Building a random forest
library(randomForest)
set.seed(1)
rf.churn=randomForest(Churn~.-customerID,data=train,ntree = 250, mtry = 3, importance =TRUE)
summary(rf.churn)
yhat.rf = predict(rf.churn,newdata=test)
table(yhat.rf,test$Churn)
# Accuracy is 80.3%, which is an improvement over single decision tree

importance(rf.churn)
varImpPlot(rf.churn)


#Support Vector Classifier-Linear kernel
library(e1071)
svmchurn=svm(Churn~.-customerID, data=train , kernel ="linear", cost =0.1, scale =FALSE)

ypred=predict(svmchurn,test)
table(ypred,test$Churn)
#Prediction accuracy is 77.8%

#Logistic regression
#Train the logistic regression model using all columns
model1 <- glm(Churn ~.-customerID, family = "binomial", data = train)
summary(model1)

#use AIC to exclude variables based on their significance and create model2
library(MASS)
model2 <- stepAIC(model1, trace = 0)
summary(model2)

model3 <- glm(formula = Churn ~  SeniorCitizen + Dependents + tenure+ PhoneService +
                PaperlessBilling +  TotalCharges +Contract+OnlineSecurity + InternetService,
              family = "binomial", data = train)
predict(model3, newdata = test, type = "response") -> test_prob
test_pred <- factor(ifelse(test_prob >= 0.5, 1, 0))
table(test_pred ,test$Churn)

#Evaluation of ensemble model

prediction<-cbind(test_pred, yhat.rf, ypred)
# Create the function for mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

s<-apply(prediction, 1, function(x) getmode(x))
table(s,test$Churn)
