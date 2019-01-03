#Importing the required libraries
library(readxl)
library(ggplot2)
library(gridExtra)
library(mice)
library(VIM)
library(dplyr)
library(caTools)
library(randomForest)
library(caret)
library(missForest)
library(rpart)
library(partykit)
library(MASS)
library(mlr)
library(corrplot)

#Sourcing for required functions
source("utilities.R")

###Importing the xls
master_data <- read_excel("Project Data.xlsx")
str(master_data)
#The data set contains 17 variables and 296 rows


##################################################### Data Cleaning ###########################################################

### Removing the ID column for further data processing 
data_clean <- master_data[,-1]
names(data_clean)<- tolower(names(data_clean))


####Check for duplicated records
sum(duplicated(master_data[,-1]))
#There is one duplicate records which can be removed

###Remove the duplicated record
data_clean <- data_clean[-which(duplicated(master_data[,-1])),]

### Checking for the number of unique values in each varible to have an idea if it is catagorical or continuous
lapply(data_clean,function(vec){length(unique(vec))})
cat_vars <- c('response','group','y1','y2','y3','y4','y5','y6','y7')
cont_var <- c('x1','x2','x3','x4','x5','x6','x7')

### Correlation of dataset
M<-cor(data_clean, use="complete.obs")
corrplot(M, method="circle")

### Setting the Catagorical data type to factor
data_clean[,names(data_clean) %in% cat_vars]<-lapply(data_clean[,names(data_clean) %in% cat_vars],as.factor)
str(data_clean)



###################################################### Univariate Analysis ###################################################

### Frequency Pot of Catagroical variables
plot1<-ggplot(data_clean,aes(response))+geom_bar(fill="steelblue") +
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Response") + ylab("Frequency")
plot1


plot2<-ggplot(data_clean,aes(group))+geom_bar(fill="darkturquoise") +
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Group") + ylab("Frequency")
plot2

plot3<-ggplot(data_clean,aes(y1))+geom_bar(fill="dodgerblue4") +
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Y1") + ylab("Frequency")
plot3

plot4<-ggplot(data_clean,aes(y2))+geom_bar(fill="deepskyblue1") +
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Y2") + ylab("Frequency")
plot4

plot5<-ggplot(data_clean,aes(y3))+geom_bar(fill="aquamarine4") +
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Y3") + ylab("Frequency")
plot5

plot6<-ggplot(data_clean,aes(y4))+geom_bar(fill="cyan4") +
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Y4") + ylab("Frequency")
plot6

plot7<-ggplot(data_clean,aes(y5))+geom_bar(fill="darkseagreen4") +
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Y5") + ylab("Frequency")
plot7

plot8<-ggplot(data_clean,aes(y6))+geom_bar(fill="turquoise") +
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Y6") + ylab("Frequency")
plot8

plot9<-ggplot(data_clean,aes(y7))+geom_bar(fill="steelblue3") +
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Y7") + ylab("Frequency")
plot9

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,nrow=2)


### Box plots of Continuous Variables
plot10<-ggplot(data_clean,aes("",x1))+geom_boxplot(fill="steelblue") + ylab("X1") + xlab("")
plot10

plot11<-ggplot(data_clean,aes("",x2))+geom_boxplot(fill="steelblue") + ylab("X2") + xlab("")
plot11

plot12<-ggplot(data_clean,aes("",x3))+geom_boxplot(fill="steelblue") + ylab("X3") + xlab("")
plot12

plot13<-ggplot(data_clean,aes("",x4))+geom_boxplot(fill="steelblue") + ylab("X4") + xlab("")
plot13

plot14<-ggplot(data_clean,aes("",x5))+geom_boxplot(fill="steelblue") + ylab("X5") + xlab("")
plot14

plot15<-ggplot(data_clean,aes("",x6))+geom_boxplot(fill="steelblue") + ylab("X6") + xlab("")
plot15

plot16<-ggplot(data_clean,aes("",x7))+geom_boxplot(fill="steelblue") + ylab("X7") + xlab("")
plot16

grid.arrange(plot10,plot11,plot12,plot13,plot14,plot15,plot16,nrow=3)



### Outlier treatment for the continuous features
data_clean[,names(data_clean) %in% cont_var]<-lapply(data_clean[,names(data_clean) %in% cont_var],winsorization)
data_clean[,names(data_clean) %in% cont_var]<-lapply(data_clean[,names(data_clean) %in% cont_var],sqrt)
data_clean[,names(data_clean) %in% cont_var]<- scale(data_clean[,names(data_clean) %in% cont_var])




################################################### Bivariate Analysis #######################################################

### Catagorical variables VS Response
plot17<-ggplot(data_clean,aes(y1,fill=response))+geom_bar()+
    geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5)) +
    xlab("Y1") + ylab("Frequency")
plot17

plot18<-ggplot(data_clean,aes(y2,fill=response))+geom_bar()+
    geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5)) +
    xlab("Y2") + ylab("Frequency")
plot18

plot19<-ggplot(data_clean,aes(y3,fill=response))+geom_bar()+
    geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5)) +
    xlab("Y3") + ylab("Frequency")
plot19

plot20<-ggplot(data_clean,aes(y4,fill=response))+geom_bar()+
    geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5)) +
    xlab("Y4") + ylab("Frequency")
plot20

plot21<-ggplot(data_clean,aes(y5,fill=response))+geom_bar()+
    geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5)) +
    xlab("Y5") + ylab("Frequency")
plot21

plot22<-ggplot(data_clean,aes(y6,fill=response))+geom_bar()+
    geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5)) +
    xlab("Y6") + ylab("Frequency")
plot22

plot23<-ggplot(data_clean,aes(y7,fill=response))+geom_bar()+
    geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5)) +
    xlab("Y7") + ylab("Frequency")
plot23

plot24<-ggplot(data_clean,aes(group,fill=response))+geom_bar()+
    geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5)) +
    xlab("Group") + ylab("Frequency")
plot24

grid.arrange(plot17,plot18,plot19,plot20,plot21,plot22,plot23,plot24,nrow=3)


### Continuous variables VS Response
plot25<-ggplot(data_clean,aes(response,x1))+geom_boxplot(fill="steelblue") + xlab("Response") + ylab("X1")
plot25

plot26<-ggplot(data_clean,aes(response,x2))+geom_boxplot(fill="steelblue") + xlab("Response") + ylab("X2")
plot26

plot27<-ggplot(data_clean,aes(response,x3))+geom_boxplot(fill="steelblue") + xlab("Response") + ylab("X3")
plot27

plot28<-ggplot(data_clean,aes(response,x4))+geom_boxplot(fill="steelblue") + xlab("Response") + ylab("X4")
plot28

plot29<-ggplot(data_clean,aes(response,x5))+geom_boxplot(fill="steelblue") + xlab("Response") + ylab("X5")
plot29

plot30<-ggplot(data_clean,aes(response,x6))+geom_boxplot(fill="steelblue") + xlab("Response") + ylab("X6")
plot30

plot31<-ggplot(data_clean,aes(response,x7))+geom_boxplot(fill="steelblue") + xlab("Response") + ylab("X7")
plot31

grid.arrange(plot25,plot26,plot27,plot28,plot29,plot30,plot31,nrow=3)


###X vs Y
plot32<-ggplot(data_clean,aes(y1,x1))+geom_boxplot(fill="steelblue") + xlab("Y1") + ylab("X1")
plot32

plot33<-ggplot(data_clean,aes(y2,x2))+geom_boxplot(fill="steelblue") + xlab("Y2") + ylab("X2")
plot33

plot34<-ggplot(data_clean,aes(y3,x3))+geom_boxplot(fill="steelblue") + xlab("Y3") + ylab("X3")
plot34

plot35<-ggplot(data_clean,aes(y4,x4))+geom_boxplot(fill="steelblue") + xlab("Y4") + ylab("X4")
plot35

plot36<-ggplot(data_clean,aes(y5,x5))+geom_boxplot(fill="steelblue") + xlab("Y5") + ylab("X5")
plot36

plot37<-ggplot(data_clean,aes(y6,x6))+geom_boxplot(fill="steelblue") + xlab("Y6") + ylab("X6")
plot37

plot38<-ggplot(data_clean,aes(y7,x7))+geom_boxplot(fill="steelblue") + xlab("Y7") + ylab("X7")
plot38

grid.arrange(plot32,plot33,plot34,plot35,plot36,plot37,plot38,nrow=3)


####################################################### Missing Value Treatment ##############################################

### Create Dummy variable for y6
data_clean<-createDummyFeatures(data_clean, cols = "y6",method='reference')
cat_vars <- c('response','group','y1','y2','y3','y4','y5','y6.1','y6.2','y7')

data_clean$y6.1<- as.factor(data_clean$y6.1)
data_clean$y6.2<- as.factor(data_clean$y6.2)


### Understanding the missing values and  treating them using MICE
na<-colSums(is.na(data_clean))
perc.na <- data.frame(colname=names(data_clean),cnt.na=na,percent.na=colSums(is.na(data_clean))/nrow(data_clean),stringsAsFactors = F)
row.names(perc.na)<-NULL
perc.na[order(-perc.na$cnt.na),]
ggplot(perc.na, aes(colname,percent.na)) + geom_bar(stat = "identity",fill="steelblue")  + xlab("Variable") + ylab("Percent of missing")
md.pattern(data_clean)


### Creating seperate dataframes for X and Y for prediction
subset_cont <- data_clean[,names(data_clean) %in% append(cont_var,c('response','group'))]
subset_cat <- data_clean[,names(data_clean) %in% cat_vars ]


### MICE Treatment - Multivariate Imputation by Chained Equations
imputed_Data_cont <- mice(subset_cont, m=7, maxit = 99, method = 'norm', seed = 2018)
completeData_cont <- complete(imputed_Data_cont,5)


#subset_cat<- cbind(completeData_cont[,names(completeData_cont) %in% cont_var ],subset_cat)


imputed_Data_cat <- mice(subset_cat, m=7, maxit = 99, method = 'logreg', seed = 2018)
completeData_cat <- complete(imputed_Data_cat,5)

"
### kNN Treatment
completeData_cont <- kNN(completeData_cont,c('x1','x2','x3','x5','x6','x7'),k=7)

completeData_cat <-  kNN(completeData_cat,c('y1','y2','y3','y5','y6','y7'),k=7)


completeData_cont <- kNN(data_clean,c('x1','x2','x3','x5','x6','x7','y1','y2','y3','y5','y6','y7'),k=7)
imputed_Data_cont <- mice(data_clean, m=7, maxit = 99, method = 'pmm', seed = 2018)
completeData_cont <- complete(imputed_Data_cont,5)

summary(completeData_cont)


### Random Forrest Treatment
data_model_building <- missForest(as.data.frame(data_clean))
data_model_building<-data_model_building$ximp

completeData_cat <- missForest(completeData_cat)
completeData_cat<-completeData_cat$ximp


completeData_cont[,names(completeData_cont) %in% cont_var]<-lapply(subset_cont[,names(subset_cont) %in% cont_var],function(vec){ifelse(is.na(vec),-9999,vec)})

str(completeData_cont$ximp)
"

#Merging both continuous and catagorical data
data_model_building<- cbind(completeData_cont[,names(completeData_cont) %in% cont_var ],completeData_cat)

#data_model_building<-completeData_cat



########################################################### Model Building ################################################### 
#splitting of train and test dataset in the ration 75:25
set.seed(2018)
train_indices <- sample.split(data_model_building$response,SplitRatio=0.75)
train <- data_model_building[train_indices,]
test <- data_model_building[!(train_indices),]


var_list_1<- c('x1','x2','x3','x4','x5','x6','x7','group')
var_list_2 <- c('y1','y2','y3','y5','y6.1','y6.2','y7','group')

#Checkong for the split of response between test and train
table(train$response)
table(test$response)


####################DECISION TREE##############################

### Decision Tree  for X
set.seed(2018)
DT_Modelx <- rpart(response~x1 + x2 + x3 + x4 + x5 + x6 + x7 +group,data =train,control= rpart.control(maxdepth=20,cp=0.0001) )
plot(as.party(DT_Modelx))
print(DT_Modelx)                
#Procedure for Pruning
print(DT_Modelx$cptable)
opt<- which.min(DT_Modelx$cptable[,"xerror"])
#Pruning the tree to the least xerror
cp<- DT_Modelx$cptable[opt,"CP"]
DT_Modelx_Pruned<- prune(DT_Modelx,cp=cp)
plot(as.party(DT_Modelx_Pruned))
dtxpredicted= predict(DT_Modelx_Pruned,test,type="prob")[,2]
class_eval_metrics(dtxpredicted,test$response)
#Accuracy 0.6849
#Sensitivity : 0.7143
#Specificity : 0.6579
var_importance(DT_Modelx_Pruned)



### Decision Tree  for Y
set.seed(2018)
DT_Modely <- rpart(response~y1 + y2 + y3 + y4 + y5 + y6.1 +y6.2 + y7 +group,data =train,control= rpart.control(maxdepth=20,cp=0.0001) )
plot(as.party(DT_Modely))
print(DT_Modely)                
#Procedure for Pruning
print(DT_Modely$cptable)
opt<- which.min(DT_Modely$cptable[,"xerror"])
#Pruning the tree to the least xerror
cp<- DT_Modely$cptable[opt,"CP"]
DT_Modely_Pruned<- prune(DT_Modely,cp=cp)
plot(as.party(DT_Modely_Pruned))
dtypredicted= predict(DT_Modely_Pruned,test,type="prob")[,2]
class_eval_metrics(dtypredicted,test$response)
#Accuracy 0.7123
#Sensitivity : 0.6000 
#Specificity : 0.8158
var_importance(DT_Modely_Pruned)



### Decision Tree  for X and Y
set.seed(2018)
DT_Modelxy <- rpart(response~. ,data =train,control= rpart.control(cp=0.0001,maxdepth=20) )
plot(as.party(DT_Modelxy))
print(DT_Modelxy)                
#Procedure for Pruning
print(DT_Modelxy$cptable)
opt<- which.min(DT_Modelxy$cptable[,"xerror"])
#Pruning the tree to the least xerror
cp<- DT_Modelxy$cptable[opt,"CP"]
DT_Modelxy_Pruned<- prune(DT_Modelxy,cp=cp)
plot(as.party(DT_Modelxy_Pruned))
dtypredicted= predict(DT_Modelxy_Pruned,test,type="prob")[,2]
class_eval_metrics(dtypredicted,test$response)
#Accuracy 0.6849
#Sensitivity : 0.7143
#Specificity : 0.7368 
var_importance(DT_Modelxy_Pruned)



####################RANDOM FOREST##############################

###Random Forest for X
##tune for ntree and mtry  
set.seed(2018)
for (i in c(199,299,399,499,599,699,799,899,999,1199)){
    print(i)
    bestmtry <- tuneRF(train[,names(train) %in% var_list_1],train$response, stepFactor=1.5, improve=1e-5, ntree=i)
    print(bestmtry)
}
set.seed(2018)
rf_modelx <- randomForest(response ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 +group , 
                          data=train, do.trace=TRUE,importance= T,ntree=499,mtry=2)
rfxpredicted<-predict(rf_modelx,test,type="prob")[,2]
class_eval_metrics(rfxpredicted,test$response)
#Accuracy : 0.8082
#Sensitivity : 0.8286
#Specificity : 0.7895
var_importance(rf_modelx)



###Random Forest for Y
set.seed(2018)
for (i in c(199,299,399,499,599,699,799,899,999,1199)){
    print(i)
    bestmtry <- tuneRF(train[,names(train) %in% var_list_2],train$response, stepFactor=1.5, improve=1e-5, ntree=i)
    print(bestmtry)
}

set.seed(2018)
rf_modely <- randomForest(response ~ y1 + y2 + y3 + y4 + y5 + y6.1 + y6.2 +y7 +group , 
                          data=train, do.trace=TRUE,importance= T,ntree=599,mtry=2)
rfypredicted<-predict(rf_modely,test,type="prob")[,2]
class_eval_metrics(rfypredicted,test$response)
#For y
#Accuracy : 0.7534 
#Sensitivity : 0.7714
#Specificity : 0.7368
var_importance(rf_modely)



###Random Forest for X and Y
set.seed(2018)
for (i in c(199,299,399,499,599,699,799,899,999,1199)){
    print(i)
    bestmtry <- tuneRF(train[,names(train) != 'response'],train$response, stepFactor=1.5, improve=1e-5, ntree=i)
    print(bestmtry)
}
set.seed(2018)
rf_modelxy <- randomForest(response ~ . , 
                           data=train, do.trace=TRUE,importance= T,ntree=799,mtry=3)
rfxypredicted<-predict(rf_modelxy,test,type="prob")[,2]
class_eval_metrics(rfxypredicted,test$response)
#For X and y
#Accuracy : 0.7534
#Sensitivity : 0.7714
#Specificity : 0.7368
var_importance(rf_modelxy)

####################LOGISTIC REGRESSION##############################


### Logistic regression for X
lg_modelx<- glm(response~x1 + x2 + x3 + x4 + x5 + x6 + x7 +group,data=test,family="binomial")
summary(lg_modelx)
#Step bidirection variable reduction for best model
lg_modelx <- stepAIC(lg_modelx,direction="both")
summary(lg_modelx)
lgxpredicted= predict(lg_modelx,test,type="response")
class_eval_metrics(lgxpredicted,test$response)
#Accuracy : 0.726
#Sensitivity : 0.7429          
#Specificity : 0.7105 
var_importance(lg_modelx)



### Logistic regression for Y
lg_modely<- glm(response~y1 + y2 + y3 + y4 + y5 + y6.1 + y6.2 +y7 +group,data=test,family="binomial")
summary(lg_modely)
#Step bidirection variable reduction for best model
lg_modely <- stepAIC(lg_modely,direction="both")
summary(lg_modely)
lgypredicted= predict(lg_modely,test,type="response")
class_eval_metrics(lgypredicted,test$response)
#Accuracy : 0.7945 
#Sensitivity : 0.8000          
#Specificity : 0.7895 
var_importance(lg_modely)




### Logistic regression for X and Y
lg_modelxy<- glm(response~.,data=test,family="binomial")
summary(lg_modelxy)
#Step bidirection variable reduction for best model
lg_modelxy <- stepAIC(lg_modelxy,direction="both")
summary(lg_modelxy)
lgxypredicted= predict(lg_modelxy,test,type="response")
class_eval_metrics(lgxypredicted,test$response)
#Accuracy : 0.7808
#Sensitivity : 0.8000          
#Specificity : 0.7632 
var_importance(lg_modelxy)


################################################# END ################################################################

