rm(list=ls())
library(dplyr)
library(tidyr)
library(rpart)
library(rpart.plot)
library(caret)
library(caTools)
library(gridExtra)
library(randomForest)
library(caret)
library(car)
library(MASS)
library(xgboost)
library(scales)
library(ROCR)
library(ggplot2)
library(broom)
library(glmnet)
library(plyr)



options("scipen"=100, "digits"=4)

#######################################Importing data###################################################################
raw_cust_data<- read.csv("train.csv",stringsAsFactors = F,na.strings = c("","NA","n/a"))
test_cust_data<- read.csv("test.csv",stringsAsFactors = F,na.strings = c("","NA","n/a"))

#Naming all the columns to lower case
names(raw_cust_data)<-tolower(names(raw_cust_data))
names(test_cust_data)<-tolower(names(test_cust_data))

#Checking for duplicate records
sum(duplicated(raw_cust_data))
sum(duplicated(test_cust_data))

any(names(raw_cust_data)[names(raw_cust_data) != "responders"] != names(test_cust_data))
train.responders<-raw_cust_data$responders
raw_cust_data$responders<-NULL

nrow(raw_cust_data)
nrow(test_cust_data)

#Creating a master data for cleaning
master.data<-rbind(raw_cust_data,test_cust_data)

#Viewing the metadata
str(master.data)
#View(head(master.data,100))


#Checking for NA's
perc.na <- data.frame(colname=names(master.data),percent.na=colSums(is.na(master.data))/nrow(master.data),stringsAsFactors = F)
row.names(perc.na)<-NULL
perc.na <- perc.na[with(perc.na,order(-percent.na)),]
na.cols<-perc.na$colname[ perc.na$percent.na >=0.3] 
length(na.cols)
#161

#Columns containing only one unique value
single.val.col <- names(master.data)[sapply(master.data,function(vec){length(unique(vec))<=1})]
length(single.val.col)
#46

#Most of the data is 0
most.value.zero.tr<-names(master.data)[sapply(master.data,function(vec){sum(vec==0,na.rm=T)/nrow(master.data)>=0.95})]
length(most.value.zero.tr)
#60

col.to.remove<-unique(c(na.cols,single.val.col,most.value.zero.tr))
length(col.to.remove)
#221

master.data <-master.data[,!(names(master.data) %in% col.to.remove)]


#Remoiving city and zipcode since they are difficult to be interpreted in a model
master.data$city<-NULL
master.data$zip<-NULL


catagorical_var<-names(master.data)[sapply(master.data,function(vector){length(unique(vector)) <= 50 })]
measure_var<-names(master.data)[sapply(master.data,function(vector){length(unique(vector)) > 50 & is.numeric(vector)})]
length(catagorical_var)+length(measure_var)
measure_var <- measure_var[measure_var!="ucic_id"]


data.frame(colname=names(master.data),percent.na=colSums(is.na(master.data)),stringsAsFactors = F)



#No_of_acc :Comparitively Very few ppl have more than 3 accounts we could bin all records more or equal to 3
master.data$no_of_accs[master.data$no_of_accs>=3]<-3

#Missing values in occup_all_new 96
master.data$occup_all_new[is.na(master.data$occup_all_new)]<-"MISSING"

#Dependents : Comparitively very few ppl have more than 5
master.data$dependents[master.data$dependents>=3]<-3
master.data$dependents[is.na(master.data$dependents)]<-0

#nA are there final_worth_prev1


#engagement_tag_prev1

master.data$engagement_tag_prev1[is.na(master.data$engagement_tag_prev1)]<-"NO"
master.data$recency_of_cr_txn[is.na(master.data$recency_of_cr_txn)]<-0
master.data$recency_of_dr_txn[is.na(master.data$recency_of_dr_txn)]<-0
master.data$recency_of_branch_txn[is.na(master.data$recency_of_branch_txn)]<-0
master.data$recency_of_activity[is.na(master.data$recency_of_activity)]<-0
master.data$final_worth_prev1[is.na(master.data$final_worth_prev1)]<- "Unknown"




for(i in catagorical_var[-length(catagorical_var)]) {
    
    readline(prompt="press enter to view plots")
    print(i)
    plot1<-plot1<-ggplot(master.data,aes(factor(master.data[,i])))+geom_bar(fill="steelblue")+
        xlab(i) + ylab("Frequency") +geom_text(stat='count',aes(label=..count..),hjust=0)+coord_flip()
    print(master.data  %>% group_by(master.data[,i]) %>% 
              summarise(percent=100*n()/length(master.data[,i])) %>% arrange(desc(percent)))
    
    plot2<-ggplot(master.data,aes(factor(master.data[,i]),fill=factor(master.data[,"responders"])))+geom_bar(position = 'fill') +
        xlab(i) + ylab("Relative perccentage") +scale_y_continuous(label=percent) + labs(fill="churn")  + coord_flip()
    grid.arrange(plot1,plot2,nrow=2)
    
}



meas_freq_line<-function(df,measure)
{
    print(summary(df[,measure]))
    plot1<-ggplot(df,aes(df[,measure]))+
        geom_histogram(bins=nclass.Sturges(df[,measure]) ,na.rm = T)+
        xlab(measure) + ylab("Frequency") 
    plot2<- ggplot(df,aes(y=df[,measure],x=measure)) + geom_boxplot()
    grid.arrange(plot1,plot2)
}




#Frequency plot of measure variables
for(i in measure_var) {
    readline(prompt="press enter to view plots")
    print(i)
    print(quantile(master.data[,i], probs = seq(0,1,0.01),na.rm=T))
    print(meas_freq_line(master.data,i))
}

#############Applying logs
#master.data.bck<-master.data
master.data<-master.data.bck

modify <- function(vec){
    squi<-squish(vec,quantile(vec,probs=c(0.05,0.95),na.rm=T))
    if(min(squi)<0)
    {
        squi
    }else if(min(squi)==0){
        log10(squi+0.0001)
        
    }else
    {
        log10(squi)
    }
    
}
master.data<-data.frame(lapply(master.data[,measure_var],modify),master.data[,catagorical_var],ucic_id=master.data$ucic_id)





#Creating dummy for all the catagorical variables
dummy_conv<-function(vector,vec_name)
{
    vector<-as.factor(vector)
    
    if(length(unique(vector))>2)
    {
        output<-as.data.frame(model.matrix(~vector))[,-1]
        names(output)<-gsub("vector",paste(vec_name,"."),names(output))
        names(output)<-gsub(" ","",names(output))
    }
    else{
        levels(vector)<-0:length(unique(vector))
        output<-as.data.frame(as.numeric(levels(vector))[vector])
        names(output)<-vec_name
        
    }
    output
}


for(i in catagorical_var)
{
    print(i)
    master.data<-cbind(master.data[,-which(names(master.data)==i)] ,dummy_conv(master.data[,i],i))
}



train_data<- master.data[1:300000,]
test_data<-master.data[300001:500000,]
train_data$responders<-train.responders



########################################## Gradient boosing Forrest ##############################################################
xg.data<-data.matrix(train_data[,!(names(train_data) %in% c("responders","ucic_id"))])
xg.test<-data.matrix(test_data[,!(names(test_data) %in% c("ucic_id"))])
set.seed(2017)
model_1<-xgboost(data =xg.data ,label = train_data$responders , max.depth = 20, eta = 0.02, nround = 350, objective = "binary:logistic")
predict_1<-predict(model_1,xg.test)
xg.final<- data.frame(UCIC_ID=test_data$ucic_id,Responders=predict_1)
write.csv(xg.final,"result3.csv")
importance_matrix <- xgb.importance(names(train_data[,!(names(train_data) %in% c("responders","ucic_id"))]), model = model_1)
xgb.plot.importance(importance_matrix[1:30])
top30<-importance_matrix[1:30]$Feature

make.names(names(train_data))
cv.ctrl <- trainControl(method = "cv",number = 5, 
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       allowParallel=T)

xgb.grid <- expand.grid(nrounds = c(200,400,600),
                        eta = c(0.01,0.05),
                        max_depth = c(4,10,15,20,25))
set.seed(2017)
y<-factor(train_data$responders)
xgb_tune <-train(train_data,y,method="xgbTree",trControl=cv.ctrl,tuneGrid=xgb.grid,verbose=T,metric="ROC")

train_data$responders<-factor(train_data$responders)
names(train_data)

param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc",
              "eta" = c(0.02,0.05), "max.depth" = c(5,10,20))

bst.cv <- xgb.cv(param=param, data = xg.data, label =train_data$responders , nfold = 10, nrounds = 999)


######################################### Regression ##################################################################
train_data_reg<-train_data[,top30]
train_data_reg<-data.frame(lapply(train_data_reg,scale))
train_data_reg$responders<-train_data$responders


test_data_reg<-test_data[,top30]
test_data_reg<-data.frame(lapply(test_data_reg,scale))




set.seed(2017)
split.indices <- sample.split(train_data_reg$responders,SplitRatio=0.7)
train1 <- train_data_reg[split.indices, ]
test1 <- train_data_reg[!split.indices, ]


summary(model_2.2)
sort(vif(model_2.2))

predict_2<-predict(mode_2.1,test1,type="response")
pred<-ifelse(predict_2>0.15,1,0)
confusionMatrix(pred,test1$responders)


model_2<-glm(responders~.,train_data_reg,family="binomial")
summary(model_2)
sort(vif(model_2))

mode_2.1<-stepAIC(model_2,direction="both")
summary(mode_2.1)

mode_2.1<-glm(formula = responders ~ eop_prev1 + cr_amb_drop_build_1 + 
                  i_aqb_prevq1 + bal_prev3 + vintage + d_prev1 + brn_code + 
                  cr_amb_drop_build_5 + cr_amb_drop_build_2 + age + bal_prev1 + 
                  cr_amb_drop_build_3 + i_cr_aqb_prevq1 + cr_amb_drop_build_4 + 
                  bal_prev2 + bal_prev6 + recency_of_cr_txn + eop_prev6 + i_nrv_prevq1 + 
                  eop_prev2 + eop_prev3 + recency_of_dr_txn + i_nrv_prevq2 + 
                  final_worth_prev1.MEDIUM + cnr_prev6 + recency_of_activity + 
                  bal_prev5, family = "binomial", data = train_data_reg)
summary(mode_2.2)
sort(vif(mode_2.2))

predict_2<-predict(mode_2.1,test_data_reg,type="response")

glm.final<- data.frame(UCIC_ID=test_data$ucic_id,Responders=predict_2)
write.csv(glm.final,"result6.csv")


train2<-as.matrix(train_data_reg[,names(train_data_reg)!="responders"])
test2<-as.matrix(test1[,names(test1)!="responders"])
?cv.glmnet
RR.cv<-cv.glmnet(train2,train_data_reg$responders,family="binomial",nfolds=10,alpha=1)
plot(RR.cv)


lambda.grid<-c(0,RR.cv$lambda.min,RR.cv$lambda.1se)

model_3<- glmnet(train2,train_data_reg$responders,alpha=1,lambda=lambda.grid,family='binomial')

predict_3<-predict(model_3,test2,type="response")
pred<-ifelse(predict_3>0.14,1,0)
confusionMatrix(pred,test1$responders)

test3<-
as.matrix(test_data_reg)
predict_3<-predict(model_3,s=1,test3,type="response")
rglm.final<- data.frame(UCIC_ID=test_data$ucic_id,Responders= predict_3)
write.csv(rglm.final,"result8.csv")

predict_3[1]


########################################## Random Forrest ##############################################################

set.seed(2017)
model_4 <- randomForest(responders ~ ., data=train_data, proximity=FALSE,ntree=499, do.trace=TRUE,na.action=na.omit)
predict_2<-predict(model_2,test_data,type="prob")[,2]
rf.final<- data.frame(UCIC_ID=test_data$ucic_id,Responders=predict_2)
write.csv(xg.final,"result2.csv")






















