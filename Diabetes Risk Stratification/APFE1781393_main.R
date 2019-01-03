##################################################################################################################################
#A care management organisation called WeCare wants to identify among its diabetic patients, the ones that are at high risk      #
#of getting re-admitted to the hospital. They wish to intervene by providing some incentive to these patients that will          #
#help them improve their health identify high-risk diabetic patients through risk stratification.                                #
#This will help the payer to decide what are the right intervention programs for these patients.                                 #
#                                                                                                                                #    
#                                                                                                                                #
#                                                                                                                                #
#                                                                                                                                #
##################################################################################################################################
options("scipen"=100, "digits"=4)

library(icd)
library(caret)
library(dplyr)
library(scales)
library(gridExtra)
library(caTools)
library(corrplot)
library(MASS)
library(car)
library(ROCR) 
library(Metrics)
library(randomForest)
library(stringr)

rm(list=ls())

setwd("C:\\Users\\johnp\\Desktop\\Risk stratification")
set.seed(2018)
#Importing the diabetes data
diabetic_data<- read.csv("diabetic_data.csv",stringsAsFactors = F, na.strings = c("NA","#DIV/0!", "","NaN","?"))
nrow(diabetic_data)
ncol(diabetic_data)

#renaming the columns to lower case
names(diabetic_data)<- tolower(names(diabetic_data))

################################################ Data cleaning ###################################################################

###Viewing the top and bottom  50 rows to identify if the data has been correctly parsed
#View(head(diabetic_data ,50))
#View(tail(diabetic_data ,50))
str(diabetic_data)

#Check for duplicated records
any(duplicated(diabetic_data))


#Describing the unique values to have a picture of data and checking for spelling 

lapply(diabetic_data,unique)

#Renaming the African American category for race
diabetic_data$race<-ifelse(diabetic_data$race =="AfricanAmerican","African American",diabetic_data$race)

#Idenifying the dependent variable and classfying it into binary
diabetic_data$readmitted<-ifelse(diabetic_data$readmitted ==">30" | diabetic_data$readmitted =="<30" ,"YES",diabetic_data$readmitted)

#Removing variables that has one unique value or very much unproportinate in the distribution of counts.
lapply(diabetic_data,table)
novariability<-names(diabetic_data)[sapply(diabetic_data, function(x){any(data.frame(table(x)*100/length(x))$Freq >=99)})]
diabetic_data<-diabetic_data[,!(names(diabetic_data) %in% novariability)]


##MISSING VALUES	
na<-colSums(is.na(diabetic_data))
perc.na <- data.frame(colname=names(diabetic_data),cnt.na=na,percent.na=colSums(is.na(diabetic_data))/nrow(diabetic_data),stringsAsFactors = F)
row.names(perc.na)<-NULL
perc.na[order(-perc.na$cnt.na),]
na.cols<-perc.na$colname[perc.na$percent.na >=0.30] 
na.impute <- perc.na$colname[perc.na$percent.na  < 0.30 & perc.na$percent.na >0]


#Removing variable that have more than 30% NA
diabetic_data<-diabetic_data[,!(names(diabetic_data) %in% na.cols)]

na.impute
#Instead of imputing some random value ,since variable with NA are of categorical type , takin a seperate category Unknown
diabetic_data$race[is.na(diabetic_data$race)]<-"unknown"


#Since icd9 codes have more 700 categories(diag_1,diag_2,diag_3) it could be difficut to analyze.Binning them based on icd9_chapter
#into 19 distinct groups.
#https://en.wikipedia.org/wiki/List_of_ICD-9_codes

icd9_chapters[[18]][2]<-"V91"
class_19<-paste("Class",toupper(letters)[1:19] ,sep=" ")
icd_classificaiton<-data.frame(type=names(icd9_chapters),class=class_19,start=sapply(icd9_chapters,'[[',1),end=sapply(icd9_chapters,'[[',2),stringsAsFactors = F)
row.names(icd_classificaiton)<-NULL

icd_classificaiton

#ICD with respect to diabetes is 250.XX create a variable to identify diabetic diagnosis
diabetic_data$diag1_diabetes<-grepl("^250",diabetic_data$diag_1)
diabetic_data$diag2_diabetes<-grepl("^250",diabetic_data$diag_2)
diabetic_data$diag3_diabetes <-grepl("^250",diabetic_data$diag_3)
diabetic_data$diag_diabetes <-ifelse(diabetic_data$diag1_diabetes | diabetic_data$diag2_diabetes | diabetic_data$diag3_diabetes,T,F) 
diabetic_data$diag1_diabetes<-NULL
diabetic_data$diag2_diabetes<-NULL
diabetic_data$diag3_diabetes <-NULL


diabetic_data$diag1_class<-"unknown"
diabetic_data$diag2_class<-"unknown"
diabetic_data$diag3_class<-"unknown"
for(i in 1:nrow(icd_classificaiton)){
    
    print(i)
    icd<-as.character(icd_short_to_decimal(icd_expand_range(icd_classificaiton$start[i],icd_classificaiton$end[i])))
    diabetic_data$diag1_class<-ifelse(str_pad(diabetic_data$diag_1,3,pad="0") %in% icd,icd_classificaiton$class[i],diabetic_data$diag1_class)
    diabetic_data$diag2_class<-ifelse(str_pad(diabetic_data$diag_2,3,pad="0") %in% icd,icd_classificaiton$class[i],diabetic_data$diag2_class)
    diabetic_data$diag3_class<-ifelse(str_pad(diabetic_data$diag_3,3,pad="0") %in% icd,icd_classificaiton$class[i],diabetic_data$diag3_class)
    
}


#Validating the count of NA
sum(diabetic_data$diag1_class=="unknown")
sum(diabetic_data$diag2_class=="unknown")
sum(diabetic_data$diag3_class=="unknown")

sum(is.na(diabetic_data$diag_1))
sum(is.na(diabetic_data$diag_2))
sum(is.na(diabetic_data$diag_3))

#Remaoving the icd codes
diabetic_data$diag_1<-NULL
diabetic_data$diag_2<-NULL
diabetic_data$diag_3<-NULL

#Indentifying the circulatory diagnostics[Class G]
diabetic_data$diag1_circulatory<- ifelse(diabetic_data$diag1_class=="Class G",T,F)
diabetic_data$diag2_circulatory<- ifelse(diabetic_data$diag2_class=="Class G",T,F)
diabetic_data$diag3_circulatory <- ifelse(diabetic_data$diag3_class=="Class G",T,F)

#Having any of the circulatory diagnostics
diabetic_data$diag_circulatory <- ifelse(diabetic_data$diag1_circulatory | diabetic_data$diag2_circulatory | diabetic_data$diag3_circulatory ,T,F)

diabetic_data$diag1_circulatory<- NULL
diabetic_data$diag2_circulatory<- NULL
diabetic_data$diag3_circulatory <- NULL

#Derived variable
#Creating a Comorbidity
diabetic_data$diag_comorbidity<- ifelse(diabetic_data$diag_circulatory & diabetic_data$diag_diabetes ,3,
                                        ifelse(diabetic_data$diag_circulatory==F & diabetic_data$diag_diabetes==T ,1,
                                               ifelse(diabetic_data$diag_circulatory==T & diabetic_data$diag_diabetes==F,2,0)))


diabetic_data$diag_circulatory<-NULL
diabetic_data$diag_diabetes<-NULL


#Checking for NA
sum(is.na(diabetic_data))


str(diabetic_data)



###########################################Exploratary data analysis #########################################################
#Based on data dictionay converting admission_type_id  , discharge_disposition_id , admission_source_id to char
diabetic_data$admission_type_id<-as.character(diabetic_data$admission_type_id)
diabetic_data$discharge_disposition_id<- as.character(diabetic_data$discharge_disposition_id)
diabetic_data$admission_source_id <- as.character(diabetic_data$admission_source_id )
diabetic_data$diag_comorbidity<- as.character(diabetic_data$diag_comorbidity)


#Binning and outlier treatment after viewing plots Data expoloration
diabetic_data<-diabetic_data[-which(diabetic_data$gender=="Unknown/Invalid"),]
diabetic_data$metformin<-ifelse(diabetic_data$metformin=="No","No","Yes")
diabetic_data$repaglinide<-ifelse(diabetic_data$repaglinide=="No","No","Yes")
diabetic_data$glimepiride<-ifelse(diabetic_data$glimepiride=="No","No","Yes")
diabetic_data$glipizide<-ifelse(diabetic_data$glipizide=="No","No","Yes")
diabetic_data$glyburide<-ifelse(diabetic_data$glyburide=="No","No","Yes")
diabetic_data$pioglitazone<-ifelse(diabetic_data$pioglitazone=="No","No","Yes")
diabetic_data$rosiglitazone<-ifelse(diabetic_data$rosiglitazone=="No","No","Yes")
diabetic_data$admission_type_id<-ifelse( diabetic_data$admission_type_id %in% c("1","2","3"),diabetic_data$admission_type_id ,"0")
diabetic_data$discharge_disposition_id<-ifelse( diabetic_data$discharge_disposition_id %in% c("1","3","6"),diabetic_data$discharge_disposition_id ,"0")
diabetic_data$admission_source_id<-ifelse( diabetic_data$admission_source_id %in% c("1","7","17"),diabetic_data$admission_source_id ,"0")
diabetic_data$age<-ifelse(diabetic_data$age %in% c("[0-10)","[10-20)","[20-30)"),'[0-30)',
                          ifelse (diabetic_data$age %in% c("[30-40)","[40-50)","[50-60)"),'[30-60)',
                                  ifelse(diabetic_data$age %in% c("[60-70)","[70-80)","[80-90)"),"[60-90)","[90-100)")))
diabetic_data$number_outpatient<-ifelse(diabetic_data$number_outpatient>1,"Yes","No")
diabetic_data$number_emergency<-ifelse(diabetic_data$number_emergency>1,"Yes","No")
diabetic_data$number_inpatient<-ifelse(diabetic_data$number_inpatient>2,">2","<2")
diabetic_data$time_in_hospital<-ifelse(diabetic_data$time_in_hospital>=10,"High",
                                       ifelse(diabetic_data$time_in_hospital>=4,"Medium","Low"))
diabetic_data$num_procedures<-ifelse(diabetic_data$num_procedures>0,"Yes","No")
diabetic_data$num_medications<-ifelse(diabetic_data$num_medications>35,35,diabetic_data$num_medications)
diabetic_data$num_lab_procedures<-ifelse(diabetic_data$num_lab_procedures>96,96,diabetic_data$num_lab_procedures)
diabetic_data$number_diagnoses<-ifelse(diabetic_data$number_diagnoses>=9,"G8","8L")

catagorical_var <- names(diabetic_data)[which(sapply(diabetic_data, is.character))]
measure_var <- names(diabetic_data)[which(sapply(diabetic_data, is.numeric))]
catagorical_var<-catagorical_var[!catagorical_var=="readmitted"]


for(i in catagorical_var) {
    
    readline(prompt="press enter to view plots")
    print(i)
    plot1<-plot1<-ggplot(diabetic_data,aes(factor(diabetic_data[,i])))+geom_bar(fill="steelblue")+
        xlab(i) + ylab("Frequency") +geom_text(stat='count',aes(label=..count..),hjust=0)+coord_flip()
    print(diabetic_data  %>% group_by(diabetic_data[,i]) %>% 
              summarise(percent=100*n()/length(diabetic_data[,i])) %>% arrange(desc(percent)))
    
    plot2<-ggplot(diabetic_data,aes(factor(diabetic_data[,i]),fill=factor(diabetic_data[,"readmitted"])))+geom_bar(position = 'fill') +
        xlab(i) + ylab("Relative perccentage") +scale_y_continuous(label=percent) + labs(fill="readmitted")  + coord_flip()
    grid.arrange(plot1,plot2,nrow=2)
    
}



meas_freq_line<-function(df,measure)
{
    plot1<-ggplot(df,aes(df[,measure]))+
        geom_histogram(bins=nclass.Sturges(df[,measure]))+
        xlab(measure) + ylab("Frequency") 
    plot2<- ggplot(df,aes(y=df[,measure],x="")) + geom_boxplot(outlier.color = "red")  + ylab(measure) 
    plot3<- ggplot(df,aes(y=df[,measure],x=df[,"readmitted"])) + geom_boxplot(outlier.color = "red") + ylab(measure) + xlab("readmitted")
    grid.arrange(plot1,plot2,plot3)
}


#Frequency plot of measure variables
for(i in measure_var) {
    readline(prompt="press enter to view plots")
    print(i)
    print(quantile(diabetic_data[,i], probs = seq(0,1,0.01),na.rm=T))
    print(mean(diabetic_data[,i]))
    print(meas_freq_line(diabetic_data,i))
    cat("\nUpper Limit:",quantile(diabetic_data[,i],0.75)+1.5*IQR(diabetic_data[,i]),"\n")
    cat("Lower Limit:",quantile(diabetic_data[,i],0.25)-1.5*IQR(diabetic_data[,i]),"\n")
}




##############################Dummy variable creation for Logistic ############################


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

diabetic_data_l<-diabetic_data
diabetic_data_l$diag1_class<-NULL
diabetic_data_l$diag2_class<-NULL
diabetic_data_l$diag3_class<-NULL

catagorical_var <- names(diabetic_data_l)[which(sapply(diabetic_data_l, is.character))]
measure_var <- names(diabetic_data_l)[which(sapply(diabetic_data_l, is.numeric))]
for(i in c(catagorical_var,"readmitted"))
{
    print(i)
    diabetic_data_l<-cbind(diabetic_data_l[,-which(names(diabetic_data_l)==i)] ,dummy_conv(diabetic_data_l[,i],i))
}

#Removing the id columns for model building
patient_nbr<-diabetic_data$patient_nbr
encounter_id<-diabetic_data$encounter_id
diabetic_data$patient_nbr<- NULL
diabetic_data$encounter_id<- NULL

#Seperate df for random forrest
diabetic_data<-data.frame(unclass(diabetic_data))



#splitting of train and test dataset
set.seed(2017)
train_indices <- sample.split(diabetic_data$readmitted,SplitRatio=0.8)
train <- diabetic_data[train_indices,]
test <- diabetic_data[!(train_indices),]



train1 <- diabetic_data_l[train_indices,]
test1 <- diabetic_data_l[!(train_indices),]




##################################################### Random Forrest& Model Evaluation #########################################
set.seed(2017)
bestmtry <- tuneRF(train[,names(train)!="readmitted"],train$readmitted, stepFactor=1.5, improve=1e-5, ntree=2000)

?randomForest
rf_model <- randomForest(readmitted ~ ., data=train,mtry=3, proximity=FALSE,ntree=1800, do.trace=TRUE,na.action=na.omit)
rfpredicted<-predict(rf_model,test,type="prob")[,2]
rpredicted <-factor(ifelse(rfpredicted>=0.7,"YES","NO"))
confusionMatrix(rpredicted,test$readmitted,positive="YES")
#Accuracy : 0.6087
#Sensitivity : 0.6118          
#Specificity : 0.6060

levels(rpredicted)<-c(0,1)
rpredicted<-levels(rpredicted)[rpredicted]
act_readmit<-test$readmitted
levels(act_readmit)<-c(0,1)
act_readmit<-levels(act_readmit)[act_readmit]
auc(rpredicted,act_readmit)
#0.6067


var.imp <- data.frame(importance(rf_model,
                                 type=2))
var.imp$variables<-row.names(var.imp)
row.names(var.imp)<-NULL
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

############################################# logistic Regression & Model evaluation  ################################################

logistic_1 <- glm(readmitted~.,data=train1,family="binomial")
summary(logistic_1)
vif(logistic_1)
#Warning message:
#glm.fit: fitted probabilities numerically 0 or 1 occurred 

logistic_2  <- stepAIC(logistic_1,direction="both")
summary(logistic_2)
vif(logistic_2)

#Removed variable based vif and lower p values

logistic_3 <-glm(formula = readmitted ~ num_medications + race.Asian + race.Hispanic + 
                     race.Other + race.unknown + gender + `age.[30-60)` + admission_type_id.2  + 
                     discharge_disposition_id.1 + discharge_disposition_id.3 + 
                     discharge_disposition_id.6  + admission_source_id.17 + 
                     admission_source_id.7 + time_in_hospital.Low + num_procedures + 
                     number_outpatient + number_emergency + number_inpatient + 
                     number_diagnoses   + 
                     `a1cresult.>8` + a1cresult.None + metformin + insulin.No   + diabetesmed + diag_comorbidity.1 + diag_comorbidity.2 + 
                     diag_comorbidity.3, family = "binomial", data = train1)

summary(logistic_3)
vif(logistic_3)

l_predic_prob<-predict(logistic_3,test1,type="response")
l_predic <- ifelse(l_predic_prob>=0.446 ,1,0)
confusionMatrix(l_predic,test1$readmitted,,positive="1")
auc(l_predic,test1$readmitted)
#0.59


##################################################### Risk stratification ###################################################

#The RandomForres Model is slightly better than Logistic regression

diabetic_data_risk<-predict(rf_model ,diabetic_data,type="prob")[,2]

diabetic_data$risk_strat<-ifelse(diabetic_data_risk>=0.7,"High risk",
                                 ifelse(diabetic_data_risk>=0.3,"Medium risk","Low risk"))
diabetic_data$risk_strat<-factor(diabetic_data$risk_strat,levels = c("Low risk","Medium risk","High risk"))
barplot(table(diabetic_data$readmitted))
barplot(table(diabetic_data$risk_strat))


diabetic_data$patient_nbr<-patient_nbr
diabetic_data$encounter_id<-encounter_id
    


#Filtering the population based on unique patient_nbr and the maximum encounter_id  
temp<-diabetic_data %>% group_by(patient_nbr) %>% summarise(encounter_id=max(encounter_id))
unique_patients<-merge(diabetic_data,temp,by=c("patient_nbr","encounter_id"))
length(unique(patient_nbr))
table(unique_patients$risk_strat)

*100/length(unique_patients$risk_strat)
barplot(table(unique_patients$risk_strat)*100/length(unique_patients$risk_strat))


barplot(table(unique_patients$readmitted))
