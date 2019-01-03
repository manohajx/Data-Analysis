########################################## Health care capstone Project ##########################################
# CMS Rating Project

# Team Members:
# SAI PAVAN (LEADER)
# JOHN PRASHANTH GNANIAH M 
# KUNAL DESHPANDE
# ROSHINI THYAGARAJ


options("scipen"=100, "digits"=4)
rm(list=ls())

set.seed(2018)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(caTools)
library(randomForest)
library(caret)
library(kernlab)
library(glmnet)
library(mice)
library(nnet)
library(DMwR)
library(class)
library(e1071)

############################################# FUnctions ########################################################################
#Gives the NA % in a df
df.na <- function(df){
    na<-colSums(is.na(df))
    perc.na <- data.frame(colname=names(df),cnt.na=na,percent.na=colSums(is.na(df))*100/nrow(df),stringsAsFactors = F)
    row.names(perc.na)<-NULL
    print(perc.na[order(-perc.na$cnt.na),])
}

#Gives the unique count of varialbe in a df
df.unique.cnt <- function(df){
    
    data.frame(lapply(df,function(vec){length(unique(vec))}))
}


#Frequency plot for dimensions
df.fac.freq.plot <- function(df,vec){
    
    for(i in vec) {
        
        readline(prompt="press enter to view plots")
        print(i)
        
        
        plot<-ggplot(df,aes(factor(df[,i])))+geom_bar(fill="steelblue")+
            xlab(i) + ylab("Frequency") +geom_text(stat='count',aes(label=..count..),hjust=0)+coord_flip()
        
        print(df  %>% group_by(df[,i]) %>% 
                  summarise(percent=100*n()/length(df[,i])) %>% arrange(desc(percent)))
        print(plot)
        
    }
}

#Proportional plot for Dimensions
df.fac.stack.plot <- function(df,vec,catag){
    
    for(i in vec) {
        
        readline(prompt="press enter to view plots")
        print(i)
        
        plot1<- ggplot(df,aes(factor(df[,i]),fill=factor(df[,catag])))+geom_bar(position = 'fill') +
            xlab(i) + ylab("Relative perccentage") +scale_y_continuous(label=percent) + labs(fill=catag)  + coord_flip()
        
        
        plot2<- ggplot(df,aes(factor(df[,i]),fill=factor(df[,catag])))+geom_bar() +xlab(i) + ylab("Frequency") +
            geom_text(stat='count',aes(label=..count..),position=position_stack(vjust = 0.5))+labs(fill=catag)  + coord_flip()
        
        
        print(grid.arrange(plot1,plot2,nrow=2))
        
    }
}


#Box plot ,histogram for FActs
df.meas.plots <-function(df,vec,catag)
{
    for(i in vec) 
    {
        readline(prompt="press enter to view plots")
        print(i)
        print(quantile(df[,i], probs = seq(0,1,0.01),na.rm=T))
        print(mean(df[,i],na.rm = T))
        cat("\nUpper Limit:",quantile(df[,i],0.75,na.rm = T)+1.5*IQR(df[,i],na.rm = T),"\n")
        cat("Lower Limit:",quantile(df[,i],0.25,na.rm = T)-1.5*IQR(df[,i],na.rm = T),"\n")
        
        plot1<-ggplot(df,aes(df[,i]))+
            geom_histogram(bins=nclass.Sturges(df[,i]),na.rm = T)+
            xlab(i) + ylab("Frequency") 
        plot2<- ggplot(df,aes(y=df[,i],x="")) + geom_boxplot(outlier.color = "red",na.rm = T)  + ylab(i) 
        plot3<- ggplot(df,aes(y=df[,i],x=factor(df[,catag]))) + geom_boxplot(outlier.color = "red",na.rm = T) + ylab(i) + xlab(catag)
        grid.arrange(plot1,plot2,plot3)
    }
}


#Function that imputes for NA
#Groups the measures into "Same as the National average" ,"Below the National average" ,"Above the National average" , NA and take mean from Shospital general information table
#Impute the mean based on category


impute.na<- function(score,national.compare){
    
    df<-data.frame(score ,national.compare)
    df<-df %>% group_by(national.compare) %>% summarise(mean=mean(score,na.rm=T))
    print(df)
    ifelse(is.na(score) & is.na(national.compare), NA ,
           ifelse(is.na(score) & national.compare=="Same as the National average",df$mean[which(df$national.compare =='Same as the National average')],
                  ifelse(is.na(score)& national.compare =="Below the National average",df$mean[which(df$national.compare =='Below the National average')],
                         ifelse(is.na(score) & national.compare =="Above the National average", df$mean[which(df$national.compare =='Above the National average')],
                                score)))) 
    
    
    
}


#Creates Dummy variables
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

##########################################Data Preparation ######################################################################

#Creating variable for the 64 measures considered for the analyiss
mortality_7_measures <- c("MORT_30_AMI","MORT_30_CABG","MORT_30_COPD","MORT_30_HF","MORT_30_PN","MORT_30_STK","PSI_4_SURG_COMP")
safetyofcare_8_measure <- c("HAI_1_SIR","HAI_2_SIR","HAI_3_SIR","HAI_4_SIR","HAI_5_SIR","HAI_6_SIR",
                            "COMP_HIP_KNEE","PSI_90_SAFETY")
readmissions_8_measure <- c("READM_30_AMI","READM_30_CABG","READM_30_COPD","READM_30_HF","READM_30_HIP_KNEE",
                            "READM_30_HOSP_WIDE","READM_30_PN","READM_30_STK" )
patientexp_11_measure <- c("H_RECMND_STAR_RATING","H_QUIET_STAR_RATING","H_HSP_RATING_STAR_RATING","H_COMP_7_STAR_RATING",
                           "H_COMP_6_STAR_RATING","H_COMP_5_STAR_RATING","H_COMP_4_STAR_RATING","H_COMP_3_STAR_RATING",
                           "H_COMP_2_STAR_RATING","H_COMP_1_STAR_RATING","H_CLEAN_STAR_RATING")
effectivenessofcare_18_measure <- c("CAC_3","IMM_2","IMM_3_OP_27_FAC_ADHPCT","OP_4","OP_22","OP_23","OP_29","OP_30","PC_01",
                                    "STK_1","STK_4","STK_6","STK_8","VTE_1","VTE_2","VTE_3","VTE_5","VTE_6")

timelinessofcare_7_measure <- c("ED_1b","ED_2b","OP_3b","OP_5","OP_18b","OP_20","OP_21")
imagingeff_5_measure <- c("OP_10","OP_11","OP_13","OP_14","OP_8")
measures_64 <-c(mortality_7_measures,readmissions_8_measure,safetyofcare_8_measure,
                patientexp_11_measure,effectivenessofcare_18_measure,timelinessofcare_7_measure,imagingeff_5_measure)


###Importing the General hospital information data

readmission.hospital.general.info <- read.csv("Hospital General Information.csv", na.strings = c("NA","#DIV/0!", "","NaN","?","Not Available"),stringsAsFactors = F ,check.names = T)
names(readmission.hospital.general.info) <- tolower(names(readmission.hospital.general.info))
str(readmission.hospital.general.info)
#Check for duplicated records
any(duplicated(readmission.hospital.general.info))
#Analyzing the NA values
df.na(readmission.hospital.general.info)
#1170 hospitals have no oveall rating available due to multiple reasons
na.rating.data<-readmission.hospital.general.info[is.na(readmission.hospital.general.info$hospital.overall.rating),]
unique(na.rating.data$hospital.overall.rating.footnote)
View(na.rating.data %>% group_by(hospital.overall.rating.footnote) %>% summarise(cnt=n()))
#Provider_id of the 1170 hospitals for filtering 
na.rating.provider.id <- na.rating.data$provider.id
#Filtered General hospital information data
hospital_info <- readmission.hospital.general.info[!(readmission.hospital.general.info$provider.id %in% na.rating.provider.id ),]
#Unique count for each variable
View(df.unique.cnt(hospital_info))
lapply(hospital_info,unique)
ls.var1<-c("provider.id","state","hospital.type","hospital.ownership","emergency.services","meets.criteria.for.meaningful.use.of.ehrs",
           "efficient.use.of.medical.imaging.national.comparison" ,"timeliness.of.care.national.comparison","effectiveness.of.care.national.comparison"
           ,"patient.experience.national.comparison","safety.of.care.national.comparison","mortality.national.comparison","readmission.national.comparison","hospital.overall.rating")

hospital_info <- hospital_info[ , names(hospital_info) %in% ls.var1]
df.na(hospital_info)


###Importing Hospital-level results for 30-day mortality and readmissions measures
readmission.deaths.hospitals <- read.csv("Readmissions and Deaths - Hospital.csv", na.strings = c("NA","#DIV/0!", "","NaN","?","Not Available"),stringsAsFactors = F ,check.names = T)
names(readmission.deaths.hospitals) <- tolower(names(readmission.deaths.hospitals))
#Filtering the 1170 hospitals 
readmission.deaths.hospitals <- readmission.deaths.hospitals[!(readmission.deaths.hospitals$provider.id %in% na.rating.provider.id ),]
str(readmission.deaths.hospitals)
#Checking for duplicates
any(duplicated(readmission.deaths.hospitals))
#Reason's for NA  score
unique(readmission.deaths.hospitals$footnote)
na.readmin<-readmission.deaths.hospitals[is.na(readmission.deaths.hospitals$score),]
length(unique(na.readmin$provider.id))
View(na.readmin)
#Selecting only score for analysis
readmission.deaths.hospitals <- select(readmission.deaths.hospitals,c("provider.id","measure.id","score"))
#Considering only the 64 measures
readmission.deaths.hospitals <- readmission.deaths.hospitals[readmission.deaths.hospitals$measure.id %in% measures_64,]
#Converting long to wide format
readmission.deaths.hospitals_wide<-spread(readmission.deaths.hospitals,key=measure.id ,value=score)
readmission.deaths.hospitals_wide[,-1]<-scale(readmission.deaths.hospitals_wide[,-1])
#miceOutput <- mice(readmission.deaths.hospitals_wide,method="rf")  # perform knn imputation.
#readmission.deaths.hospitals_wide<-mice::complete(miceOutput)
df.na(readmission.deaths.hospitals_wide)


###Importing Hospital-level results for surgical complications measures
complications.hospital <- read.csv("Complications - Hospital.csv", na.strings = c("NA","#DIV/0!", "","NaN","?","Not Available"),stringsAsFactors = F ,check.names = T)
names(complications.hospital) <- tolower(names(complications.hospital) )
#Filtering the 1170 hospitals 
complications.hospital <- complications.hospital[!(complications.hospital$provider.id %in% na.rating.provider.id ),]
str(complications.hospital)
#Checking for duplicates
any(duplicated(complications.hospital))
#Reason's for NA  score
unique(complications.hospital$footnote)
#Selecting only score for analysis
complications.hospital <- select(complications.hospital,c("provider.id","measure.id","score"))
#Considering only the 64 measures
complications.hospital <- complications.hospital[complications.hospital$measure.id %in% measures_64,]
#Converting long to wide format
complications.hospital_wide<-spread(complications.hospital,key=measure.id ,value=score)
complications.hospital_wide[,-1]<-scale(complications.hospital_wide[,-1])
#miceOutput <- mice(complications.hospital_wide,method="rf")  # perform knn imputation.
#complications.hospital_wide<-mice::complete(miceOutput)


###Importing Hospital-level results for healthcare-associated infections measures
healthcare.associated.infections.hospital <- read.csv("Healthcare Associated Infections - Hospital.csv", na.strings = c("NA","#DIV/0!", "","NaN","?","Not Available"),stringsAsFactors = F ,check.names = T)
names(healthcare.associated.infections.hospital) <- tolower(names(healthcare.associated.infections.hospital))
#Filtering the 1170 hospitals 
healthcare.associated.infections.hospital <- healthcare.associated.infections.hospital[!(healthcare.associated.infections.hospital$provider.id %in% na.rating.provider.id ),]
str(healthcare.associated.infections.hospital)
#Checking for duplicates
any(duplicated(healthcare.associated.infections.hospital))
#Reason's for NA  score
unique(healthcare.associated.infections.hospital$footnote)
#Selecting only score for analysis
healthcare.associated.infections.hospital <- select(healthcare.associated.infections.hospital,c("provider.id","measure.id","score"))
#Considering only the 64 measures
healthcare.associated.infections.hospital <- healthcare.associated.infections.hospital[healthcare.associated.infections.hospital$measure.id %in% measures_64,]
#Converting long to wide format
healthcare.associated.infections.hospital_wide<-spread(healthcare.associated.infections.hospital,key=measure.id ,value=score)
healthcare.associated.infections.hospital_wide[,-1]<-scale(healthcare.associated.infections.hospital_wide[,-1])
#miceOutput <- mice(healthcare.associated.infections.hospital_wide,method="rf")  # perform knn imputation.
#healthcare.associated.infections.hospital_wide<-mice::complete(miceOutput)


###Importing Hospital-level results for the Hospital Consumer Assessment of Healthcare Providers and Systems
hcahps.hospital <- read.csv("HCAHPS - Hospital.csv", na.strings = c("NA","#DIV/0!", "","NaN","?","Not Available"),stringsAsFactors = F ,check.names = T)
names(hcahps.hospital) <- tolower(names(hcahps.hospital) )
#Filtering the 1170 hospitals 
hcahps.hospital <- hcahps.hospital[!(hcahps.hospital$provider.id %in% na.rating.provider.id ),]
str(hcahps.hospital)
#Checking for duplicates
any(duplicated(hcahps.hospital))
#Reason's for NA  score
unique(hcahps.hospital$patient.survey.star.rating.footnote)
#Selecting only score for analysis
hcahps.hospital <- select(hcahps.hospital,c("provider.id","hcahps.measure.id","patient.survey.star.rating"))
#Considering only the 64 measures
hcahps.hospital <- hcahps.hospital[hcahps.hospital$hcahps.measure.id %in% measures_64,]
#Converting long to wide format
hcahps.hospital_wide<-spread(hcahps.hospital,key=hcahps.measure.id ,value=patient.survey.star.rating)
hcahps.hospital_wide<-data.frame(provider.id=hcahps.hospital_wide$provider.id,lapply(hcahps.hospital_wide[,-1],as.factor))
#miceOutput <- mice(hcahps.hospital_wide,method="rf")  # perform knn imputation.
#hcahps.hospital_wide<-mice::complete(miceOutput)


###Importing Hospital-level results for Process of Care measures
timely.effective.care.hospital <- read.csv("Timely and Effective Care - Hospital.csv", na.strings = c("NA","#DIV/0!", "","NaN","?","Not Available"),stringsAsFactors = F ,check.names = T)
names(timely.effective.care.hospital) <- tolower(names(timely.effective.care.hospital) )
#Filtering the 1170 hospitals 
timely.effective.care.hospital <- timely.effective.care.hospital[!(timely.effective.care.hospital$provider.id %in% na.rating.provider.id ),]
str(timely.effective.care.hospital)
#CHecking for dupliates
any(duplicated(timely.effective.care.hospital))
#Reason's for NA  score
unique(timely.effective.care.hospital$footnote)
#Selecting only score for analysis
timely.effective.care.hospital <- select(timely.effective.care.hospital, c("provider.id","measure.id","score"))
#Considering only the 64 measures
timely.effective.care.hospital <- timely.effective.care.hospital[timely.effective.care.hospital$measure.id %in% measures_64,]
#Converting long to wide format
timely.effective.care.hospital_wide<-spread(timely.effective.care.hospital,key=measure.id ,value=score)
str(timely.effective.care.hospital_wide)
timely.effective.care.hospital_wide<-data.frame(provider.id=timely.effective.care.hospital_wide[,1],lapply(timely.effective.care.hospital_wide[,2:ncol(timely.effective.care.hospital_wide)],as.numeric))
timely.effective.care.hospital_wide[,-1]<-scale(timely.effective.care.hospital_wide[,-1])
#miceOutput <- mice(timely.effective.care.hospital_wide,method="rf")  # perform knn imputation.
#timely.effective.care.hospital_wide<-mice::complete(miceOutput)


###Importing Hospital-level results for measures of the use of medical imaging
imaging.efficiency.hospital <- read.csv("Outpatient Imaging Efficiency - Hospital.csv", na.strings = c("NA","#DIV/0!", "","NaN","?","Not Available"),stringsAsFactors = F ,check.names = T)
names(imaging.efficiency.hospital) <- tolower(names(imaging.efficiency.hospital) )
#Filtering the 1170 hospitals 
imaging.efficiency.hospital <- imaging.efficiency.hospital[!(imaging.efficiency.hospital$provider.id %in% na.rating.provider.id ),]
str(imaging.efficiency.hospital)
#Checking for duplicates
any(duplicated(imaging.efficiency.hospital))
#Reason's for NA  score
unique(imaging.efficiency.hospital$footnote)
#Selecting only score for analysis
imaging.efficiency.hospital <- select(imaging.efficiency.hospital,c("provider.id","measure.id","score"))
#Considering only the 64 measures
imaging.efficiency.hospital <- imaging.efficiency.hospital[imaging.efficiency.hospital$measure.id %in% measures_64,]
#Converting long to wide format
imaging.efficiency.hospital_wide<-spread(imaging.efficiency.hospital,key=measure.id ,value=score)
str(imaging.efficiency.hospital_wide)
imaging.efficiency.hospital_wide[,-1]<-scale(imaging.efficiency.hospital_wide[,-1])
#miceOutput <- mice(imaging.efficiency.hospital_wide,method="rf")  # perform knn imputation.
#imaging.efficiency.hospital_wide<-mice::complete(miceOutput)



## Joining all the 7 classification and hospital general information into single df
master <- inner_join(inner_join(inner_join(inner_join(inner_join(inner_join(readmission.deaths.hospitals_wide,imaging.efficiency.hospital_wide,by="provider.id"),
                                                                 complications.hospital_wide, by="provider.id"),hcahps.hospital_wide,by="provider.id"),
                                           timely.effective.care.hospital_wide,by="provider.id"),healthcare.associated.infections.hospital_wide,by="provider.id"),
                     hospital_info , by="provider.id")
df.na(master)
provider_id <- master$provider.id
master$provider.id<- NULL
str(master)

#Based on the national.comparison for each of 7 classification the mean of 3 classificaiton is considered
#Mortalitiy NA imputation
mortality<-master[,names(master) %in% c("mortality.national.comparison",mortality_7_measures)]
unique(mortality$mortality.national.comparison)
mortality<-data.frame(lapply(mortality[,names(mortality) %in% mortality_7_measures],impute.na,mortality$mortality.national.comparison))

#safetyofcare NA imputation
safetyofcare <-master[,names(master) %in% c(safetyofcare_8_measure,"safety.of.care.national.comparison")]
unique(mortality$mortality.national.comparison)
safetyofcare<-data.frame(lapply(safetyofcare[,names(safetyofcare) %in% safetyofcare_8_measure],impute.na,national.compare=safetyofcare$safety.of.care.national.comparison))

#readmissions NA imputation
readmissions <-master[,names(master) %in% c(readmissions_8_measure,"readmission.national.comparison")]
unique(readmissions$readmission.national.comparison)
readmissions<-data.frame(lapply(readmissions[,names(readmissions) %in% readmissions_8_measure],impute.na,readmissions$readmission.national.comparison))

#patientexp NA imputation
patientexp <-master[,names(master) %in% c(patientexp_11_measure,"patient.experience.national.comparison")]
unique(patientexp$patient.experience.national.comparison)
patientexp<-data.frame(lapply(lapply(patientexp[,names(patientexp) %in% patientexp_11_measure],impute.na,national.compare=patientexp$patient.experience.national.comparison),round))

#effectivenessofcare NA imputation
effectivenessofcare <-master[,names(master) %in% c(effectivenessofcare_18_measure,"effectiveness.of.care.national.comparison")]
unique(effectivenessofcare$effectiveness.of.care.national.comparison)
effectivenessofcare<-data.frame(lapply(effectivenessofcare[,names(effectivenessofcare) %in% effectivenessofcare_18_measure],impute.na,national.compare=effectivenessofcare$effectiveness.of.care.national.comparison))

#timelinessofcare NA imputation
timelinessofcare <-master[,names(master) %in% c(timelinessofcare_7_measure,"timeliness.of.care.national.comparison")]
unique(timelinessofcare$timeliness.of.care.national.comparison)
timelinessofcare<-data.frame(lapply(timelinessofcare[,names(timelinessofcare) %in% timelinessofcare_7_measure],impute.na,national.compare=timelinessofcare$timeliness.of.care.national.comparison))

#imagingeff NA imputation
imagingeff <-master[,names(master) %in% c(imagingeff_5_measure,"efficient.use.of.medical.imaging.national.comparison")]
unique(imagingeff$efficient.use.of.medical.imaging.national.comparison)
imagingeff<-data.frame(lapply(imagingeff[,names(imagingeff) %in% imagingeff_5_measure],impute.na,national.compare=imagingeff$efficient.use.of.medical.imaging.national.comparison))

#List of catagory variables
var_ls_1<- c("hospital.type","hospital.ownership","emergency.services","meets.criteria.for.meaningful.use.of.ehrs","hospital.overall.rating")


#Combine the 7 classification into master
master<-data.frame(master[,names(master) %in% var_ls_1 ],mortality,safetyofcare,readmissions,patientexp,effectivenessofcare,timelinessofcare,imagingeff)
str(master)

#Sets NA to N
master$meets.criteria.for.meaningful.use.of.ehrs<-ifelse(is.na(master$meets.criteria.for.meaningful.use.of.ehrs),'N', master$meets.criteria.for.meaningful.use.of.ehrs)

#List of catagory variables
var_ls_1<- c("hospital.type","hospital.ownership","emergency.services","meets.criteria.for.meaningful.use.of.ehrs","hospital.overall.rating")
#List of Catagorical variables converted to factor and dummy
cat_var<-c(var_ls_1,patientexp_11_measure)
meas_var<- measures_64[!(measures_64 %in% cat_var)]
master[,names(master) %in% cat_var] <- data.frame(lapply(master[,names(master) %in% cat_var],as.factor))

#knn imputation for NA
master_impute<-master[,names(master) %in% measures_64]
master_impute_knn<- knnImputation(master_impute,k=15)
master<- data.frame(master_impute_knn,master[,names(master) %in% var_ls_1])
#str(master_data)
df.na(master)


####################################################### EDA #####################################################################
#Frequency plot for catagorical variables
df.fac.freq.plot(master,cat_var)
cat_var_1<-cat_var[cat_var!="hospital.overall.rating"]
df.fac.stack.plot(master,cat_var_1,"hospital.overall.rating")


#Boxplots for measure
df.meas.plots(master,meas_var,"hospital.overall.rating")

################################################################################################################################

###Outler treatment for measure variables
#Outlier Capping 
master_measure<- master[,meas_var]
for(i in meas_var)
{
    print(i)
    uplimit <- quantile(master_measure[,i],0.75)+1.5*IQR(master_measure[,i])
    lolimit <- quantile(master_measure[,i],0.25)-1.5*IQR(master_measure[,i])
    master_measure[,i]<-ifelse(master_measure[,i]>uplimit ,uplimit,master_measure[,i])  
    master_measure[,i]<-ifelse(master_measure[,i]<lolimit ,lolimit,master_measure[,i])
}
master<- data.frame(master[,!(names(master) %in% meas_var)],master_measure)
#df.meas.plots(master,meas_var,"hospital.overall.rating")

#Onlye one value present
unique(master$MORT_30_CABG )
master$MORT_30_CABG <- NULL

master_data<-data.frame(master[,names(master) %in%  measures_64],hospital.overall.rating=as.factor(master$hospital.overall.rating))

############################################################# Model Building ##################################################

#splitting of train and test dataset
set.seed(2018)
train_indices <- sample.split(master_data$hospital.overall.rating,SplitRatio= 0.8)
train <- master_data[train_indices,]
test <- master_data[!(train_indices),]

########################################################### Random Forrest #################################################

set.seed(2018)
rf_model <- randomForest(hospital.overall.rating ~ ., data=train, proximity=FALSE, do.trace=TRUE)
rfpredicted<-predict(rf_model,test)
confusionMatrix(rfpredicted,test$hospital.overall.rating)
#Accuracy 0.768

#Hyper-parameter tuning
tree<-c(999,1499,1999)
best.m<-NULL
for(i in tree)
{
    print(i)
    set.seed(2018)
    bestmtry <- tuneRF(train[,!(names(train) %in% 'hospital.overall.rating')], train$hospital.overall.rating, stepFactor=1.5, improve=0.01, ntreeTry = i, mtryStart = 2)
    print(bestmtry)
    best.m <- rbind(best.m,c(i,bestmtry[bestmtry[, 2] == min(bestmtry[, 2]), 1]))
    
}

best.m

#Based on the tuning the hyperparameter ntree=1499 and mtry = 42
set.seed(2018)
rf_model <- randomForest(hospital.overall.rating ~ ., data=train, proximity=FALSE, do.trace=TRUE,mtry=42,ntree=1499)
rfpredicted<-predict(rf_model,test)
confusionMatrix(rfpredicted,test$hospital.overall.rating)
#Accuracy 0.807

'
rfpredicted<-predict(rf_model,test,type="prob")
rfpredicted_2<-predict(rf_model,test,type="response")
range(rfpredicted)#rf predicted is 1.24 to 4.89

rfpredicted<-cut(rfpredicted,c(0,1,2,3,4,5))
levels(rfpredicted)<-c(1:5)
confusionMatrix(rfpredicted,test$hospital.overall.rating)
unique(test$hospital.overall.rating)
#VAriable importance
as.factor(rfpredicted,levels=c(1,2,3,4,5))
table(factor(rfpredicted, levels=min(test$hospital.overall.rating):max(test$hospital.overall.rating)), factor(test$hospital.overall.rating, levels=min(test$hospital.overall.rating):max(test$hospital.overall.rating)))
'


varImpPlot(rf_model)
var.imp <- data.frame(importance(rf_model,
                                 type=2))
var.imp$variables<-row.names(var.imp)
row.names(var.imp)<-NULL
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]


###################################################### SVM ##################################################################

##Creating dummy variables
master_catagorical<-master_data[,names(master_data) %in% patientexp_11_measure]

#Dummy variable creation
for(i in names(master_catagorical))
{
    print(i)
    master_catagorical<-cbind(master_catagorical[,-which(names(master_catagorical)==i)] ,dummy_conv(master_catagorical[,i],i))
}


master_data_dummy <- data.frame(master_data[,!(names(master_data) %in% patientexp_11_measure)],master_catagorical)


train_1 <- master_data_dummy[train_indices,]
test_1 <- master_data_dummy[!(train_indices),]



###Using Linear Kernel
Model_linear <- ksvm(hospital.overall.rating~ ., data = train_1, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test_1)
#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test_1$hospital.overall.rating)
#Linear kernel gives an accuracy of 86.8%

###Using Polynomial Kernel

Model_poly <- ksvm(hospital.overall.rating~ ., data = train_1, scale = FALSE, kernel = "polydot")
Eval_poly<- predict(Model_poly, test_1)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_poly,test$hospital.overall.rating)
#Polynomial kernel gives an accuracy of 86.8%

###Using RBF Kernel
Model_RBF <- ksvm(hospital.overall.rating~ ., data = train_1, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test_1)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_RBF,test$hospital.overall.rating)
#RBF kernel gives an accuracy of 84.5%


############################################Multinominal REGRESSION #########################################################

muti_model_1 <- multinom(hospital.overall.rating~., data = train_1)
multi_pred<-predict(muti_model_1,test_1)
confusionMatrix(multi_pred,test_1$hospital.overall.rating)
#Accuracy 88.2%


################################### Naive Bayes #########################################################################

bayes_model <- naiveBayes(hospital.overall.rating~., data = train_1)
bayes_pred <- predict(bayes_model,test_1)
confusionMatrix(bayes_pred,test_1$hospital.overall.rating)
#Accuracy 18.7%


############################################# KNN ##############################################################################

prc_test_pred <- knn(train = train_1[,names(train_1)!="hospital.overall.rating"], test = test_1[,names(test_1)!="hospital.overall.rating"],cl = train_1$hospital.overall.rating, k=19)
confusionMatrix(prc_test_pred,test$hospital.overall.rating)
#Accuracy 68.7%

######################################################################################################################################

################################################################Unsupervised Method############################################

master$provider.id <- provider_id

#Factor Analysis
#Getting group wise data
mortality_wide<-data.frame(provider.id=master$provider.id)
mortality_wide<-cbind(mortality_wide,master[,(colnames(master)%in%mortality_7_measures)])
colnames(mortality_wide)

safety_of_care_wide<-data.frame(provider.id=master$provider.id)
safety_of_care_wide<-cbind(safety_of_care_wide,master[,(colnames(master)%in%safetyofcare_8_measure)])
colnames(safety_of_care_wide)

readmission_wide<-data.frame(provider.id=master$provider.id)
readmission_wide<-cbind(readmission_wide,master[,(colnames(master)%in%readmissions_8_measure )])
colnames(readmission_wide)


patientexp_wide<-data.frame(provider.id=master$provider.id)
patientexp_wide<-cbind(patientexp_wide,master[,(colnames(master)%in%patientexp_11_measure)])
colnames(patientexp_wide)
patientexp_wide<- data.frame(lapply(patientexp_wide,as.integer))


imaging.efficiency.hospital_wide<-data.frame(provider.id=master$provider.id)
imaging.efficiency.hospital_wide<-cbind(imaging.efficiency.hospital_wide,master[,(colnames(master)%in% imagingeff_5_measure)])
colnames(imaging.efficiency.hospital_wide)



Effectiveness_Wide<-data.frame(provider.id=master$provider.id)
Effectiveness_Wide<-cbind(Effectiveness_Wide,master[,(colnames(master)%in% effectivenessofcare_18_measure)])
colnames(Effectiveness_Wide)


Timeliness_Wide<-data.frame(provider.id=master$provider.id)
Timeliness_Wide<-cbind(Timeliness_Wide,master[,(colnames(master)%in% timelinessofcare_7_measure)])
colnames(Timeliness_Wide)




factanal(mortality_wide[,-1],1,rotation="varimax")#0.221
factanal(mortality_wide[,-1],2,rotation="varimax")#0.288
factanal(mortality_wide[,-1],3,rotation="varimax")#0.356
factanal(mortality_wide[,-1],4,rotation="varimax")#4 factors are too many
#We choose 3 factors
mortality_wide_factor<-factanal(mortality_wide[,-1],3,rotation="varimax",scores="regression")
mortality_wide_loadings<-mortality_wide_factor$loadings
mortality_wide_scores<-mortality_wide_factor$scores
mortality_scores_df<-as.data.frame(mortality_wide_scores)
#The measures MORT_30_HF,MORT_30_PN and MORT_30_AMI are key variables for mortatlity group

factanal(safety_of_care_wide[,-1],1,rotation="varimax")#Explains 12.3% of variation
factanal(safety_of_care_wide[,-1],2,rotation="varimax")#24.7% of variation
factanal(safety_of_care_wide[,-1],3,rotation="varimax")#36.3% of variation
factanal(safety_of_care_wide[,-1],4,rotation="varimax")#33.2% of variation

#We choose 3 factors as the variation explained decreases when moving to 4 factos
safety_of_care_factor<-factanal(safety_of_care_wide[,-1],3,rotation="varimax",scores="regression")
safety_of_care_loadings<-safety_of_care_factor$loadings
safety_of_care_scores<-safety_of_care_factor$scores
safety_of_care_scores_df<-as.data.frame(safety_of_care_scores)
#In safety of care HAI_6_SIR is the most important variable from factor analysis

factanal(readmission_wide[,-1],1,rotation="varimax")#Explains 31.1% of variation
factanal(readmission_wide[,-1],2,rotation="varimax")#35.0% of variation
factanal(readmission_wide[,-1],3,rotation="varimax")#46.9% of variation
factanal(readmission_wide[,-1],4,rotation="varimax")#54.5% of variation
factanal(readmission_wide[,-1],5,rotation="varimax")#5 factors are too many for 8 variables
#We choose 4 factors
readmission_factor<-factanal(readmission_wide[,-1],4,rotation="varimax",scores="regression")
readmission_loadings<-readmission_factor$loadings
readmission_scores<-readmission_factor$scores
readmission_scores_df<-as.data.frame(readmission_scores)
#Readmission READM_30_HOSP_WIDE  has the highest loading for 0.846 for Factor 1, which is in line with observation from Supervised learning

#Patient Experience Factor Analysis
factanal(patientexp_wide[,-1],1,rotation="varimax")#Explains 59.1% of variation
factanal(patientexp_wide[,-1],2,rotation="varimax")#64.2% of variation
factanal(patientexp_wide[,-1],3,rotation="varimax")#68.8% of variation
factanal(patientexp_wide[,-1],4,rotation="varimax")#68.2% of variation
factanal(patientexp_wide[,-1],5,rotation="varimax")#73.7% of variation
factanal(patientexp_wide[,-1],6,rotation="varimax")#74.8% of variation
factanal(patientexp_wide[,-1],7,rotation="varimax")#7 factors are too many, we choose 6 factors

patientexp_factor<-factanal(patientexp_wide[,-1],6,rotation="varimax",scores="regression")
patientexp_loadings<-patientexp_factor$loadings
patientexp_scores<-patientexp_factor$scores
patientexp_scores_df<-as.data.frame(patientexp_scores)
#Key variables based on factor loadings H_COMP_7_STAR RATING, H_COMP_4_START_RATING AND H_CLEAN_STAR_RATING


factanal(imaging.efficiency.hospital_wide[,-1],1,rotation="varimax")#Explains 16.9% of variation
factanal(imaging.efficiency.hospital_wide[,-1],2,rotation="varimax")#29.9% of variation
factanal(imaging.efficiency.hospital_wide[,-1],3,rotation="varimax")#3 factors are too many for 5 variables

imaging.efficiency.hospital_wide_factor<-factanal(imaging.efficiency.hospital_wide[,-1],2,rotation="varimax",scores="regression")
imaging.efficiency.hospital_wide_loadings<-imaging.efficiency.hospital_wide_factor$loadings
imaging.efficiency.hospital_wide_scores<-imaging.efficiency.hospital_wide_factor$scores
imaging.efficiency.hospital_wide_scores_df<-as.data.frame(imaging.efficiency.hospital_wide_scores)

'
str(timely.effective.care.hospital_wide)
#We divide the measures into timeliness and effectiveness measures
Timeliness_Measures<-c("ED_1b","ED_2b","OP_18b","OP_20","OP_21","OP_5","OP_3b")
Timeliness_Wide<-timely.effective.care.hospital_wide[,1]
Timeliness_data<-timely.effective.care.hospital_wide[,colnames(timely.effective.care.hospital_wide)%in%Timeliness_Measures]
Timeliness_Wide<-cbind(Timeliness_Wide,Timeliness_data)
colnames(Timeliness_Wide)[1]<-colnames(timely.effective.care.hospital_wide[1])
Effectiveness_Wide<-timely.effective.care.hospital_wide[,!(colnames(timely.effective.care.hospital_wide)%in%Timeliness_Measures)]

'

factanal(Timeliness_Wide[,-1],1,rotation="varimax")#Explains 37.3% of variation
factanal(Timeliness_Wide[,-1],2,rotation="varimax")#50% of variation
factanal(Timeliness_Wide[,-1],3,rotation="varimax")#54.6% of variation 
#We choose 3 factors for Timeliness
Timeliness_factors<-factanal(Timeliness_Wide[,-1],3,rotation="varimax",scores="regression")
Timeliness_loadings<-Timeliness_factors$loadings
Timeliness_scores<-Timeliness_factors$scores
Timeliness_scores_df<-as.data.frame(Timeliness_scores)

factanal(Effectiveness_Wide[,-1],1,rotation="varimax")#Explains 15.7% of variation
factanal(Effectiveness_Wide[,-1],2,rotation="varimax")#19.8% of variation
factanal(Effectiveness_Wide[,-1],3,rotation="varimax")#23.3% of variation
factanal(Effectiveness_Wide[,-1],4,rotation="varimax")#26.1% of variation
factanal(Effectiveness_Wide[,-1],5,rotation="varimax")#27.8% of variation
factanal(Effectiveness_Wide[,-1],6,rotation="varimax")#30.1
factanal(Effectiveness_Wide[,-1],7,rotation="varimax")#36.5% variation
factanal(Effectiveness_Wide[,-1],8,rotation="varimax")#38.9% variation 
#So we choose 7 factors
Effectiveness_factors<-factanal(Effectiveness_Wide[,-1],8,rotation="varimax",scores="regression")
Effectiveness_loadings<-Effectiveness_factors$loadings
Effectiveness_scores<-Effectiveness_factors$scores
Effectiveness_scores_df<-as.data.frame(Effectiveness_scores)

#Transforming Factor Scores to Group scores
#Mortality Group
mortality_wide_loadings
#Prop variance 0.178,0.113,0.046
mortality_scores_df$meanscore<-NULL
for (i in 1:nrow(mortality_scores_df)){
    mortality_scores_df$meanscore[i]<-sum(mortality_scores_df$Factor1[i]*0.178,mortality_scores_df$Factor2[i]*0.113,mortality_scores_df$Factor3[i]*0.046)
}

mortality_scores_df$meanscore<-mortality_scores_df$meanscore/0.356

#Safety of Care 
safety_of_care_loadings
#Prop variance 0.124,0.122,0.117
safety_of_care_scores_df$meanscore<-NULL
for (i in 1:nrow(safety_of_care_scores_df)){
    safety_of_care_scores_df$meanscore[i]<-sum(safety_of_care_scores_df$Factor1[i]*0.124,safety_of_care_scores_df$Factor2[i]*0.122,safety_of_care_scores_df$Factor3[i]*0.117)
}

safety_of_care_scores_df$meanscore<-safety_of_care_scores_df$meanscore/0.363

#readmission group
readmission_loadings
#Prop variance: 0.292,0.128,0.116,0.009

readmission_scores_df$meanscore<-NULL
for (i in 1:nrow(readmission_scores_df)){
    readmission_scores_df$meanscore[i]<-sum(readmission_scores_df$Factor1[i]*0.292,readmission_scores_df$Factor2[i]*0.128,readmission_scores_df$Factor3[i]*0.116,readmission_scores_df$Factor4*0.009)
}

readmission_scores_df$meanscore<-readmission_scores_df$meanscore/0.545

#Patient Experience group
patientexp_loadings  
#patientexp_scores
patientexp_scores_df$meanscore<-NULL
for (i in 1:nrow(patientexp_scores_df)){
    patientexp_scores_df$meanscore[i]<-sum(patientexp_scores_df$Factor1[i]*0.244,patientexp_scores_df$Factor2[i]*0.196,patientexp_scores_df$Factor3[i]*0.157,patientexp_scores_df$Factor4*0.120,patientexp_scores_df$Factor5*0.021,patientexp_scores_df$Factor6*0.010)
}

patientexp_scores_df$meanscore<-patientexp_scores_df$meanscore/0.748



#Imagine efficiency
imaging.efficiency.hospital_wide_loadings
#Prop variance 0.213,0.085
imaging.efficiency.hospital_wide_scores_df$meanscore<-NULL

for (i in 1:nrow(imaging.efficiency.hospital_wide_scores_df)){
    imaging.efficiency.hospital_wide_scores_df$meanscore[i]<-sum(imaging.efficiency.hospital_wide_scores_df$Factor1[i]*0.213,imaging.efficiency.hospital_wide_scores_df$Factor2[i]*0.085)
}
imaging.efficiency.hospital_wide_scores_df$meanscore<-imaging.efficiency.hospital_wide_scores_df$meanscore/0.299


Timeliness_scores_df$meanscore<-NULL
Timeliness_loadings#0.284, 0.204,0.058
for (i in 1:nrow(Timeliness_scores_df)){
    Timeliness_scores_df$meanscore[i]<-sum(Timeliness_scores_df$Factor1[i]*0.284,Timeliness_scores_df$Factor2[i]*0.204,Timeliness_scores_df$Factor3*0.058)
}
Timeliness_scores_df$meanscore<-Timeliness_scores_df$meanscore/0.546


#Effectiveness measures
Effectiveness_scores_df$meanscore<-NULL
Effectiveness_loadings
#Prop 0.088,0.062,0.056,0.048,0.047,0.039,0.028,0.022
for (i in 1:nrow(Effectiveness_scores_df)){
    Effectiveness_scores_df$meanscore[i]<-sum(Effectiveness_scores_df$Factor1[i]*0.088,Effectiveness_scores_df$Factor2[i]*0.062,Effectiveness_scores_df$Factor3*0.056,Effectiveness_scores_df$Factor4*0.048,Effectiveness_scores_df$Factor5*0.039,Effectiveness_scores_df$Factor7*0.028,Effectiveness_scores_df$Factor8*0.022)
}

Effectiveness_scores_df$meanscore<-Effectiveness_scores_df$meanscore/0.389

group_Scores<-as.data.frame(cbind(master$provider.id,mortality_scores_df$meanscore,safety_of_care_scores_df$meanscore,readmission_scores_df$meanscore,patientexp_scores_df$meanscore,Timeliness_scores_df$meanscore,Effectiveness_scores_df$meanscore,imaging.efficiency.hospital_wide_scores_df$meanscore))

colnames(group_Scores)<-c("provider.id","mortality_score","safetyofcare_score","readmissions_score","patientexp_score","timelinessofcare_score","effectivenessofcare_score","imagingeff_score")

for(i in 1:nrow(group_Scores))
{
    group_Scores[i,"final_score"] <- sum(group_Scores$mortality_score[i]*0.22*(-1),group_Scores$safetyofcare_score[i]*0.22,group_Scores$readmissions_score[i]*0.22*(-1),group_Scores$patientexp_score[i]*0.22,group_Scores$timelinessofcare_score[i]*0.04*(-1),group_Scores$effectivenessofcare_score[i]*0.04,group_Scores$imagingeff_score[i]*0.04)
}
set.seed(2018)
clus5 <- kmeans(group_Scores$final_score, centers = 5, iter.max = 100, nstart = 100,algorithm="Forgy")

group_scores_clus <-cbind(group_Scores,clus5$cluster)
str(group_scores_clus)
score_clusters<- group_by(group_scores_clus, clus5$cluster)
tab1<- summarise(score_clusters, Mean_amount=mean(final_score))
tab1

#Given that we are assigning ratings in order of score 

# confusion matrix
pred_actual_values <- merge(group_scores_clus, hospital_info, by = "provider.id" , all.x = TRUE)
colnames(pred_actual_values)[10]<-"cluster"
pred_actual_values$predrating[pred_actual_values$cluster==1]<-1
pred_actual_values$predrating[pred_actual_values$cluster==5]<-2
pred_actual_values$predrating[pred_actual_values$cluster==4]<-3
pred_actual_values$predrating[pred_actual_values$cluster==2]<-4
pred_actual_values$predrating[pred_actual_values$cluster==3]<-5


#names(pred_actual_values)[names(pred_actual_values) == "clus5$cluster"] <- "pred_rating"

conf_matrix <- confusionMatrix(pred_actual_values$predrating, pred_actual_values$hospital.overall.rating)
conf_matrix

#41.8% Accuracy for K means

clus5 <- kmeans(group_Scores$final_score, centers = 5, iter.max = 100, nstart = 100,algorithm="MacQueen")

group_scores_clus <-cbind(group_Scores,clus5$cluster)
str(group_scores_clus)
score_clusters<- group_by(group_scores_clus, clus5$cluster)
tab1<- summarise(score_clusters, Mean_amount=mean(final_score))
tab1

#Given that we are assigning ratings in order of score 

# confusion matrix
pred_actual_values <- merge(group_scores_clus, hospital_info, by = "provider.id" , all.x = TRUE)
colnames(pred_actual_values)[10]<-"cluster"
#Given that we are assigning ratings in order of score 
pred_actual_values$predrating[pred_actual_values$cluster==1]<-1
pred_actual_values$predrating[pred_actual_values$cluster==2]<-2
pred_actual_values$predrating[pred_actual_values$cluster==4]<-3
pred_actual_values$predrating[pred_actual_values$cluster==3]<-4
pred_actual_values$predrating[pred_actual_values$cluster==3]<-5


#names(pred_actual_values)[names(pred_actual_values) == "clus5$cluster"] <- "pred_rating"

conf_matrix <- confusionMatrix(pred_actual_values$predrating, pred_actual_values$hospital.overall.rating)
conf_matrix
#32.8% Accuracy


#Using hierarchical clustering
group_dist<-dist(group_Scores$final_score)
CMS_hclust1<- hclust(group_dist, method="complete")
plot(CMS_hclust1)

## Visualising the cut in the dendrogram

rect.hclust(CMS_hclust1, k=5, border="red")
## Making the cut in the dendrogram

clusterCut <- cutree(CMS_hclust1, k=5)

## Appending the ClusterIDs to  data

group_hclust<-cbind(group_Scores,clusterCut)

score_clusters<- group_by(group_hclust, group_hclust$clusterCut)
tab1<- summarise(score_clusters, Mean_amount=mean(final_score))
tab1

pred_actual_values <- merge(group_hclust, hospital_info, by = "provider.id" , all.x = TRUE)
#Arranging in order of mean score
pred_actual_values$predstar[pred_actual_values$clusterCut==4]<-1
pred_actual_values$predstar[pred_actual_values$clusterCut==2]<-2
pred_actual_values$predstar[pred_actual_values$clusterCut==1]<-3
pred_actual_values$predstar[pred_actual_values$clusterCut==3]<-4
pred_actual_values$predstar[pred_actual_values$clusterCut==5]<-5
conf_matrix <- confusionMatrix(pred_actual_values$predstar, pred_actual_values$hospital.overall.rating)
conf_matrix
#Hierarchical we get 55.3% accuracy
