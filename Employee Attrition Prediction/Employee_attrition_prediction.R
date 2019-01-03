################################################ EMPLOYEE ATTRITION  ##########################################################
#A large company named XYZ, employs, at any given point of time, around 4000 employees. However, every year,                   #
#around 15% of its employees leave the company and need to be replaced with the talent pool available in the job market        #
#                                                                                                                              #
#They want to understand what factors they should focus on, in order to curb attrition. In other words, they want to know what #
#changes they should make to their workplace, in order to get most of their employees to stay. Also, they want to know which   #
#of these variables is most important and needs to be addressed right away.                                                    #
#                                                                                                                              #
##############################################R version: R-3.4.0 ###############################################################

library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(caTools)
library(scales)
library(MASS)
library(car)
library(caret)

#Importing the data
employee_survey_data<- read.csv("employee_survey_data.csv",stringsAsFactors = F)
general_dataemployee_survey_data<- read.csv("general_data.csv",stringsAsFactors = F)
manager_survey_data<- read.csv("manager_survey_data.csv",stringsAsFactors = F)
in_time<- read.csv("in_time.csv",stringsAsFactors = F,,check.names = F)
out_time<- read.csv("out_time.csv",stringsAsFactors = F,check.names = F)

#Understanding the structure of data
str(employee_survey_data) #4410 obs. of  4 variables
str(general_dataemployee_survey_data) #4410 obs. of  24 variables
str(manager_survey_data) #4410 obs. of  3 variable
str(in_time) #4410 obs. of  262 variables: It is in wide format suitable need  to be converted into
str(out_time) #4410 obs. of  262 variables: It is in wide format suitable need  to be converted into

#Converting all the column names to lowercase for easy coding
names(employee_survey_data)<-tolower(names(employee_survey_data))
names(general_dataemployee_survey_data)<-tolower(names(general_dataemployee_survey_data))
names(manager_survey_data)<-tolower(names(manager_survey_data))
names(in_time)<-tolower(names(in_time))
names(out_time)<-tolower(names(out_time))

#Extracting suitable information from in_time and  out_time
#Identifying and removing columns(date) when no employee worked in dataframes in_time and out_time
valid.in_time.col<-sapply(in_time,
                          function(vector){
                              !sum(is.na(vector))==length(vector)})
valid.out_time.col<-sapply(out_time,
                           function(vector){
                               !sum(is.na(vector))==length(vector)})

in_time<-in_time[,valid.in_time.col]
out_time<-out_time[,valid.out_time.col]


#Converting the dataframe into long format for summarizing
long_in_time<-gather(in_time,date,in_time,-1)
long_out_time<-gather(out_time,date,out_time,-1)

#Naming the employeeid column
colnames(long_in_time)[1]<-"employeeid"
colnames(long_out_time)[1]<-"employeeid"

#converting the data into R date format
long_in_time$date<-as.Date(long_in_time$date)
long_out_time$date<-as.Date(long_out_time$date)
long_in_time$in_time<-as.POSIXct(long_in_time$in_time)
long_out_time$out_time<-as.POSIXct(long_out_time$out_time)


#Merging long_in_time and long_out_time to calculate the hours worked
employee_timings<-merge(long_in_time,long_out_time,by = c("date","employeeid"))

#Calculating the daily working hour of every employee
employee_timings$wrk.hrs<-as.numeric(round(employee_timings$out_time-employee_timings$in_time,2))


#Summarzing the employee_timings with employee granuarity
emp_avg_time<-as.data.frame(employee_timings %>% group_by(employeeid) %>% 
                                summarise(avg_time=mean(wrk.hrs,na.rm=T),absent=sum(is.na(wrk.hrs)))) 


setdiff(general_dataemployee_survey_data$employeeid,emp_avg_time$employeeid)
setdiff(general_dataemployee_survey_data$employeeid,manager_survey_data$employeeid)
setdiff(general_dataemployee_survey_data$employeeid,employee_survey_data$employeeid)

emp_master<-merge(general_dataemployee_survey_data,manager_survey_data,by="employeeid")
emp_master<-merge(emp_master,employee_survey_data,by="employeeid")
emp_master<-merge(emp_master,emp_avg_time,by="employeeid")


#Identyfying the duplicate records in
table(duplicated(emp_master))


#on observing the values of NumCompaniesWorked,  there are few records  for which NumCompaniesWorked is zero but Totalwrking experience has value greater than zero 
#and several other inconsistent observations were noticed, to address those, the below assumption is made

#Based on data definition 
#NumCompaniesWorkde denotes count of total no of companies the employee worked including the current company
#As per this assumption, changes were done in the values of NumCopanies Worked column.

sum(emp_master$yearsatcompany==0 & emp_master$totalworkingyears==0 & emp_master$numcompaniesworked !=0,na.rm = T)
emp_master$numcompaniesworked[which(emp_master$yearsatcompany==0 & emp_master$totalworkingyears==0 & emp_master$numcompaniesworked !=0)]<-0

emp_master$numcompaniesworked[which((emp_master$totalworkingyears==emp_master$yearsatcompany+1)
                                    & (emp_master$numcompaniesworked<2 ))] <- 2


#Identifying NA's
colSums(is.na(emp_master))
colSums(is.na(emp_master))[colSums(is.na(emp_master))>0]
sum(rowSums(is.na(emp_master))>0)

#View(emp_master[rowSums(is.na(emp_master))>0,])
##View(emp_master[which(is.na(emp_master$numcompaniesworked)),])
##View(emp_master[which(is.na(emp_master$totalworkingyears)),])
  

#Imputing numcompaniesworked for possible rows
emp_master$numcompaniesworked[which(is.na(emp_master$numcompaniesworked) & (emp_master$totalworkingyears==emp_master$yearsatcompany))] <-1
emp_master$numcompaniesworked[which(is.na(emp_master$numcompaniesworked) & (emp_master$totalworkingyears==emp_master$yearsatcompany+1))]<-2

#Imputing totalworkingyears for possible rows          
emp_master$totalworkingyears[which(is.na(emp_master$totalworkingyears) & (emp_master$numcompaniesworked==1))]<-
emp_master$yearsatcompany[which(is.na(emp_master$totalworkingyears) & (emp_master$numcompaniesworked==1))]
          
#The number of NA is 98 rows we can remove it to reduce complexicity
          
sum(rowSums(is.na(emp_master))>0)
emp_master<-na.omit(emp_master)

col.to.rm<-sapply(emp_master,function(vector){!length(unique(vector))<=1})
emp_master<-emp_master[,col.to.rm]          

          
          
catagorical_var<- c("attrition","businesstravel","department","education","gender","educationfield",
                              "joblevel","jobrole","maritalstatus","stockoptionlevel","jobinvolvement",
                              "performancerating","environmentsatisfaction","jobsatisfaction","worklifebalance")
          
measure_var<-names(emp_master)[!(names(emp_master) %in% catagorical_var)]
          
         
###Exploring the data using multiple plots
 
#Frequency plots to understand thedata

for(i in catagorical_var) {
 
  readline(prompt="press enter to View plots")
  print(i)
  plot<-ggplot(emp_master,aes(factor(emp_master[,i])))+geom_bar(fill="steelblue") +xlab(i) + ylab("Frequency") +
      geom_text(stat='count',aes(label=..count..),hjust=0)+coord_flip()
  print(plot)
  
}
          
          
for(i in measure_var[-1]) {
    
readline(prompt="press enter to View plots")
print(i)
plot1<-ggplot(emp_master,aes(emp_master[,i]))+geom_histogram(bins=nclass.Sturges(emp_master[,i]),fill="steelblue") +
  xlab(i) + ylab("Frequency")
plot2<-ggplot(emp_master,aes(y=emp_master[,i],x=emp_master[,'attrition']))+ geom_boxplot( outlier.size = 1, outlier.colour="red")
print("Yes")
print(quantile(emp_master[emp_master$attrition=="Yes",i],seq(0,1,0.01)))
print("No")
print(quantile(emp_master[emp_master$attrition=="No",i],seq(0,1,0.01)))
grid.arrange(plot1,plot2,nrow=2)

}

#Outlier detection based on the above plots and cupping values for better results
emp_master$age[which(emp_master$age > 53)] <- 53
emp_master$monthlyincome[emp_master$monthlyincome > 171230.0 ] <- 171230.0
emp_master$numcompaniesworked[emp_master$numcompaniesworked > 8 ] <- 8
emp_master$totalworkingyears[emp_master$totalworkingyears > 23] <- 23
emp_master$trainingtimeslastyear[emp_master$trainingtimeslastyear > 5] <- 5
emp_master$trainingtimeslastyear[emp_master$trainingtimeslastyear < 1] <- 1
emp_master$yearsatcompany[emp_master$yearsatcompany > 17] <- 17
emp_master$yearssincelastpromotion[emp_master$yearssincelastpromotion > 9] <- 9
emp_master$yearswithcurrmanager[which(emp_master$yearswithcurrmanager > 9)] <- 9
emp_master$yearswithcurrmanager[which(emp_master$avg_time > 10.486078)] <- 10.486078


#Understanding the factors affecting  attrition 

for(i in catagorical_var[-1]) {
    
readline(prompt="press enter to View plots")
print(i)
plot1<-ggplot(emp_master,aes(factor(emp_master[,i]),fill=emp_master[,"attrition"]))+geom_bar() +xlab(i) + ylab("Frequency") +
  geom_text(stat='count',aes(label=..count..),position=position_stack(vjust = 0.5))+labs(fill="attrition") # + coord_flip()
plot2<-ggplot(emp_master,aes(factor(emp_master[,i]),fill=emp_master[,"attrition"]))+geom_bar(position = 'fill') +
  xlab(i) + ylab("Relative perccentage") +scale_y_continuous(label=percent) + labs(fill="attrition") # + coord_flip()
grid.arrange(plot1,plot2,nrow=2)

}


#####OBSERVATIONS FROM THE ABOVE PLOTS

# Attrition rate is 16%
# Max attrition  percent on  category travel_frequently on BusinessTravel
# Max attrition percent occurs in HR department 
# Max attrition percent occurs in HR Education Field
# Max attrition percent occurs in Research Director JobRole 
# Max attrition percent occurs in Single Marital Status
# Max attrition percent occurs in employee group who gave Evironmentsstisfaction score 1 
# Max attrition percent occurs in employee group who gave Jobsatisfaction score 1 
# Max attrition percent occurs in employee group who gave Worklifebalance score 1 
# Max attrition percent occurs in employee group who gave Jobinvolvement score 1 
# Max attrition percent occurs in employee group who got performanceRating as 4


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
emp_master<-cbind(emp_master[,-which(names(emp_master)==i)] ,dummy_conv(emp_master[,i],i))
}

emp_master<-  emp_master[,-1]



#Scaling all the numerical variables for uniformity
for (i in measure_var[-1]){

emp_master[,i]<-scale(emp_master[,i])    
}


##################################################### MODEL BUILDING ##########################################################
#splitting of train and test dataset
set.seed(2017)
train_indices <- sample.split(emp_master$attrition,SplitRatio=0.7)
train <- emp_master[train_indices,]
##View(train)
test <- emp_master[!(train_indices),]
##View(test)
colnames(train)
#View(train)


#MODEL:model_1
#Base model
model_1 <- glm(attrition~.,data=train,family="binomial")
summary(model_1)
#AIC 2150.8

#MODEL:model_2
#Removed: By StepAIC
model_2 <- stepAIC(model_1,direction="both")
summary(model_2)
as.data.frame(sort(vif(model_2)))
#AIC 2118.7

#Removing businesstravel.Travel_Rarely    
#Model:model_3
model_3<-glm(formula = attrition ~ 
               age 
             + monthlyincome 
             + numcompaniesworked 
             + totalworkingyears 
             + trainingtimeslastyear 
             + yearsatcompany 
             + yearssincelastpromotion 
             + yearswithcurrmanager 
             + avg_time 
             + businesstravel.Travel_Frequently 
             + `department.Research&Development` 
             + department.Sales 
             + education.2 
             + education.5 
             + joblevel.3 
             + joblevel.5 
             + jobrole.LaboratoryTechnician 
             + jobrole.ResearchDirector 
             + jobrole.ResearchScientist 
             + jobrole.SalesExecutive 
             + maritalstatus.Married 
             + maritalstatus.Single 
             + stockoptionlevel.1 
             + stockoptionlevel.3 
             + jobinvolvement.3 
             + environmentsatisfaction.2 
             + environmentsatisfaction.3 
             + environmentsatisfaction.4 
             + jobsatisfaction.2 
             + jobsatisfaction.3 
             + jobsatisfaction.4 
             + worklifebalance.2 
             + worklifebalance.3 
             + worklifebalance.4, family = "binomial", 
             data = train)

summary(model_3)
as.data.frame(sort(vif(model_3)))
#AIC 2123.4

#Removing `department.Research&Development`         
#Model:model_4
model_4<-glm(formula = attrition ~ 
                 age 
             + monthlyincome 
             + numcompaniesworked 
             + totalworkingyears 
             + trainingtimeslastyear 
             + yearsatcompany 
             + yearssincelastpromotion 
             + yearswithcurrmanager 
             + avg_time 
             + businesstravel.Travel_Frequently 
             + department.Sales 
             + education.2 
             + education.5 
             + joblevel.3 
             + joblevel.5 
             + jobrole.LaboratoryTechnician 
             + jobrole.ResearchDirector 
             + jobrole.ResearchScientist 
             + jobrole.SalesExecutive 
             + maritalstatus.Married 
             + maritalstatus.Single 
             + stockoptionlevel.1 
             + stockoptionlevel.3 
             + jobinvolvement.3 
             + environmentsatisfaction.2 
             + environmentsatisfaction.3 
             + environmentsatisfaction.4 
             + jobsatisfaction.2 
             + jobsatisfaction.3 
             + jobsatisfaction.4 
             + worklifebalance.2 
             + worklifebalance.3 
             + worklifebalance.4, family = "binomial", 
             data = train)

summary(model_4)
as.data.frame(sort(vif(model_4)))
#AIC 2137.3


#Removing  yearsatcompany            
#Model:model_5
model_5<-glm(formula = attrition ~ 
                 age 
             + monthlyincome 
             + numcompaniesworked 
             + totalworkingyears 
             + trainingtimeslastyear 
             + yearssincelastpromotion 
             + yearswithcurrmanager 
             + avg_time 
             + businesstravel.Travel_Frequently 
             + department.Sales 
             + education.2 
             + education.5 
             + joblevel.3 
             + joblevel.5 
             + jobrole.LaboratoryTechnician 
             + jobrole.ResearchDirector 
             + jobrole.ResearchScientist 
             + jobrole.SalesExecutive 
             + maritalstatus.Married 
             + maritalstatus.Single 
             + stockoptionlevel.1 
             + stockoptionlevel.3 
             + jobinvolvement.3 
             + environmentsatisfaction.2 
             + environmentsatisfaction.3 
             + environmentsatisfaction.4 
             + jobsatisfaction.2 
             + jobsatisfaction.3 
             + jobsatisfaction.4 
             + worklifebalance.2 
             + worklifebalance.3 
             + worklifebalance.4, family = "binomial", 
             data = train)

summary(model_5)
as.data.frame(sort(vif(model_5)))
#AIC 2138.7


#Removing worklifebalance.2            
#Model:model_6
model_6<-glm(formula = attrition ~ 
                 age 
             + monthlyincome 
             + numcompaniesworked 
             + totalworkingyears 
             + trainingtimeslastyear 
             + yearssincelastpromotion 
             + yearswithcurrmanager 
             + avg_time 
             + businesstravel.Travel_Frequently 
             + department.Sales 
             + education.2 
             + education.5 
             + joblevel.3 
             + joblevel.5 
             + jobrole.LaboratoryTechnician 
             + jobrole.ResearchDirector 
             + jobrole.ResearchScientist 
             + jobrole.SalesExecutive 
             + maritalstatus.Married 
             + maritalstatus.Single 
             + stockoptionlevel.1 
             + stockoptionlevel.3 
             + jobinvolvement.3 
             + environmentsatisfaction.2 
             + environmentsatisfaction.3 
             + environmentsatisfaction.4 
             + jobsatisfaction.2 
             + jobsatisfaction.3 
             + jobsatisfaction.4 
             + worklifebalance.3 
             + worklifebalance.4
             , family = "binomial", 
             data = train)

summary(model_6)
as.data.frame(sort(vif(model_6)))
#AIC 2156.9


#Removing  worklifebalance.4           
#Model:model_7
model_7<-glm(formula = attrition ~ 
                 age 
             + monthlyincome 
             + numcompaniesworked 
             + totalworkingyears 
             + trainingtimeslastyear 
             + yearssincelastpromotion 
             + yearswithcurrmanager 
             + avg_time 
             + businesstravel.Travel_Frequently 
             + department.Sales 
             + education.2 
             + education.5 
             + joblevel.3 
             + joblevel.5 
             + jobrole.LaboratoryTechnician 
             + jobrole.ResearchDirector 
             + jobrole.ResearchScientist 
             + jobrole.SalesExecutive 
             + maritalstatus.Married 
             + maritalstatus.Single 
             + stockoptionlevel.1 
             + stockoptionlevel.3 
             + jobinvolvement.3 
             + environmentsatisfaction.2 
             + environmentsatisfaction.3 
             + environmentsatisfaction.4 
             + jobsatisfaction.2 
             + jobsatisfaction.3 
             + jobsatisfaction.4 
             + worklifebalance.3 
             , family = "binomial", 
             data = train)
summary(model_7)
as.data.frame(sort(vif(model_7)))
#AIC 2155.5



#Removing  department.Sales            
#Model:model_8
model_8<-glm(formula = attrition ~ 
                 age 
             + monthlyincome 
             + numcompaniesworked 
             + totalworkingyears 
             + trainingtimeslastyear 
             + yearssincelastpromotion 
             + yearswithcurrmanager 
             + avg_time 
             + businesstravel.Travel_Frequently 
             + education.2 
             + education.5 
             + joblevel.3 
             + joblevel.5 
             + jobrole.LaboratoryTechnician 
             + jobrole.ResearchDirector 
             + jobrole.ResearchScientist 
             + jobrole.SalesExecutive 
             + maritalstatus.Married 
             + maritalstatus.Single 
             + stockoptionlevel.1 
             + stockoptionlevel.3 
             + jobinvolvement.3 
             + environmentsatisfaction.2 
             + environmentsatisfaction.3 
             + environmentsatisfaction.4 
             + jobsatisfaction.2 
             + jobsatisfaction.3 
             + jobsatisfaction.4 
             + worklifebalance.3 
             , family = "binomial", 
             data = train)

summary(model_8)
as.data.frame(sort(vif(model_8)))
#AIC 2155.1



#Removing   stockoptionlevel.3           
#Model:model_9
model_9<-glm(formula = attrition ~ 
                 age 
             + monthlyincome 
             + numcompaniesworked 
             + totalworkingyears 
             + trainingtimeslastyear 
             + yearssincelastpromotion 
             + yearswithcurrmanager 
             + avg_time 
             + businesstravel.Travel_Frequently 
             + education.2 
             + education.5 
             + joblevel.3 
             + joblevel.5 
             + jobrole.LaboratoryTechnician 
             + jobrole.ResearchDirector 
             + jobrole.ResearchScientist 
             + jobrole.SalesExecutive 
             + maritalstatus.Married 
             + maritalstatus.Single 
             + stockoptionlevel.1 
             + jobinvolvement.3 
             + environmentsatisfaction.2 
             + environmentsatisfaction.3 
             + environmentsatisfaction.4 
             + jobsatisfaction.2 
             + jobsatisfaction.3 
             + jobsatisfaction.4 
             + worklifebalance.3 
             , family = "binomial", 
             data = train)

summary(model_9)
as.data.frame(sort(vif(model_9)))



#Removing stockoptionlevel.1          
#Model:model_9
model_10<-glm(formula = attrition ~ 
                  age 
              + monthlyincome 
              + numcompaniesworked 
              + totalworkingyears 
              + trainingtimeslastyear 
              + yearssincelastpromotion 
              + yearswithcurrmanager 
              + avg_time 
              + businesstravel.Travel_Frequently 
              + education.2 
              + education.5 
              + joblevel.3 
              + joblevel.5 
              + jobrole.LaboratoryTechnician 
              + jobrole.ResearchDirector 
              + jobrole.ResearchScientist 
              + jobrole.SalesExecutive 
              + maritalstatus.Married 
              + maritalstatus.Single 
              + jobinvolvement.3 
              + environmentsatisfaction.2 
              + environmentsatisfaction.3 
              + environmentsatisfaction.4 
              + jobsatisfaction.2 
              + jobsatisfaction.3 
              + jobsatisfaction.4 
              + worklifebalance.3 
              , family = "binomial", 
              data = train)

summary(model_10)
as.data.frame(sort(vif(model_10)))

#AIC 2154.9


#Removing education.2          
#Model:model_11
model_11<-glm(formula = attrition ~ 
                  age 
              + monthlyincome 
              + numcompaniesworked 
              + totalworkingyears 
              + trainingtimeslastyear 
              + yearssincelastpromotion 
              + yearswithcurrmanager 
              + avg_time 
              + businesstravel.Travel_Frequently 
              + education.5 
              + joblevel.3 
              + joblevel.5 
              + jobrole.LaboratoryTechnician 
              + jobrole.ResearchDirector 
              + jobrole.ResearchScientist 
              + jobrole.SalesExecutive 
              + maritalstatus.Married 
              + maritalstatus.Single 
              + jobinvolvement.3 
              + environmentsatisfaction.2 
              + environmentsatisfaction.3 
              + environmentsatisfaction.4 
              + jobsatisfaction.2 
              + jobsatisfaction.3 
              + jobsatisfaction.4 
              + worklifebalance.3 
              , family = "binomial", 
              data = train)
summary(model_11)
as.data.frame(sort(vif(model_11)))
#AIC 2154.9



#Removing   joblevel.3           
#Model:model_12
model_12<-glm(formula = attrition ~ 
                  age 
              + monthlyincome 
              + numcompaniesworked 
              + totalworkingyears 
              + trainingtimeslastyear 
              + yearssincelastpromotion 
              + yearswithcurrmanager 
              + avg_time 
              + businesstravel.Travel_Frequently 
              + education.5 
              + joblevel.5 
              + jobrole.LaboratoryTechnician 
              + jobrole.ResearchDirector 
              + jobrole.ResearchScientist 
              + jobrole.SalesExecutive 
              + maritalstatus.Married 
              + maritalstatus.Single 
              + jobinvolvement.3 
              + environmentsatisfaction.2 
              + environmentsatisfaction.3 
              + environmentsatisfaction.4 
              + jobsatisfaction.2 
              + jobsatisfaction.3 
              + jobsatisfaction.4 
              + worklifebalance.3 
              , family = "binomial", 
              data = train)

summary(model_12)
as.data.frame(sort(vif(model_12)))
#AIC 2155.8



#Removing jobinvolvement.3        
#Model:model_13
model_13<-glm(formula = attrition ~ 
                  age 
              + monthlyincome 
              + numcompaniesworked 
              + totalworkingyears 
              + trainingtimeslastyear 
              + yearssincelastpromotion 
              + yearswithcurrmanager 
              + avg_time 
              + businesstravel.Travel_Frequently 
              + education.5 
              + joblevel.5 
              + jobrole.LaboratoryTechnician 
              + jobrole.ResearchDirector 
              + jobrole.ResearchScientist 
              + jobrole.SalesExecutive 
              + maritalstatus.Married 
              + maritalstatus.Single 
              + environmentsatisfaction.2 
              + environmentsatisfaction.3 
              + environmentsatisfaction.4 
              + jobsatisfaction.2 
              + jobsatisfaction.3 
              + jobsatisfaction.4 
              + worklifebalance.3 
              , family = "binomial", 
              data = train)



summary(model_13)
as.data.frame(sort(vif(model_13)))
#AIC2157.3

#Removing education.5        
#Model:model_14
model_14<-glm(formula = attrition ~ 
                  age 
              + monthlyincome 
              + numcompaniesworked 
              + totalworkingyears 
              + trainingtimeslastyear 
              + yearssincelastpromotion 
              + yearswithcurrmanager 
              + avg_time 
              + businesstravel.Travel_Frequently 
              + joblevel.5 
              + jobrole.LaboratoryTechnician 
              + jobrole.ResearchDirector 
              + jobrole.ResearchScientist 
              + jobrole.SalesExecutive 
              + maritalstatus.Married 
              + maritalstatus.Single 
              + environmentsatisfaction.2 
              + environmentsatisfaction.3 
              + environmentsatisfaction.4 
              + jobsatisfaction.2 
              + jobsatisfaction.3 
              + jobsatisfaction.4 
              + worklifebalance.3 
              , family = "binomial", 
              data = train)

summary(model_14)
as.data.frame(sort(vif(model_14)))
#AIC 2159.6


#Removing  monthlyincome         
#Model:model_15
model_15<-glm(formula = attrition ~ 
                  age 
              + numcompaniesworked 
              + totalworkingyears 
              + trainingtimeslastyear 
              + yearssincelastpromotion 
              + yearswithcurrmanager 
              + avg_time 
              + businesstravel.Travel_Frequently 
              + joblevel.5 
              + jobrole.LaboratoryTechnician 
              + jobrole.ResearchDirector 
              + jobrole.ResearchScientist 
              + jobrole.SalesExecutive 
              + maritalstatus.Married 
              + maritalstatus.Single 
              + environmentsatisfaction.2 
              + environmentsatisfaction.3 
              + environmentsatisfaction.4 
              + jobsatisfaction.2 
              + jobsatisfaction.3 
              + jobsatisfaction.4 
              + worklifebalance.3 
              , family = "binomial", 
              data = train)

summary(model_15)
as.data.frame(sort(vif(model_15)))

#AIC 2161.8

#Removing joblevel.5             
#Model:model_16
model_16<-glm(formula = attrition ~ 
                  age 
              + numcompaniesworked 
              + totalworkingyears 
              + trainingtimeslastyear 
              + yearssincelastpromotion 
              + yearswithcurrmanager 
              + avg_time 
              + businesstravel.Travel_Frequently 
              + jobrole.LaboratoryTechnician 
              + jobrole.ResearchDirector 
              + jobrole.ResearchScientist 
              + jobrole.SalesExecutive 
              + maritalstatus.Married 
              + maritalstatus.Single 
              + environmentsatisfaction.2 
              + environmentsatisfaction.3 
              + environmentsatisfaction.4 
              + jobsatisfaction.2 
              + jobsatisfaction.3 
              + jobsatisfaction.4 
              + worklifebalance.3 
              , family = "binomial", 
              data = train)

summary(model_16)
as.data.frame(sort(vif(model_16)))

#AIC 2163.8



#Removing maritalstatus.Married          
#Model:model_17
model_17<-glm(formula = attrition ~ 
                  age 
              + numcompaniesworked 
              + totalworkingyears 
              + trainingtimeslastyear 
              + yearssincelastpromotion 
              + yearswithcurrmanager 
              + avg_time 
              + businesstravel.Travel_Frequently 
              + jobrole.LaboratoryTechnician 
              + jobrole.ResearchDirector 
              + jobrole.ResearchScientist 
              + jobrole.SalesExecutive 
              + maritalstatus.Single 
              + environmentsatisfaction.2 
              + environmentsatisfaction.3 
              + environmentsatisfaction.4 
              + jobsatisfaction.2 
              + jobsatisfaction.3 
              + jobsatisfaction.4 
              + worklifebalance.3 
              , family = "binomial", 
              data = train)


summary(model_17)
as.data.frame(sort(vif(model_17)))

#AIC 2168.1


#Removing  jobrole.ResearchScientist           
#Model:model_18
model_18<- glm(formula = attrition ~ 
                   age 
               + numcompaniesworked 
               + totalworkingyears 
               + trainingtimeslastyear 
               + yearssincelastpromotion 
               + yearswithcurrmanager 
               + avg_time 
               + businesstravel.Travel_Frequently 
               + jobrole.LaboratoryTechnician 
               + jobrole.ResearchDirector 
               + jobrole.SalesExecutive 
               + maritalstatus.Single 
               + environmentsatisfaction.2 
               + environmentsatisfaction.3 
               + environmentsatisfaction.4 
               + jobsatisfaction.2 
               + jobsatisfaction.3 
               + jobsatisfaction.4 
               + worklifebalance.3 
               , family = "binomial", 
               data = train)

summary(model_18)
as.data.frame(sort(vif(model_18)))

#AIC 2172.7



#Removing jobrole.LaboratoryTechnician       
#Model:model_19
model_19<-glm(formula = attrition ~ 
                  age 
              + numcompaniesworked 
              + totalworkingyears 
              + trainingtimeslastyear 
              + yearssincelastpromotion 
              + yearswithcurrmanager 
              + avg_time 
              + businesstravel.Travel_Frequently 
              + jobrole.ResearchDirector 
              + jobrole.SalesExecutive 
              + maritalstatus.Single 
              + environmentsatisfaction.2 
              + environmentsatisfaction.3 
              + environmentsatisfaction.4 
              + jobsatisfaction.2 
              + jobsatisfaction.3 
              + jobsatisfaction.4 
              + worklifebalance.3 
              , family = "binomial", 
              data = train)


summary(model_19)
as.data.frame(sort(vif(model_19)))
#AIC: 2174.4


#Removing jobrole.SalesExecutive       
#Model:model_20
model_20<-glm(formula = attrition ~ 
                  age 
              + numcompaniesworked 
              + totalworkingyears 
              + trainingtimeslastyear 
              + yearssincelastpromotion 
              + yearswithcurrmanager 
              + avg_time 
              + businesstravel.Travel_Frequently 
              + jobrole.ResearchDirector 
              + maritalstatus.Single 
              + environmentsatisfaction.2 
              + environmentsatisfaction.3 
              + environmentsatisfaction.4 
              + jobsatisfaction.2 
              + jobsatisfaction.3 
              + jobsatisfaction.4 
              + worklifebalance.3 
              , family = "binomial", 
              data = train)

summary(model_20)
as.data.frame(sort(vif(model_20)))
#AIC: 2174.9

final_model<-model_20 

##################################################### MODEL EVALUATION ########################################################

#Prediting probability based on the final model 
test_pred <- predict(final_model,type="response",newdata =test)
summary(test_pred)
test$prob <- test_pred

#For a 50/50 attrition probability checking how the accuracy
test_pred_left <- factor(ifelse(test_pred>=0.5,"Yes","No"))
test_act_left <- factor(ifelse(test$attrition==1,"Yes","No"))
test_conf <- confusionMatrix(test_pred_left, test_act_left, positive = "Yes")
test_conf
#Accuracy :0.864
#Sensitivity :0.23445
#Specificity : 0.98525


##Identifying the ideal cutoff probability

#Function that gives the accuracy , sensitivity and specificity for a cutoff value
perform_fn <- function(cutoff) 
{
predicted_left <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
conf <- confusionMatrix(predicted_left, test_act_left, positive = "Yes")
acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]
out <- t(as.matrix(c(sens, spec, acc))) 
colnames(out) <- c("sensitivity", "specificity", "accuracy")
return(out)
}

# Dividing the predictied probabilities  into 1000 parts and generating a matrix of accuracy , sensitivity and specificity
#to det the optimum cutoff

summary(test_pred)
s = seq(0.002555,0.779744  ,length=1000)
OUT = matrix(0,1000,3)


for(i in 1:1000)
{
OUT[i,] = perform_fn(s[i])
} 

#PLotting the accuracy , sensitivity and specificity to identify the pattern
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,0.01),cex.lab=1.5)
axis(2,seq(0,1,0.01),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#Picking up a cutoff where the difference between sensitiviyt and specificity is nearly 0
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.001)]
#optimal cutoff is 
cutoff
#0.1659281

#Final list of attrition
test_cutoff_left <- factor(ifelse(test_pred >=0.1659281, "Yes", "No"))
conf_final <-confusionMatrix(test_cutoff_left, test_act_left, positive = "Yes")
conf_final
#Accuracy :0.7457
#Sensitivity :0.7464 
#Specificity : 0.7464 


#COnverting the attrition to numeric
test_cutoff_left <- ifelse(test_cutoff_left=="Yes",1,0)
test_act_left <- ifelse(test_act_left=="Yes",1,0)

library(gains)
gains<-gains(test_act_left, test_pred, groups = 10)

#Creating a Evaluation table containg gain , lift 

#Dataframe with the test actual attrition and test predicted attrition
nw<-as.data.frame(cbind(test_act_left,test_pred))

evaluation_tbl<- as.data.frame( nw %>% arrange(desc(test_pred)) %>% 
                    mutate(deceile=rep(1:10, each=floor(length(test_act_left)/10), length.out=length(test_act_left))) %>% 
                    group_by(deceile) %>% 
                    summarise(attrition=sum(test_act_left),observations=n(),non.attri=observations-attrition) %>% 
                    mutate(cum.attri=cumsum(attrition),gain.per=cum.attri*100/sum(attrition),
                    non.cum.attri=cumsum(non.attri),non.cum.perc=non.cum.attri*100/sum(non.attri),
                    random=seq(10,100,10),lift=gain.per/random,diff.cum.perc=gain.per-non.cum.perc))

evaluation_tbl

#Gain chart
ggplot(evaluation_tbl,aes(deceile,gain.per)) + geom_line(col="dark green") +geom_line(aes(deceile,random),col="blue") +scale_x_continuous(breaks=c(1:10))

#For the 3rd deceile we have around 73% of attrition predicted

#Lift chart
ggplot(evaluation_tbl,aes(deceile,lift)) + geom_line(col="dark green") +geom_line(aes(deceile,c(rep(1,10))),col="blue") +scale_x_continuous(breaks=c(1:10))


 ### KS -statistic - Test Data ######
         
ksstatistic<- max(evaluation_tbl$diff.cum.perc)
deceile<-   evaluation_tbl[which(max(evaluation_tbl$diff.cum.perc)==evaluation_tbl$diff.cum.perc),'deceile']      
ksstatistic
deceile
#THe Ks statistic is around 51.27 % and happens in the 3rd deceile good models should have more than 40% KS statistics 


######################################################CONCLUSION###############################################################
#From the final model, its is found that the key driving factors of Attrition of employee of The XYZ                          #
#company are :                                                                                                                #
#. Age                                                                                                                        #
#. NumCompaniesWorked                                                                                                         #
#. TotalWorkingYears                                                                                                          #
#. trainingtimeslastyear                                                                                                      #
#. YearsSinceLastPromotion                                                                                                    #
#. YearsWithCurrManager                                                                                                       #
#. avg_time                                                                                                                   #
#. BusinessTravel                                                                                                             #
#. EnvironmentSatisfaction                                                                                                    #
#. JobRole                                                                                                                    #
#. JobSatisfaction                                                                                                            #
#. worklifebalance                                                                                                            #
#The final model has accuracy of ~ 74% on using the optimal cut-off value as 0.1659281 . And the KS-statistics is             #
#found to be 51% which lies in the 3rd decile .                                                                               #
###############################################################################################################################
