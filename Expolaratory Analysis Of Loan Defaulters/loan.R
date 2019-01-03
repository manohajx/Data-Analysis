##########################################  EXPLORATORY DATA ANALYSIS LOAN DEFAULTERS #########################################
#The aim of the analysis is to explore the consumer finance company data and to understand                                    #
#the driving factors (or driver variables) behind loan default, i.e. the variables which are strong indicators of default.    #
#the company can utilise this knowledge for its portfolio and risk assessment.                                                #
#                                                                                                                             #
#This data set contains the information about past loan applicants and whether they ‘defaulted’ or not. The aim is to         #
#identify patterns which indicate if a person is likely to default, which may be used for taking actions such as denying the  #                                                                                                                     #
#loan, reducing the amount of loan, lending (to risky applicants) at a higher interest rate, etc.                             #
#                                                                                                                             #
##############################################R version: R-3.4.0 ##############################################################
library(stringr)
library(zoo)
library(ggplot2)
library(dplyr)
library(scales)

View(head(loan))
str(loan)

#Importing the loan dataset into R
loan<-read.csv("loan.csv",stringsAsFactors = F, na.strings = c("","NA","n/a"))

#Removing the records where the loan status is current ,since these records are neither defaulter nor non defaulter
loan<-loan[loan$loan_status!="Current",]

#Checking if duplicate rows exists in the dataframe
nrow(unique(loan))!=nrow(loan)


#These columns are obtained only after the loan has been disbursed ,may not be useful to identify the defaulters before hand
#hence these can be eliminated from the analysis
not_useful_var<-c( 
    "delinq_2yrs",
    "earliest_cr_line",
    "inq_last_6mths",
    "open_acc",
    "pub_rec",
    "revol_bal",
    "revol_util",
    "total_acc",
    "out_prncp",
    "out_prncp_inv",
    "total_pymnt",
    "total_pymnt_inv",
    "total_rec_prncp",
    "total_rec_int",
    "total_rec_late_fee",
    "recoveries",
    "collection_recovery_fee",
    "last_pymnt_d",
    "last_pymnt_amnt",
    "next_pymnt_d",
    "last_credit_pull_d",
    "application_type")

loan.clean<-loan[,!(names(loan)%in% 
                        not_useful_var)]

#These column are either user given ,or long textual data ,which may not serve good for this analysis
loan.clean<-loan.clean[,!(names(loan.clean) %in% c('title','emp_title','url','desc','zip_code'))]


#Removing all the columns where more than 60% of data is missing or exists only one unique value
valid.columns<-sapply(loan.clean,
                      function(vector){
                          !(length(unique(vector))==1 | 
                                sum(vector %in% c(NA,'0'))/length(vector)*100 >=60) })

loan.clean<-loan.clean[,valid.columns]

#If the id columns are unique then they can removed for simplicity
sum(duplicated(loan.clean$id))
sum(duplicated(loan.clean$member_id))

loan.clean<-loan.clean[,!(names(loan.clean)%in% 
                              c('id','member_id'))]


#Checking for NA's
colSums(is.na(loan.clean))
colSums(is.na(loan.clean))/nrow(loan.clean)*100
#Can omit the Na since it forms only 2% of data
loan.clean<-na.omit(loan.clean)


#Cleaning data and setting them into correct data type
loan.clean$term<-as.integer(str_trim(str_replace_all(loan.clean$term, "months",  "")))
loan.clean$int_rate<-as.numeric(str_trim(str_replace_all(loan.clean$int_rate, "%",  "")))
loan.clean$emp_length<-as.integer(str_trim(str_replace_all(loan.clean$emp_length, 
                                                           c("years"="","year"="" ,"\\+"="","< 1"="0"))))

#Converting the date columns
loan.clean$issue_d<-as.yearmon(loan.clean$issue_d,"%b-%y")

#issue_d may not lead to useful information we may extract the month from issue_d to check if there exist any pattern in months
#and Year may not be useful because year cannot reoccur
loan.clean$issue_month<-factor(format(loan.clean$issue_d,"%B"),levels=month.name)
loan.clean$issue_year<-factor(format(loan.clean$issue_d,"%Y"))

#####################################BASIC UNDERSTANDING OF DATA THROUGH PLOTS ################################################
#Approximately dividing the columns into groups
catagorical_var<-names(loan.clean)[sapply(loan.clean,function(vector){length(unique(vector)) <= 100 })]
measure_var<-names(loan.clean)[sapply(loan.clean,function(vector){length(unique(vector)) > 100 & is.numeric(vector)})]

#Function to plot frequency bar graph of categories
cata_freq_bar<-function(df,catagorical_variable){
    plot<-ggplot(df,aes(factor(df[,catagorical_variable])))+geom_bar(fill="steelblue")+
        xlab(catagorical_variable) + ylab("Frequency")
    if (length(unique(df[,catagorical_variable]))>10)
    {
        plot+geom_text(stat='count',aes(label=..count..),hjust=0)+coord_flip()
    }
    else{
        plot +geom_text(stat='count',aes(label=..count..),vjust=-0.2)         
    }
}

#Frequency plot of category variables
for(i in catagorical_var) {
    
    readline(prompt="press enter to view plots")
    print(i)
    print(cata_freq_bar(loan.clean,i))
    print(loan.clean  %>% group_by(loan.clean[,i]) %>% 
              summarise(percent=100*n()/length(loan.clean[,i])) %>% arrange(desc(percent)))
}
#Inferences
#1. 75% of the data falls under 36 months term and the rest 60 months
#2.Grades A,B,C makes about 70% of data ,and E,E,F,G the rest 30%.Grade G is onlye 0.77%
#3.There are about 35 sub-grades which may not be quite intutive.
#4. 2% of data is NA's There are considerable many people with employment more than 10 
#   ,it would be better to bin these for better understading
#5. Home ownership has NONE and OTHER with less than 1 percent they could be removed from the data
#6. verification satutus more or less ahve equal distribution
#7.Around 14% of historical loans are charged off ,
#8.There are 14 purposes for which loans have been applied for we could pick the top 4 purposes for dicing
#.Addr_state distinct items are around 50 , and the distribution of data is not equal to draw conclusions
#10.Loans disbursed increase gradually from Jan to Dec.
#11.There is huge increase in financial requests from 2007-20011

#Removing sub grade and state_addr

loan.clean$sub_grade<-NULL
loan.clean$addr_state<-NULL
loan.clean$issue_d<-NULL

unique(loan.clean$home_ownership)
#Filtering out home ownership of type NONE and OTHER as their frequency is less
loan.clean<-subset(loan.clean, !(home_ownership %in%  c('NONE','OTHER')))

unique(loan.clean$purpose)
#Keeping only the top 4 products as they make 70% of the total data
loan.clean <- subset(loan.clean, (purpose %in%  c('debt_consolidation','credit_card','home_improvement','major_purchase')))

loan.clean$emp_length_bin <- factor(ifelse(loan.clean$emp_length<=1,"freshers",
                                           ifelse(loan.clean$emp_length>1&loan.clean$emp_length<=3,"junior",
                                                  ifelse(loan.clean$emp_length>3&loan.clean$emp_length<=7,"senior","expert"))),
                                    levels=c('freshers',"junior","senior","expert"))

#Function to plot frequency ploygon of measures
meas_freq_line<-function(df,measure)
{
    print(summary(df[,measure]))
    ggplot(df,aes(df[,measure]))+
        geom_histogram(bins=nclass.Sturges(df[,measure]) ,na.rm = T)+
        xlab(measure) + ylab("Frequency")  
}

#Frequency plot of measure variables
for(i in measure_var) {
    readline(prompt="press enter to view plots")
    print(i)
    print(quantile(loan.clean[,i], probs = c(0.95),na.rm=T))
    print(meas_freq_line(loan.clean,i))
}

#1. The loan amount spans from 500 to 35000 and the median is 9600 
#2. The Funded amount spans from 500 to 35000 and the median is 9550 ,is similar in patter to loan amount
#3. The Funded amount investment spans from 0 to 35000 and the median is 8733
#4. Interest rate varies from 5 to 25 % with a median of 11.71 %
#5. Installments vary from 12 to 1305 with a median of 277.90   
#6. Annual income is have outliers the data vary from 4000 to 6000000 and the median is 58870
#7.dti value varies from 0 to 30 and the median is around 30


#Classifying the meassure variables into 5 groups ,after studying the distribution of data for better visualization

class.levels<-c("Very low","Low","Medium","High","Very High")

#annual_inc
loan.clean$annual_inc_class<-ifelse(loan.clean$annual_inc <=20000,"Very low",
                                    ifelse(loan.clean$annual_inc <=40000,"Low",
                                           ifelse(loan.clean$annual_inc <=80000,"Medium",
                                                  ifelse(loan.clean$annual_inc <=100000,"High","Very High"))))

loan.clean$annual_inc_class<-factor(loan.clean$annual_inc_class,levels=class.levels)

#funded_amnt
loan.clean$funded_amnt_class<-ifelse(loan.clean$funded_amnt <=8000,"Very low",
                                     ifelse(loan.clean$funded_amnt <=16000,"Low",
                                            ifelse(loan.clean$funded_amnt <=32000,"Medium",
                                                   ifelse(loan.clean$funded_amnt <=40000,"High","Very High"))))
loan.clean$funded_amnt_class<-factor(loan.clean$funded_amnt_class,levels = class.levels)



#installments
loan.clean$installment_class<-ifelse(loan.clean$installment <=200,"Very low",
                                     ifelse(loan.clean$installment <=400,"Low",
                                            ifelse(loan.clean$installment <=800,"Medium",
                                                   ifelse(loan.clean$installment <=1000,"High","Very High"))))

loan.clean$installment_class<-factor(loan.clean$installment_class,levels = class.levels)

#Interest rate
loan.clean$int_rate_bin<-cut(loan.clean$int_rate,
                             seq(floor(min(loan.clean$int_rate,na.rm = T))-1,
                                 ceiling(max(loan.clean$int_rate,na.rm = T))+1,2))



#Calculating the total dti consisting of the emi value of the current loan(total_dti=dti without LC + emi/monthly income)

loan.clean$incl_dti<- loan.clean$dti + (100*loan.clean$installment/(loan.clean$annual_inc/12))


loan.clean$incl_dti_bin<-cut(loan.clean$incl_dti,
                             seq(floor(min(loan.clean$incl_dti,na.rm = T)),
                                 ceiling(max(loan.clean$incl_dti,na.rm = T))+10,11),
                             include.lowest = T)

#loan.clean[sapply(loan, is.character),] <- data.frame(loan.clean[sapply(loan, is.character),],stringsAsFactors = T)

bi_var<-c('term',
          'grade',
          'emp_length_bin',
          'home_ownership',
          'verification_status',
          'purpose',
          'int_rate_bin',
          'incl_dti_bin',
          'issue_month',
          'issue_year',
          'installment_class',
          'funded_amnt_class',
          'annual_inc_class')



#Function to plot stacked bar graph of 2 categories
cat_stacked_bar<-function(df,catvar1,catvar2){
    
    plot<-ggplot(df,aes(x=factor(loan.clean[,catvar2]),fill=loan.clean[,catvar1]))  +
        geom_bar(position = 'fill') + scale_y_continuous(label=percent) +
        xlab(catvar2) + ylab("Percentage") + labs(fill=catvar1) 
    
    if (length(unique(df[,catvar2]))>8)
    {
        plot+geom_text(stat='count',aes(label=..count..),position = position_fill(vjust = 0.5)) +
            coord_flip()
    }
    else{
        plot +geom_text(stat='count',aes(label=..count..),position = position_fill(vjust = 0.5))         
    }
}


#Plotting a 100% stacked barchart of frequency of multiple catergories with respect to loan status
for(i in bi_var){
    readline(prompt="press enter to view plots")
    print(i)
    print(cat_stacked_bar(loan.clean,'loan_status',i))
    rp<-loan.clean %>% group_by(loan.clean[,i]) %>% 
        summarise(relat_per=sum(loan_status=='Charged Off')*100/sum(loan_status!='Charged Off'))
    print(rp)
}


#Conclusion
#These are variables that considerabaly affect the loan charge off
#Term:most loans with term 60 months are defaulters when campared to 30 months term 
#grade : There is increase in defaulters from grade A to G 5% - 41%
#int_rate_bin : As the interest rate increase so is the defaulters
#incl_dti_bin :As the dti value increase so does the defaulters
#funded_amnt_class :With the increase in loaned amout the defaulters also increases
#annual_inc_class : People with very high salary tend less defaulters than with people with very low salary
#verification_status Slight rise in defaulters between Not verified and verified