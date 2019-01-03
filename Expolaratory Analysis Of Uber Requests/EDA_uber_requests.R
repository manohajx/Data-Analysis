########################################## UBER SUPPLY DEMAND GAP EXPLORATORY ANALYSIS ########################################
#The aim of the analysis is to explore the uber data and identify the bussiness problems faced by Uber with city and Airport  #
#Pickups due to cancecllation and unavialability of cars                                                                      #
#                                                                                                                             #
#This data set is a masked data subset which is similar to what data analysts at Uber handle contains only important data     #
#points                                                                                                                       #
#There are six attributes associated with each request made by a customer:                                                    #
#                                                                                                                             #
#1. Request id: A unique identifier of the request                                                                            #
#2. Time of request: The date and time at which the customer made the trip request                                            #
#3. Drop-off time: The drop-off date and time, in case the trip was completed                                                 # 
#4. Pick-up point: The point from which the request was made                                                                  #
#5. Driver id: The unique identification number of the driver                                                                 #
#6. Status of the request: The final status of the trip, that can be either completed, cancelled by the driver or no cars     # 
#   available                                                                                                                 #
##############################################R version: R-3.4.0 ##############################################################

library(dplyr)
library(gridExtra)
library(ggplot2)

#Importing the uber request data into R
uber_request_data<-read.csv("Uber Request Data.csv",stringsAsFactors = F , header = T)


#################################################PREPARING THE DATA FOR ANALYSIS ##############################################

#Understanding the dataset
str(uber_request_data)
#View(head(uber_request_data,30))


#Converting column names to lower case for ease
colnames(uber_request_data)<-tolower(colnames(uber_request_data))

#Check for duplicate rows ,if any
any(duplicated(uber_request_data))

#Check for NA in each column
colSums(is.na(uber_request_data))

#Checking if the NA in drop.timestamp are valid ()
any(is.na(uber_request_data$drop.timestamp) & uber_request_data$status=="Trip Completed")

##Checking if the NA in driver.id are valid
any(is.na(uber_request_data$driver.id) & !is.na(uber_request_data$drop.timestamp))


#Converting the date into proper formats

#Function to change the timestamp column containing 2 different foramts into a standardized R timestamp format
char_to_ts<-function(ts)
{
    ts_temp<-strptime(ts,"%d/%m/%Y %H:%M")
    ts_temp[is.na(ts_temp)]<-strptime(ts,"%d-%m-%Y %H:%M")[is.na(ts_temp)]
    ts_temp
}

uber_request_data$request.timestamp<-char_to_ts(uber_request_data$request.timestamp)
uber_request_data$drop.timestamp<-char_to_ts(uber_request_data$drop.timestamp)

#unclass(as.POSIXlt(Sys.time()))

##Deriving metrics

Days_of_week<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

#Deriving day ,hour and week of day
uber_request_data$request.day<-uber_request_data$request.timestamp$mday    
uber_request_data$request.hr<-uber_request_data$request.timestamp$hour
uber_request_data$request.week.of.day<-Days_of_week[uber_request_data$request.timestamp$wday]
uber_request_data$drop.hr<-uber_request_data$drop.timestamp$hour

#Derived metric to identify the time taken for a request to be completed
uber_request_data$time_taken<-uber_request_data$drop.timestamp - uber_request_data$request.timestamp
uber_request_data$time_taken<-as.numeric(uber_request_data$time_taken)
any(!is.na(uber_request_data$time_taken) & uber_request_data$time_taken<1)

#Converting the date to POSIXct ,since dplyr doesnt support POSIXlt
uber_request_data$request.timestamp<-as.POSIXct(uber_request_data$request.timestamp)
uber_request_data$drop.timestamp<-as.POSIXct(uber_request_data$drop.timestamp)
    

############################## Univariate analysis to understand the frequency of variables ####################################


#The frequency of requests on different pickup points
plot1<-ggplot(uber_request_data,aes(pickup.point))+geom_bar(fill="steelblue") +
      geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Pickup Point") + ylab("No. of Requests")
plot1
#The frequency of requests on the status
plot2<-ggplot(uber_request_data,aes(status))+geom_bar(fill="steelblue")+
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Status") + ylab("No. of Requests")
plot2
#Freqeuncy of request based on the day
plot3<-ggplot(uber_request_data,aes(request.day))+geom_bar(fill="steelblue") + 
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Day if month") + ylab("No. of Requests")
plot3
#The freqency of requests during a particular hour 
plot4<-ggplot(uber_request_data,aes(request.hr))+geom_histogram(fill="steelblue",bins=24)+
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Hour of the day") + ylab("No. of Requests")
plot4

#Freqeuncy of request based on Day of  week
plot5<-ggplot(uber_request_data,aes(request.week.of.day))+geom_bar(fill="steelblue") + 
    geom_text(stat='count',aes(label=..count..),vjust=-0.2) + xlab("Day of Week") + ylab("No. of Requests")
plot5

#Mean request completion time
avg_time_segment<-uber_request_data %>% group_by(pickup.point) %>% 
    summarise(avg.req.completion=round(mean(time_taken,na.rm=T),2))

plot6<-ggplot(avg_time_segment,aes(pickup.point,avg.req.completion))+geom_bar(stat="identity",fill="steelblue")+
    geom_text(stat='identity',aes(label=avg.req.completion),vjust=-0.2) + xlab("Pickup Point") + ylab("Mean request completion time\n(minutes)")
plot6


grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,nrow=3)

#Inferences:
#1. There are around 4000 opportunities not converted into revenue either due to unavialibility of cabs or cancelled requests.
#2. There is considerably higher demand of cabs during morning and evening  
#3. There is no impact in the count of requests based the day of week or day of month
#4.The average time take to complete a request to airport is almost equal to and from ariport
#Question:
#Drill down to identify if exist any pattern with respect to the status of a cab request

###############################################Bivariate anlysis ###############################################################

plot7<-ggplot(uber_request_data,aes(pickup.point,fill=status))+geom_bar()+
    geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5)) +
     xlab("Pickup Point") + ylab("No. of Requests")
plot7

request_rel<-uber_request_data %>% group_by(pickup.point,status) %>% 
    summarise(n=n()) %>% mutate(relative_percent=round(100*n/sum(n),2)) %>% arrange(pickup.point)

plot8<-ggplot(request_rel,aes(pickup.point,relative_percent,fill=status))+geom_bar(stat="identity") +
    geom_text(stat='identity',aes(label=relative_percent),position = position_stack(vjust = 0.5)) +
     xlab("Pickup Point") + ylab("Percentage")
plot8

grid.arrange(plot7,plot8,nrow=1)

#Inferences: 
#1. Around 52% of requests from airport is suffering with no avialable cars
#2. 30% of reequest from city to airport are cancelled

#Question
#Look for any pattern with respect to the hour of the day

#Status of request with respect to the hour of the day wrapped across different days ofthe  month
plot9 <- ggplot(uber_request_data, aes(x = as.factor(request.hr),fill = status))+geom_bar(position = "dodge") + 
    facet_wrap( ~ uber_request_data$request.day, nrow =5, ncol = 1) + labs(x = "Hour", y = "Number of Requests", fill = "status" )
plot9

#Status of request with respect to the hour of the day wrapped across different days of the month
plot10 <- ggplot(uber_request_data, aes(x = as.factor(request.hr),fill = pickup.point))+geom_bar(position = "dodge") + 
    facet_wrap( ~ uber_request_data$request.day, nrow =5, ncol = 1) + labs(x = "Hour", y = "Number of Requests", fill = "pickup point" )
plot10

#Hourly pattern for the request for the pickup points 
plot11<-ggplot(uber_request_data,aes(request.hr,colour=pickup.point))+geom_line(stat="count")+
    xlab("Hour of the day") + ylab("No. of Requests") +labs(colour='Pickup Point')
plot11

#Inferences: 
#The number of request are more during mornings and evening

#Try to identify which part of day have demand to supply gap
#Binning a hour of day into groups   

levels.hr.bin<-c("Pre-Morning","Morning","Daytime","Evening" ,"Night")

fun.bin.hr<-function(hr){
    ifelse(hr<=4,"Pre-Morning",
           ifelse(hr<=10,"Morning",
                  ifelse(hr<=16,"Daytime",
                         ifelse(hr<=21,"Evening","Night"))))
}

uber_request_data$demand.hr.bin<-fun.bin.hr(uber_request_data$request.hr)
uber_request_data$demand.hr.bin<-factor(uber_request_data$demand.hr.bin,levels=levels.hr.bin)
#uber_request_data$supply.hr.bin<-fun.bin.hr(uber_request_data$drop.hr)
#uber_request_data$supply.hr.bin<-factor(uber_request_data$supply.hr.bin,levels=levels.hr.bin)

#The no of request during different time slots
plot12 <-ggplot(uber_request_data, aes(x = as.factor(demand.hr.bin), fill= as.factor(status))) + geom_bar()+ 
     labs(x = "Time Slot", y = "Number of Requests", fill = "Status" )
plot12

plot13 <-ggplot(uber_request_data, aes(x = as.factor(demand.hr.bin), fill= as.factor(status))) + geom_bar(position="fill")+
    labs(x = "Time Slot", y = "Number of Requests", fill = "Status" )
plot13

grid.arrange(plot12,plot13,nrow=2)

#Problems identified:
#1 Higher cancellation rate in the Morning
#2 Unavaialbility of cars in the Evenings


#Slicing the dataset with only the morning data to address Higher cancellation rate in the Morning
morning_uber_req<- subset(uber_request_data, demand.hr.bin == "Morning")

plot14 <-ggplot(morning_uber_req, aes(x = as.factor(pickup.point), fill= as.factor(status))) + geom_bar()+ 
    labs(x = "pickup point", y = "Number of Requests", fill = "status" )
plot14

plot15 <-ggplot(morning_uber_req, aes(x = as.factor(pickup.point), fill= as.factor(status))) + geom_bar(position="fill")+
    labs(x = "pickup point", y = "Number of Requests", fill = "status" )
plot15

#There is higher cancellation in the city in mornings 
city_morn_req <- subset(morning_uber_req, pickup.point == "City")

relative_city_morn_req<-city_morn_req %>% group_by(status) %>% 
    summarise(n=n()) %>% mutate(relative_percent=round(100*n/sum(n),2)) 

plot16<-ggplot(relative_city_morn_req,aes(status,relative_percent))+geom_bar(stat="identity",fill="steel blue") +
    geom_text(stat='identity',aes(label=relative_percent),position = position_stack(vjust = 0.5)) +
    xlab("status") + ylab("Percentage")
plot16

#Supply in the city during morning
nrow(subset(city_morn_req,status =="Trip Completed" ))
#Demand in the city during morning
nrow(subset(city_morn_req))

#Supply/Demand
nrow(subset(city_morn_req,status =="Trip Completed" ))/nrow(subset(city_morn_req))*100


#Slicing the dataset with only the Evening data to address Unavaialbility of cars in the Evenings
evening_uber_req<- subset(uber_request_data, demand.hr.bin == "Evening")

plot17 <-ggplot(evening_uber_req, aes(x = as.factor(pickup.point), fill= as.factor(status))) + geom_bar()+ 
    labs(x = "pickup point", y = "Number of Requests", fill = "Status" )
plot17

plot18 <-ggplot(evening_uber_req, aes(x = as.factor(pickup.point), fill= as.factor(status))) + geom_bar(position="fill")+
    labs(x = "pickup point", y = "Number of Requests", fill = "Status" )
plot18
  

#There is higher car unavialability in the airport in evening
airport_morn_req <- subset(evening_uber_req, pickup.point == "Airport")

relative_airport_morn_req<-airport_morn_req %>% group_by(status) %>% 
    summarise(n=n()) %>% mutate(relative_percent=round(100*n/sum(n),2)) 

plot16<-ggplot(relative_airport_morn_req,aes(status,relative_percent ))+geom_bar(stat="identity",fill="steel blue") +
    geom_text(stat='identity',aes(label=relative_percent),position = position_stack(vjust = 0.5)) +
    xlab("Pickup Point") + ylab("Percentage")
plot16

#Supply in the city during morning
nrow(subset(airport_morn_req,status =="Trip Completed" ))
#Demand in the city during morning
nrow(subset(airport_morn_req))

#Supply/Demand
nrow(subset(airport_morn_req,status =="Trip Completed" ))/nrow(subset(airport_morn_req))*100

#Inferences
#1.73% of requests were cancelled due to no cars available in the airport
#2.Only around 20% of requests in the airport in evening has been successfuly converted into revenue


#Conclusion
#1.47% of requests were cancelled by drivers from city to airport
#2.Only around 29% of requests in the city in mornings has been successfully converted into revenue
#3.73% of requests were cancelled due to no cars available in the airport in evenings
#4.Only around 20% of requests in the airport in evenings has been successfuly converted into revenue


