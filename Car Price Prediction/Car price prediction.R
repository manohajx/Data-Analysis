################################################ AUTOMOBILE PRICE PREDICITION ##################################################
#A Chinese automobile company Geely Auto aspires to enter the US market by setting up their manufacturing unit there           #
#and producing cars locally to give competition to their US and European counterparts.                                         #
#                                                                                                                              #
#They want to understand the factors on which the pricing of a car depends.                                                    #
#Specifically, they want to understand the factors affecting the pricing of cars in the American marketing,                    #
#since those may be very different from the Chinese market.                                                                    #
#                                                                                                                              #
#Dataset :https://archive.ics.uci.edu/ml/datasets/Automobile                                                                   #
##############################################R version: R-3.4.0 ###############################################################
setwd("C:/Users/johnp/Desktop/DA Academic Projects/Car Price Prediction")
set.seed(2016)
list.of.packages <- c("ggplot2", "tidyr","stringr","car","MASS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggplot2)
library(tidyr)
library(stringr)
library(car)
library(MASS)

#Importing the CARPRICE dataset
carprice<- read.csv("CarPrice_Assignment.csv" ,stringsAsFactors = F)

###############################################Data Cleaning######################################################
#Removing the id column
carprice$car_ID <- NULL

#Checking for duplicates and missing values
any(duplicated(carprice))
sum(is.na(carprice))

#Renaming all the column names to lower case ,for ease
names(carprice)<-tolower(names(carprice))

#Extracting carbrand from carname 
carprice$carbrand<-gsub(" .*$", "", carprice$carname)
table(carprice$carbrand)
carprice$carbrand<-tolower(carprice$carbrand)
carprice$carbrand<-str_replace_all(carprice$carbrand,
                                   c("maxda"="mazda","porcshce"="porsche",
                                     "toyouta"="toyota","vokswagen"="volkswagen","vw"="volkswagen"))
table(carprice$carbrand)
carprice$carname<-NULL


##Converting categorical variables into numerics(dummy variables)
#fueltype
table(carprice$fueltype)
carprice$fueltype<-as.factor(carprice$fueltype)
#levels:diesel-0,gas-1
levels(carprice$fueltype)<-c(0,1)
carprice$fueltype<-as.numeric(levels(carprice$fueltype))[carprice$fueltype]

#aspiration
table(carprice$aspiration)
carprice$aspiration<-as.factor(carprice$aspiration)
#levels:std-0 , turbo-1
levels(carprice$aspiration)<-c(0,1)
carprice$aspiration<-as.numeric(levels(carprice$aspiration))[carprice$aspiration]

#enginelocation
table(carprice$enginelocation)
carprice$enginelocation<-as.factor(carprice$enginelocation)
#levels:front-0 , rear-1
levels(carprice$enginelocation)<-c(0,1)
carprice$enginelocation<-as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

#doornumber
table(carprice$doornumber)
carprice$doornumber<-as.factor(carprice$doornumber)
#levels:two-0 , four-1
levels(carprice$doornumber)<-c(1,0)
carprice$doornumber<-as.numeric(levels(carprice$doornumber))[carprice$doornumber]

#cylindernumber
table(carprice$cylindernumber)
#For better distribution ,binning the cylinder 1-4 as atmost4 and more than 4 as atleat5
carprice$cylindernumber<-ifelse(carprice$cylindernumber %in% c("two","three","four"),
                                "atmost4","atleast5")
carprice$cylindernumber<-as.factor(carprice$cylindernumber)
#levels:atleast5-1 , atmost4-0
levels(carprice$cylindernumber)<-c(1,0)
carprice$cylindernumber<-as.numeric(levels(carprice$cylindernumber))[carprice$cylindernumber]

#carbody
table(carprice$carbody)
carprice<-cbind(carprice[,-which(names(carprice)=='carbody')] ,
                as.data.frame(model.matrix(~carbody,data=carprice))[,-1])

#drivewheel
table(carprice$drivewheel)
carprice<-cbind(carprice[,-which(names(carprice)=='drivewheel')] ,
                as.data.frame(model.matrix(~drivewheel,data=carprice))[,-1])

#enginetype
table(carprice$enginetype)
carprice<-cbind(carprice[,-which(names(carprice)=='enginetype')] ,
                as.data.frame(model.matrix(~enginetype,data=carprice))[,-1])


#fuelsystem
table(carprice$fuelsystem)
carprice<-cbind(carprice[,-which(names(carprice)=='fuelsystem')] ,
                as.data.frame(model.matrix(~fuelsystem,data=carprice))[,-1])


#carbrand
table(carprice$carbrand)
carprice<-cbind(carprice[,-which(names(carprice)=='carbrand')] ,
                as.data.frame(model.matrix(~carbrand,data=carprice))[,-1])


#symboling
table(carprice$symboling)
#Grouping the symboling into safe ,neutral and risky based on their value
carprice$symboling<-ifelse(carprice$symboling<0,"safe",
                           ifelse(carprice$symboling>0,"risky","neutral"))
carprice<-cbind(carprice[,-which(names(carprice)=='symboling')] ,
                as.data.frame(model.matrix(~symboling,data=carprice))[,-1])


##Derived metrics
#Merging citympg and highwaympg into one
carprice$mpg<- (carprice$citympg+carprice$highwaympg)/2
carprice<- carprice[-which(names(carprice) %in% c("citympg","highwaympg"))]

#Dividing the data into measures and categories 
catagorical_var<-names(carprice)[sapply(carprice,function(vector){length(unique(vector)) <= 15 })]
measure_var<-names(carprice)[sapply(carprice,function(vector){length(unique(vector)) > 15 })]
measure_var<-measure_var[-which(measure_var=="price")]

#############################Exploring the variables to understand the data############################################

#corr_data_pt :list of variables names to be used in the linear model
corr_data_pt<-vector()

#outlier:A flag to mark if the a record is an outlier.
carprice$outlier<-NULL

#Analysing the measure variables and picking variables that are at least 0.2 correlated with the price
#,and to identify the outliers.
for(i in measure_var)
{
    readline(prompt="Press enter to check the plots and distribution of variable...")
    cat(i,":\n")
    print("The distribution of data through every 1 percentile-->")
    #Percentile
    print(quantile(carprice[,i],seq(0,1,0.01)))
    #Scatter plot of price and the measure variable
    print(ggplot(carprice,aes(x=carprice[,i],y=price))+geom_point(na.rm = T)+
              geom_smooth(method = "lm",col="green")  + 
              xlab(i) + ylab("price"))
    correlation<-cor(carprice$price,carprice[,i])
    cat("\ncorrelation-->",correlation,"\n")
    #Identifying variable with a correlation of at least 0.2
    if(correlation>=0.2 | correlation <=-0.2 ){
        corr_data_pt<-append(corr_data_pt,i)
    }
    
}
#Flagging the outliers identified based on the plots
carprice$outlier<-ifelse(carprice$wheelbase>115.544,1,0)
carprice$outlier<-ifelse(carprice$carlength>202.480,1,0)
carprice$outlier<-ifelse(carprice$curbweight<1819.72,1,0)
carprice$outlier<-ifelse(carprice$enginesize>302.16 ,1,0)
carprice$outlier<-ifelse(carprice$boreratio<2.9100,1,0)
carprice$outlier<-ifelse(carprice$horsepower>184.00,1,0)
carprice$outlier<-ifelse(carprice$peakrpm>6000,1,0)
carprice$outlier<-ifelse(carprice$mpg>42.42,1,0)

#Checking the number of outliers
sum(carprice$outlier)

#Removing the outliers from the main dataset
carprice<-carprice[carprice$outlier==0,]

for(i in catagorical_var)
{
    readline(prompt="Press enter to check the plots and distribution of variable...")   
    #Scatter plot of price and the dummy categorical variables
    cat(i,":\n")
    print(ggplot(carprice,aes(x=carprice[,i],y=price))+geom_point(na.rm = T)+
              geom_smooth(method = "lm",col="green")  + 
              xlab(i) + ylab("price") )
    #Correlation
    correlation<-cor(carprice$price,carprice[,i])
    cat("correlation-->",correlation,"\n")
    #Identifying variable with a correlation of at least 0.2 and at least 3 data points
    if((correlation>=0.2 | correlation <=-0.2) & sum(carprice[,i])>=3){
        corr_data_pt<-append(corr_data_pt,i)
    }
    
}

#Printing out the list of variables to be used for modelling
cat(corr_data_pt,sep='+\n')

#####################################Linear Regression##################################

#Segregating the data in to test and train
set.seed(2017)
trainindices= sample(1:nrow(carprice), 0.7*nrow(carprice))
train = carprice[trainindices,-64]
test = carprice[-trainindices,-64]

#Model building

#Model based on the columns obtained from corr_data_pt  
model_1 <-lm(price~ 
                 wheelbase+
                 carlength+
                 carwidth+
                 curbweight+
                 enginesize+
                 boreratio+
                 horsepower+
                 mpg+
                 enginelocation+
                 cylindernumber+
                 carbodyhardtop+
                 carbodyhatchback+
                 drivewheelfwd+
                 drivewheelrwd+
                 enginetypeohc+
                 enginetypeohcv+
                 fuelsystem2bbl+
                 fuelsystemmpfi+
                 carbrandbmw+
                 carbrandbuick+
                 carbrandjaguar+
                 carbrandporsche+
                 symbolingrisky
             ,data=train)
#Model summary
summary(model_1)

#Model_1 is better than the base model


#Passing the model_1 into the stepAIC to identify less significant varaiable for removal
step <- stepAIC(model_1, direction="both")
summary(step)

#Removed all the variables that are less significant variables given but the AIC
model_2 <- lm(formula = price ~ 
                  wheelbase + 
                  carwidth +
                  curbweight + 
                  horsepower + 
                  enginelocation + 
                  carbodyhardtop + 
                  carbodyhatchback + 
                  drivewheelfwd + 
                  enginetypeohcv + 
                  carbrandbmw + 
                  carbrandbuick + 
                  carbrandjaguar + 
                  carbrandporsche +
                  symbolingrisky
              , data = train)
#summary of the model
summary(model_2)
#R2=0.9514 and adj R2=0.9459
#VIF for the model
as.data.frame(vif(model_2))
#VIF are pretty high for few variables need to cut them short


#Removing curbweight ,as it has a high vif of 10.5 and a insignificant P value(>0.05)
model_3 <-    lm(formula = price ~ 
                     wheelbase + 
                     carwidth +
                     horsepower + 
                     enginelocation + 
                     carbodyhardtop + 
                     carbodyhatchback + 
                     drivewheelfwd + 
                     enginetypeohcv + 
                     carbrandbmw + 
                     carbrandbuick + 
                     carbrandjaguar + 
                     carbrandporsche +
                     symbolingrisky
                 , data = train)

#summary of the model
summary(model_3)
#R2=0.95 and adj R2=0.9449
#VIF for the model
as.data.frame(vif(model_3))
#VIF are pretty high for few variables

#Removing carbodyhatchback,as it has a insignificant P value (>0.05)
model_4 <-   lm(formula = price ~ 
                    wheelbase + 
                    carwidth +
                    horsepower + 
                    enginelocation + 
                    carbodyhardtop + 
                    drivewheelfwd + 
                    enginetypeohcv + 
                    carbrandbmw + 
                    carbrandbuick + 
                    carbrandjaguar + 
                    carbrandporsche +
                    symbolingrisky
                , data = train)

#summary of the model
summary(model_4)
#R2=0.9489 and adj R2=0.944
#VIF for the model
as.data.frame(vif(model_4))
#VIF values are high for some variables have to lower the 


#Removing symbolingrisky ,as it has a insignificant P value (>0.05)
model_5 <-  lm(formula = price ~ 
                   wheelbase + 
                   carwidth +
                   horsepower + 
                   enginelocation + 
                   carbodyhardtop + 
                   drivewheelfwd + 
                   enginetypeohcv + 
                   carbrandbmw + 
                   carbrandbuick + 
                   carbrandjaguar + 
                   carbrandporsche 
               , data = train)

#summary of the model
summary(model_5)
#R2=0.9481 and adj R2=0.9437
#VIF for the model
as.data.frame(vif(model_5))
#VIF are pretty high for few variables


##Removing enginetypeohcv ,as it has a insignificant P value (>0.05)
model_6 <-  lm(formula = price ~ 
                   wheelbase + 
                   carwidth +
                   horsepower + 
                   enginelocation + 
                   carbodyhardtop + 
                   drivewheelfwd + 
                   carbrandbmw + 
                   carbrandbuick + 
                   carbrandjaguar + 
                   carbrandporsche 
               , data = train)
#summary of the model
summary(model_6)
#R2=0.9468 and adj R2=0.9426
#VIF for the model
as.data.frame(vif(model_6))
#VIF are pretty high for few variables

#Removing wheelbase has a comparitive high vif value and it is not as significant as carwidth
model_7 <-  lm(formula = price ~ 
                   carwidth +
                   horsepower + 
                   enginelocation + 
                   carbodyhardtop + 
                   drivewheelfwd + 
                   carbrandbmw + 
                   carbrandbuick + 
                   carbrandjaguar + 
                   carbrandporsche 
               , data = train)
#summary of the model
summary(model_7)
#R2=0.9448 and adj R2=0.9409
#VIF for the model
as.data.frame(vif(model_7))
#VIF are pretty high for few variables

#Removing carbrandporsche ,as it has a insignificant P value (>0.05)
model_8 <-  lm(formula = price ~ 
                   carwidth +
                   horsepower + 
                   enginelocation + 
                   carbodyhardtop + 
                   drivewheelfwd + 
                   carbrandbmw + 
                   carbrandbuick + 
                   carbrandjaguar 
               , data = train)
#summary of the model
summary(model_8)
#R2=0.944 and adj R2=0.9405
#VIF for the model
as.data.frame(vif(model_8))
#VIF are pretty high for few variables

#Removing carbodyhardtop, as it is only 95% significant whereas the rest is 99% significant
model_9 <-   lm(formula = price ~ 
                    carwidth +
                    horsepower + 
                    enginelocation + 
                    drivewheelfwd + 
                    carbrandbmw + 
                    carbrandbuick + 
                    carbrandjaguar 
                , data = train)
#summary of the model
summary(model_9)
#R2=0.9417 and adj R2=0.9386
#VIF for the model
as.data.frame(vif(model_9))
#VIF are pretty high for few variables


###############################Model Evaluation and choosing the right one ###############

#Models_2 to Model_9 all have pretty good r-squared value,checking which one is optimum
models<-list(
    model_2,
    model_3,
    model_4,
    model_5,
    model_6,
    model_7,
    model_8,
    model_9)

models<-setNames(models, c("model_2",
                           "model_3",
                           "model_4",
                           "model_5",
                           "model_6",
                           "model_7",
                           "model_8",
                           "model_9"))


#Looping through all the models to understand RSquared for the test data and
#Mean and standard deviation of RSquared  calculated for a sample(full dataset) of size 30
count=0
for(i in models)
{
    readline("Press enter for the model statistics")
    Predicted <- predict(i,test[,-which(names(test)=="price")])
    # Calculate Rsquared for the test data
    rsquared <- cor(test$price,Predicted)^2
    count=count+1
    
    #Taking out samples form the carprice to calculate the rsquared for 1000 iterations
    sample_rsquared<-NULL
    for(j in 1:500)
    {
        random_index<-sample(1:nrow(carprice),30)
        samp_predicted<-predict(i,carprice[random_index,-which(names(carprice)=="price")])
        sample_rsquared<-c(sample_rsquared,cor(samp_predicted,carprice[random_index,]$price)^2)
    }
    #Printing the required data
    cat(names(models[count]),":\n","rsquared: ", 
        rsquared,"\n sample mean rsquared: ",mean(sample_rsquared),
        "\n sample sd rsquared: ",sd(sample_rsquared))
    #Residual plot of errors
    plot(test$price-Predicted,main = "Residuals plot",ylab='Residuals')
    abline(h=0, col = "green")
    rm(random_index,samp_predicted,sample_rsquared,Predicted,rsquared)
}

#From the above data it seems all the 8 models are having a good rsquared values,but all
#are not parsimonious.We pick model_9 which is concise, 99% significant and has standard deviation
#of about 0.03 for 500 smaples which is low and this makes it quite stable model.



#THe intercept formula
#-76193.90 + 1257.06xcarwidth + 59.42xhorsepower + 17712.96xenginelocation - 1640.12xdrivewheelfwd + 10363.21*carbrandbmw  + 
#14587.17xcarbrandbuick + 12144.57xcarbrandjaguar

##########################################################Inference###########################################################

#The driving  factor of the car price in america
#Interpretting the linear equation
#These are the most significant predictors of the carprice 
#carwidth:unit increase in car width increases the price by 1257.06
#horsepower:unit increase in car width increases the price by 59.42
#enginelocation:Rear engine car has a  increase in price by 17712.96
#drivewheelfwd:If the car is drivewheenfwd then the price decrease by 1640.12
#carbrandbmw :If the car is carbrandbmw then the price decrease by 10363.21
#carbrandbuick :If the car is carbrandbuick then the price decrease by 14587.17
#carbrandjaguar:If the car is carbrandjaguar then the price decrease by 12144.57



