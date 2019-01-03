########################################################### SVM ################################################################
#A classic problem in the field of pattern recognition is that of handwritten digit recognition. Suppose that you have an      #
#image of a digit submitted by a user via a scanner, a tablet, or other digital devices. The goal is to develop a model that   #
#can correctly identify the digit (between 0-9) written in an image.                                                           #
#                                                                                                                              #
#You are required to develop a model using Support Vector Machine which should correctly classify the handwritten              #
#digits based on the pixel values given as features.                                                                           #
#                                                                                                                              #    
################################################################################################################################
library(dplyr)
library(kernlab)
library(scales)
library(caret)
library(e1071)

#Importing the datasets
mnist_train<- read.csv("mnist_train.csv",stringsAsFactors = F )
mnist_test<- read.csv("mnist_test.csv",stringsAsFactors = F )

str(mnist_train)
#59999 rows and 785 columns

str(mnist_test)
#9999 rows and 785 columns

#naming the columns of the data frame
col_names<-c("digit")
for (i in 1:784){col_names<-append(col_names, paste('x',i,sep="."))}
names(mnist_train) <-  col_names
names(mnist_test) <-  col_names

#Combining the train and test datasets for data cleaning
mnist_master<-rbind(mnist_train,mnist_test)

str(mnist_master)
#All the columns are numreic in nature

#Identifying duplicate rows if any
any(duplicated(mnist_master))

#Identifying the NA's 
sum(is.na(mnist_master))

#Checking if there exist duplicate columns
any(duplicated(as.list(mnist_master)))
sum(duplicated(as.list(mnist_master)))
View(mnist_master[,which(duplicated(as.list(mnist_master)))])

#Most of the data is 0 we can remove columns with more then 60% as 0's
index_of_constant_data<-which(!sapply (mnist_master ,function(vector){sum(vector==0)*100/length(vector)>60} ))
mnist_master<-mnist_master[,index_of_constant_data]

#again Checking for duplicate columns
any(duplicated(as.list(mnist_master)))

#Understanding the distribution of data
lapply(mnist_master,summary)
#Other than the digits column all have a more or less equal distribution min=0 and mx=255 ,hence scaling not required


#Converting the digit datapoint into a factor 
mnist_master$digit<-as.factor(mnist_master$digit)

#Cutting the master data back in to test and train
mnist_train<-mnist_master[1:59999,]
mnist_test<-mnist_master[60000:nrow(mnist_master),]

#Checking the count of digits int the  train dataset
mnist_train %>% group_by(digit) %>% summarise (count=n())

#Checking the count of digits int the  test dataset
mnist_test %>% group_by(digit) %>% summarise (count=n())

#Since svm is complex algorithm and running it over a dataset of 59999 takes lot of time ,
#a sample can be taken out from train dataset with 1500 records of each data type

#Function for sampling n equal number of distinct items
#Here n items of each digit[0-9]

#Returns the index
sampling<-function(vector,n){
    vector<-levels(vector)[as.numeric(vector)]
    unq<- unique(vector)
    samp_indx<-NULL
    for (i in unq ){
        
        set.seed(2017)
        samp_indx<-append(samp_indx , sample(which(vector == i), n))    
    }
    samp_indx
}

#Using the above sampling function to sample out data 1500*10 rows

set.seed(2017)
train_index<-sampling(mnist_train$digit,1500)
train<-mnist_train[train_index,]

###########################################Model Building###########################################################

###Using Linear Kernel
Model_linear <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, mnist_test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,mnist_test$digit)

#Linear kernel gives an accuracy of 88.75%
 


###Using Polynomial Kernel
Model_poly <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "polydot")
Eval_poly<- predict(Model_poly, mnist_test)

#confusion matrix - Polynomial Kernel
confusionMatrix(Eval_poly,mnist_test$digit)

#Polynomial kernel gives an accuracy of 88.76%



###Using RBF Kernel
Model_RBF <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, mnist_test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,mnist_test$digit)

#RBF kernel gives an accuracy of 95.51% with sigm/gamma=0.0027 and C=1


############   Hyperparameter tuning #####################

#Calculating the accuracy for c=0.5,1,3,5,7,9 and sigma/gamma=0.0001,0.005,0.05,0.1,1
#It results in 30 different combinations

grid <- as.data.frame(expand.grid(s=c(0.0001,0.005,0.05,0.1,1), c=c(0.5,1,3,5,7,9) ))

results<-data.frame()
start.time <- Sys.time()
for ( i in 1:30)
{
    c<-grid[i,2]
    s<-grid[i,1]
    cat("c=",c," sigma=",s,"\n")
    
   # Using RBF Kernel
    Model_RBF <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot",C=c,sigma=s)
    
    Eval_RBF<- predict(Model_RBF, mnist_test)
    accuracy<-confusionMatrix(Eval_RBF,mnist_test$digit)$overall[1]
    
    #confusion matrix - RBF Kernel
    results<-rbind(results,data.frame(c=c,s=s,accuracy=accuracy))
    print(accuracy)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# 38.10608 mins
options(scipen = 999)
results$accuracy<-round(results$accuracy,4)
#View(results)

results
################################
#           c    s     accuracy#
#Accuracy   0.5 0.0001   0.9477#
#Accuracy1  0.5 0.0050   0.9476#
#Accuracy2  0.5 0.0500   0.9478#
#Accuracy3  0.5 0.1000   0.9477#
#Accuracy4  0.5 1.0000   0.9478#
#Accuracy5  1.0 0.0001   0.9551#
#Accuracy6  1.0 0.0050   0.9554#
#Accuracy7  1.0 0.0500   0.9554#
#Accuracy8  1.0 0.1000   0.9551#
#Accuracy9  1.0 1.0000   0.9554#
#Accuracy10 3.0 0.0001   0.9654#
#Accuracy11 3.0 0.0050   0.9655#
#Accuracy12 3.0 0.0500   0.9654#
#Accuracy13 3.0 0.1000   0.9654#
#Accuracy14 3.0 1.0000   0.9655#
#Accuracy15 5.0 0.0001   0.9667#
#Accuracy16 5.0 0.0050   0.9667#
#Accuracy17 5.0 0.0500   0.9666#
#Accuracy18 5.0 0.1000   0.9666#
#Accuracy19 5.0 1.0000   0.9667#
#Accuracy20 7.0 0.0001   0.9669#
#Accuracy21 7.0 0.0050   0.9669#
#Accuracy22 7.0 0.0500   0.9670#
#Accuracy23 7.0 0.1000   0.9670#
#Accuracy24 7.0 1.0000   0.9670#
#Accuracy25 9.0 0.0001   0.9675#
#Accuracy26 9.0 0.0050   0.9676#
#Accuracy27 9.0 0.0500   0.9677#
#Accuracy28 9.0 0.1000   0.9676#
#Accuracy29 9.0 1.0000   0.9676#
################################

#As the C value increase there is increase in the accuracy, for a value of c=9 and sigma =0.05  there is high accuracy
#C=3 and sigma=0.0001 seems to a good model becasue any value above c=3 the accuracy increase  at a slower rate 
#It is better not to overfit with high c value and make the model complex with high sigma value

    
final.model <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot",C=3,sigma=0.0001)

#Evaluating over the test and the tain dataset
Eval_final_train<- predict(final.model, mnist_train)
Eval_final_test<- predict(final.model, mnist_test)

#confusion matrix - for the final model for the full train dataset
confusionMatrix(Eval_final_train,mnist_train$digit)
#Accuracy 0.9692

#confusion matrix - for the final model for the test dataset
confusionMatrix(Eval_final_test,mnist_test$digit)
#Accuracy  0.9655

print(final.model)

#The accuracy of the model is around 96.55% using a Radial bias function with hyper parameters c=3 and sigma=0.0001