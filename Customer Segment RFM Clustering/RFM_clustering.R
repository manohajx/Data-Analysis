################################################ ECUSTOMER CLUSTERING  #########################################################
#An Online store wants to identify the data driven segments for the sales dataset.Perform RFM anlaysis to identify customer    #
#segments using clustering algorithm                                                                                           #
#The dataset consists of order level details for around 500k records                                                           #
##############################################R version: R-3.4.0 ###############################################################
library(dplyr)
library(tidyr)
library(ggplot2)

# Importing the data
online.retail <- read.csv("Online Retail.csv", stringsAsFactors=FALSE)
str(online.retail)

#Converting the datapoints names to lowercase 
names(online.retail)<- tolower(names(online.retail))

## Identifying the NA values
colSums(is.na(online.retail))

## Many customerids are NA , remove NA values
order_wise <- na.omit(online.retail)

#Converting invoicedate into R date format
order_wise$invoicedate <- as.Date(order_wise$invoicedate,"%d-%m-%Y %H:%M")

#Calculating the maximum invoicedate 
maximum_dt<-max(order_wise$invoicedate) + 1
maximum_dt

#Calculateing the difference between maximum invoice date and invoice to find the date difference
order_wise$date.diff.frm.max <- maximum_dt-order_wise$invoicedate 

#Calculating the amount for each order
order_wise$amount <- order_wise$quantity * order_wise$unitprice



#Summarizing and calculating customer wise recency , frequeny and monetary

cust_wise<-as.data.frame(order_wise %>% group_by(customerid) %>% 
                             summarise(recency=min(date.diff.frm.max),frequency=n(),monetary=sum(amount)))

#Removing the customerids
RFM<-cust_wise[,-1]
str(RFM)

#Converting the datediff format to numeric for modelling
RFM$recency<-as.numeric(RFM$recency)

## Outlier treatment by just removing outliers if any
box <- boxplot.stats(RFM$monetary)
out <- box$out
RFM1 <- RFM[ !RFM$monetary %in% out, ]

RFM <- RFM1

box <- boxplot.stats(RFM$frequency)
out <- box$out

RFM1 <- RFM[ !RFM$frequency %in% out, ]

RFM <- RFM1

box <- boxplot.stats(RFM$recency)
out <- box$out

RFM1 <- RFM[ !RFM$recency %in% out, ]

RFM <- RFM1


## Standardisation of data
RFM_norm<-sapply(RFM,scale)


##############################################K-Means MODEL BUILDING ##########################################################
## Implementing K-Means algorithm

## Finding the optimal value of K by plotting the r_sq value which represents maximum interclcuster variance and minimum
#intra cluster variance

r_sq<- rnorm(15)

for (number in 1:15){clus <- kmeans(RFM_norm, centers = number, nstart = 100,iter.max = 50)
r_sq[number]<- clus$betweenss/clus$totss
}
plot(r_sq)

## Running the K-Means algorithm for the elbow point k=5
clus5 <- kmeans(RFM_norm, centers = 5, iter.max = 50, nstart = 100)


## Appending the ClusterIDs to RFM data

RFM_km <-cbind(RFM,clus5$cluster)

colnames(RFM_km)[4]<- "clusterid"

#################################################### Cluster Analysis ##########################################################

#Summarizing to identify the mean RFM value in each cluster
mean_summarized1<-as.data.frame(RFM_km %>% group_by(clusterid) %>% 
                                   summarise(mean_amount=mean(monetary), mean_freq=mean(frequency), mean_recency=mean(recency)))
mean_summarized_gather1 <- gather(mean_summarized1,RFM,mean,mean_amount:mean_recency)


#Plots to understand how the mean RFM value vary between clusters

plot1<-ggplot(mean_summarized1, aes(x= factor(clusterid), y=mean_amount)) + geom_bar(stat = "identity",fill="steel blue")
plot2<-ggplot(mean_summarized1, aes(x= factor(clusterid), y=mean_freq)) + geom_bar(stat = "identity",,fill="steel blue")
plot3<-ggplot(mean_summarized1, aes(x= factor(clusterid), y=mean_recency)) + geom_bar(stat = "identity",,fill="steel blue")

grid.arrange(plot1,plot2,plot3,nrow=3)

ggplot(mean_summarized_gather1,aes(factor(RFM),scale(mean)))+geom_bar(stat = "identity",fill="steel blue") +
    facet_wrap(~factor(clusterid))

#Inference
#Cluster 1 has the customers with high monetary and frequency and less recency which is the best 
#segement of customer from bussiness point of view.
#Cluster 3 has the customers with low monetary and frequency and high recency value which is the worst 
#segement of customer from bussiness point of view.
######################################### Hierarchical clustering MODEL BUILDING ################################################

## Calcualting the distance matrix

RFM_dist<- dist(RFM_norm)

## Constructing the dendrogram using single linkage

RFM_hclust1<- hclust(RFM_dist, method="single")
plot(RFM_hclust1)

## Constructing the dendrogram using complete linkage

RFM_hclust2<- hclust(RFM_dist, method="complete")
plot(RFM_hclust2)

## Visualising the cut in the dendrogram

rect.hclust(RFM_hclust2, k=5, border="red")

## Making the cut in the dendrogram

clusterCut <- cutree(RFM_hclust2, k=)

## Appending the ClusterIDs to RFM data

RFM_hc <-cbind(RFM,clusterCut)

colnames(RFM_hc)[4]<- "clusterid"

## Cluster Analysis

mean_summarized2<-as.data.frame(RFM_hc %>% group_by(clusterid) %>% 
                                   summarise(mean_amount=mean(monetary), mean_freq=mean(frequency), mean_recency=mean(recency)))
mean_summarized_gather2 <- gather(mean_summarized,RFM,mean,mean_amount:mean_recency)

ggplot(mean_summarized_gather2,aes(factor(RFM),scale(mean)))+geom_bar(stat = "identity",fill="steel blue") +
    facet_wrap(~factor(clusterid))
plot4<-ggplot(mean_summarized2, aes(x= factor(clusterid), y=mean_amount)) + geom_bar(stat = "identity",fill="steel blue")
plot5<-ggplot(mean_summarized2, aes(x= factor(clusterid), y=mean_freq)) + geom_bar(stat = "identity",,fill="steel blue")
plot6<-ggplot(mean_summarized2, aes(x= factor(clusterid), y=mean_recency)) + geom_bar(stat = "identity",,fill="steel blue")

grid.arrange(plot4,plot5,plot6,nrow=3)

#Inference
#Cluster 3 has the customers with high monetary and frequency and less recency which is the best 
#segement of customer from bussiness point of view.
#Cluster 5 has the customers with low monetary and frequency and high recency value which is the worst 
#segement of customer from bussiness point of view.