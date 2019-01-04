install.packages('stringdist')
library(stringdist)
library(dplyr)
library(stringr)
library(tm)
library(caTools)
library(caret)
library(ROCR)
library(car)
library(e1071)
dir()
rm(list=ls())

sum(para_ques_data$target)
nrow(para_ques_data)

#Importing the data set
para_ques_data <- read.csv("train_task1_latest.csv",stringsAsFactors = F,encoding = "UTF-8")
main_test <- read.csv("sdsj_A_test.csv",stringsAsFactors = F,encoding = "UTF-8")

#Checking for duplicates
any(duplicated(para_ques_data))
colSums(is.na(para_ques_data))

any(duplicated(main_test))
colSums(is.na(main_test))

#Feature extraction

para_ques_data$para_word_cnt<- sapply(para_ques_data$paragraph,function(vec){length(scan_tokenizer(vec))})
para_ques_data$ques_word_cnt<- sapply(para_ques_data$question,function(vec){length(scan_tokenizer(vec))})

main_test$para_word_cnt<- sapply(main_test$paragraph,function(vec){length(scan_tokenizer(vec))})
main_test$ques_word_cnt<- sapply(main_test$question,function(vec){length(scan_tokenizer(vec))})


#Removing the stop words
russian.stop.words<-stopwords("russian")

for (i in 1:nrow(para_ques_data))
{
    print(i)
    
    para_ques_data$question1[i]<-tolower(gsub("[[:punct:]]", "",para_ques_data$question[i]))
    para_ques_data$question1[i] <- 
        paste(scan_tokenizer(para_ques_data$question1[i])[!(scan_tokenizer(para_ques_data$question1[i]) %in% russian.stop.words) ],collapse = " ")
    
    para_ques_data$paragraph1[i]<-tolower(gsub("[[:punct:]]", "",para_ques_data$paragraph[i]))
    para_ques_data$paragraph1[i] <- 
        paste(scan_tokenizer(para_ques_data$paragraph1[i])[!(scan_tokenizer(para_ques_data$paragraph1[i]) %in% russian.stop.words) ],collapse = " ")
    
}

question<-para_ques_data$question
paragraph<-para_ques_data$paragraph 
para_ques_data$question<-para_ques_data$question1
para_ques_data$paragraph <- para_ques_data$paragraph1
para_ques_data$question1<-NULL
para_ques_data$paragraph1<-NULL


#Removing the stop words
russian.stop.words<-stopwords("russian")

for (i in 1:nrow(main_test))
{
    print(i)
    
    main_test$question1[i]<-tolower(gsub("[[:punct:]]", "",main_test$question[i]))
    main_test$question1[i] <- 
        paste(scan_tokenizer(main_test$question1[i])[!(scan_tokenizer(main_test$question1[i]) %in% russian.stop.words) ],collapse = " ")
    
    main_test$paragraph1[i]<-tolower(gsub("[[:punct:]]", "",main_test$paragraph[i]))
    main_test$paragraph1[i] <- 
        paste(scan_tokenizer(main_test$paragraph1[i])[!(scan_tokenizer(main_test$paragraph1[i]) %in% russian.stop.words) ],collapse = " ")
    
}

questiont<-main_test$question
paragrapht<-main_test$paragraph 
main_test$question<-main_test$question1
main_test$paragraph <- main_test$paragraph1
main_test$question1<-NULL
main_test$paragraph1<-NULL




word_match_count<- function(para,ques){
    
    ques<-unlist(scan_tokenizer(ques))
    ques<-unique(ques)
    count=0
    cnt=0
    cnt_vec<-0
    for ( i in 1:length(ques)) 
    { 
        temp_cnt<-str_count(para,paste("\\b",ques[i],"\\b",sep=""))
        cnt_vec<-c(cnt_vec,temp_cnt)
            
            count<- count + temp_cnt

        if(temp_cnt>=1){
            cnt=cnt+1
        }
        
    }
    
    return(c(count,max(cnt_vec),cnt))    
}


word_match_count2<- function(para,ques){
    
    ques<-unlist(scan_tokenizer(ques))
    ques<-unique(ques)
    count=0
    cnt=0
    cnt_vec<-0
    for ( i in 1:(length(ques)-1)) 
    { 
            temp_cnt<-str_count(para,paste("\\b",ques[i]," ",ques[i+1],"\\b",sep=""))
            cnt_vec<-c(cnt_vec,temp_cnt)
                
                count<- count + temp_cnt

            if(temp_cnt>=1){
                cnt=cnt+1
            }
        
    }
    
    return(c(count,max(cnt_vec),cnt))    
}


word_match_count3<- function(para,ques){
    
    ques<-unlist(scan_tokenizer(ques))
    ques<-unique(ques)
    count=0
    cnt=0
    cnt_vec<-0
    for ( i in 1:(length(ques)-2)) 
    { 
        temp_cnt<-str_count(para,paste("\\b",ques[i]," ",ques[i+1]," ",ques[i+2],"\\b",sep=""))
        cnt_vec<-c(cnt_vec,temp_cnt)
        
        count<- count + temp_cnt
        
        if(temp_cnt>=1){
            cnt=cnt+1
        }
        
    }
    
    return(c(count,max(cnt_vec),cnt))    
}





match.cnt<-NULL
max.cnt<-NULL
uniq.cnt<-NULL
match.cnt2<-NULL
max.cnt2<-NULL
uniq.cnt2<-NULL

for (i in 1: nrow(para_ques_data)){
    
    print(i)
    res<-word_match_count(para_ques_data$paragraph[i],para_ques_data$question[i])
    res2<-word_match_count2(para_ques_data$paragraph[i],para_ques_data$question[i])

    match.cnt<-c(match.cnt,res[1])
    max.cnt <- c(max.cnt,res[2])
    uniq.cnt <- c(uniq.cnt,res[3])
    match.cnt2<-c(match.cnt2,res2[1])
    max.cnt2 <- c(max.cnt2,res2[2])
    uniq.cnt2 <- c(uniq.cnt2,res2[3])

    
}

para_ques_data$match.cnt <-match.cnt
para_ques_data$match.cnt.ratio <- para_ques_data$match.cnt/(para_ques_data$ques_word_cnt)
para_ques_data$max.cnt<-max.cnt
para_ques_data$word.cnt.ratio <- para_ques_data$para_word_cnt/para_ques_data$ques_word_cnt
para_ques_data$uniq.cnt<-uniq.cnt



para_ques_data$match.cnt2 <-match.cnt2
para_ques_data$match.cnt.ratio2 <- para_ques_data$match.cnt2/(para_ques_data$ques_word_cnt)
para_ques_data$max.cnt2<-max.cnt2
para_ques_data$uniq.cnt2<-uniq.cnt2






tmatch.cnt<-NULL
tmax.cnt<-NULL
tuniq.cnt<-NULL
tmatch.cnt2<-NULL
tmax.cnt2<-NULL
tuniq.cnt2<-NULL
for (i in 1: nrow(main_test)){
    
    print(i)
    res<-word_match_count(main_test$paragraph[i],main_test$question[i])
    res2<-word_match_count2(main_test$paragraph[i],main_test$question[i])
    #res3<-word_match_count3(para_ques_data$paragraph[i],para_ques_data$question[i])
    tmatch.cnt<-c(tmatch.cnt,res[1])
    tmax.cnt <- c(tmax.cnt,res[2])
    tuniq.cnt <- c(tuniq.cnt,res[3])
    tmatch.cnt2<-c(tmatch.cnt2,res2[1])
    tmax.cnt2 <- c(tmax.cnt2,res2[2])
    tuniq.cnt2 <- c(tuniq.cnt2,res2[3])
    #match.cnt3<-c(match.cnt3,res3[1])
    #max.cnt3 <- c(max.cnt3,res3[2])
    #uniq.cnt3 <- c(uniq.cnt3,res3[3])
    
}


main_test$match.cnt <-tmatch.cnt
main_test$match.cnt.ratio <- main_test$match.cnt/(main_test$ques_word_cnt)
main_test$max.cnt<-tmax.cnt
main_test$word.cnt.ratio <- main_test$para_word_cnt/main_test$ques_word_cnt
main_test$uniq.cnt<-tuniq.cnt

main_test$match.cnt2 <-tmatch.cnt2
main_test$match.cnt.ratio2 <- main_test$match.cnt2/(main_test$ques_word_cnt)
main_test$max.cnt2<-tmax.cnt2
main_test$uniq.cnt2<-tuniq.cnt2



osa<-NULL
qgram<-NULL
cosi<-NULL
jacc<-NULL
jw<-NULL
for (i in 1: nrow(para_ques_data) )
{
    print(i)
    re1<-stringdist(para_ques_data$paragraph[i],para_ques_data$question[i],method = 'osa')
    re6<-stringdist(para_ques_data$paragraph[i],para_ques_data$question[i],method = 'qgram')
    re7<-stringdist(para_ques_data$paragraph[i],para_ques_data$question[i],method = 'cosine')
    re8<-stringdist(para_ques_data$paragraph[i],para_ques_data$question[i],method = 'jaccard')
    re9<-stringdist(para_ques_data$paragraph[i],para_ques_data$question[i],method = 'jw')
    
    osa<-c(osa,re1)
    qgram<- c(qgram,re6)
    cosi<- c(cosi,re7)
    jacc<- c(jacc,re8)
    jw<- c(jw,re9)

}

para_ques_data$osa<-osa
para_ques_data$qgram<-qgram
para_ques_data$cosi<-cosi
para_ques_data$jacc<-jacc
para_ques_data$jw<-jw




osat<-NULL
qgramt<-NULL
cosit<-NULL
jacct<-NULL
jwt<-NULL
for (i in 1: nrow(main_test) )
{
    print(i)
    re1<-stringdist(main_test$paragraph[i],main_test$question[i],method = 'osa')
    re6<-stringdist(main_test$paragraph[i],main_test$question[i],method = 'qgram')
    re7<-stringdist(main_test$paragraph[i],main_test$question[i],method = 'cosine')
    re8<-stringdist(main_test$paragraph[i],main_test$question[i],method = 'jaccard')
    re9<-stringdist(main_test$paragraph[i],main_test$question[i],method = 'jw')
    
    osat<-c(osat,re1)
    qgramt<- c(qgramt,re6)
    cosit<- c(cosit,re7)
    jacct<- c(jacct,re8)
    jwt<- c(jwt,re9)
    
}

main_test$osa<-osat
main_test$qgram<-qgramt
main_test$cosi<-cosit
main_test$jacc<-jacct
main_test$jw<-jwt




set.seed(2)
train_indices <- sample.split(para_ques_data$target,SplitRatio=0.7)
train <- para_ques_data[train_indices,]

test <- para_ques_data[!(train_indices),]

model_1<-  glm(target ~ 
                scale(match.cnt.ratio)   
               #+ scale(max.cnt) 
               + scale(uniq.cnt2)
               + scale(ques_word_cnt)
               + scale(max.cnt2)
               + scale(osa)
               + scale(jw)
               #+ scale(match.cnt.ratio2)
               , data=train,family="binomial")
summary(model_1)
vif(model_1)



main_test$target <- predict(model_2,type="response",newdata =main_test)
test$predicted_prob<- predict(model_1,type="response",newdata =test)
test$predicted <- ifelse(test$predicted_prob > 0.3 ,1,0)

confusionMatrix(test$predicted,test$target)

ROCRpred <- prediction(test$predicted, test$target)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
ROCRperf

performance(ROCRpred,'auc')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))




#Function that gives the accuracy , sensitivity and specificity for a cutoff value
perform_fn <- function(cutoff) 
{
    predicted <- factor(ifelse(test$predicted_prob >= cutoff, 1, 0))
    actual<-factor(test$target)
    conf <- confusionMatrix(predicted,actual)
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
}

# Dividing the predictied probabilities  into 1000 parts and generating a matrix of accuracy , sensitivity and specificity
#to det the optimum cutoff


s = seq(min(test$predicted_prob)+0.001,max(test$predicted_prob)-0.001  ,length=1000)
OUT = matrix(0,1000,3)




for(i in 1:1000)
{print(i)
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
cutoff<-mean(cutoff)
cutoff
#0.1861704

#Final list of attrition
test$cutoff.predicted<- ifelse( test$predicted_prob >=cutoff, 1, 0)
conf_final <-confusionMatrix(test$cutoff.predicted, test$target)
conf_final
#Accuracy :0.915
#Sensitivity :0.914
#Specificity : 0.914



#Dataframe with the test actual attrition and test predicted attrition

nw<-test[,names(test) %in% c("target","cutoff.predicted")]





evaluation_tbl<- as.data.frame( nw %>% arrange(desc(cutoff.predicted)) %>% 
                                    mutate(deceile=rep(1:10, each=floor(length(target)/10), length.out=length(target))) %>% 
                                    group_by(deceile) %>% 
                                    summarise(match=sum(cutoff.predicted),observations=n(),non.match=observations-match) %>% 
                                    mutate(cum.match=cumsum(match),gain.per=cum.match*100/sum(match),
                                           non.cum.match=cumsum(non.match),non.cum.perc=non.cum.match*100/sum(non.match),
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





library(ROCR)
ROCRpred <- prediction(test$cutoff.predicted, test$target)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')

performance(ROCRpred,'auc')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))






model_2<- glm(target ~ 
                  scale(match.cnt.ratio)   
              #+ scale(max.cnt) 
              + scale(uniq.cnt2)
              + scale(ques_word_cnt)
              + scale(max.cnt2)
              + scale(osa)
              + scale(jw)
              #+ scale(match.cnt.ratio2)
              , data=para_ques_data,family="binomial")
summary(model_2)
vif(model_2)




main_test$target <- predict(model_2,type="response",newdata =main_test)

View(main_test[,c(1,2,ncol(main_test))])
final<-main_test[,c(1,2,ncol(main_test))]
names(final)[3]<-"prediction"

final$prediction<- ifelse( final$prediction >=cutoff, 1, 0)

write.csv(final,"test_task16.csv")
