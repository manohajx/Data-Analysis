
############################################ LIST OF FUNCTIONS #################################################################
options(warn=-1)
rm(list=ls())

#Function for 90% winzorisation
winsorization<-function(x){
    qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
    caps <- quantile(x, probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(x, na.rm = T)
    x[x < (qnt[1] - H)] <- caps[1]
    x[x > (qnt[2] + H)] <- caps[2]   
    x
}


#Function to get predicted probabilites to identify the right cut off and result the confusion matrix
class_eval_metrics<- function(probs, test_resp){
    
    
    s = seq(min(probs),max(probs),length=1000)
    OUT = matrix(0,1000,3)  
    for(i in 1:1000)
    {
        OUT[i,] = perform_fn(s[i],probs,test_resp)
    } 
    
    #Picking up a cutoff where the difference between sensitiviyt and specificity is nearly 0
    cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.05)]
    #optimal cutoff is 
    cutoff<-ifelse(is.na(mean(cutoff)),s[which(abs(OUT[,1]-OUT[,2])<0.3)],cutoff)
    print(cutoff)
    test_cutoff_left <- factor(ifelse(probs >=mean(cutoff), 1, 0))
    conf_final <-confusionMatrix(test_cutoff_left, test_resp,positive= "1")
    conf_final
}

#Function that gives the accuracy , sensitivity and specificity for a cutoff value
perform_fn <- function(cutoff,probs,test_resp) 
{
    predicted <- factor(ifelse(probs >=cutoff ,1,0))
    conf <- confusionMatrix(predicted, test_resp,positive= "1")
    acc <- conf$overall[1]
    sens <- conf$byClass[1]
    spec <- conf$byClass[2]
    out <- t(as.matrix(c(sens, spec, acc))) 
    colnames(out) <- c("sensitivity", "specificity", "accuracy")
    return(out)
}

#Function to print importance plot for Decision Tree, Regression and Random Forrest

var_importance <- function(model){
    
    if (any(class(model) %in% c('rpart','glm','lm')))
    {
        var.imp <- data.frame(varImp(model,
                                         type=2))
    }
    if (any(class(model) %in% 'randomForest'))
    {
    var.imp <- data.frame(importance(model,
                                     type=2))
    }
    var.imp$variables<-row.names(var.imp)
    row.names(var.imp)<-NULL
    names(var.imp)<- c('importance','variables')
    var.imp<-var.imp[order(var.imp$importance,decreasing = T),]
    var.imp$variables<- factor(var.imp$variables,levels=var.imp$variables[order(-var.imp$importance)])
    plot<-ggplot(var.imp, aes(x=variables,y=importance)) +geom_bar(stat = "identity",fill="steelblue4") 
    print(plot)
    print(var.imp)
    
}