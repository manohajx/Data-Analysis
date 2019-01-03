""" * Gender prediction using Twitter data *

This code builds the data model with the data present in twitter_data_model_duilding.csv

Three algorithms are applied here:
1.Logistic Regression( regularised)
2.Support Vector Machine
3.RandomForest

"""
from sklearn.model_selection import train_test_split
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from sklearn import metrics
import pandas as pd
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
from sklearn.svm import SVC
import seaborn as sns
from sklearn.model_selection import cross_val_score


#Importing the Dataset for Model Building
twitter_data=pd.read_csv(r'C:\Users\johnp\Desktop\ML assignment\twitter_data_model_duilding.csv',sep=",",index_col='Unnamed: 0')


#Removing features that have correlation of 0.75 or more
new_twitter_data= twitter_data.drop('gender',axis=1)
# Create correlation matrix
corr_matrix = new_twitter_data.corr().abs()

# Select upper triangle of correlation matrix
upper = corr_matrix.where(np.triu(np.ones(corr_matrix.shape), k=1).astype(np.bool))

# Find index of feature columns with correlation greater than 0.95
to_drop = [column for column in upper.columns if any(upper[column] >= 0.70)]


twitter_data=twitter_data.drop(to_drop,axis=1)
#Correlation Plot
f, ax = plt.subplots(figsize=(8, 8))
corr = new_twitter_data.corr()
sns.heatmap(corr, mask=np.zeros_like(corr, dtype=np.bool), cmap=sns.diverging_palette(220, 10, as_cmap=True),
            square=True, ax=ax)


#Performace evaluation function for 
def PerformanceEvaluationMetrics(y_test,y_pred):
    #print("_________________________PERFORMACE EVALUATION_______________________________________")
    confusionMatrix=metrics.confusion_matrix(y_true=y_test, y_pred=y_pred)
    print(confusionMatrix)
    print("\n\n")
    plt.clf()
    plt.imshow(confusionMatrix, cmap=plt.cm.Wistia)
    classNames = ['Female','Male']
    plt.title('Male or Female Confusion Matrix')
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    tick_marks = np.arange(len(classNames))
    plt.xticks(tick_marks, classNames, rotation=45)
    plt.yticks(tick_marks, classNames)
    s = [['TN','FP'], ['FN', 'TP']]
    for i in range(2):
        for j in range(2):
            plt.text(j,i, str(s[i][j])+" = "+str(confusionMatrix[i][j]))
    plt.show()
    print("\n")
    
    TP = confusionMatrix[0,0] # true positive 
    TN = confusionMatrix[1,1] # true negatives
    FP = confusionMatrix[0,1] # false positives
    FN = confusionMatrix[1,0] # false negatives

    # accuracy
    print("accuracy:", round(metrics.accuracy_score(y_test, y_pred),2))
    # precision
    print("precision:", round(metrics.precision_score(y_test, y_pred),2))
    # recall/sensitivity
    print("recall:", round(metrics.recall_score(y_test, y_pred),2))  
    # Sensitivity
    print("sensitivity:",round(TP / float(TP+FN),2))
    # Specificity
    print("specificity:",round(TN / float(TN+FP),2))
    #AUC
    auc_score = metrics.roc_auc_score( y_test, y_pred )
    print("AUC:", round(auc_score,3))
    
    #ROC
    fpr, tpr, thresholds = metrics.roc_curve( y_test, y_pred,
                                                  drop_intermediate = False ) 
    plt.figure(figsize=(6, 4))
    plt.plot( fpr, tpr, label='ROC curve (area = %0.2f)' % auc_score )
    plt.plot([0, 1], [0, 1], 'k--')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate or [1 - True Negative Rate]')
    plt.ylabel('True Positive Rate')
    plt.title('Receiver operating characteristic example')
    plt.legend(loc="lower right")
    plt.show()


#Function for 90% winsorization 
def winsorizarion_90(series):
    low_limit=series.quantile(0.05)
    high_limit=series.quantile(0.95)
    series[series<low_limit]=low_limit
    series[series>high_limit]=high_limit
    return series  

#90% winsorization 
twitter_data.loc[:,twitter_data.apply(lambda x: x.nunique()) > 2] = twitter_data.loc[:,twitter_data.apply(lambda x: x.nunique()) > 2].apply(winsorizarion_90)

    
#Twitter Data Scaling and Diving data into Test and Train
x = twitter_data.drop('gender',axis=1)
y = twitter_data.gender
colnames=x.columns 
sc = StandardScaler()  
X  = sc.fit_transform(x)  
#X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state = 0)



###############################Logistic Regression###################################### 
#Logistic regression
# Make the instance of the model
#log_coeff=pd.DataFrame(model_col,columns=['log_coeff'])

logisticRegrModel = LogisticRegression(random_state = 2018)
logisticRegrModel.fit(X,y)
#logisticPredictions = logisticRegrModel.predict(X_test)
#PerformanceEvaluationMetrics(y_test,logisticPredictions)


params = {"C": [0.005,0.01,0.05,0.1,0.5,1,1.5,2.5,5,10]}
Log_model_cv = GridSearchCV(estimator = logisticRegrModel, param_grid = params, 
                        scoring= ['accuracy','roc_auc','f1'], 
                        cv = 10, 
                        verbose = 4,
                       refit=False,return_train_score = True)
Log_model_cv.fit(X,y)
log_cv_results=pd.DataFrame(Log_model_cv.cv_results_)
log_cv_results

best_accuracy = log_cv_results.mean_test_accuracy.idxmax()
print(round(pd.DataFrame(Log_model_cv.cv_results_).mean_test_accuracy[best_accuracy],3))
print(round(pd.DataFrame(Log_model_cv.cv_results_).std_test_accuracy[best_accuracy],3))
#0.639
#0.013

print(round(pd.DataFrame(Log_model_cv.cv_results_).mean_test_roc_auc[best_accuracy],3))
print(round(pd.DataFrame(Log_model_cv.cv_results_).std_test_roc_auc[best_accuracy],3))
#0.695
#0.014

#print(round(pd.DataFrame(Log_model_cv.cv_results_).mean_test_f1[best_accuracy],3))
#print(round(pd.DataFrame(Log_model_cv.cv_results_).std_test_f1[best_accuracy],3))
#0.636
#0.032


logisticRegrModel = LogisticRegression(random_state = 2018,C=0.01)
logisticRegrModel.fit(X,y)
print(logisticRegrModel.coef_)

co=logisticRegrModel.coef_.reshape(63,)
feat_importances_lg=pd.DataFrame({'features':x.columns ,'logistic_regression_coeff':co})
feat_importances_lg=feat_importances_lg.sort_values('logistic_regression_coeff')
feat_importances_lg.plot.barh(x='features',y='logistic_regression_coeff',figsize=(15,10))




#Building a Base SVM
'''svc= SVC(kernel='poly',random_state=2018)
svc.fit(X_train, y_train)
svm_y= svc.predict(X_test)
PerformanceEvaluationMetrics(y_test,svm_y)

svc= SVC(kernel='linear',random_state=2018)
svc.fit(X_train, y_train)
svm_y= svc.predict(X_test)
PerformanceEvaluationMetrics(y_test,svm_y)'''

svc= SVC(kernel='rbf',random_state=2018)
svm_params = {"C": [1.5]}
svm_model_cv = GridSearchCV(estimator = svc, param_grid = svm_params, 
                        scoring= ['accuracy','roc_auc'], 
                        cv = 10, 
                        verbose = 4,
                       refit=False,return_train_score=True )
svm_model_cv.fit(X,y)


svm_cv_results=pd.DataFrame(svm_model_cv.cv_results_)
svm_cv_results.columns

svm_best_accuracy = svm_cv_results.mean_test_accuracy.idxmax()
print(round(pd.DataFrame(svm_cv_results).mean_test_accuracy[svm_best_accuracy],3))
print(round(pd.DataFrame(svm_cv_results).std_test_accuracy[svm_best_accuracy],3))
#0.655
#0.013

print(round(pd.DataFrame(svm_cv_results).mean_test_roc_auc[svm_best_accuracy],3))
print(round(pd.DataFrame(svm_cv_results).std_test_roc_auc[svm_best_accuracy],3))
#0.713
#0.017

#print(round(pd.DataFrame(svm_cv_results.cv_results_).mean_test_f1[svm_best_accuracy],3))
#print(round(pd.DataFrame(svm_cv_results.cv_results_).std_test_f1[svm_best_accuracy],3))



#0.65 Accuracy

# Instantiate RandomForrest
rf1 = RandomForestClassifier(random_state=2018,n_jobs=5,verbose=3,oob_score=True)
# Train the model on training data

param_grid = { 
    'n_estimators': [999],
    'max_features': ['sqrt'],
    'criterion':['entropy'],
     'min_samples_split' : [2,3,6,8]
}

CV_rfc=GridSearchCV(estimator = rf1, param_grid = param_grid, 
                        scoring= ['accuracy','roc_auc','f1'], 
                        cv = 10, 
                        verbose = 4,
                       refit=False,return_train_score=True )
CV_rfc.fit(X,y)

rfc_cv_results=pd.DataFrame(CV_rfc.cv_results_)
rfc_cv_results.columns

rfc_best_accuracy = rfc_cv_results.mean_test_accuracy.idxmax()
print(round(pd.DataFrame(rfc_cv_results).mean_test_accuracy[rfc_best_accuracy],3))
print(round(pd.DataFrame(rfc_cv_results).std_test_accuracy[rfc_best_accuracy],3))
#0.652
#0.014
print(round(pd.DataFrame(rfc_cv_results).mean_test_roc_auc[rfc_best_accuracy],3))
print(round(pd.DataFrame(rfc_cv_results).std_test_roc_auc[rfc_best_accuracy],3))
#0.715
#0.018


rf_final_model = RandomForestClassifier(random_state=2018,n_jobs=5,verbose=3,oob_score=True,n_estimators=999,max_features='sqrt',criterion='entropy')
rf_final_model.fit(X,y)
feat_importances_rf=pd.DataFrame({'features':colnames,'feature_importance':rf_final_model.feature_importances_})
feat_importances_rf=feat_importances_rf.sort_values('feature_importance')
feat_importances_rf.plot.barh(x='features',y='feature_importance',figsize=(15,10))

feat_importances_rf[:2]



importances = rf_final_model.feature_importances_
std = np.std([tree.feature_importances_ for tree in rf_final_model.estimators_],
             axis=0)
indices = np.argsort(importances)[::-1] [:15] 

# Print the feature ranking
print("Feature ranking:")


# Plot the feature importances of the forest
plt.figure(figsize=(15,10))
plt.title("Feature importances")
plt.barh(x.columns[indices], importances[indices],
       color="r", xerr=std[indices], align="center")
plt.show()




