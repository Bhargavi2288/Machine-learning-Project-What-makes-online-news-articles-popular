####Read the file
mydata_file<- read.csv("OnlineNewsPopularity.csv")
head(mydata_file)
mydata_file<-mydata_file[1:5000,]
dim(mydata_file)

##### remove outliers from shares 
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

#### call outlier function
dt<-outlierKD(mydata_file, shares)

#### extract only the rows with no outliers ### 526 outliers removed
mydata_new_file <-data.frame(dt)
xdata <- mydata_new_file[complete.cases(mydata_new_file), ]
dim(xdata)
#### convert the variables to factors
xdata$data_channel_is_bus =factor(xdata$data_channel_is_bus)
is.factor(xdata$data_channel_is_bus)
xdata$data_channel_is_lifestyle =factor(xdata$data_channel_is_lifestyle)
is.factor(xdata$data_channel_is_lifestyle)

xdata$data_channel_is_entertainment =factor(xdata$data_channel_is_entertainment)
is.factor(xdata$data_channel_is_entertainment)

xdata$data_channel_is_socmed =factor(xdata$data_channel_is_socmed)
is.factor(xdata$data_channel_is_socmed)
xdata$data_channel_is_world =factor(xdata$data_channel_is_world)
xdata$data_channel_is_tech =factor(xdata$data_channel_is_tech)

is.factor(xdata$data_channel_is_world)
is.factor(xdata$data_channel_is_tech)

xdata$is_weekend =factor(xdata$is_weekend)
is.factor(xdata$is_weekend)
xdata$weekday_is_monday =factor(xdata$weekday_is_monday)
is.factor(xdata$weekday_is_monday)
xdata$weekday_is_tuesday =factor(xdata$weekday_is_tuesday)
is.factor(xdata$weekday_is_tuesday)
xdata$weekday_is_wednesday =factor(xdata$weekday_is_wednesday)
is.factor(xdata$weekday_is_wednesday)
xdata$weekday_is_thursday =factor(xdata$weekday_is_thursday)
is.factor(xdata$weekday_is_thursday)
xdata$weekday_is_friday =factor(xdata$weekday_is_friday)
is.factor(xdata$weekday_is_friday)
xdata$weekday_is_sunday =factor(xdata$weekday_is_sunday)
is.factor(xdata$weekday_is_sunday)
xdata$weekday_is_saturday =factor(xdata$weekday_is_saturday)
is.factor(xdata$weekday_is_saturday)


### remove url column
xdata<-xdata[,-1]
dim(xdata)
###install.packages("dummies")
### create dummy variables 
library(dummies)
xdata.new <- dummy.data.frame(xdata, sep = ".")
names(xdata.new)

## standardise and  center the data for columns except factor variables and remove timedelta column 
dim(xdata.new)
xdata_standard<-xdata.new[, c(-1,-13:-24,-37:-52,-74)]

for (i in 1:ncol(xdata_standard))
{
  
  xdata_standard[, i]<-scale(xdata_standard[,i], scale = TRUE, center = TRUE)
  
}

myxdf<-xdata[,c(13:18,31:38,60)]

head(xdata_standard)
### construct a dataframe for the data with standardised variables
mydf<- data.frame( xdata_standard, myxdf)

dim(mydf)
head(mydf)
num_obs<-nrow(mydf)
num_obs
#################################################
set.seed(123456)
##install.packages("kernlab")
library(kernlab)
##install.packages("e1071")
library(e1071)
randomsort <- sample(1:num_obs, num_obs, replace=FALSE)

# lists of observations for train, validate, test
trainloc <- randomsort[1:floor(0.5*num_obs)]
valloc <- randomsort[(floor(0.5*num_obs)+1):floor(0.75*num_obs)]
testloc <- randomsort[(floor(0.75*num_obs)+1):num_obs]
### Number of records in train , validate and test
length(trainloc)
length(valloc)
length(testloc)

df_file<- data.frame(mydf[,])
### write the file to be loaded to JMP
write.csv(df_file, file = 'data_JMP.csv', row.names = T)

####******************************************************SVM for regression###############################

### train data  for linear kernel
model_svm <- svm(shares ~ . , data=mydf[trainloc,], kernel="linear")
### tune the SVM on Validation data set for different parameters  for radial basis kernel
tuneResult <- tune(svm, shares ~ .,  data = mydf[valloc,],
                     ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

summary(tuneResult)
###tuning for linear kernel
tuneResult_Linear <- tune(svm, shares ~ .,  data = mydf[valloc,],kernel="linear",
                   ranges = list(epsilon = seq(0,1,0.5), cost = 2^(2:4)))
summary(tuneResult_Linear)
###tuning for  polynomial kernel
tuneResult_polynomial <- tune(svm, shares ~ .,  data = mydf[valloc,],kernel="polynomial",degree=3,
                          ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:4)))

summary(tuneResult_polynomial)


####select the best model
tunedModel<-tuneResult$best.model
tunedModel.linear<-tuneResult_Linear$best.model
tunedModel.poly<-tuneResult_polynomial$best.model 
#Print optimum value of parameters for tuned Model
print(tunedModel)
print(tunedModel.linear)
print(tunedModel.poly)

### applying the best svm  from each of the kernels on  validation data set SVM
validsvm1 <- svm( shares ~ .,  data = mydf[valloc,],kernel="radial",
                 gamma=0.0169, epsilon =0.6, cost=4)

validsvm2 <- svm( shares ~ .,  data = mydf[valloc,],kernel="linear",  gamma=0.0169, epsilon =0.5, cost=16)

validsvm3 <- svm( shares ~ .,  data = mydf[valloc,],kernel="polynomial", degree=3, 
                     gamma=0.0169, epsilon =1, cost=4, coef.0=0)


### new variables xnewval and ynewval
xnewval<- mydf[valloc, -73]
ynewval<- mydf$shares[valloc]
prd_data_val <- predict(validsvm1, data=xnewval) 

length(prd_data_val) 
length(ynewval)

### validation error
error_val <- ynewval -prd_data_val 

#### validation model RMSE for radial basis
valmodelRMSE_SVM1 <- sqrt(mean((ynewval -prd_data_val )^2))
valmodelRMSE_SVM1

prd_data_val2 <- predict(validsvm2, data=xnewval) 

length(prd_data_val2) 
length(ynewval)

### validation error
error_val2 <- ynewval -prd_data_val2 

#### validation model RMSE for linear kernel
valmodelRMSE_SVM2 <- sqrt(mean((ynewval -prd_data_val2 )^2))
valmodelRMSE_SVM2

prd_data_val3 <- predict(validsvm3, data=xnewval) 

length(prd_data_val3) 
length(ynewval)

### validation error
error_val3 <- ynewval -prd_data_val 

#### validation model RMSE for polynomial kernel
valmodelRMSE_SVM3 <- sqrt(mean((ynewval -prd_data_val3 )^2))
valmodelRMSE_SVM3


## applying the best settings for SVM on test data
###radial kernel is the best with lowest RMSE

###Build the test model on best settings for SVM  
bestsvmtest <- svm( shares ~ .,  data = mydf[trainloc,],kernel="radial",  gamma=0.0169, epsilon =0.5, cost=4)
summary(bestsvmtest)

### create ynew and xnew for actual values of response and predictors for testdata
ynew<- mydf$shares[testloc]
xnew<- mydf[testloc, -73]
dim(xnew)
length(ynew)
## predict using the best SVM for test data
prd_data_test <- predict(bestsvmtest, data=xnew) 
 
length(prd_data_test)
### difference between actual and predicted
error <- ynew-prd_data_test 
### calculate the RMSE for test data
testmodelRMSE_SVM <- sqrt(mean((ynew -prd_data_test )^2))
testmodelRMSE_SVM

################### regression tree#########################################3
library(rpart)
#### fit the regression tree on training set
fit_tree <- rpart(shares~., data=mydf[trainloc,],control=rpart.control(cp=0.01,
  minsplit =20, xval = 10, maxdepth = 30))
summary(fit_tree)
### tuning for number of splits  and split index for validation data set
tuneSplits <- function(k,i) {
  myrpart <- rpart(shares ~ ., data=mydf[valloc,],
                   parms=list(split=splitCriteria[i]),
                   control=rpart.control(minsplit=minsplits[k]))
  myrpartPredict <- predict(myrpart, newdata=mydf[valloc,],
                            type="vector")

  average_squared_Error_Val=sqrt(mean((ynewval-myrpartPredict)^2))
  average_squared_Error_Val
}

trainRpart <- function(i) {
  average_squared_Error_Val <- lapply(1:length(minsplits), FUN=tuneSplits, i)
  average_squared_Error_Val
}

minsplits <- c(5, 10, 20, 30, 40)
splitCriteria <- c("gini", "information")
rpartResults <- lapply(1:length(splitCriteria), FUN=trainRpart)
rpartResults

### build the tree on the best split criterion
### Gini with min split =5  or information gain with minsplit= 5 gives the lowest RMSE
fit_tree_val <- rpart(shares~., data=mydf[valloc,],parms="gini",control=rpart.control(
                                                                       minsplit =5))
summary(fit_tree_val)

### Use the tree to predict the validation data
predicted_data_val<-predict(fit_tree_val, data=mydf[valloc,],
                            type = c("vector"),
                            na.action = na.pass)
### build the tree for the test data set
fit_tree_test <- rpart(shares~., data=mydf[trainloc,],parms="gini",control=rpart.control(
  minsplit =5))
summary(fit_tree_test)
fit_tree_test$variable.importance

library(rpart.plot)
### decision tree plot
prp(fit_tree_test, main="Regression tree for estimating online news popularity",
    type=3,extra =1, fallen=T, branch=.8, round=0, leaf.round=9,
    clip.right.labs=F, under.cex=2,
    box.palette="GnYlRd",
    branch.col="gray", branch.lwd=5,
    under=T, lt=" < ", ge=" >= ", varlen=0 , faclen=0, tweak=0.8)
### predict the test data
predicted_data_test <-predict(fit_tree_test, data=mydf[testloc,],
                         type = c("vector"),
                         na.action = na.pass)
length(predicted_data_test)
length(ynew)
### calculate RMSE for validation and test
average_squared_Errorval=sqrt(mean((ynewval-predicted_data_val)^2))
average_squared_Errorval
### RMSE for test
average_squared_Error_test=sqrt(mean((ynew-predicted_data_test)^2))
average_squared_Error_test


###########################random forest#####################
library(randomForest)

### train the model to grow 500 trees
onlinenews.rf=randomForest(shares~ . ,
                           data =mydf[trainloc,], importance=TRUE,ntree=500 )

### apply the same model on test dataset
###onlinenews.rf.test=randomForest(shares~ . ,
                           ####data =mydf[testloc,], importance=TRUE,ntree=500 )
### print the variables which are important
onlinenews.rf.test$importance
### plot the variable importance plot
varImpPlot(onlinenews.rf, sort=TRUE, n.var=min(10, nrow(onlinenews.rf$importance)),
           type=NULL, class=NULL, scale=TRUE, 
           main="Variable Importance Plot")
### estimate using the model
predict_test_rf<-predict(onlinenews.rf, data=mydf[testloc,])

### construct a dataframe of actual and predicted
z<-data.frame(predict_test_rf, ynew)
### average squared error for random forest
average_squared_Error_test_rf=sqrt(mean((ynew-predict_test_rf)^2))
average_squared_Error_test_rf

###### variable importance model ####
### Cosntruct an SVM 
varimpsvm <- svm( shares ~ LDA_04+weekday_is_saturday+is_weekend+kw_max_avg+kw_avg_avg+kw_avg_max+n_unique_tokens+
                    LDA_00+avg_positive_polarity+data_channel_is_socmed, 
                  data = mydf[testloc,],kernel="radial",
                  gamma=0.0169, epsilon =0.6, cost=4)

prd_data_var_imp <- predict(varimpsvm, data=mydf[testloc,]) 

#### variable importance model RMSE
varmodelRMSE <- sqrt(mean((ynew -prd_data_var_imp )^2))
varmodelRMSE
##############*****************************************************************************
