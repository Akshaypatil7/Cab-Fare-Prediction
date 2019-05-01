# Checking normality of multivariate data
# https://campus.datacamp.com/courses/multivariate-probability-distributions-in-r/multivariate-normal-distribution?ex=10
uniplot()
# Cab Fare Prediction 
rm(list = ls())
setwd("C:/Users/admin/Documents/R files")
# #loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "usdm", "caret", "randomForest", "e1071",
      "DataCombine", "doSNOW", "inTrees", "rpart.plot", "rpart")
#load Packages
lapply(x, require, character.only = TRUE)
rm(x)

# The details of data attributes in the dataset are as follows:
# pickup_datetime - timestamp value indicating when the cab ride started.
# pickup_longitude - float for longitude coordinate of where the cab ride started.
# pickup_latitude - float for latitude coordinate of where the cab ride started.
# dropoff_longitude - float for longitude coordinate of where the cab ride ended.
# dropoff_latitude - float for latitude coordinate of where the cab ride ended.
# passenger_count - an integer indicating the number of passengers in the cab ride.


# loading datasets
train = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
test = read.csv("test.csv")

# Structure of data
str(train)
str(test)
summary(train)
summary(test)
head(train,5)
head(test,5)

# Exploratory Data Analysis
# Chnaging the data types of variables
df$dteday <- format(as.Date(df$dteday,format="%Y-%m-%d"), "%d")

#########################################################################
#          Visualizing teh data
#########################################################################
hist(df$casual)
hist(df$registered)
hist(df$cnt)

#library(ggplot2)
# CNT according to Season
ggplot(df, aes(fill=cnt, x=season)) +
  geom_bar(position="dodge") + labs(title="cnt ~ season")

# CNT according to holiday
ggplot(df, aes(fill=cnt, x=holiday)) +
  geom_bar(position="dodge") + labs(title="cnt ~ holiday")

# CNT according to season by yr
ggplot(df, aes(fill=cnt, x=season)) +
  geom_bar(position="dodge") + facet_wrap(~yr)+
  labs(title="CNT according to season by yr")

# CNT according to season by workingday
ggplot(df, aes(fill=cnt, x=season)) +
  geom_bar(position="dodge") + facet_wrap(~workingday)+
  labs(title="CNT according to season by workingday")

# CNT according to season by workingday
ggplot(df, aes(fill=cnt, x=workingday)) +
  geom_bar(position="dodge") + facet_wrap(~weekday)+
  labs(title="CNT according to workingday by weekday")

################################################################
#               Outlier Analysis
################################################################

#  #We are skipping outliers analysis becoz we already have an Class Imbalance problem.

#create Box-Plot for outlier analysis-
library(ggplot2)    #Library for visualization-
for(i in 1:length(num_var)){
  assign(paste0("AB",i),ggplot(aes_string(x="cnt",y=(num_var[i])),d=df)+
           geom_boxplot(outlier.color = "Red",outlier.shape = 18,outlier.size = 2,
                        fill="Purple")+theme_get()+
           stat_boxplot(geom = "errorbar",width=0.5)+
           labs(x="Count of Bike",y=num_var[i])+
           ggtitle("Boxplot of count of bikes with",cnames[i]))
}

gridExtra::grid.arrange(AB1,AB2,AB3,ncol=3)
gridExtra::grid.arrange(AB4,AB5,ncol=2)

#Removing outlier by replacing with NA and then impute
for(i in c('temp', 'atemp', 'hum', 'windspeed')){
  print(i)
  outv = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  print(length(outv))
  df[,i][df[,i] %in% outv] = NA
}

#checking all the missing values
#library(DMwR)
sum(is.na(df))

df$hum[is.na(df$hum)] = mean(df$hum,na.rm = T)

df$windspeed[is.na(df$windspeed)] = mean(df$windspeed, na.rm = T)

#df = knnImputation(df, k=3)

sum(is.na(df))
################################################################
#               Feacture Selection
################################################################

#Here we will use corrgram library to find corelation

##Correlation plot
# library(corrgram)
num_var = c('temp', 'atemp', 'hum', 'windspeed','casual','registered','cnt')

corrgram(df[,num_var],
         order = F,  #we don't want to reorder
         upper.panel=panel.pie,
         lower.panel=panel.shade,
         text.panel=panel.txt,
         main = 'CORRELATION PLOT')
#We can see var the highly corr related var in plot marked dark blue. 
#Dark blue color means highly positive cor related

df = subset(df, select=-c(atemp,casual,registered))

# #Checking dependency among different categorical variables
cat_var = c('dteday','season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday','weathersit')
cat_df = df[,cat_var]

for (i in cat_var){
  for (j in cat_var){
    print(i)
    print(j)
    print(chisq.test(table(cat_df[,i], cat_df[,j]))$p.value)
  }
}

#anova test

anova_season =(lm(cnt ~ season, data = df))
summary(anova_season)

anova_year =(lm(cnt ~ yr, data = df))
summary(anova_year)

anova_month =(lm(cnt ~ mnth, data = df))
summary(anova_month)

anova_holiday =(lm(cnt ~ holiday, data = df))
summary(anova_holiday)

anova_weekday =(lm(cnt ~ weekday, data = df))
summary(anova_weekday)

anova_workingday =(lm(cnt ~ workingday, data = df))
summary(anova_workingday)

anova_weathersit =(lm(cnt ~ weathersit, data = df))
summary(anova_weathersit)

anova_season =(lm(cnt ~ dteday, data = df))
summary(anova_season)


################################################
# #check multicollearity
################################################
# #Linear Regression
# library(usdm)

vif(df)
#vifcor(df[,c(7,8,9)])

df = subset(df, select=-c(holiday, workingday,dteday))
#dteday

####################################
## Feature Scaling
####################################
#min(df$cnt) ---->  22
#max(df$cnt) ---->  8714
hist(df$cnt)
colnames(df)

# #Normalization of cnt
#df$total_cnt = (df$cnt - min(df$cnt)) / (max(df$cnt) - min(df$cnt))

################################################################
#               Sampling of Data
################################################################

#sampling
set.seed(12345)
t_index = sample(1:nrow(df), 0.8*nrow(df))
train = df[t_index,] 
test = df[-t_index,]



#Removing All the custom variable from memory
#library(DataCombine)
rmExcept(c("test","train","original_data",'df'))

#library(caret)

#mape
mape = function(actual, predict){
  mean(abs((actual-predict)/actual))*100
}

###########################################
# # # ??? Linear Regression
# ########################################
#
##Linear regression
dumy = dummyVars(~., df)
dummy_df = data.frame(predict(dumy, df))

set.seed(101)
dum_index = sample(1:nrow(dummy_df), 0.8*nrow(dummy_df))
dum_train_df = dummy_df[dum_index,]
dum_test_df = dummy_df[-dum_index,]

#Linear model
lr_model = lm(cnt ~. , data = dum_train_df)
summary(lr_model)

#predictions on Train data set
LR_predict_train = predict(lr_model, dum_train_df[,-32])
plot(dum_train_df$cnt, LR_predict_train,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'LR model')

#evaluation
postResample(LR_predict_train, dum_train_df$cnt)#R-sq = 0.85
mape(dum_train_df$cnt, LR_predict_train)


#predictions on test
LR_predict_test = predict(lr_model, dum_test_df[,-32])
plot(dum_test_df$cnt, LR_predict_test,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'LR model')

#evaluation
postResample(LR_predict_test, dum_test_df$cnt)#R-sq = 0.85
mape(dum_test_df$cnt, LR_predict_test)


# ###########################################
# # # # Decision Tree
# # ########################################
#
##Decison tree

# library(rpart.plot)
# library(rpart)

set.seed(121)
#model
dt_model = rpart(cnt~. , data = train, method = "anova")
summary(dt_model)
plt = rpart.plot(dt_model, type = 5, digits = 2, fallen.leaves = TRUE)

#predictions on train
DT_Predict_train = predict(dt_model, train[,-9])
plot(train$cnt, DT_Predict_train,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'DT model')

#evaluation
postResample(DT_Predict_train, train$cnt)
mape(train$cnt, DT_Predict_train)

#predictions on test
DT_Predict_test = predict(dt_model, test[,-9])
plot(test$cnt, DT_Predict_test,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'DT model')

#evaluation
postResample(DT_Predict_test, test$cnt)
mape(test$cnt, DT_Predict_test)


# ###########################################
# # # # ??? Random Forest
# # ########################################
#
##Random forest
#library(randomForest)
#library(inTrees)
set.seed(101)
#model
rf_model = randomForest(cnt ~. , train, importance = TRUE, ntree = 500)
rf_model

#error plotting
plot(rf_model)

#Variable Importance plot
varImpPlot(rf_model)

#Plotting predict train data using RF model
RF_predict_train = predict(rf_model, train[,-9])
plot(train$cnt, RF_predict_train,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'RF model')

#Train Result
postResample(RF_predict_train, train$cnt)#R-sq = 0.89
mape(train$cnt, RF_predict_train)


#Plotting predict test data using RF model
RF_predict_test = predict(rf_model, test[,-9])
plot(test$cnt, RF_predict_test,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'RF model')

#Test Result
postResample(RF_predict_test, test$cnt)#R-sq = 0.89
mape(test$cnt, RF_predict_test)



#K-fold cross validation function
kfold_train <- function(model){
  x=trainControl(method = "cv",number = 10)
  model= train(cnt ~.,data=train,metric="RMSE",method=model,trControl=x)
  print(model)
  return(model)
}
result <- function(model){
  model = model
  set.seed(101)
  pred = predict(model, train[,-9])
  print(postResample(pred, train$cnt))
  print(mape(train$cnt, pred))
  
  print('Test Results____')
  pred = predict(model,test[,-9])
  print(postResample(pred, test$cnt))
  print(mape(test$cnt, pred))
}

############################################ We have commented KFOLD validation code and Hyper parameter tuning as it takes a lot time ##############
# ###########################
# #CV-Fold check
# ###########################
# library(doSNOW)
# cl <- makeCluster(10) #clustering approach using doSNOW pkg
# registerDoSNOW(cl)

#Random Forest # R2 = 87
# forest = kfold_train('rf')
# result(forest)
# 
# stopCluster(cl)
# 
# #Linear Regression # R2 = 83
# lm_model = kfold_train('lm')
# result(lm_model)
# 
# #Decision Tree # R2 = 60
# dtree = kfold_train('rpart')
# result(dtree)
# # 
# #SVR # R2 = 86
# svr_model = kfold_train('svmPoly')
# result(svr_model)
# # 
# # # stopCluster(cl)



###################################################################
# #             Knowing the right hyper parameters tuning
# # As this process will take a bit time so here i have commented the code 
###################################################################

#Using doSNOW lib for segmenting the clustering onto task as a faster approch
# library(doSNOW)

# # #Best mtry  ======   found best as = 4 
# cl <- makeCluster(6) #clustering approach using doSNOW pkg
# registerDoSNOW(cl)
# 
# trControl <- trainControl(method = "cv",number = 10,search = "grid")
# set.seed(101)
# tuneGrid <- expand.grid(.mtry = c(2:8))
# rf_mtry <- train(cnt~.,data = train,method = "rf",metric = "RMSE",
#                  tuneGrid = tuneGrid,trControl = trControl,importance = TRUE,ntree = 800)
# best_mtry <- rf_mtry$bestTune$mtry              
# print(best_mtry)

# # #Looking for best ntree  ====  found best as = 500
# store_maxtrees <- list()
# tuneGrid <- expand.grid(.mtry = best_mtry)
# for (ntree in c(200, 300, 350, 400, 450, 500, 550, 600, 700,800, 1000)) {
#   set.seed(101)
#   rf_maxtrees <- train(cnt~.,data = train,method = "rf",metric = "RMSE",tuneGrid = tuneGrid,
#                        trControl = trControl,importance = TRUE,ntree = ntree)
#   key <- toString(ntree)
#   store_maxtrees[[key]] <- rf_maxtrees
# }
# results_tree <- resamples(store_maxtrees)
# summary(results_tree)
# 
# stopCluster(cl)



# #############################################################
# ## Final Model Random Forest
# #############################################################
# 
final_model = randomForest(cnt ~. , train, importance = TRUE, ntree = 500)
final_model

#error plotting
plot(final_model)

#Variable Importance plot
varImpPlot(final_model)

#Plotting predict train data using RF model
Final_predict_train = predict(final_model, train[,-9])
plot(train$cnt, Final_predict_train,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'RF model')

#Train Result
postResample(Final_predict_train, train$cnt)#R-sq = 0.89
mape(train$cnt, Final_predict_train)


#Plotting predict test data using RF model
Final_predict_test = predict(final_model, test[,-9])
plot(test$cnt, Final_predict_test,
     xlab = 'Actual values',
     ylab = 'Predicted values',
     main = 'RF model')

#Test Result
postResample(Final_predict_test, test$cnt)#R-sq = 0.89
mape(test$cnt, Final_predict_test)

########################################################################
## Saving the output
rmExcept(c("final_model",'mape',"original_data",'df'))
df$predict_cnt <-  round(predict(final_model, df[,-9]))
original_data$predict_cnt <- df$predict_cnt
write.csv(original_data, 'output_R.csv',row.names = F)
write.csv(original_data[,c("dteday","weathersit","season","mnth",'temp',"hum","windspeed","cnt","predict_cnt")], 'output_R.csv',row.names = F)


