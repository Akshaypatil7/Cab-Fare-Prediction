# Cab Fare Prediction 
rm(list = ls())
setwd("C:/Users/admin/Documents/R files")
# #loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "usdm", "caret", "randomForest", "e1071",
      "DataCombine", "doSNOW", "inTrees", "rpart.plot", "rpart",'MASS','xgboost','stats')
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

############# Exploratory Data Analysis #######################
# Changing the data types of variables
train$fare_amount = as.numeric(as.character(train$fare_amount))
train$passenger_count=round(train$passenger_count)

### Removing values which are not within desired range(outlier) depending upon basic understanding of dataset.

# 1.Fare amount has a negative value, which doesn't make sense. A price amount cannot be -ve and also cannot be 0. So we will remove these fields.
train[which(train$fare_amount < 1 ),]
nrow(train[which(train$fare_amount < 1 ),])
train = train[-which(train$fare_amount < 1 ),]

#2.Passenger_count variable
for (i in seq(4,11,by=1)){
  print(paste('passenger_count above ' ,i,nrow(train[which(train$passenger_count > i ),])))
  }
# so 20 observations of passenger_count is consistenly above from 6,7,8,9,10 passenger_counts, let's check them.
train[which(train$passenger_count > 6 ),]
# Also we need to see if there are any passenger_count==0
train[which(train$passenger_count <1 ),]
nrow(train[which(train$passenger_count <1 ),])
# We will remove these 58 observations and 20 observation which are above 6 value because a cab cannot hold these number of passengers.
train = train[-which(train$passenger_count < 1 ),]
train = train[-which(train$passenger_count > 6),]
# 3.Latitudes range from -90 to 90.Longitudes range from -180 to 180.Removing which does not satisfy these ranges
print(paste('pickup_longitude above 180=',nrow(train[which(train$pickup_longitude >180 ),])))
print(paste('pickup_longitude above -180=',nrow(train[which(train$pickup_longitude < -180 ),])))
print(paste('pickup_latitude above 90=',nrow(train[which(train$pickup_latitude > 90 ),])))
print(paste('pickup_latitude above -90=',nrow(train[which(train$pickup_latitude < -90 ),])))
print(paste('dropoff_longitude above 180=',nrow(train[which(train$dropoff_longitude > 180 ),])))
print(paste('dropoff_longitude above -180=',nrow(train[which(train$dropoff_longitude < -180 ),])))
print(paste('dropoff_latitude above -90=',nrow(train[which(train$dropoff_latitude < -90 ),])))
print(paste('dropoff_latitude above 90=',nrow(train[which(train$dropoff_latitude > 90 ),])))
# There's only one outlier which is in variable pickup_latitude.So we will remove it with nan.
# Also we will see if there are any values equal to 0.
nrow(train[which(train$pickup_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])
nrow(train[which(train$dropoff_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])
# there are values which are equal to 0. we will remove them.
train = train[-which(train$pickup_latitude > 90),]
train = train[-which(train$pickup_longitude == 0),]
train = train[-which(train$dropoff_longitude == 0),]

# Make a copy
df=train
# train=df

############# Missing Value Analysis #############
missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val

unique(train$passenger_count)
unique(test$passenger_count)
train[,'passenger_count'] = factor(train[,'passenger_count'], labels=(1:6))
test[,'passenger_count'] = factor(test[,'passenger_count'], labels=(1:6))
# 1.For Passenger_count:
# Actual value = 1
# Mode = 1
# KNN = 1
train$passenger_count[1000]
train$passenger_count[1000] = NA
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Mode Method
getmode(train$passenger_count)
# We can't use mode method because data will be more biased towards passenger_count=1

# 2.For fare_amount:
# Actual value = 18.1,
# Mean = 15.117,
# Median = 8.5,
# KNN = 18.28
sapply(train, sd, na.rm = TRUE)
# fare_amount   pickup_datetime  pickup_longitude 
# 435.968236       4635.700531          2.659050 
# pickup_latitude dropoff_longitude  dropoff_latitude 
# 2.613305          2.710835          2.632400 
# passenger_count 
# 1.266104
train$fare_amount[1000]
train$fare_amount[1000]= NA

# Mean Method
mean(train$fare_amount, na.rm = T)

#Median Method
median(train$fare_amount, na.rm = T)

# kNN Imputation
train = knnImputation(train, k = 181)
train$fare_amount[1000]
train$passenger_count[1000]
sapply(train, sd, na.rm = TRUE)
# fare_amount   pickup_datetime  pickup_longitude 
# 435.661952       4635.700531          2.659050 
# pickup_latitude dropoff_longitude  dropoff_latitude 
# 2.613305          2.710835          2.632400 
# passenger_count 
# 1.263859 
sum(is.na(train))
str(train)
summary(train)

df1=train
# train=df1
#####################Outlier Analysis ##################
# We Will do Outlier Analysis only on Fare_amount just for now and we will do outlier analysis after feature engineering laitudes and longitudes.
# Boxplot for fare_amount
pl1 = ggplot(train,aes(x = factor(passenger_count),y = fare_amount))
pl1 + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)

# Replace all outliers with NA and impute
vals = train[,"fare_amount"] %in% boxplot.stats(train[,"fare_amount"])$out
train[which(vals),"fare_amount"] = NA

#lets check the NA's
sum(is.na(train$fare_amount))

#Imputing with KNN
train = knnImputation(train,k=3)

# lets check the missing values
sum(is.na(train$fare_amount))
str(train)

# df2=train
train=df2
################## Feature Engineering ##########################
# 1.Feature Engineering for timestamp variable
# we will derive new features from pickup_datetime variable
# new features will be year,month,day_of_week,hour
#Convert pickup_datetime from factor to date time
train$pickup_date = as.Date(as.character(train$pickup_datetime))
train$pickup_weekday = as.factor(format(train$pickup_date,"%u"))# Monday = 1
train$pickup_mnth = as.factor(format(train$pickup_date,"%m"))
train$pickup_yr = as.factor(format(train$pickup_date,"%Y"))
pickup_time = strptime(train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
train$pickup_hour = as.factor(format(pickup_time,"%H"))

#Add same features to test set
test$pickup_date = as.Date(as.character(test$pickup_datetime))
test$pickup_weekday = as.factor(format(test$pickup_date,"%u"))# Monday = 1
test$pickup_mnth = as.factor(format(test$pickup_date,"%m"))
test$pickup_yr = as.factor(format(test$pickup_date,"%Y"))
pickup_time = strptime(test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
test$pickup_hour = as.factor(format(pickup_time,"%H"))

# Now we will use month,weekday,hour to derive new features like sessions in a day,seasons in a year,week:weekend/weekday
f = function(x){
  if ((x >=5)& (x <= 11)){
    return ('morning')
  }
  if ((x >=12) & (x <= 16)){
    return ('afternoon')
  }
  if ((x >=17) & (x <= 20)){
    return ('evening')
  }
  if ((x >=21) & (x <= 23)){
    return ('night (PM)')
  }
  if ((x >=0) & (x <= 4)){
    return ('night (AM)')
  }
}
# 2.Calculate the distance travelled using longitude and latitude
deg_to_rad = function(deg){
  (deg * pi) / 180
}
haversine = function(long1,lat1,long2,lat2){
  #long1rad = deg_to_rad(long1)
  phi1 = deg_to_rad(lat1)
  #long2rad = deg_to_rad(long2)
  phi2 = deg_to_rad(lat2)
  delphi = deg_to_rad(lat2 - lat1)
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  R = 6371e3
  R * c / 1000 #1000 is used to convert to meters
}
# Using haversine formula to calculate distance fr both train and test
train$dist = haversine(train$pickup_longitude,train$pickup_latitude,train$dropoff_longitude,train$dropoff_latitude)
test$dist = haversine(test$pickup_longitude,test$pickup_latitude,test$dropoff_longitude,test$dropoff_latitude)

# We will remove the variables which were used to feature engineer new variables
train = subset(train,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,pickup_datetime,pickup_date,pickup_time))
test = subset(test,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,pickup_datetime,pickup_date,pickup_time))

# outlier Analysis on dist variable
# Boxplot for fare_amount
pl1 = ggplot(train,aes(x = factor(passenger_count),y = dist))
pl1 + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)

# Replace all outliers with NA and impute
vals = train[,"dist"] %in% boxplot.stats(train[,"dist"])$out
train[which(vals),"dist"] = NA

#lets check the NA's
sum(is.na(train$dist))

#Imputing with KNN
train = knnImputation(train,k=3)

# lets check the missing values
sum(is.na(train$dist))

head(train)
head(test)
str(train)
str(test)
num_var=c('fare_amount','dist')
cat_var=c('passenger_count','pickup_weekday','pickup_mnth','pickup_yr','pickup_hour')

df3=train
# train=df3

################Feature selection###################

#Correlation analysis for numeric variables
corrgram(train[,num_var],upper.panel=panel.pie, main = "Correlation Plot")

#ANOVA for categorical variables with target numeric variable

#aov_results = aov(fare_amount ~ passenger_count * pickup_hour * pickup_weekday,data = df)
aov_results = aov(fare_amount ~ passenger_count + pickup_hour + pickup_weekday + pickup_mnth + pickup_yr,data = train)

summary(aov_results)

# pickup_weekday has p value less than 0.05
train = subset(df,select=-pickup_weekday)

#remove from test set
test = subset(test,select=-pickup_weekday)

library(car)
dev.off()
par(mfrow=c(1,2))
qqPlot(train$fare_amount)                             # qqPlot, it has a x values derived from gaussian distribution, if data is distributed normally then the sorted data points should lie very close to the solid reference line 
truehist(train$fare_amount)                           # truehist() scales the counts to give an estimate of the probability density.
lines(density(train$fare_amount))  # left skewed      # lines() and density() functions to overlay a density plot on histogram

#################### Splitting train into train and validation subsets ###################
set.seed(1000)
tr.idx = createDataPartition(train$fare_amount,p=0.75,list = FALSE)
train_data = train[tr.idx,]
test_data = train[-tr.idx,]

rmExcept(c("test","train","df",'df1','df2','df3','test_data','train_data'))
###################Model Selection################
#Error metric used to select model is RMSE

#############Linear regression#################
lm_model = lm(fare_amount ~.,data=train_data)

summary(lm_model)

plot(lm_model$fitted.values,rstandard(lm_model),main = "Residual plot",
     xlab = "Predicted values of fare_amount",
     ylab = "standardized residuals")


lm_predictions = predict(lm_model,test_data[,1:5])

qplot(x = test_data[,6], y = lm_predictions, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,6],lm_predictions)
#############Random forest#####################
rf_model = randomForest(fare_amount ~.,data=train)

summary(rf_model)

rf_predictions = predict(rf_model,test_data[,1:5])

qplot(x = test_data[,6], y = rf_predictions, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,6],rf_predictions)
############XGBOOST###########################
train_data_matrix = as.matrix(sapply(train[-6],as.numeric))
test_data_data_matrix = as.matrix(sapply(test_data[-6],as.numeric))

xgboost_model = xgboost(data = train_data_matrix,label = train$fare_amount,nrounds = 15,verbose = FALSE)

summary(xgboost_model)
xgb_predictions = predict(xgboost_model,test_data_data_matrix)

qplot(x = test_data[,6], y = xgb_predictions, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,6],xgb_predictions)
#############Apply on test set####################
###############XGBoost#######################
train_data_matrix2 = as.matrix(sapply(df[-6],as.numeric))
test_data_matrix2 = as.matrix(sapply(test,as.numeric))

xgboost_model2 = xgboost(data = train_data_matrix2,label = df$fare_amount,nrounds = 15,verbose = FALSE)

xgb_predictions2 = predict(xgboost_model2,test_data_matrix2)

xgb_pred_results = data.frame(test_pickup_datetime,"predictions" = xgb_predictions2)
write.csv(xgb_pred_results,"xgb_predictions_R.csv",row.names = FALSE)
          