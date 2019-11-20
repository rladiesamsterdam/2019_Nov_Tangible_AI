#install.packages("ggplot2")
library(ggplot2)

#READ DATA
data_model <- read.csv("C:\\Users\\XXX\\Desktop\\Data\\191118 Step 4 - Data.csv", sep=";")
data_model$date <- as.Date(data_model$date)

#ESTIMATE MODEL
start_train <- which(data_model$date=='2014-01-01')
end_train   <- which(data_model$date=='2015-10-31')
data_train  <- data_model[start_train:end_train,]

data_train$checkins_pred

#=====================================================================================================#
#=====================================================================================================#

# ==  Drivers  == #
#date  
#weekday	
#weekend_jn	
#calls_last_year	
#calls_last_year_same_weekday	
#calls_last_year_same_weekday_specialdaycorrection	
#checkins_known	
#checkouts_known	
#checkins_pred	
#checkouts_pred

# ==  choose drivers here  == #
model <- lm(calls ~ XXX + YYY + , data=data_train)
summary(model)

#=====================================================================================================#
#=====================================================================================================#

#PREDICT MODEL
start_predict <- which(data_model$date=='2015-11-01')
end_predict   <- which(data_model$date=='2016-02-29')
data_predict  <- data_model[start_predict:end_predict,]

Prediction = predict(model,data_predict)

#CALCULATE MEAN ABSOLUTE PERCENTAGE ERROR
mape <- mean(abs(data_predict$calls - Prediction)/data_predict$calls)
mape


-------------------------------------------------------------------------------------------------------
  

#PLOT

#make data set for plotting

#actuals
n <- nrow(data_model)
data_plot1        <- data.frame(1:n)
data_plot1$date   <- data_model$date
data_plot1$calls  <- data_model$calls
data_plot1$metric <- 'actuals'

#predictions
n <- end_predict - start_predict + 1
data_plot2        <- data.frame(1:n)
data_plot2$date   <- data_predict$date
data_plot2$calls  <- Prediction
data_plot2$metric <- 'prediction'

data_plot         <- rbind(data_plot1,data_plot2)
data_plot$date    <- as.Date(data_plot$date)

#PLOT PREDICTIONS VERSUS REALIZATIONS
ggplot(data=data_plot, 
       aes(x=date, y=calls, color=metric, group = 1)) +
  geom_line() +
  ggtitle("model forecast")

