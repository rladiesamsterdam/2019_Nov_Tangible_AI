#install.packages("ggplot2")
library(ggplot2)

#READ DATA
data_model <- read.csv("C:\\Users\\XXX\\Desktop\\Data\\191118 Step 4 - Data.csv", sep=";")

#Make sure the variable date has the correct data type (Date)
data_model$date <- XXX

#ESTIMATE MODEL
# We want the trainset to consist of observations between 01-01-2014 and 31-10-2015
start_train <- which(data_model$date==XXX)
end_train   <- which(data_model$date==XXX)
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
model <- lm(calls ~ XXX + XXX + , XXX)
summary(model)

#=====================================================================================================#
#=====================================================================================================#

#PREDICT MODEL
# We want to predict from 01-11-2015 until 29-02-2016
start_predict <- XXX
end_predict   <- XXX
data_predict  <- XXX

Prediction = predict(model,data_predict)

#CALCULATE MEAN ABSOLUTE PERCENTAGE ERROR
mape <- mean(abs(XX - XX)/XX)
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

