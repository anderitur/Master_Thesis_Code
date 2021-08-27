library(ggplot2)

#Functions:
create_error_plot = function(type, observed_data){
  '
  Function to create the violin error plots of the LSTM model.
  
  Args:
    type (int): Type from create the error plot.
    observed (data.frame): Dataframe with the actual observed values.
    
  Returns:
    df_error (data.frame): Dataframe with two columns (error, error_day) that
      has the information of the error and the no. of days ahead prediction.
    error_plot (ggplot instance): Plot of the violin error plot.
  
  '
  bucket = paste("type", as.character(type), sep="")
  
  data1 = read.csv(paste(paste(paste(bucket, "/out-test1t", sep=""), as.character(type), sep=""), ".csv", sep=""))
  data1 = data1[-nrow(data1),]
  error = data1[,bucket] - observed_data[,bucket]
  error_day = rep(1, nrow(observed_data))
  
  data2 = read.csv(paste(paste(paste(bucket, "/out-test2t", sep=""), as.character(type), sep=""), ".csv", sep=""))
  data2 = data2[-nrow(data2),]
  error = c(error, data2[,bucket]- observed_data[,bucket])
  error_day = c(error_day, rep(2, nrow(observed_data)))
  
  data3 = read.csv(paste(paste(paste(bucket, "/out-test3t", sep=""), as.character(type), sep=""), ".csv", sep=""))
  data3 = data3[-nrow(data3),]
  error = c(error, data3[,bucket] - observed_data[,bucket])
  error_day = c(error_day, rep(3, nrow(observed_data)))
  
  data4 = read.csv(paste(paste(paste(bucket, "/out-test4t", sep=""), as.character(type), sep=""), ".csv", sep=""))
  data4 = data4[-nrow(data4),]
  error = c(error, data4[,bucket] - observed_data[,bucket])
  error_day = c(error_day, rep(4, nrow(observed_data)))
  
  data5 = read.csv(paste(paste(paste(bucket, "/out-test5t", sep=""), as.character(type), sep=""), ".csv", sep=""))
  data5 = data5[-nrow(data5),]
  error = c(error, data5[,bucket] - observed_data[,bucket])
  error_day = c(error_day, rep(5, nrow(observed_data)))
  
  data6 = read.csv(paste(paste(paste(bucket, "/out-test6t", sep=""), as.character(type), sep=""), ".csv", sep=""))
  data6 = data6[-nrow(data6),]
  error = c(error, data6[,bucket] - observed_data[,bucket])
  error_day = c(error_day, rep(6, nrow(observed_data)))
  
  data7 = read.csv(paste(paste(paste(bucket, "/out-test7t", sep=""), as.character(type), sep=""), ".csv", sep=""))
  data7 = data7[-nrow(data7),]
  error = c(error, data7[,bucket] - observed_data[,bucket])
  error_day = c(error_day, rep(7, nrow(observed_data)))
  
  df_error = data.frame(error, error_day)
  
  error_plot = ggplot(df_error, aes(x=factor(error_day), y=error, color=factor(error_day)))+
    geom_violin()+
    xlab("Day of predictions ahead") +
    ylab('Absolute error') +
    ggtitle(sprintf('Violin plot for error of type %i',type)) +
    theme_bw() +
    theme(legend.position="none", plot.title = element_text(hjust = 0.5))
  
  
  return (list(df_error, error_plot))
}


create_LSTM_predictions = function(type, observed_data, date){
  '
  Function to create two dataframes sorted by actual calls and date for
  just one day ahead prediction.
  
  Args:
    type (int): Type from create the error plot.
    observed (data.frame): Dataframe with the actual observed values.
    date (list): List with the date values.
    
  Returns:
    df_observed (data.frame): Dataframe with two columns (value, date) that
      has the information of the observed value of each day.
    df_predicted (data.frame): Dataframe with two columns (value, date) that
      has the information of the predicted value of each day.
  
  '
  bucket = paste("type", as.character(type), sep="")
  value = observed_data[,bucket]
  df_observed = data.frame(value, date)
  value = read.csv(paste(paste(paste(bucket, "/out-test1t", sep=""), as.character(type), sep=""), ".csv", sep=""))[, bucket] 
  value = value[-length(value)]
  df_predicted = data.frame(value, date)
  
  return (list(df_observed, df_predicted))
}

#Download and prepare general data
data=read.csv("calls_final.csv")[,-c(1,2)]
Date=paste(as.character(data$y+6),as.character(data$m),sep="-")
Date=paste(Date,as.character(data$d), sep="-")

first_train = '2007-9-1'
first_train_index = which(Date == first_train)
last_train = '2015-6-25'
last_train_index = which(Date == last_train)
data_train = data[first_train_index: last_train_index,]

first_val = '2015-6-26'
first_val_index = which(Date == first_val)
last_val = '2017-8-18'
last_val_index = which(Date == last_val)
data_val = data[first_val_index: last_val_index,]

reference = read.csv("type0/out-test7t0.csv")
first_test_day = reference$X0[1]
first_test_day_index = which(Date == first_test_day)
datatest = data[first_test_day_index: (nrow(data)-6),]
datetest = Date[first_test_day_index: (nrow(data)-6)]
datetest = as.Date(datetest)

#################################################
#################### Type 0 #####################
#################################################


error = create_error_plot(type=0, observed_data = datatest[-nrow(datatest),])
error_plot = error[2]
error_plot
df_error = data.frame(error[1])
mae = vector()
for(i in 1:length(unique(df_error$error_day))){
  df_loop = df_error[df_error$error_day==i,]
  mae = c(mae, sum(abs(df_loop$error))/nrow(df_loop))
}
mae

df_type0 = create_LSTM_predictions(type=0, observed_data = datatest[-nrow(datatest),], 
                                   date=datetest[-length(datetest)])
df_observed = data.frame(df_type0[1])
df_predicted = data.frame(df_type0[2])

ggplot(df_observed, aes(x=date,y=value)) +
  geom_line(aes(color="Observed")) +
  geom_line(data=df_predicted, aes(color="LSTM estimation")) +
  labs(color="Type:") +
  ggtitle("Plot of emergency calls for type 0") +
  xlab("Date") +
  ylab('Emergency calls of type 0') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%b")


#################################################
#################### Type 1 #####################
#################################################


error = create_error_plot(type=1, observed_data = datatest[-nrow(datatest),])
error_plot = error[2]
error_plot
df_error = data.frame(error[1])
mae = vector()
for(i in 1:length(unique(df_error$error_day))){
  df_loop = df_error[df_error$error_day==i,]
  mae = c(mae, sum(abs(df_loop$error))/nrow(df_loop))
}
mae

df_type1 = create_LSTM_predictions(type=1, observed_data = datatest[-nrow(datatest),], 
                                   date=datetest[-length(datetest)])
df_observed = data.frame(df_type1[1])
df_predicted = data.frame(df_type1[2])

ggplot(df_observed, aes(x=date,y=value)) +
  geom_line(aes(color="Observed")) +
  geom_line(data=df_predicted, aes(color="LSTM estimation")) +
  labs(color="Type:") +
  ggtitle("Plot of emergency calls for type 1") +
  xlab("Date") +
  ylab('Emergency calls of type 1') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%b")


#################################################
#################### Type 2 #####################
#################################################


error = create_error_plot(type=2, observed_data = datatest[-nrow(datatest),])
error_plot = error[2]
error_plot
df_error = data.frame(error[1])
mae = vector()
for(i in 1:length(unique(df_error$error_day))){
  df_loop = df_error[df_error$error_day==i,]
  mae = c(mae, sum(abs(df_loop$error))/nrow(df_loop))
}
mae

df_type2 = create_LSTM_predictions(type=2, observed_data = datatest[-nrow(datatest),], 
                                   date=datetest[-length(datetest)])
df_observed = data.frame(df_type2[1])
df_predicted = data.frame(df_type2[2])

ggplot(df_observed, aes(x=date,y=value)) +
  geom_line(aes(color="Observed")) +
  geom_line(data=df_predicted, aes(color="LSTM estimation")) +
  labs(color="Type:") +
  ggtitle("Plot of emergency calls for type 2") +
  xlab("Date") +
  ylab('Emergency calls of type 2') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%b")


#################################################
#################### Type 3 #####################
#################################################


error = create_error_plot(type=3, observed_data = datatest[-nrow(datatest),])
error_plot = error[2]
error_plot
df_error = data.frame(error[1])
mae = vector()
for(i in 1:length(unique(df_error$error_day))){
  df_loop = df_error[df_error$error_day==i,]
  mae = c(mae, sum(abs(df_loop$error))/nrow(df_loop))
}
mae

df_type3 = create_LSTM_predictions(type=3, observed_data = datatest[-nrow(datatest),], 
                                   date=datetest[-length(datetest)])
df_observed = data.frame(df_type3[1])
df_predicted = data.frame(df_type3[2])

ggplot(df_observed, aes(x=date,y=value)) +
  geom_line(aes(color="Observed")) +
  geom_line(data=df_predicted, aes(color="LSTM estimation")) +
  labs(color="Type:") +
  ggtitle("Plot of emergency calls for type 3") +
  xlab("Date") +
  ylab('Emergency calls of type 3') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%b")

