library(ggplot2)

#FUNCTIONS
predict_bayesian = function(type, days_ahead, observed_data){
  '
  Function to calculate the posterior predictive distribution of the 
  testing setand create a plot with the 99-CI.
  
  Args:
    type (int): Emergency call type to be used.
    days_ahead (int): The days ahead prediction to calculate its 
      posterior predictive distribution.
    observed_data (data.frame): Dataframe with the real observed 
      values.
    
  Returns:
    results (data.frame): Dataframe with the prediction and its 
      credible intervals and the real observed values for each day.
    prediction_plot (ggplot): Plot of the prediction and its 99-CI 
      with theactual observed values.
  '
  #Download testing predictions from LSTM network
  file = sprintf("type%i/out-test%it%s.csv", type, days_ahead, type)
  LSTM = read.csv(file)
  LSTM = LSTM[-nrow(LSTM),]
  
  #Calculate priors of the model
  LSTM_pred = LSTM[, sprintf('type%i', type)]
  LSTM_var = mean(LSTM[, sprintf('type%i', type)] - 
                    observed_data[, sprintf('type%i', type)])^2
  
  #Calculated posterior predictive distribution with 99% CI
  a = LSTM_pred^2/LSTM_var
  b = LSTM_pred/LSTM_var
  p = b/(b+1)
  post_pred_summary = t(apply(cbind(a,p), 1, 
                              function(x) qnbinom(c(0.005,0.5,0.995), 
                              size=x[1], prob=x[2])))
  colnames(post_pred_summary) = c("ici","median","sci")
  
  #Save predictions and actual observed values
  results = data.frame(day=LSTM$X0, 
                       observations=observed_data[, 
                                    sprintf('type%i', type)], 
                       lstm=LSTM[, 
                            sprintf('type%i', type)], 
                       post_pred_summary)
  results$day = as.Date(results$day)
  
  #Save prediction plot
  prediction_plot = ggplot(results,aes(x=day,y=observations)) + 
    geom_line(group=1, colour='cyan3') +
    geom_line(aes(x=day,y=median),colour="red",group=1) +
    geom_ribbon(aes(x=day, ymin=ici, ymax=sci), alpha = 0.5, group=1) +
    theme_bw() +
    ggtitle(sprintf("Plot of emergency calls for type %i", type)) +
    xlab("Date") +
    ylab(sprintf('Emergency calls of type %i', type)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_date(date_labels = "%Y-%b")
  
  results = list(results, prediction_plot)
  return(results)
}


predict_bayesian_unobserved = function(type, starting_date, observed_data, 
                                       observed_data_test, input_date){
  '
  Function to calculate the prediction for the last unobserved day, 
  consideringhaving observed the previous days to calculate prior 
  variance.
  
  Args:
    type (int): Emergency call type to be used
    starting_date (Date): last date of the testing dataset. The first 
    one to be considered as unobserved.
    observed_data (data.frame): Full dataset with all the days, 
      including unobserved.
    observed_data_test (data.frame): Dataset with only the observed 
      days.
    input_date (list of Date): List with the dates of observed_data
    
  Returns:
    
  '
  LSTM_pred = vector()
  LSTM_var = vector()
  date = vector()
  observed = vector()
  for (days_ahead in 1:7){
    #Download dataframes
    test_file = sprintf("type%i/out-test%it%s.csv", type, days_ahead,
                  type)
    LSTM_test = read.csv(test_file)
    LSTM_test = LSTM_test[-nrow(LSTM_test),]
    LSTM_scenario = read.csv(test_file)
    LSTM_scenario = LSTM_scenario[
      LSTM_scenario[, 'X0'] == starting_date,]
    
    #Calculate prior paramaters
    LSTM_pred = c(LSTM_pred, LSTM_scenario[, sprintf('type%i', type)])
    LSTM_var = c(LSTM_var,
                 mean(LSTM_test[, sprintf('type%i', type)] 
                      - observed_data_test[, 
                            sprintf('type%i', type)])^2)
    
    date = c(date, starting_date + days_ahead - 1)
  }
  
  #Calculate posterior predictive distribution
  a = LSTM_pred^2/LSTM_var
  b = LSTM_pred/LSTM_var
  p = b/(b+1)
  post_pred_summary = t(apply(cbind(a,p), 1, 
                              function(x) qnbinom(c(0.005,0.5,0.995), 
                              size=x[1], prob=x[2])))
  colnames(post_pred_summary) = c("ici","median","sci")
  
  #Save unobserved dates
  date = as.Date(date, origin="1970-01-01")
  for(i in 1:length(date)){
    observed = c(observed,
                 observed_data[input_date==date[i], sprintf('type%i', type)])
  }
  
  #Save the posterior predictive distribution for unobserved days and ob. values
  results = data.frame(post_pred_summary, date, observed)
  
  #Plot of predictions, 99-CI and actual values.
  prediction_plot = ggplot(results,aes(x=date, y=observed)) + 
    geom_line(group=1, colour='cyan3') +
    geom_line(aes(x=date, y=median), colour="red", group=1) +
    geom_ribbon(aes(x=date, ymin=ici ,ymax=sci), alpha = 0.5, group=1) +
    theme_bw() +
    ggtitle(sprintf("Predictive power for type %i", type)) +
    xlab("Date") +
    ylab(sprintf('Emergency calls of type %i', type)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_date(date_labels = "%Y-%b-%d")
  
  results = list(results, prediction_plot)
  return(results)
  
}

#Download general data

load("calls.RData")
data=dat[22:(nrow(dat)-6),c(1,2,3,4,6,7)]
dattrain=dat[22:(2855-6),c(1,2,3,4,6,7)]
dattest=dat[(22+3671):(nrow(dat)-6),c(1,2,3,4,6,7)]

Date=paste(as.character(dat$y+6),as.character(dat$m),sep="-")
Date=paste(Date,as.character(dat$d), sep="-")

#################################################
#################### Type 0 #####################
#################################################


results_type0 = predict_bayesian(type=0, days_ahead=1, observed_data=dattest[-nrow(dattest),])
prediction_type0 = data.frame(results_type0[1])
prediction_plot_type0 = results_type0[2]
prediction_plot_type0

predictive_results_type0 = predict_bayesian_unobserved(type=0, 
                          starting_date=as.Date('2018-10-25'), observed_data=dat,
                          observed_data_test=dattest[-nrow(dattest),], input_date=as.Date(Date))

prediction_type0 = data.frame(predictive_results_type0[1])
prediction_plot_type0 = predictive_results_type0[2]
prediction_plot_type0


#################################################
#################### Type 1 #####################
#################################################


results_type1 = predict_bayesian(type=1, days_ahead=1, observed_data=dattest[-nrow(dattest),])
prediction_type1 = data.frame(results_type1[1])
prediction_plot_type1 = results_type1[2]
prediction_plot_type1

predictive_results_type1 = predict_bayesian_unobserved(type=1, 
                                                       starting_date=as.Date('2018-10-25'), observed_data=dat,
                                                       observed_data_test=dattest[-nrow(dattest),], input_date=as.Date(Date))

prediction_type1 = data.frame(predictive_results_type1[1])
prediction_plot_type1 = predictive_results_type1[2]
prediction_plot_type1


#################################################
#################### Type 2 #####################
#################################################


results_type2 = predict_bayesian(type=2, days_ahead=1, observed_data=dattest[-nrow(dattest),])
prediction_type2 = data.frame(results_type2[1])
prediction_plot_type2 = results_type2[2]
prediction_plot_type2

predictive_results_type2 = predict_bayesian_unobserved(type=2, 
                                                       starting_date=as.Date('2018-10-25'), observed_data=dat,
                                                       observed_data_test=dattest[-nrow(dattest),], input_date=as.Date(Date))

prediction_type2 = data.frame(predictive_results_type2[1])
prediction_plot_type2 = predictive_results_type2[2]
prediction_plot_type2


#################################################
#################### Type 3 #####################
#################################################


results_type3 = predict_bayesian(type=3, days_ahead=1, observed_data=dattest[-nrow(dattest),])
prediction_type3 = data.frame(results_type3[1])
prediction_plot_type3 = results_type3[2]
prediction_plot_type3

predictive_results_type3 = predict_bayesian_unobserved(type=3, 
                                                       starting_date=as.Date('2018-10-25'), observed_data=dat,
                                                       observed_data_test=dattest[-nrow(dattest),], input_date=as.Date(Date))

prediction_type3 = data.frame(predictive_results_type3[1])
prediction_plot_type3 = predictive_results_type3[2]
prediction_plot_type3

