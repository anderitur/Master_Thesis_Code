library(ggplot2)
library(reshape2)
library(GGally)

#Exploratory Data Analysis
data = read.csv("calls_final.csv")[,-c(1,2)]
Date = paste(as.character(data$y+6),as.character(data$m),sep="-")
Date = paste(Date,as.character(data$d), sep="-")

first_train_ts = '2013-1-1'
first_train_ts_index = which(Date == first_train_ts)
last_train_ts = '2014-12-31'
last_train_ts_index = which(Date == last_train_ts)

first_train = '2007-9-1'
first_train_index = which(Date == first_train)
last_train = '2015-6-25'
last_train_index = which(Date == last_train)
data_train = data[first_train_index: last_train_index,]

#################################################
#################### Type 0 #####################
#################################################

# Time-series from 2013-2014
df = data.frame('type0'= c(data$type0[first_train_ts_index:last_train_ts_index]), 
                'date' = c(Date[first_train_ts_index:last_train_ts_index]))
df$date = as.Date(df$date)
ggplot(df, aes(x=date, y=type0)) +
  geom_line( color="cyan3") + 
  ggtitle("Plot of emergency calls for type 0") +
  xlab("Date") +
  ylab('Emergency calls of type 0') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%b")


# Frequency histogram (Should be done with ggplot):
y <- data$type0
counts <- table(y)
plot(sort(unique(y)),counts/sum(counts),type='h',xlim=c(0,95),ylim=c(0,0.07),
     xlab="Number of calls of type 0", ylab="frequency", 
     main = 'Frequency histogram for calls of type 0', col = 'cyan3')
ytick <- seq(0,0.07,by=0.02)
axis(side=2, at=ytick, labels = ytick)


# Boxplots
## Precipitation
ggplot(data_train, aes(x=factor(prec, labels=c('Not rain', 'Rain')), y=type0, color=factor(prec, labels=c('Not rain', 'Rain')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 0 \n categorised by precipitation") +
  ylab('Emergency calls for type 0') +
  xlab('Precipitation') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

## Month
ggplot(data_train, aes(x=factor(m, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')),
                       y=type0, color=factor(m, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 0 \n categorised by month") +
  ylab('Emergency calls for type 0') +
  xlab('Months') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

## Weekday
ggplot(data_train, aes(x=factor(wd, levels=c('mon', 'tue', 'wed', 'thu', 'fry', 'sat', 'sun')),
                       y=type0, color=factor(wd, levels=c('mon', 'tue', 'wed', 'thu', 'fry', 'sat', 'sun')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 0 \n categorised by weekday") +
  ylab('Emergency calls for type 0') +
  xlab('Weekdays') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))


# Pairs scatterplot
ggpairs(data_train, columns=1:4,
        aes(color="cyan3"),
        diag = list(discrete = "barDiag"),
        upper = list(continuous = "blank"),
        lower = list(discrete = 'ratio')
        ) +
  ggtitle("Pairs scatterplot of all four emergency calls types") +
  scale_fill_manual(values=c("cyan3")) +
  scale_color_manual(values=c('cyan3')) +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))
#################################################
#################### Type 1 #####################
#################################################

# Time-series from 2013-2014
df = data.frame('type1'= c(data$type1[first_train_ts_index:last_train_ts_index]), 
                'date' = c(Date[first_train_ts_index:last_train_ts_index]))
df$date = as.Date(df$date)
ggplot(df, aes(x=date, y=type1)) +
  geom_line( color="cyan3") + 
  ggtitle("Plot of emergency calls for type 1") +
  xlab("Date") +
  ylab('Emergency calls of type 1') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%b")


# Frequency histogram (Should be done with ggplot):
y <- data$type1
counts <- table(y)
plot(sort(unique(y)),counts/sum(counts),type='h',xlim=c(0,30),ylim=c(0,0.15),
     xlab="Number of calls of type 1", ylab="frequency", 
     main = 'Frequency histogram for calls of type 1', col = 'cyan3')
ytick <- seq(0,0.15,by=0.02)
axis(side=2, at=ytick, labels = ytick)


# Boxplots
## Precipitation
ggplot(data_train, aes(x=factor(prec, labels=c('Not rain', 'Rain')), y=type1, color=factor(prec, labels=c('Not rain', 'Rain')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 1 \n categorised by precipitation") +
  ylab('Emergency calls for type 1') +
  xlab('Precipitation') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

## Month
ggplot(data_train, aes(x=factor(m, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')),
                       y=type1, color=factor(m, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 1 \n categorised by month") +
  ylab('Emergency calls for type 1') +
  xlab('Months') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

## Weekday
ggplot(data_train, aes(x=factor(wd, levels=c('mon', 'tue', 'wed', 'thu', 'fry', 'sat', 'sun')),
                       y=type1, color=factor(wd, levels=c('mon', 'tue', 'wed', 'thu', 'fry', 'sat', 'sun')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 1 \n categorised by weekday") +
  ylab('Emergency calls for type 1') +
  xlab('Weekdays') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))


#################################################
#################### Type 2 #####################
#################################################

# Time-series from 2013-2014
df = data.frame('type2'= c(data$type2[first_train_ts_index:last_train_ts_index]), 
                'date' = c(Date[first_train_ts_index:last_train_ts_index]))
df$date = as.Date(df$date)
ggplot(df, aes(x=date, y=type2)) +
  geom_line( color="cyan3") + 
  ggtitle("Plot of emergency calls for type 2") +
  xlab("Date") +
  ylab('Emergency calls of type 2') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%b")


# Frequency histogram (Should be done with ggplot):
y <- data$type2
counts <- table(y)
plot(sort(unique(y)),counts/sum(counts),type='h',xlim=c(0,10),ylim=c(0,0.9),
     xlab="Number of calls of type 2", ylab="frequency", 
     main = 'Frequency histogram for calls of type 2', col = 'cyan3')
ytick <- seq(0,0.9,by=0.1)
axis(side=2, at=ytick, labels = ytick)


# Boxplots
## Precipitation
ggplot(data_train, aes(x=factor(prec, labels=c('Not rain', 'Rain')), y=type2, color=factor(prec, labels=c('Not rain', 'Rain')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 2 \n categorised by precipitation") +
  ylab('Emergency calls for type 2') +
  xlab('Precipitation') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

## Month
ggplot(data_train, aes(x=factor(m, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')),
                       y=type2, color=factor(m, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 2 \n categorised by month") +
  ylab('Emergency calls for type 2') +
  xlab('Months') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

## Weekday
ggplot(data_train, aes(x=factor(wd, levels=c('mon', 'tue', 'wed', 'thu', 'fry', 'sat', 'sun')),
                       y=type2, color=factor(wd, levels=c('mon', 'tue', 'wed', 'thu', 'fry', 'sat', 'sun')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 2 \n categorised by weekday") +
  ylab('Emergency calls for type 2') +
  xlab('Weekdays') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))


#################################################
#################### Type 3 #####################
#################################################

# Time-series from 2013-2014
df = data.frame('type3'= c(data$type3[first_train_ts_index:last_train_ts_index]), 
                'date' = c(Date[first_train_ts_index:last_train_ts_index]))
df$date = as.Date(df$date)
ggplot(df, aes(x=date, y=type3)) +
  geom_line( color="cyan3") + 
  ggtitle("Plot of emergency calls for type 3") +
  xlab("Date") +
  ylab('Emergency calls of type 3') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%b")


# Frequency histogram (Should be done with ggplot):
y <- data$type3
counts <- table(y)
plot(sort(unique(y)),counts/sum(counts),type='h',xlim=c(0,5),ylim=c(0,0.9),
     xlab="Number of calls of type 3", ylab="frequency", 
     main = 'Frequency histogram for calls of type 3', col = 'cyan3')
ytick <- seq(0,0.9,by=0.1)
axis(side=2, at=ytick, labels = ytick)


# Boxplots
## Precipitation
ggplot(data_train, aes(x=factor(prec, labels=c('Not rain', 'Rain')), y=type3, color=factor(prec, labels=c('Not rain', 'Rain')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 3 \n categorised by precipitation") +
  ylab('Emergency calls for type 3') +
  xlab('Precipitation') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

## Month
ggplot(data_train, aes(x=factor(m, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')),
                       y=type3, color=factor(m, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 3 \n categorised by month") +
  ylab('Emergency calls for type 3') +
  xlab('Months') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))

## Weekday
ggplot(data_train, aes(x=factor(wd, levels=c('mon', 'tue', 'wed', 'thu', 'fry', 'sat', 'sun')),
                       y=type3, color=factor(wd, levels=c('mon', 'tue', 'wed', 'thu', 'fry', 'sat', 'sun')))) +
  geom_boxplot() +
  ggtitle("Boxplot of number of emergency calls for type 3 \n categorised by weekday") +
  ylab('Emergency calls for type 3') +
  xlab('Weekdays') +
  theme_bw() +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))


