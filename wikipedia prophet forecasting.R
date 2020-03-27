#predicitng data from wikipedia page
#getting wikipedia trend data
library(wikipediatrend)
data <- wp_trend( page = "Shah_Rukh_Khan"
          , from = "2008-01-01"
          , to = "2020-03-20")
data
summary(data)
#replacing missing values by mean of the values
 data$views[data$views==0] <- mean(data$views)
 
library(ggplot2)
par(mfrow = c(1,1))
plot(data$views)
ggplot(data,aes(x=date, y = views))+ geom_point(position = "jitter",alpha = .2) + stat_smooth()

#making a data frame with date and views only
ds <- data$date
y <- data$views
df <- data.frame(ds,y)
qplot(ds,log(y),data = df)
# Forecasting with Facebook's Prophet
library(prophet)
f <- prophet(df)
f

#prediction
future <- make_future_dataframe(f, periods = 365)
tail(future)
forecast <- predict(f, future)
tail(forecast [c('ds','yhat','yhat_lower','yhat_upper')])

# forecast plot
plot(f,forecast)
par(mfrow = c(1,3))
prophet_plot_components(f,forecast)


