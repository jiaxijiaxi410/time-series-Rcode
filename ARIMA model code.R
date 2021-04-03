rm(all)
library(forecast)

##We will R to simulate and plot some data from simple ARIMA models.

##Use the following R code to generate data from an AR(1) model with ϕ1=0.6ϕ1=0.6 and σ2=1.
#The process starts with y0=0.

y <- ts(numeric(500))

e <- rnorm(500)

for(i in 2:500)
  y[i] <- 0.6*y[i-1] + e[i]

#Produce a time plot for the series.
plot(y)
acf(y)
pacf(y)

# How does the plot change as you change ϕ1?

y2 <- ts(numeric(500))

for(i in 2:500)
  y2[i] <- 0.9*y2[i-1] + e[i]

plot(y2)
acf(y2)
pacf(y2)

#compare the two graphs

par(mfrow=c(2,1))
plot(y)
plot(y2)

#Write your own code to generate data from an MA(1) model with θ1=0.6 and σ2=1.
y3 <- ts(numeric(500))

for(i in 2:500)
  y3[i] <- 0.6*e[i-1] + e[i]



#Produce a time plot for the series. How does the plot change as you change θ1?
plot(y3)

y4 <- ts(numeric(500))

for(i in 2:500)
  y4[i] <- 0.95*e[i-1] + e[i]

par(mfrow=c(2,1))
plot(y3)
plot(y4)


#Generate data from an ARMA(1,1) model with ϕ1 = 0.6 and θ1=0.6 and σ2=1.

y5 <- ts(numeric(500))

for(i in 2:500)
  y5[i] <- 0.6*y5[i-1]+0.6*e[i-1] + e[i]



#Generate data from an AR(2) model with ϕ1=0.3 and ϕ2=-0.8 and σ2=1. (Note that these parameters will give a non-stationary series.)

y6 <- ts(numeric(500))

for(i in 3:500)
  y6[i] <- 0.3*y6[i-1]-0.8*y6[i-2] + e[i]

#Graph the latter two series and compare them.

par(mfrow=c(2,1))
plot(y5)
plot(y6)

#We can try to fit an AR(1) and check the coefficients

fit<-arima(y,order=c(1,0,0))
summary(fit)

#Now try fitting an AR(2) on y. How does it perform?

#the auto.arima command uses an algorithm to pin down the best arima model.
auto.arima(y)

#Try auto.arima on y2-y6.

#Look at the residuals
#The ACF plot of the residuals from the fit model shows all correlations within the threshold limits
#indicating that the residuals are behaving like white noise.

res<-residuals(fit)
acf(res)
pacf(res)

#A portmanteau test returns a large p-value, also suggesting the residuals are white noise.
Box.test(res)

#forecast
plot(forecast(fit, h=20, level=c(80, 95) ))

