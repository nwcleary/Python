#Final Exam Nathan Cleary
library(urca)
library(tidyverse)
library(fpp2)
library(ggplot2)
library(forecast)


?elecdemand
view(elecdemand)
autoplot(elecdemand)
df = elecdemand
# 29,25,20,15,23  AVG:22.4

#Heating Degree HD
DA = ifelse(df[,"Temperature"]<18, df[,"Temperature"], 0)
DA_mean = mean(DA)
HD = max(18-DA_mean)

#Cooling Degree CD
DA2 = ifelse(df[,"Temperature"]>18, df[,"Temperature"], 0)
DA2_mean = mean(DA2)
CD = max(DA2_mean-18)

#New df
df2 =data.frame(HD =c(DA),CD=c(DA2))

#Combining DF's
final_df = cbind(df, df2$HD,df2$CD)
view(final_df)

#fitting model
elec_model = tslm(df.Temperature ~ df2$HD+ df2$CD, data=final_df)
summary(elec_model)

#Auto arima
?arima
arimaHD = auto.arima(final_df[,'df.Demand'],xreg=final_df[,'df2$HD'])
arimaCD = auto.arima(final_df[,'df.Demand'],xreg=final_df[,'df2$CD'])


#FORECAST 29,25,20,15,23  AVG:22.4
data = c(29,25,20,15,23)

HDplot = forecast(arimaHD,xreg=data,h=5)
CDplot = forecast(arimaCD,xreg=data,h=5)
autoplot(HDplot)+
  autolayer(CDplot)



                                                        #BITCOIN

df = btc_hr

df = ts(df,start=c(10/1/18, 0:00),end =c(10/19/18, 13:00), frequency = 24)
view(df)

#I've never converted hourly data to a time series this was confounding 
df=ts(df,frequency = 24) #I ended up defaulting to this
view(df)
df_train = window(df, start=( 1),end=(17))
df_test = window(df, start=(18))
view(df)

#Arima
arima1 = auto.arima(df[,2])
summary(arima1)

#ets
ets1 = forecast(ets(df[,2]), h=h, level=95)
summary(ets1)

#out of all three methods arima had the best RMSE Score

#Time series cross validation 
#tsCV(df, fc, xreg=pred)  didn't have time to build out the variables inside of it
