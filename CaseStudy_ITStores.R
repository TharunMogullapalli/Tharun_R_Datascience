myts <- scan()

mycounts <- ts(myts, frequency = 12,start = 1 )

plot(mycounts, ylab= "Customer Count", xlab = "Weeks" )

library(forecast)

monthplot(mycounts, label = 1:12, xlab= "Bidaily Units")

seasonplot(mycounts, season.labels = F, xlab ="")

plot(forecast(auto.arima(mycounts)))

library(lubridate)

ymd(20181123)

mytimepoint <- ymd_hm("2018-11-23 11:23",tz = "Europe/Prague")

olson_time_zones()

OlsonNames()

year(mytimepoint)

month(mytimepoint)

hour(mytimepoint)

minute(mytimepoint)

with_tz(mytimepoint, tz = "US/Pacific")


