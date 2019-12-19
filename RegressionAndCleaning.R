library(tidyverse)
library(ggplot2)
library(MASS)
library(caret)
library(readr)
cab <- read_csv("Desktop/Capstone Data/cab_rides.csv")
weather <- read_csv("Downloads/weather.csv")

#test = merge(head(cab), head(weather), by="date_stamp")
#cabtest = head(cab)
#View(cabtest)

##Used to test code when i needed to, rewritten a few times can ignore

cab = na.omit(cab)
##Remove NA values, lost about 8% of data, not huge but worth mentioning
cab = as.data.frame(cab)
weather = as.data.frame(weather)
##Make sure they are data frames and not matricies, makes them into readable formats for some functrions 
cab[,3] = as.POSIXct(cab[,3]/1000, origin="1970-01-01")
weather[,6] = as.POSIXct(weather[,6], origin="1970-01-01")

weather[,9] = format(as.POSIXct(strptime(weather$time_stamp, "%Y-%m-%d %H:%M:%S", tz="")) , format = "%H ")
weather[,10] = format(as.POSIXct(strptime(weather$time_stamp, "%Y-%m-%d %H:%M:%S", tz="")) , format = "%Y-%m-%d")
weather = weather[, -6]
names(weather) = c("temp", "source", "clouds", "pressure", "rain", "humidity", "wind", "hour_stamp", "date_stamp")
##Cleaning dates into a somewhat usable format
cab[,11] = format(as.POSIXct(strptime(cab$time_stamp, "%Y-%m-%d %H:%M:%S", tz="")) , format = "%H ")
cab[,12] = format(as.POSIXct(strptime(cab$time_stamp, "%Y-%m-%d %H:%M:%S", tz="")) , format = "%Y-%m-%d")
cab = cab[, -3]
names(cab) = c("distance", "cab_type", "destination", "source", "price", "surge_multiplier","id", "product_id", "name", "hour_stamp", "date_stamp")

cab = cab[, -8]
cab = cab[, -3]
bigstupiddf = merge(cab, weather, by = c("date_stamp", "source", "hour_stamp"))
cleanBoi = distinct(bigstupiddf, bigstupiddf$id, .keep_all = T)
cleanBoi = cleanBoi[, -8]
##Used the 'id' column to filter back out unique entires post merge.
View(cleanBoi)

lyftsample = sample_n(filter(cleanBoi, cleanBoi$cab_type == 'Lyft'), (0.1*nrow(filter(cleanBoi, cleanBoi$cab_type == 'Lyft'))), replace = F)
ubersample = sample_n(filter(cleanBoi, cleanBoi$cab_type == 'Uber'), (0.1*nrow(filter(cleanBoi, cleanBoi$cab_type == 'Uber'))), replace = F)
cabsample = rbind(lyftsample, ubersample)
View(cabsample)

numsample = select_if(cleanBoi, is.numeric)
numsample[is.na(numsample)] = 0
linModel1 = lm(price~., numsample)
linModel2 = stepAIC(linModel1)
summary(linModel2)
anova(linModel2)

##It would appear as though we have several culprits for increasing price. Distance, Surge_multiplier, Temperature, what is most interesting though
## is that rain has a proposed coefficient that is negative, however it should be noted that the coefficient is withing 2 standard deviations of zero
## This means we can reasonably assume rain has no statistically meaningful impact on price. Though the idea of Rain driving a price down is interesting. 
## Can we assume people will simply ride share less and plan on making fewer trips on days predicted to have bad weather?