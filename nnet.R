library(tidyverse)
library(ggplot2)
library(MASS)
library(caret)
library(readr)
cab <- read_csv("Desktop/Capstone Data/cab_rides.csv")
weather <- read_csv("Downloads/weather.csv")

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

library(nnet)
numsample = select_if(cleanBoi, is.numeric)
numsample[,10] = cleanBoi$name
names(numsample) = c("distance", "price", "surge_multiplier", "temp", "clouds", "pressure", "rain", "humidity", "wind", "name")
numsample[is.na(numsample)] = 0
numsample$name = as.integer(numsample$name)
numsample = numsample[, -10]

targets = class.ind( c(rep("lyft", length(which(cleanBoi$name == 'Lyft'))), rep("Shared", length(which(cleanBoi$name == "Shared"))), rep("Lyft XL", length(which(cleanBoi$name == "Lyft XL"))),
                        rep("Lux Black XL", length(which(cleanBoi$name == "Lux Black XL"))), rep("Lux", length(which(cleanBoi$name == "Lux"))),
                        rep("Black", length(which(cleanBoi$name == "Black"))), rep("UberX", length(which(cleanBoi$name == "UberX"))), 
                        rep("Lux Black", length(which(cleanBoi$name == "Lux Black"))), rep("WAV", length(which(cleanBoi$name == "WAV"))),
                        rep("UberXL", length(which(cleanBoi$name == "UberXL"))), rep("UberPool", length(which(cleanBoi$name == "UberPool"))),
                        rep("Black SUV", length(which(cleanBoi$name == "Black SUV")))))

samp = c( sample(1:90749, 63525), sample(90750:181498, 63525), sample(181499:272247, 63524),
          sample(272248:362995, 63525), sample(362996:453744, 63524), sample(453744:544493, 63524), 
          sample(544494:635242, 63524))

## irl = nnet(x=ir[samp,],y=targets[samp,], size = 6, rang = 0.1, decay = 5e-4, maxit = 200)

#traceback()
#options(warn=2)
#rerun
#debug(nnet)
#options(warn=1)
#rerun
test = cbind(numsample, targets)

## irl = nnet(x=ir[samp,],y=targets[samp,], size = 6, rang = 0.1, decay = 5e-4, maxit = 200)
irl = nnet(numsample[samp,], targets[samp,], size = 4, decay = 5e-4, maxit=200)
irl2 = nnet(numsample[samp,], targets[samp,], size = 20, decay = 5e-4, maxxit = 200)
irl3 = nnet(numsample[samp,], targets[samp,], size = 40, decay = 5e-4, maxit=200)

test.cl = function(true, pred) {
  true = max.col(true)
  cres = max.col(pred)
  table(true, cres)
}

test.cl(targets[-samp,], predict(irl, numsample[-samp,]))
