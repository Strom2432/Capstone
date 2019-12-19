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
##Cleaning dates into a somewhat usable format
lyftsample = sample_n(filter(cleanBoi, cleanBoi$cab_type == 'Lyft'), (0.1*nrow(filter(cleanBoi, cleanBoi$cab_type == 'Lyft'))), replace = F)
ubersample = sample_n(filter(cleanBoi, cleanBoi$cab_type == 'Uber'), (0.1*nrow(filter(cleanBoi, cleanBoi$cab_type == 'Uber'))), replace = F)
cabsample = rbind(lyftsample, ubersample)
View(cabsample)

numsample = select_if(cabsample, is.numeric)
numsample[is.na(numsample)] = 0
numsample[, 10] = cabsample$cab_type
names(numsample) = c("distance", "price", "surge_multiplier", "temp", "clouds", "pressure", "rain", "humidity", "wind", "cab_type")


Train = numsample$cab_type %>% createDataPartition(p = 0.8, list = F)
training = numsample[Train, ]
testing = numsample[-Train, ]

preproc.param = training %>% preProcess(method = c("center", "scale"))
training.trans = preproc.param %>% predict(training)
testing.trans = preproc.param %>% predict(testing)

model1 = lda(cab_type~., data=training.trans)
predictions = model1 %>% predict(testing.trans)
predictions$class = factor(predictions$class, levels=c("Uber", "Lyft"), ordered = T)
mean(predictions$class == testing$cab_type)

## Unfortunately the LDA is only slightly better than random accuracy... 53~54%
lyftsample = sample_n(filter(cleanBoi, cleanBoi$cab_type == 'Lyft'), (0.2*nrow(filter(cleanBoi, cleanBoi$cab_type == 'Lyft'))), replace = F)
ubersample = sample_n(filter(cleanBoi, cleanBoi$cab_type == 'Uber'), (0.2*nrow(filter(cleanBoi, cleanBoi$cab_type == 'Uber'))), replace = F)
numsample = select_if(rbind(lyftsample, ubersample), is.numeric)
# View(numsample)


numsample[is.na(numsample)] = 0
numsample[, 10] = cabsample$name
names(numsample) = c("distance", "price", "surge_multiplier", "temp", "clouds", "pressure", "rain", "humidity", "wind", "name")



numsample2 = filter(numsample2, numsample2$name != "Black")
numsample2 = filter(numsample2, numsample2$name != "Black SUV")
numsample2 = filter(numsample2, numsample2$name != "Shared")
numsample2 = filter(numsample2, numsample2$name != "UberPool")
numsample2 = filter(numsample2, numsample2$name != "UberX")
numsample2 = filter(numsample2, numsample2$name != "UberXL")
numsample2 = filter(numsample2, numsample2$name != "WAV")

Train2 = numsample$name %>% createDataPartition(p = 0.8, list = F)
training2 = numsample[Train2, ]
testing2 = numsample[-Train2, ]

preproc.param = training2 %>% preProcess(method = c("center", "scale"))
training.trans = preproc.param %>% predict(training2)
testing.trans = preproc.param %>% predict(testing2)


model2 = qda(name~., data=training.trans)
predictions = model2 %>%  predict(testing.trans)
predictions$class = factor(predictions$class, levels = c("Lyft XL", "Lyft", "Lux Black XL", "Lux", "Lux Black", "Shared", "UberX", "UberXL", "UberPool", "Black", "WAV", "Black SUV"), ordered = T)
mean(predictions$class == testing2$name)
## When you add weather data into the mix it confuses the model and it becomes far less accurate

## Six classification of ride had to be discarded due to insuficient rank.
## This could primarily be sampling error. 
## However we got tremendous accuracy with over 80% classification rate. Second pass got up near 90% with some tweaking, could be variance in sampling


bigCab = cab[, -3]
bigCab = bigCab[, -3]
bigCab = bigCab[, -3]
bigCab = bigCab[, -2]
View(bigCab)

#bigCab = filter(bigCab, bigCab$name != "Black")
#bigCab = filter(bigCab, bigCab$name != "Black SUV")
#bigCab = filter(bigCab, bigCab$name != "Shared")
#bigCab = filter(bigCab, bigCab$name != "UberPool")
#bigCab = filter(bigCab, bigCab$name != "UberX")
#bigCab = filter(bigCab, bigCab$name != "UberXL")
#bigCab = filter(bigCab, bigCab$name != "WAV")

Train3 = bigCab$name %>% createDataPartition(p = 0.8, list = F)
training3 = bigCab[Train3, ]
testing3 = bigCab[-Train3, ]

preproc.param = training3 %>% preProcess(method = c("center", "scale"))
training.trans = preproc.param %>% predict(training3)
testing.trans = preproc.param %>% predict(testing3)

count(testing.trans, name)

model3 = qda(name~., data = training.trans)
predictions = model3 %>% predict(testing.trans)
predictions$class = factor(predictions$class, levels = c("Lyft XL", "Lux Black", "Lyft", "Lux Black XL", "Lux"))
mean(predictions$class == testing3$name)
##Larger sample removed the problem with rank difciency 
##Classification improved, However something to note as you increase variable count...
## is that we may be constructing a model that is over fit. However it is worth taking into account
## That this is very useful because you may be able to develope a third party app that uses lyft and uber API's
## In order to set "Maximum price, distance, party size, etc." specifications in addition to a destination 
## This app could then querey any local ride share app and classify that which would have the lowest price based on customer preferences
## Both Uber and Lyft could also use this inorder to querey eachother's prices and compete.