install.packages("factoextra")
library('factoextra')
library(ggplot2
        )
set.seed(8008135)

numsample = select_if(cleanBoi, is_numeric)
numsample[is.na(numsample)] = 0

numsample[,10] = cleanBoi$source

names(numsample) = c("distance", "price", "surge_multiplier", "temp", "clouds", "pressure", "rain", "humidity", "wind", "source")
sources = numsample[order(numsample$source), ]
test = numsample[order(numsample$source),]

freq = table(test$source)
freq = as.data.frame(freq)
##Seems like an easy way to count all the entries so i know where to partition my lables

numsample = numsample %>% arrange(source)

numsample$label = rep("red", length(numsample))
numsample$label[52996:105670] = rep("blue")
numsample$label[105671:158610] = rep("brown")
numsample$label[158611:211543] = rep("darkorchid")
numsample$label[211544:265491] = rep("darkseagreen")
numsample$label[265492:318433] = rep("turquoise")
numsample$label[318434:371346] = rep("coral")
numsample$label[371347:424280] = rep("violet")
numsample$label[424281:476602] = rep("black")
numsample$label[476603:529532] = rep("purple")
numsample$label[529533:582505] = rep("yellow")
numsample$label[582506:635242] = rep("green")

numsample$label = factor(numsample$label, levels = c("red", "blue", "brown", "darkorchid", "darkseagreen", "turquoise", 
                                                     "coral", "violet", "black", "purple", "yellow", "green") )

x = numsample[1:2]
y = numsample[10]

cabClust = kmeans(numsample[,1:2], 12, nstart = 15)
cabClust

a = cabClust$cluster
b = numsample$label
table(a, b)

cabClust$cluster = as.factor(cabClust$cluster)

ggplot(data = numsample, aes(x=numsample$distance, y=numsample$source, color=cabClust$cluster)) + geom_point()

fviz_cluster(dfclust, data=numsample, geom="point", stand = F, ellipse.type="norm")
