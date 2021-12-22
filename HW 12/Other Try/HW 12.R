# you will predict the tripduration of Citybike trips taken in May2019

citiTrain <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/CSVs/citybike.train.csv")
citiTestB <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/CSVs/citybike.test10000B.csv") 

citiTest20k <- read.csv("/Users/hemin/Desktop/Data 101/HW 12/citybike.test20000.csv") # Have to test on here and submit 
# Can also test Model on test100 & 10000A, as both of them have withduration.csv files to see if we predicted correctly 

citiTest100 <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/CSVs/citybike.test100withduration.csv") #100
citiTestA <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/CSVs/citybike.test10000Awithduration.csv") #10000A


citiTrain$Day <- as.numeric(substr(citiTrain$starttime,9,10)) 
citiTest100$Day <- as.numeric(substr(citiTest100$starttime,9,10))
citiTestA$Day <- as.numeric(substr(citiTestA$starttime,9,10))
citiTestB$Day <- as.numeric(substr(citiTestB$starttime,9,10))
citiTest20k$Day <- as.numeric(substr(citiTest20k$starttime,9,10))

#---------------------------------------------------------------------------------------------------#
# Converting Data in CitiTest20k file (The prediction csv)
# # convert date into POSIXct format 
date20k <- as.POSIXct(citiTest20k$starttime)

# extract time from date
time20k <- format(date20k, format = "%H:%M:%S")

citiTest20k$NewDateFormat <- date20k
citiTest20k$Time <- time20k

citiTest20k$hour <- as.numeric(substr(citiTest20k$starttime, 12,13))
citiTest20k$min <- as.numeric(substr(citiTest20k$starttime, 15,16))
citiTest20k$seconds <- as.numeric(substr(citiTest20k$starttime, 18,19))
#----------------------------------------------------------------------#
# Converting Data in citiTrain file (The training csv)
# # convert date into POSIXct format 
date <- as.POSIXct(citiTrain$starttime)

# extract time from date
time <- format(date, format = "%H:%M:%S")

citiTrain$NewDateFormat <- date
citiTrain$Time <- time

citiTrain$hour <- as.numeric(substr(citiTrain$starttime, 12,13))
citiTrain$min <- as.numeric(substr(citiTrain$starttime, 15,16))
citiTrain$seconds <- as.numeric(substr(citiTrain$starttime, 18,19))
#----------------------------------------------------------------------#
# Converting Data in citiTest100 file (The testing csv)
# # convert date into POSIXct format 
dateTest100 <- as.POSIXct(citiTest100$starttime)

# extract time from date
timeTest100 <- format(dateTest100, format = "%H:%M:%S")

citiTest100$NewDateFormat <- dateTest100
citiTest100$Time <- timeTest100

citiTest100$hour <- as.numeric(substr(citiTest100$starttime, 12,13))
citiTest100$min <- as.numeric(substr(citiTest100$starttime, 15,16))
citiTest100$seconds <- as.numeric(substr(citiTest100$starttime, 18,19))

#---------------------------------------------------------------------------------------------------#
citiTrain$lat <-citiTrain$start.station.latitude + citiTrain$end.station.latitude 
citiTrain$long <- citiTrain$start.station.longitude + citiTrain$end.station.longitude

citiTest100$lat <-citiTest100$start.station.latitude + citiTest100$end.station.latitude 
citiTest100$long <- citiTest100$start.station.longitude + citiTest100$end.station.longitude

citiTest20k$lat <-citiTest20k$start.station.latitude + citiTest20k$end.station.latitude 
citiTest20k$long <- citiTest20k$start.station.longitude + citiTest20k$end.station.longitude
#---------------------------------------------------------------------------------------------------#
library(rpart)

# my.model <- lm(tripduration ~ start.station.latitude + poly(end.station.latitude^2), data=citiTrain) 

#best use this 
# my.model <- rpart(tripduration ~ hour + start.station.latitude + end.station.latitude, data=citiTest100) # 638.9788 
# my.model <- rpart(tripduration ~ hour + I(min^2) + I(seconds^3) +start.station.latitude + end.station.latitude, data=citiTest100) #637.3949
# # this is best, yet low score on kaggle
# my.model <- rpart(tripduration ~ birth.year + start.station.latitude + end.station.latitude, data=citiTrain) #636.7716


#USED THIS score 911 on kaggle
my.model <- lm(tripduration ~ Day + bikeid + birth.year + gender + start.station.id + usertype + abs(lat) + abs(long), data = citiTrain) # 772.7762



# rmse has to be under 820
# testing on 100
lm.model.predictions <- predict(my.model, citiTest100)
regr.error(lm.model.predictions, citiTest100$tripduration) 

View(lm.model.predictions)

# testing on 10000A 
lm.model.predictions1 <- predict(my.model, citiTestA)
regr.error(lm.model.predictions1, citiTestA$tripduration) 


# This is the submission - Where we are predicting
# predictions for 20K
predictions.20k <- predict(my.model, citiTest20k)

View(predictions.20k)


write.csv(predictions.20k, file = "/Users/hemin/Desktop/Data 101/HW 12/HW12_CitiPredictions.csv")

# Code to edit column names
CitiPredictions <- read.csv("/Users/hemin/Desktop/Data 101/HW 12/HW12_CitiPredictions.csv")
colnames(CitiPredictions) <- c("Id","Predicted")
write.csv(CitiPredictions, file = "/Users/hemin/Desktop/Data 101/HW 12/HW12_CitiPredictions.csv",row.names=FALSE)

########################################
