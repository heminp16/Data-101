# Submission file. Other document works as well, however, using this model beats all test cases. 
# Whereas, in the other file, I failed 2 test cases. 

citiTrain <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/CSVs/citybike.train.csv")
citiTestB <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/CSVs/citybike.test10000B.csv") 

CCCitiTest20k <- read.csv("/Users/hemin/Desktop/Data 101/HW 12/2citybike.test20000.csv") # Have to test on here and submit 
# Can also test Model on test100 & 10000A, as both of them have withduration.csv files to see if we predicted correctly 

citiTest100 <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/CSVs/citybike.test100withduration.csv") #100
citiTestA <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/CSVs/citybike.test10000Awithduration.csv") #10000A

#---------------------------------------------------------------------------------------------------#
citiTrain$Day <- as.numeric(substr(citiTrain$starttime,9,10)) 
citiTest100$Day <- as.numeric(substr(citiTest100$starttime,9,10))
citiTestA$Day <- as.numeric(substr(citiTestA$starttime,9,10))
citiTestB$Day <- as.numeric(substr(citiTestB$starttime,9,10))
CCCitiTest20k$Day <- as.numeric(substr(CCCitiTest20k$starttime,9,10))


citiTrain$lat <-citiTrain$start.station.latitude + citiTrain$end.station.latitude 
citiTrain$long <- citiTrain$start.station.longitude + citiTrain$end.station.longitude
citiTrain$latdiff <-citiTrain$start.station.latitude - citiTrain$end.station.latitude 
citiTrain$longdiff <- citiTrain$start.station.longitude - citiTrain$end.station.longitude

citiTest100$lat <-citiTest100$start.station.latitude + citiTest100$end.station.latitude 
citiTest100$long <- citiTest100$start.station.longitude + citiTest100$end.station.longitude

CCCitiTest20k$lat <-CCCitiTest20k$start.station.latitude + CCCitiTest20k$end.station.latitude 
CCCitiTest20k$long <- CCCitiTest20k$start.station.longitude + CCCitiTest20k$end.station.longitude
CCCitiTest20k$latdiff <-CCCitiTest20k$start.station.latitude - CCCitiTest20k$end.station.latitude 
CCCitiTest20k$longdiff <- CCCitiTest20k$start.station.longitude - CCCitiTest20k$end.station.longitude
#---------------------------------------------------------------------------------------------------#

#This model beats all the test cases.
my.model <- lm(tripduration ~ start.station.id + I(gender^2)+ usertype + bikeid + Day + abs(latdiff) + abs(longdiff), data = citiTrain)

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
predictions.20k <- predict(my.model, CCCitiTest20k)

View(predictions.20k)


write.csv(predictions.20k, file = "/Users/hemin/Desktop/Data 101/HW 12/HW12_CitiPredictions_Submit.csv")

# Code to edit column names
CitiPredictions <- read.csv("/Users/hemin/Desktop/Data 101/HW 12/HW12_CitiPredictions_Submit.csv")
colnames(CitiPredictions) <- c("Id","Predicted")
write.csv(CitiPredictions, file = "/Users/hemin/Desktop/Data 101/HW 12/HW12_CitiPredictions_Submit.csv",row.names=FALSE)

HW12_CitiPredictions_Submit.csv