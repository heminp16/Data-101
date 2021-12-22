# you will predict the tripduration of Citybike trips taken in May2019

citiTrain <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/citybike.train.csv")
citiTestB <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/citybike.test10000B.csv") # Have to test on here and submit 
# Can also test Model on test100 & 10000A, as both of them have withduration.csv files to see if we predicted correctly 

citiTest100 <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/citybike.test100withduration.csv") #100
citiTestA <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/citybike.test10000Awithduration.csv") #10000A


citiTrain$Day <- as.numeric(substr(citiTrain$starttime,9,10)) 
citiTest100$Day <- as.numeric(substr(citiTest100$starttime,9,10))
citiTestA$Day <- as.numeric(substr(citiTest100$starttime,9,10))
citiTestB$Day <- as.numeric(substr(citiTest100$starttime,9,10))


# my.model <- lm(tripduration ~ Day + start.station.id + end.station.id, data=citiTrain) # 822.4

my.model <- lm(tripduration ~ start.station.id + end.station.id + start.station.longitude + end.station.longitude, data=citiTrain) # 826.1880 


# testing on 100
lm.model.predictions <- predict(my.model, citiTest100)
regr.error(lm.model.predictions, citiTest100$tripduration)  #826.1880   

# testing on 10000A 
lm.model.predictions1 <- predict(my.model, citiTestA)
regr.error(lm.model.predictions1, citiTestA$tripduration) #13295.66

# predictions for 10000B

predictions.10000B <- predict(my.model, citiTestB)

View(predictions.10000B)

write.csv(predictions.10000B, file = "/Users/hemin/Desktop/Data 101/HW 11/CitiPredictions.csv")

# Code to edit column names
CitiPredictions <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/CitiPredictions.csv")
colnames(CitiPredictions) <- c("Id","Predicted")
write.csv(CitiPredictions, file = "/Users/hemin/Desktop/Data 101/HW 11/CitiPredictions.csv",row.names=FALSE)



# part 2 // FAIL worse
my.model2 <- lm(tripduration ~ birth.year + end.station.id, data=citiTrain) 


# testing on 100
lm.model.predictions2 <- predict(my.model2, citiTest100)
regr.error(lm.model.predictions2, citiTest100$tripduration) #824.0093    

# testing on 10000A 
lm.model.predictions3 <- predict(my.model2, citiTestA)
regr.error(lm.model.predictions3, citiTestA$tripduration) # 13295.28

# predictions for 10000B

predictions10000B <- predict(my.model2, citiTestB)

View(predictions10000B)

write.csv(predictions10000B, file = "/Users/hemin/Desktop/Data 101/HW 11/CitiPredictions3.csv")

# Code to edit column names
CitiPredictions2 <- read.csv("/Users/hemin/Desktop/Data 101/HW 11/CitiPredictions3.csv")
colnames(CitiPredictions2) <- c("Id","Predicted")
write.csv(CitiPredictions2, file = "/Users/hemin/Desktop/Data 101/HW 11/CitiPredictions3.csv",row.names=FALSE)


