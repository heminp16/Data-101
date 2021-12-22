# Historic Temp // HW 6 // Hemin 

#Argument: Global Warming is Real 

# You should use two of the following techniques discussed in class to support your arguments:
# - hypothesis testing using z-scores and P-values
# - hypothesis testing using the Permutation test
# - p-hacking  (test various variables and sample sizes) (This is especially helpful if you want to make a bad faith argument)
# - Bonferroni correction

og_temp <- read.csv("/Users/hemin/Desktop/Data 101/HW 6/historic_temp.csv")
temp <- na.omit(og_temp) #dropping NA
# summary(temp) // min 1861, median 1965, max 2005 

library(dplyr)
temp2<- filter(temp, Year >= 1995 & Year <= 2005, AverageCelsiusTemperature > -99999.0) #removed the AvgTemp with val -99999.0 (inaccurate)
#summary(temp2) // min 1995, median 1999, max 2005 
                #MinCelsiusTemp: min -39.200, median 10.600, max 31.800  MEAN 9.024 == 48.2432 // all countries tho
                #MaxCelsiusTemp: -33.70, median 22.60, max 45.10         MEAN 19.76 == 67.568 // all countries tho




---------------------------------------------------------------------------------
temp2$Country <- trimws(as.character(temp2$Country))
Aus.weather <- subset(temp2, temp2$Country == "Australia")
US.weather=subset(temp2,temp2$Country == "United States")


# Mean 
c.Aus= mean(Aus.weather$AverageCelsiusTemperature)
c.US= mean(US.weather$AverageCelsiusTemperature)

# Standard Deviation 
sd.Aus= sd(temp2$MaxCelsiusTemp) 
sd.US= sd(temp2$MinCelsiusTemp)

#Length 
num.Aus = length(temp2$MaxCelsiusTemp)
num.US = length(temp2$MinCelsiusTemp)

sd.m.f = sqrt(sd.US^2/num.US + sd.Aus^2/num.Aus)
z.scoree= (c.US - c.Aus)/sd.m.f
p.valuee = 1 - pnorm(z.scoree) 

---------------------------------------------------------------------------------
#P-Hacking // Global Warming is not real
temp3<- filter(temp,  AverageCelsiusTemperature == -99999.0) 
sd.3 = sd(temp$AverageCelsiusTemperature)
z.score.3 = (c.Max_mean)/sd.3  #1435.164
p.value.3 = 1 - pnorm(z.score.3) 
# p val of 0.494 -> less than threshold cannot reject null hypo


---------------------------------------------------------------------------------
# Mean 
# c.Max_mean= mean(temp2$MaxCelsiusTemp)
# c.Min_mean= mean(temp2$MinCelsiusTemp)
# 
# # Standard Deviation 
# sd.Max= sd(temp2$MaxCelsiusTemp) 
# sd.Min= sd(temp2$MinCelsiusTemp)
# 
# #Length 
# num.Max = length(temp2$MaxCelsiusTemp)
# num.Min = length(temp2$MinCelsiusTemp)
# 
# sd.m.f = sqrt(sd.Min^2/num.Min + sd.Max^2/num.Max)
# z.score= (c.Min_mean - c.Max_mean)/sd.m.f
# p.value = 1 - pnorm(z.score) 

# p val of 1 == Hypo is correcto 
---------------------------------------------------------------------------------

# convertCtoF <- function(x){
#   x <- (x * 9/5) + (32)
#   return (x)
# }
# 
# temp2$MaxCelsiusTemp <- sapply(temp2$MaxCelsiusTemp, FUN=convertCtoF)
# temp2$MinCelsiusTemp <- sapply(temp2$MinCelsiusTemp, FUN=convertCtoF)
  
  
#   
# AustraliaTemp <- historic_temp[historic_temp$Country=="Australia",]
# Permutation(1000,AustraliaTemp,"Year","AverageCelsiusTemp",1980,2000,"L")
