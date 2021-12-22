
anon <- read.csv("/Users/hemin/Desktop/Data 101/HW 9/F21DataSurveyWithSectionsAnonymized.csv")
anon <- na.omit(anon)
                 
 # & (anon$RightLeftHanded) & (anon$RightLeftHanded),)
# ------------------------------------------------------------------------------------------------------------------------------------

# R:L:Both

#removing more ppl
anon <- anon[!grepl("specific things", anon$RightLeftHanded),] #removed a person that typed in the "Other" box (mostly right-handed, but left-handed with very specific things)
anon <- anon[!grepl("2597", anon$ID),] #had to hard code, !is.na & na.omit did not work when trying to remove na in RightLeftHanded
anon <- anon[!grepl("7218", anon$ID),] #had to hard code, na in temp

table(anon$RightLeftHanded) 

# temp
table(anon$ThermostatTemperature) 

# section 
table(anon$OddEvenSection)



# Tree 1:  ChocolateOrVanilla
library(rpart)
library(rpart.plot)
tree1 <- rpart(ChocolateOrVanilla ~ DressColor + Floaters + RightLeftHanded + ThermostatTemperature, data= anon, method="class")
rpart.plot(tree1)

printcp(tree1)
# Root node error: 118/238 = 0.4958
# 
# n= 238 
# 
# CP nsplit rel error xerror     xstd
# 1 0.059322      0   1.00000 1.0593 0.065287
# 2 0.025424      1   0.94068 1.0763 0.065222
# 3 0.016949      2   0.91525 1.1525 0.064699
# 4 0.010593      4   0.88136 1.1102 0.065036
# 5 0.010000      8   0.83898 1.1102 0.065036


# Tree 2:  ThermostatTemperature
library(rpart)
library(rpart.plot)
tree2 <- rpart(ThermostatTemperature ~ RightLeftHanded + Floaters + ChocolateOrVanilla + CanRollTongue, data= anon, method="class")
rpart.plot(tree2)

printcp(tree2)
# Root node error: 97/238 = 0.40756  #low error, good
# 
# n= 238 
# 
# CP nsplit rel error xerror     xstd
# 1 0.025773      0   1.00000 1.0000 0.078151
# 2 0.012887      2   0.94845 1.0928 0.079046
# 3 0.010309      6   0.89691 1.0928 0.079046
# 4 0.010000      8   0.87629 1.0103 0.078274

# Tree 3:  DressColor
library(rpart)
library(rpart.plot)
tree3 <- rpart(DressColor ~ Floaters + RightLeftHanded + ChocolateOrVanilla + CanRollTongue, data= anon, method="class", cp=0.001)
rpart.plot(tree3)

printcp(tree3)

# Root node error: 111/238 = 0.46639
# 
# n= 238 
# 
# CP nsplit rel error xerror     xstd
# 1 0.0045045      0   1.00000  1.000 0.069335
# 2 0.0010000      2   0.99099  1.036 0.069453

