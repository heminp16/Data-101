
anon <- read.csv("/Users/hemin/Desktop/Data 101/HW 9/F21DataSurveyWithSectionsAnonymized.csv")

# Columns I am using
# RightLeftHanded 
# fav Animal subset: Dog 

# I am wondering how many Right handed people like Dogs compared to Left and Both.

# ------------------------------------------------------------------------------------------------------------------------------------
# Prior: How many Right handed ppl are in the survey?
anon <- anon[!grepl("specific things", anon$RightLeftHanded),] #removed a person that typed in the "Other" box 
anon <- anon[!grepl("2597", anon$ID),] #had to hard code, !is.na & na.omit did not work when trying to remove na in RightLeftHanded


table(anon$RightLeftHanded) # removed 2 ppl
# Ambidextrous  Left-handed Right-handed 
# 11           20          214 
nrow(anon[anon$RightLeftHanded=="Ambidextrous",])/nrow(anon) # 0.04489796
nrow(anon[anon$RightLeftHanded=="Left-handed",])/nrow(anon)  # 0.08163265
nrow(anon[anon$RightLeftHanded=="Right-handed",])/nrow(anon) # 0.8734694  I assumed there would be more Right-handed ppl, that's why I chose this for my prior

# ***** 87.35% of the class is Right-Handed ***** #

# ---------------
# How many Right-Handed ppl like Dogs compared to Left & Both
anon2<- subset(anon, anon>0, c(5,7))
anon2 <- na.omit(anon2)
library(stringr)
anon2$Animal = str_to_title(anon2$Animal) 

Righty <- subset(anon2, anon2$RightLeftHanded == "Right-handed") 
Dog <- subset(anon2, anon2$Animal == "Dog" | anon2$Animal == "dog") # including both ppl who wrote "Dog" and "dog" 

table(Dog)
# Animal Ambidextrous Left-handed Right-handed
# Dog            3          11           38

nrow(Dog[Dog$RightLeftHanded=="Ambidextrous",])/nrow(Dog)  # 0.05769231
nrow(Dog[Dog$RightLeftHanded=="Left-handed",])/nrow(Dog)   # 0.2115385
nrow(Dog[Dog$RightLeftHanded=="Right-handed",])/nrow(Dog)  # 0.7307692  // Out of both Ambidextrous && Left-handed, Right-handed ppl like Dog more

# ***** 73.08% of the class is Right-Handed and likes Dogs ***** #

#                                  I wrote 245 instead of 247 since I excluded 2 ppl at the beginning of this script
# Support P(Ambidextrous|Dog) =  3/245 = 1.22449 %
# Support P(Left-handed|Dog)  = 11/245 = 4.489796 %
# Support P(Right-handed|Dog) = 38/245 = 15.5102 %

# ---------------
# How many Dog lovers + R-Handed ppl saw Blue/Black or White/Gold
anon3<- subset(anon, anon>0, c(5,3,7))
anon3<- na.omit(anon3)
library(stringr)
anon3$Animal = str_to_title(anon3$Animal)

Dog2 <- subset(anon3, anon3$Animal == "Dog" | anon3$Animal == "dog") #subset of Animal = dog, RLHanded, Dress Color

RL_Dress <- subset(Dog2, Dog2>0, c(2,3)) #Dropped the row that said "Dog"
RL_Dress <- na.omit(RL_Dress)

table(RL_Dress) #Table based on Dog lovers, their dominant hand, and what dress color combo that saw first. 

# DressColor                            Ambidextrous Left-handed Right-handed
# A color combination not listed here            0           0            3
# Blue and black                                 1           4           22   // There's more Right-handed ppl who saw Blue & Black 
# I don't remember                               1           2            4
# What dress?                                    0           2            4
# White and gold                                 1           3            5


nrow(Dog2[Dog2$DressColor=="A color combination not listed here" & Dog2$RightLeftHanded=="Right-handed",])/nrow(Dog2)*100  # 5.769231
nrow(Dog2[Dog2$DressColor=="Blue and black" & Dog2$RightLeftHanded=="Right-handed",])/nrow(Dog2)*100                       # 42.30769  // Out of all Both, L, & R handed ppl, most of them saw "Blue and Black" dress color (22/52)
nrow(Dog2[Dog2$DressColor=="I don't remember" & Dog2$RightLeftHanded=="Right-handed",])/nrow(Dog2)*100                     # 7.692308
nrow(Dog2[Dog2$DressColor=="What dress?" & Dog2$RightLeftHanded=="Right-handed",])/nrow(Dog2)*100                          # 7.692308
nrow(Dog2[Dog2$DressColor=="White and gold" & Dog2$RightLeftHanded=="Right-handed",])/nrow(Dog2)*100                       # 9.615385
                                                                                                                           # All equals 0.7307692, which is how many R in dataset; 38/52, so correcto (just my own test, to verify is code is correct)

# Test to see ALL L/R/BOTH 
# nrow(Dog2[Dog2$DressColor=="A color combination not listed here",])/nrow(Dog2)  # 0.05769231
# nrow(Dog2[Dog2$DressColor=="Blue and black",])/nrow(Dog2)                       # 0.5192308  // Out of all Both, L, & R handed ppl, most of them saw "Blue and Black" dress color
# nrow(Dog2[Dog2$DressColor=="I don't remember",])/nrow(Dog2)                     # 0.1346154  
# nrow(Dog2[Dog2$DressColor=="What dress?",])/nrow(Dog2)                          # 0.1153846
# nrow(Dog2[Dog2$DressColor=="White and gold",])/nrow(Dog2)                       # 0.1730769
#                                                       
# ---------------
# How many Dog lovers + RLHanded ppl like Chocolate or Vanilla 
anon4 <- subset(anon, anon>0, c(5, 6,7))
anon4 <- na.omit(anon4)
library(stringr)
anon4$Animal = str_to_title(anon3$Animal)

Dog3 <- subset(anon4, anon4$Animal == "Dog" | anon4$Animal == "dog") #Subset of Animal == Dog, RLHanded, ChocolateOrVanilla

RL_icecream <- subset(Dog3, Dog3>0, c(2,3)) #Dropped the row that said "Dog"
RL_icecream <- na.omit(RL_icecream)

table(RL_icecream) #Table based on Dog lovers, their dominant hand, and their favorite icecream flavor.

# ChocolateOrVanilla Ambidextrous Left-handed Right-handed
# Chocolate            1           4           19         
# Vanilla              2           7           19
                                          # Comparing the table value, there is an == number of R-handed ppl who equally like Chocolate and Vanilla (19 ppl)

nrow(Dog3[Dog3$ChocolateOrVanilla=="Chocolate",])/nrow(Dog3)*100  # 46.15385
nrow(Dog3[Dog3$ChocolateOrVanilla=="Vanilla",])/nrow(Dog3)*100    # 53.84615  // Out of all Both, L, R-handed ppl, most of them prefer "Vanilla" 

nrow(Dog3[Dog3$ChocolateOrVanilla=="Chocolate" & Dog3$RightLeftHanded=="Right-handed",])/nrow(Dog3)*100  # 36.53846
nrow(Dog3[Dog3$ChocolateOrVanilla=="Vanilla" & Dog3$RightLeftHanded=="Right-handed",])/nrow(Dog3)*100    # 36.53846  // TIE


# ------------------------------------------------------------------------------------------------------------------------------------
# How to use this data

# For my study, I focused on students' dominant hands and if they are dog lovers. Then I explored other data set to see if they saw the same color dress 
# and what their favorite ice cream flavor is. My goal of the study was to find a correlation of Right-handed dog lovers and see if they share similar likings/views.
# Moreover, even though I focused on just Right-Handed ppl, I also tested Left-Handed and Ambidextrous. 
