# For my first Data 101 HW, I chose my project about Spotify and the top songs from 2010-2019. So I thought it would 
# be a fantastic choice for my final project to see what I can predict now as I know more. However, I realized in one 
# of the examples, a student used the same dataset link I used in the past. However, as I still want to pursue this idea, 
# I created my own dataset consisting of the Top 50 songs from 2018 and 2019. Moreover, I created a dataset consisting of 
# Top 50 songs from 2020 and random 50 songs from my own playlist.

top18_19 <- read.csv("~/Desktop/Data 101/Final Project/Top2018_2019.csv") #https://www.kaggle.com/heminp16/spotify-top-2018-2019-songs
top20    <- read.csv("~/Desktop/Data 101/Final Project/Top2020.csv") #https://www.kaggle.com/heminp16/spotify-top-2020-songs
Random   <- read.csv("~/Desktop/Data 101/Final Project/Random songs from my playlist.csv") #https://www.kaggle.com/heminp16/random-songs-test-case
#------------------------------------------------------------------------------------------------------------------------------------#
# Data Visualization & Predictions (on the Random test case, to see how my songs compare to the Top songs)

# Data 
#--------------------------------------------------------------------#
# Top Genre vs Energy 
library(plotly)
aggregate(nrgy ~ top.genre, data = top18_19, FUN= mean)
k <- aggregate(nrgy ~ top.genre, data = top18_19, FUN= mean)
write.csv(k,'~/Desktop/Data 101/Final Project/18_19AggregateData.csv')
agg_18_19 <- read.csv("~/Desktop/Data 101/Final Project/18_19AggregateData.csv")
# Energy18_19 <- agg_18_19$nrgy

attach(agg_18_19)
fig <- plot_ly(x = c(top.genre), y = c(nrgy), width = 500, height = 500, type = 'bar')%>% 
  layout(title= list(text = "Average Correlation Between Top Genre and Energy",font = "Times New Roman"), font="Times New Roman", 
         xaxis = list(title = list(text ='Top Genre', font = "Times New Roman")), 
         yaxis = list(title = list(text ='Energy', font = "Times New Roman")),
         plot_bgcolor='#e5ecf6', paper_bgcolor='#31d27a')
fig
#------------------------------------------------------------------------------------------------------------------#
# Message for the code below: 
# I was trying to see the correlation between these two dataset. Sadly, after testing, I realized the genres did not match up.
# However, that's how data is, and it shows how the songs in my playlist aren't the same genre as the Top 2020 songs. Although 
# other attributes might closely relate, where the genre failed. 
Mix1 <- subset(top20[,c(1,4)])
Mix2 <- subset(Random[,c(1,4)])

library(dplyr)
Distinct2020 <- distinct(Mix1, top.genre)
DistinctRandom <- distinct(Mix2, top.genre)
names(DistinctRandom)[1] <- "top.genre2"

Mixed <- merge(data.frame(Distinct2020, row.names=NULL), data.frame(DistinctRandom, row.names=NULL), by = 0, all = TRUE)[-1]

MixedTable <- Mixed %>%
  count(top.genre, top.genre2, sort= FALSE)
colnames(Mixed) <- c("Top Genre 2020", "Top Genre Random")
fig <- plot_ly(
  type='table',
  header=list(
    values=names(Mixed), 
    align = c("center", "center"),
    line = list(width = 1, color = 'white'),
    fill= list(color= c("#113b22", "#113b22")),
    font= list(family = "San Serif", size =15, color = "white"),
    height= 30),          
  cells=list(
    values=unname(Mixed),
    align = c("center", "center"),
    line = list(width = 1, color = 'white'),
    fill= list(color= c("#31d27a", "#31d27a")),
    font= list(family = "San Serif", size =15, color = "white"),
    height= 30)) %>% 
  layout(paper_bgcolor='#31d27a')
fig 
#------------------------------------------------------------------------------------------------------------------#
#2020 - mean(BPM & Energy)  Random - mean(BPM & Energy)

Agg2020 <-  aggregate(bpm ~ nrgy, data= top20, FUN= mean)
AggRandom<- aggregate(bpm ~ nrgy, data= Random, FUN= mean)

AggMixed <- merge(data.frame(Agg2020, row.names=NULL), data.frame(AggRandom, row.names=NULL), by = 0, all = TRUE)[-1]
# write.csv(AggMixed, '~/Desktop/Data 101/Final Project/AggMixed.csv')
AggMixed <- read.csv('~/Desktop/Data 101/Final Project/AggMixed.csv')

library(plotly)
fig <- plot_ly(AggMixed, x = ~nrgy.x, y = ~bpm.x, type = 'bar', name = 'Top 2020 Songs')
fig <- fig %>% add_trace(y = ~bpm.y, name = 'Random Songs')
fig <- fig %>% layout(yaxis = list(title = 'BPM'),
                      xaxis = list(title = list(text ='Energy'),
                      barmode = 'group'),
                      paper_bgcolor='#31d27a')
fig

#Looking at this chart, it is apparent that my songs and the Top 2020 songs match closely in these two attributes. 
# As I mentioned before, even though the genres might be different, they might relate in other ways, and this chart
# certainly proves that. 
#------------------------------------------------------------------------------------------------------------------------------------#
# Predicting if the Random songs in my playlist would fit with the Top 2018/2019 songs and Top 2020 songs

# Predictions 
#--------------------------------------------------------------------#
#Top 2018/2019 = Test case -> testing on Random

# summary(top18_19)
# I ran this to see what I could include in my prediction test. bpm, energy, and danceability all had high avgs, so I decided those should be a factor
# When running the prediction, the rmse value range around >56, so I decided to add year which dropped the rmse to 11.91.


my.model <- lm(nrgy ~ bpm + dnce + year, data = top18_19) 
lm.model.predictions <- predict(my.model, Random) #rmse: 11.9136822 
regr.error(lm.model.predictions, Random$nrgy) 

View(lm.model.predictions)

# summary(lm.model.predictions)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 53.07   57.56   59.75   60.57   62.67   74.93 

# According to this data, Random songs with the energy level around 60.57 would typically fit best in Top 2018 and 2019 songs. 

# When taking a quick look at the data, it seems there are not many songs around that mean, therefore I will include from 59-61 energy level. (+-1 factor)

library(dplyr)
Prediction1 <- select(Random, title, nrgy) #selecting both Title of song and energy lvl
Prediction1 <- filter(Prediction1, Random$nrgy == "59" | Random$nrgy == "60" | Random$nrgy == "61")
Prediction1 <- arrange(Prediction1, nrgy) #Sorting so energy lvl ascends 

# For The Night (feat. Lil Baby & DaBaby) 59
# The Box 59
# Molly Girl 59
# Sanguine Paradise 60
# Hold On 60
# Woah 60
# Aim For The Moon (feat. Quavo) 61


#------------------------------------------------------------------------------------------------------------------#
#Top 2020 = Test case -> testing on Random
my.model2 <- lm(nrgy ~ bpm + year, data = top20) 
lm.model.predictions2 <- predict(my.model2, Random) #rmse: 11.0448944 
regr.error(lm.model.predictions2, Random$nrgy) 

# summary(lm.model.predictions2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 58.97   60.41   61.66   61.50   62.54   63.87 

# According to this data, Random songs with the energy level around  61.50 would typically fit best in Top 2020 songs. 
# Like in the previous code, I will include a +-2 factor, resulting in songs around 59-63 energy level. (The factor is more as I took a attribute out for better rmse)

library(dplyr)
Prediction2 <- select(Random, title, nrgy) #selecting both Title of song and energy lvl
Prediction2 <- filter(Prediction2, Random$nrgy == "59" | Random$nrgy == "60" | Random$nrgy == "61" | Random$nrgy == "62" | Random$nrgy == "63")
Prediction2 <- arrange(Prediction2, nrgy) #Sorting so energy lvl ascends 

# For The Night (feat. Lil Baby & DaBaby) 59
# The Box 59 ** In top 2020 songs
# Molly Girl 59
# Sanguine Paradise 60
# Hold On 60
# Woah 60
# Aim For The Moon (feat. Quavo) 61
# Leaked 62
#------------------------------------------------------------------------------------------------------------------#

# Now its hard to predict if these songs would be included in Spotify's Top 2021 songs, so what I will do is see if the artists appeared in the past Top datasets.

library(stringr)
#Prediction1                                                                        
sum(str_count(top18_19, "Pop Smoke|DaBaby|Roddy Rich|iann dior|Lil Tjay|Lil Baby|Quavo|Lil Uzi Vert")) #4 -> 4/6= 66.67%  # Random Artist listed 4 times in 2018/2019; 4 out of 6 Random predicted songs 
#Prediction2
sum(str_count(top20, "Pop Smoke|DaBaby|Roddy Rich|iann dior|Lil Tjay|Lil Baby|Quavo|Lil Uzi Vert")) #3 -> 3/8= 37.5% # Random Artist listed 3 times in 2020; 3 out of 8 Random predicted songs

