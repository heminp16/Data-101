library(readxl)
library("ggplots2")

top10s <- read_excel("~/Desktop/Data 101/HW 1/top10s.xlsx")
View(top10s)

#Valence vs Danceability 
#scatterplot
attach(top10s)
plot(Danceability, Valence, main = "Correlation Between Danceability and Song Mood",
     xlab= "Danceability", ylab= "Valence", pch=20)
lines(lowess(Danceability, Valence), col="red") #lowess line
abline(lm(Danceability~Valence), col="blue") #regression line
legend (x= 25, y=95, c("Regression Line", "Lowess Line"), cex= .8, col= c("blue", "red"), lwd= c(2,2))
par(bg = "transparent")


# Top Genre vs Energy 
library(plotly)
aggregate(Energy ~ `Top Genre2`, data = top10s, FUN= mean)
k <- aggregate(Energy ~ `Top Genre2`, data = top10s, FUN= mean)
write.csv(k,'AggregateData.csv')
agg_top10s <- read.csv("~/Desktop/Data 101/HW 2/AggregateData.csv")
Energy2 <- agg_top10s$Energy

fig <- plot_ly(x = c(Top.Genre2), y = c(Energy2), width = 700, height = 700, type = 'bar')%>% 
 layout(title= list(text = "Average Correlation Between Top Genre and Energy",font = "Times New Roman"), font="Times New Roman", 
       xaxis = list(title = list(text ='Top Genre', font = "Times New Roman")), 
       yaxis = list(title = list(text ='Energy', font = "Times New Roman")),
       plot_bgcolor='#e5ecf6', paper_bgcolor='#31d27a')
api_create(p=fig, filename = "Top Genre vs Energy")
fig


#Speechiness vs Danceability
library(plotly)
aggregate(BPM ~ Danceability, data= top10s, FUN= mean)
b<- aggregate(BPM ~ Danceability, data= top10s, FUN= mean)
write.csv(b, 'agg_BPM_D.csv')
agg_BPM_D <- read.csv("~/Desktop/Data 101/HW 2/agg_BPM_D.csv")
BPM2 <- agg_BPM_D$BPM
Danceability2 <- agg_BPM_D$Danceability


fig <- plot_ly(x = c(Danceability2), y = c(BPM2), width = 600, height = 600, type = 'bar')%>% 
  layout(title= list(text = "Average Correlation Between Speechiness and Danceability",font = "Times New Roman"), font="Times New Roman", 
         xaxis = list(title = list(text ='Danceability', font = "Times New Roman")), 
         yaxis = list(title = list(text ='Speechiness', font = "Times New Roman")),
         plot_bgcolor='#e5ecf6', paper_bgcolor='#31d27a')
api_create(p=fig, filename = "Speech vs Dance")
fig



