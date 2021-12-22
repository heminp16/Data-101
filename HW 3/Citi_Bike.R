#Your assignment is to create 2-3 plots that identify useful insights from the dataset 
#trips are shorter in the morning, direction of trips varies during the day, men ride longer than women
# Think of interesting patterns and explain how they can be leveraged by the
# Citybike organization (e.g., targeted advertising, moving bikes during the day to better
# address demand). You should submit a some actionable suggestions on how to use your insights.

Citi_Bike <- read.csv("/Users/hemin/Desktop/Data 101/HW 3/citibike.May20.50K.csv")
View(Citi_Bike)
Citi_Bike$gender[Citi_Bike$gender == 0] <- "Unknown"
Citi_Bike$gender[Citi_Bike$gender == 1] <- "Male"
Citi_Bike$gender[Citi_Bike$gender == 2] <- "Female"

library(ggplot2)
library(plotly)
# ------------------------------------------------
#Popular locations

Start_Count <- table(Citi_Bike$start.station.name)
View(Start_Count)                   

library(leaflet)
library(sp)
data <- read.csv("/Users/hemin/Desktop/Data 101/HW 3/Start.csv")
View(data)
data$long <- as.numeric(data$long)
data$lat <- as.numeric(data$lat)
data.SP <- SpatialPointsDataFrame (data[,c(2,3)], data[,-c (2,3)])
data$num = cut(data$most,
               breaks= c(322, 317, 257, 252, 251, 250, 243, 235, 234, 232), right=FALSE,
               labels= c("1","2", "3", "4", "5", "6", "7", "8", "9", "10"))
pal= colorFactor(palette = c("red", "blue", "yellow", "brown", "orange", "pink", "green", "white", "black", "violet"), domain=data$num)
Chart <- leaflet() %>%
  addTiles() %>%
  setView(-74.00, 40.71, zoom = 12) %>%
  addCircleMarkers(data=data, lng= ~long, lat= ~lat, popup= ~name, color= ~pal(num), opacity= 100, fillColor = "black", fillOpacity = 0.4) %>% 
  addLegend(
    position = "bottomright",
    colors = c("red", "blue", "yellow", "brown", "orange", "pink", "limegreen", "white", "black", "violet"),
    labels = c("12 Ave & W 40 St", "West St & Chambers St", "Central Park S & 6 Ave", "Pier 40 - Hudson River Park", "S 5 Pl & S 5 St", "1 Ave & E 68 St","   
    Broadway & W 60 St", "Central Park West & W 72 St", "Little West St & 1 Pl", "5 Ave & E 73 St"),
    title = "Most Frequented Start Stations",
    opacity=100)
Chart

# ------------------------------------------------
#Count of User Types' Gender
library(dplyr)
library(plotly)
M_F_User <- Citi_Bike %>%
  count(usertype, gender, sort= FALSE)
colnames(M_F_User) <- c("User Type", "Gender", "Count")
fig <- plot_ly(
  type='table',
  header=list(
    values=names(M_F_User), 
    align = c("center", "center"),
    line = list(width = 1, color = 'white'),
    fill= list(color= c("#003B70", "#003B70")),
    font= list(family = "San Serif", size =15, color = "white"),
    height= 30),          
  cells=list(
    values=unname(M_F_User),
    align = c("center", "center"),
    line = list(width = 1, color = 'white'),
    fill= list(color= c("#58A0DB", "#58A0DB")),
    font= list(family = "San Serif", size =15, color = "white"),
    height= 30)) %>% 
  layout(paper_bgcolor='#58A0DB')
# api_create(p=fig, filename = "Count of Customers & Their Genders")
fig 
# ------------------------------------------------
#Count of Genders Using Citi Bikes
M_F <- aggregate(usertype ~ gender, data= Citi_Bike, FUN = length)
write.csv(M_F,'M_F.csv')
M_F1 <- read.csv("~/Desktop/Data 101/HW 3/M_F.csv")
library(plotly)

t <- list(
  family = "sans serif",
  size= 16,
  color = 'white')
fig <- plot_ly(x = c(M_F1$gender), y = c(M_F1$usertype), width = 700, height = 700, type = 'bar', marker = list(color = '#003B70')) %>% 
  layout(title= list(text = "Count of Genders Using Citi Bikes",font = t), font=t, 
         xaxis = list(title = list(text ='Gender', font = t)), 
         yaxis = list(title = list(text ='Amount of People', font = t)),
         plot_bgcolor='#e5ecf6', paper_bgcolor='#58A0DB')
# api_create(p=fig, filename = "Count of Genders Using Citi Bikes")
fig

# ------------------------------------------------
#Avg Age of User Types
agealc <- function(birth.year, current){
  require(Citi_Bike.table)
  current <- 2020
  y <- year(current) - year(birth.year) -1
  age <- y 
  return (age)
}
k <- aggregate(Age ~ usertype, data= Citi_Bike, FUN = mean) #Grader Notes: replaced "mean" for "median," "min," and "max" to calculate those values. 
write.csv(k, 'Age_User.csv')
Age_User <- read.csv("~/Desktop/Data 101/HW 3/Age_User.csv")

t <- list(
  family = "sans serif",
  size= 16,
  color = 'white')

fig <- plot_ly(x = c(Age_User$usertype), y = c(Age_User$Age), width = 700, height = 700, type = 'bar', marker = list(color = '#003B70')) %>%
  layout(title= list(text = "Average Age of User Types",font = t), font=t,
         xaxis = list(title = list(text ='User Types', font = t)),
         yaxis = list(title = list(text ='Average Age of Users', font = t)),
         plot_bgcolor='#e5ecf6', paper_bgcolor='#58A0DB')
# api_create(p=fig, filename = "Average Age of User Types")
fig
#--------------------------
#Sub & Cust Count

Sub_Cust <- table(Citi_Bike$usertype)
View(Sub_Cust)
Sub_Cust1 <- write.csv(Sub_Cust, "Sub_Cust.csv")
data_nohead <- read.csv("Sub_Cust.csv", header = TRUE)
colnames(data_nohead) <- c(" ", "User Type", "Count")
View(data_nohead)
