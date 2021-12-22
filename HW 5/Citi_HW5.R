Citi_Bike <- read.csv("/Users/hemin/Desktop/Data 101/HW 3/citibike.May20.50K.csv")
View(Citi_Bike)
Citi_Bike$gender[Citi_Bike$gender == 0] <- "Unknown"
Citi_Bike$gender[Citi_Bike$gender == 1] <- "Male"
Citi_Bike$gender[Citi_Bike$gender == 2] <- "Female"

citi.Male = subset(Citi_Bike, Citi_Bike$gender == "Male")
citi.Female = subset(Citi_Bike, Citi_Bike$gender == "Female")

# Hypo 1: On an average, Women typically ride longer than Men.
# Null Hypo 1: On an average, both Women and Men typically ride the same duration.

# Mean 
c.MD_mean = mean(citi.Male$tripduration) #1398.589
c.FD_mean = mean(citi.Female$tripduration) #1721.517sa

# Standard Deviation 
sd.MD = sd(citi.Male$tripduration) # 12093.15
sd.FD = sd(citi.Female$tripduration) #14905.09

num.MD = length(citi.Male$tripduration)
num.FD = length(citi.Female$tripduration)


sd.c.s = sqrt(sd.MD^2/num.MD + sd.FD^2/num.FD)
z.score.1= (c.MD_mean - c.FD_mean)/sd.c.s
p.value.1 = 1 - pnorm(z.score.1) 

# p.value = 0.9894566
# z.score= -2.306427

plot(x=seq(from = -22, to= 22, by=0.1),y=dnorm(seq(from = -22, to= 22,  by=0.1),mean=1),type='l', xlab = 'Mean difference',  ylab='Possibility') 
abline(v= z.score.1, col = "blue")
par(bg = "transparent")






# Hypo 2: There are more Male subscribers rather than Female subscribers.
# Null Hypo: Null: There is an equal amount of Male and Female subscribers. 

Citi_Sub= subset(Citi_Bike, Citi_Bike$usertype == "Subscriber")
c.Fem_mean= mean(Citi_Sub$gender == "Female")
c.Mal_mean= mean(Citi_Sub$gender == "Male")

# Standard Deviation 
sd.Fem= sd(Citi_Sub$gender == "Female") 
sd.Mal= sd(Citi_Sub$gender == "Male")

#Length - Count of Men and Women 
num.Fem = length(Citi_Sub$gender == "Female")
num.Mal = length(Citi_Sub$gender == "Male")


sd.m.f = sqrt(sd.Mal^2/num.Mal + sd.Fem^2/num.Fem)
z.score.2= (c.Mal_mean - c.Fem_mean)/sd.m.f
p.value.2 = 1 - pnorm(z.score.2) 


# P-value is 0 == the p-value is so small, it returns 0. 

plot(x=seq(from = -22, to= 100, by=0.1),y=dnorm(seq(from = -22, to= 100,  by=0.1),mean=1),type='l', xlab = 'Mean difference',  ylab='Possibility') 
abline(v= z.score.2, col = "blue")
par(bg = "transparent")


