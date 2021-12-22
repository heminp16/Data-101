convertDecDate <- function(decdate){
  year <- floor(decdate)
  decday <- decdate-year
  if(((year%%4)==0) & (year!=1900)){
    numdays=366
    months = c(31,60,91,121,152,182,213,244,274,305,335,366)
  } 
  else{
    numdays=365
    months = c(31,59,90,120,151,181,212,243,272,304,334,365)
    }
  day = decday*numdays + 0.5
  month=1
  while((month < 12) & (day>months[month])){
    month <- month+1
  }
  return(c(round(year,0),round(month,0),round(day,0)))
  #code used with data frames
   temp$Year <- sapply(temp$Date,FUN=convertDecDate)[1,]
   temp$Month <- sapply(temp$Date,FUN=convertDecDate)[2,]
}