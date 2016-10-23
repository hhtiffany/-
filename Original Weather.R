library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("~/Documents/BU/MA 675 STAT PRAC 1/Project COB/original data/andy/combined_for_analysis.csv")

# number of events per month
month <- df %>% 
              group_by(year, month, day, CADEventLocationIdentifier) %>%
              summarise(n= length(Start.Calendar.Date)) %>%
              group_by(year, month) %>%
              summarise(n=length(day))

month <- month %>% 
           mutate(y=year, m=month) %>%
           unite(date, y, m, sep="-")

m <- month
m$n[m$n<mean(m$n)] <- -10
mean(month$n) #47

plot(month$date, month$n, col=factor(month$year), xlab="Month", ylab="Number of Events", main="Number of Events per Month")
lines(month$date, month$n)
points(m$n, col="red", pch=20, cex=3)

# weather analysis
dw <-read.csv("~/Documents/BU/MA 675 STAT PRAC 1/Project COB/original data/weather/weather with event amounts.csv")

# huge wind vs less wind 11
meanwind <- subset(dw, select=c(year, month, Mean.Wind.SpeedMPH, Events, n))
meanwind <- filter(meanwind, month==12 | month <=5)

list(mean(meanwind$Mean.Wind.SpeedMPH[1:148]), mean(meanwind$Mean.Wind.SpeedMPH[149:294])) # 11.6  11.3
mean(meanwind$Mean.Wind.SpeedMPH) # 11.5

windday <- meanwind %>%
           filter(Mean.Wind.SpeedMPH > 11) %>%
           group_by(year, month) %>%
           summarise(nwin=mean(n)) %>%
           mutate(y=year, m=month) %>%
           unite(date, y, m, sep="-")

lesswindday <- meanwind %>%
           filter(Mean.Wind.SpeedMPH <= 11) %>%
           group_by(year, month) %>%
           summarise(nlesswin=mean(n)) %>%
           mutate(y=year, m=month) %>%
           unite(date, y, m, sep="-")

ww <- inner_join(windday, lesswindday, by=c("year"="year", "month"="month", "X"="X", "date"="date")) 


plot(ww$date, ww$nwin, col=factor(ww$year), 
     xlab="Month", ylab="Average Events per day", main="Average Events per Windy/Lesswindy day Level=11")
 points(ww$nwin, pch=20)
 lines(ww$date, ww$nwin)
 points(ww$nlesswin, pch=20, col="blue")
 lines(ww$date, ww$nlesswin, pch=2, col="blue")

# huge wind vs less wind 15
wind <- meanwind %>%
          filter(Mean.Wind.SpeedMPH > 15) %>%
          group_by(year, month) %>%
          summarise(nwin=mean(n)) %>%
          mutate(y=year, m=month) %>%
          unite(date, y, m, sep="-")

lesswind <- meanwind %>%
          filter(Mean.Wind.SpeedMPH <= 15) %>%
          group_by(year, month) %>%
          summarise(nlesswin=mean(n)) %>%
          mutate(y=year, m=month) %>%
          unite(date, y, m, sep="-")


ww15 <- inner_join(wind, lesswind, by=c("year"="year", "month"="month", "X"="X", "date"="date")) 


plot(ww15$date, ww15$nwin, col=factor(ww15$year), 
     xlab="Month", ylab="Average Events per day", main="Average Events per Windy/Lesswindy day Level=15")
 points(ww15$nwin, pch=20)
 lines(ww15$date, ww15$nwin)
 points(ww15$nlesswin, pch=20, col="blue")
 lines(ww15$date, ww15$nlesswin, pch=2, col="blue")
