library(tidyr)
library(dplyr)

weather <- read.csv("~/Documents/BU/MA 675 STAT PRAC 1/Project COB/original data/weather/Complete Weather Data")

df <- read.csv("~/Documents/BU/MA 675 STAT PRAC 1/Project COB/original data/andy/combined_for_analysis.csv")

# event count
df_e <- df %>%
  group_by(CAD.Event.Type.Name) %>%
  summarise(n=length(CAD.Event.Type.Name)) %>%
  arrange(desc(n))

# weather data with only event amounts per day
dayevent <- df %>% 
              group_by(year, month, day) %>%
              summarise(n= length(Start.Calendar.Date))


w <- weather %>% 
              select(EDT, contains("Temper"), contains("Wind"), PrecipitationIn, CloudCover, Events) %>%
              mutate(date=EDT) %>%
              separate(EDT, c("year", "month", "day"), sep="-") %>%
              group_by(year, month, day) 
            

w$year <- as.numeric(w$year)
w$month <- as.numeric(w$month)
w$day <- as.numeric(w$day)

dw <- inner_join(dayevent, w, by=c("year"="year", "month"="month", "day"="day")) 

write.csv(dw, 'weather with event amounts')

# combine weather data with Andy's combined data
df <- df %>%
         group_by(year, month, day) %>%
         arrange(year, month, day)

weather.c <- weather %>% 
        mutate(date=EDT) %>%
        separate(EDT, c("year", "month", "day"), sep="-") %>%
        group_by(year, month, day) 

weather.c $year <- as.numeric(weather.c $year)
weather.c $month <- as.numeric(weather.c $month)
weather.c $day <- as.numeric(weather.c $day)

com.data.with.wea <- left_join(df, weather.c,
                               copy=TRUE,
                               by=c("year"="year", "month"="month", "day"="day")) 

write.csv(com.data.with.wea, 'comdatawithwea')
