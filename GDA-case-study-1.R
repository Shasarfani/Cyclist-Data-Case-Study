##installing packages and loading them

install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages('ggplot2')

library('tidyverse')
library('janitor')
library('lubridate')
library('ggplot2')

#importing 2021 trip data

tripdata_202101 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202101-divvy-tripdata.csv", header = TRUE)
tripdata_202102 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202102-divvy-tripdata.csv", header = TRUE)
tripdata_202103 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202103-divvy-tripdata.csv", header = TRUE)
tripdata_202104 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202104-divvy-tripdata.csv", header = TRUE)
tripdata_202105 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202105-divvy-tripdata.csv", header = TRUE)
tripdata_202106 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202106-divvy-tripdata.csv", header = TRUE)
tripdata_202107 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202107-divvy-tripdata.csv", header = TRUE)
tripdata_202108 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202108-divvy-tripdata.csv", header = TRUE)
tripdata_202109 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202109-divvy-tripdata.csv", header = TRUE)
tripdata_202110 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202110-divvy-tripdata.csv", header = TRUE)
tripdata_202111 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202111-divvy-tripdata.csv", header = TRUE)
tripdata_202112 <- read.csv("C:\\Users\\alish\\Downloads\\cyclists_data\\cyclists_tripdata_2021\\202112-divvy-tripdata.csv", header = TRUE)




#reviewing the structure of the data

str(tripdata_202101)
str(tripdata_202102)
str(tripdata_202103)
str(tripdata_202104)
str(tripdata_202105)
str(tripdata_202106)
str(tripdata_202107)
str(tripdata_202108)
str(tripdata_202109)
str(tripdata_202110)
str(tripdata_202111)
str(tripdata_202112)

#converting "start_station_id and end_station_id" as integers

tripdata_202101 <- mutate(tripdata_202101, start_station_id = as.integer(start_station_id))
tripdata_202102 <- mutate(tripdata_202102, start_station_id = as.integer(start_station_id))
tripdata_202103 <- mutate(tripdata_202103, start_station_id = as.integer(start_station_id))
tripdata_202104 <- mutate(tripdata_202104, start_station_id = as.integer(start_station_id))
tripdata_202105 <- mutate(tripdata_202105, start_station_id = as.integer(start_station_id))
tripdata_202106 <- mutate(tripdata_202106, start_station_id = as.integer(start_station_id))
tripdata_202107 <- mutate(tripdata_202107, start_station_id = as.integer(start_station_id))
tripdata_202108 <- mutate(tripdata_202108, start_station_id = as.integer(start_station_id))
tripdata_202109 <- mutate(tripdata_202109, start_station_id = as.integer(start_station_id))
tripdata_202110 <- mutate(tripdata_202110, start_station_id = as.integer(start_station_id))
tripdata_202111 <- mutate(tripdata_202111, start_station_id = as.integer(start_station_id))
tripdata_202112 <- mutate(tripdata_202112, start_station_id = as.integer(start_station_id))

tripdata_202101 <- mutate(tripdata_202101, end_station_id = as.integer(end_station_id))
tripdata_202102 <- mutate(tripdata_202102, end_station_id = as.integer(end_station_id))
tripdata_202103 <- mutate(tripdata_202103, end_station_id = as.integer(end_station_id))
tripdata_202104 <- mutate(tripdata_202104, end_station_id = as.integer(end_station_id))
tripdata_202105 <- mutate(tripdata_202105, end_station_id = as.integer(end_station_id))
tripdata_202106 <- mutate(tripdata_202106, end_station_id = as.integer(end_station_id))
tripdata_202107 <- mutate(tripdata_202107, end_station_id = as.integer(end_station_id))
tripdata_202108 <- mutate(tripdata_202108, end_station_id = as.integer(end_station_id))
tripdata_202109 <- mutate(tripdata_202109, end_station_id = as.integer(end_station_id))
tripdata_202110 <- mutate(tripdata_202110, end_station_id = as.integer(end_station_id))
tripdata_202111 <- mutate(tripdata_202111, end_station_id = as.integer(end_station_id))
tripdata_202112 <- mutate(tripdata_202112, end_station_id = as.integer(end_station_id))

#confirming if the structure of the above mention 2 columns changes by using the str function to review thw structure

str(tripdata_202101)
str(tripdata_202102)
str(tripdata_202103)
str(tripdata_202104)
str(tripdata_202105)
str(tripdata_202106)
str(tripdata_202107)
str(tripdata_202108)
str(tripdata_202109)
str(tripdata_202110)
str(tripdata_202111)
str(tripdata_202112)

#data cleaning

#combining the data

tripdata_2021 <- rbind (tripdata_202101, tripdata_202102, tripdata_202103, tripdata_202104, tripdata_202105, 
                        tripdata_202106, tripdata_202107, tripdata_202108, tripdata_202109, tripdata_202110, 
                        tripdata_202111, tripdata_202112)

#counting the total rows of the data

nrow(tripdata_2021)

#reviewing the col names

head(tripdata_2021)

#reviewing the structure of the combined data
str(tripdata_2021)


#Adding 2 new columns with week of the day (where 1 = sunday) and the month extracted from the "started_at" date

tripdata_2021 <- mutate(tripdata_2021, day_of_the_week = wday(tripdata_2021$started_at))

tripdata_2021$month <- format(as.Date(tripdata_2021$started_at), "%b")

tripdata_2021$start_hour <- format(as.POSIXct(tripdata_2021$started_at), format = "%H")

tripdata_2021$end_hour <- format(as.POSIXct(tripdata_2021$ended_at), format = "%H")



#removing any duplicate rows based on the rider's id

#dropping any NA's from the data

tripdata_2021 <- na.omit(tripdata_2021)

tripdata_2021_revised <- tripdata_2021[!duplicated(tripdata_2021$ride_id), ]
print(paste("Removed", nrow(tripdata_2021) - nrow(tripdata_2021_revised), "duplicate rows"))

tripdata_2021_revised <- tripdata_2021 %>% distinct(ride_id, .keep_all = TRUE)
print(paste("Removed", nrow(tripdata_2021) - nrow(tripdata_2021_revised), "duplicate rows"))


#Adding a new column with the ride length
tripdata_2021_revised <- mutate(tripdata_2021_revised, ride_length = difftime(ended_at, started_at, units = "mins"))


#reviewing the col names 

head(tripdata_2021_revised)

##Data manipulation



#removing any rows that have "ride_length" that is less than 0 mins

nrow(tripdata_2021_revised[tripdata_2021_revised$ride_length < 0,])
tripdata_2021_revised <- tripdata_2021_revised[!tripdata_2021_revised$ride_length <0,]

nrow(tripdata_2021_revised)

#reviewing the data summary 

glimpse(tripdata_2021_revised)
View(tripdata_2021_revised)
 

#Saving the dataframe in a compressed file

write_rds(tripdata_2021_revised, 
          file ="tripdata_2021_revised.rds", compress="bz2")

#creating a table with the total rides by the membership type
rider_type <- table(tripdata_2021_revised$member_casual)
View(rider_type)

#Calculating the mean, median and mode of the ride length

trip_statistics <- tripdata_2021_revised %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length), standard_deviation = sd(ride_length), median_ride_length = median(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length))
head(trip_statistics)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

weekday_mode <- getmode(tripdata_2021_revised$day_of_the_week)
print(weekday_mode)

#determining the most popular day by the rider type

tripdata_2021_revised$day_of_the_week <- ordered(tripdata_2021_revised$day_of_the_week, levels = c("1", "2", "3","4", "5", "6", "7"))

tripdata_2021_revised %>%
  group_by(member_casual, day_of_the_week) %>%
  summarise(total_riders =n(), average_ride_length = mean(ride_length)) %>%
  arrange(member_casual, day_of_the_week)


#determining the most popular month based on the number of rides and length of the ride

Month_highest_rides <- tripdata_2021_revised %>%
  group_by(month) %>%
  summarise(number_of_rides = n(), averrage_ride_length = mean(ride_length)) %>%
  arrange(-number_of_rides)

View(Month_highest_rides)


#determining the popular start station

tripdata_2021_revised %>% count(start_station_name, sort = TRUE)

#determining the popular start station member vs casual rider

Popular_start_station_casual <- tripdata_2021_revised %>%
  filter(member_casual == 'casual') %>%
  group_by(start_station_name) %>%
  summarise(number_of_starts = n()) %>%
  arrange( - number_of_starts)
    
head(Popular_start_station_casual)

Popular_start_station_member <- tripdata_2021_revised %>%
  filter(member_casual == 'member') %>%
  group_by(start_station_name) %>%
  summarise(number_of_starts = n()) %>%
  arrange( - number_of_starts)

head(Popular_start_station_member)

#determining the popular hour to start a ride by causal and member riders

Popular_hour_casual_riders <- tripdata_2021_revised %>%
  filter(member_casual == 'casual') %>%
  group_by(hour) %>%
  summarise(popular_start_hour= n()) %>%
  arrange(-popular_start_hour)

head(Popular_hour_casual_riders)

popular_hour_member_riders <- tripdata_2021_revised %>%
  filter(member_casual == 'member') %>%
  group_by(hour) %>%
  summarise(popular_start_hour= n()) %>%
  arrange(-popular_start_hour)

head(popular_hour_member_riders)

#determining the popular hour to end a ride by causal and member riders


popular_end_hour_casual <- tripdata_2021_revised %>%
  filter(member_casual == 'casual') %>%
  group_by(end_hour) %>%
  summarise(popular_end_hour= n()) %>%
  arrange(-popular_end_hour)

head(popular_end_hour_casual)


popular_end_hour_member <- tripdata_2021_revised %>%
  filter(member_casual == 'member') %>%
  group_by(end_hour) %>%
  summarise(popular_end_hour= n()) %>%
  arrange(-popular_end_hour)

head(popular_end_hour_member)


#Let's add one more column named season that will help us determine the most popular season.

tripdata_2021_revised <- mutate(tripdata_2021,
                        Season = if_else(tripdata_2021_revised$'month' %in% c(Mar, Apr, May), "Spring"|
                                           if_else(tripdata_2021_revised$'month' %in% c(Jun, Jul, Aug), "Summer"|
                                                     if_else(tripdata_2021_revised$'month' %in% c(Sep, Oct, Nov), "Fall"|
                                                               if_else(tripdata_2021_revised$'month' %in% c(Dec, Jan, Feb), "Winter")))))



tripdata_2021 <- mutate %>%
  tripdata_2021[month %in% c("Jun","Jul","Aug"), Season := "Summer"] %>%
  tripdata_2021[month %in% c("Dec","Jan","Feb"), Season := "Winter"] %>%
  tripdata_2021[month %in% c("Sep","Oct","Nov"), Season := "Fall"] %>%
  tripdata_2021[is.na(Month), Season := "Spring"]


#rideable type most used by members vs casual rides



#save the cleaned .Rdata file.

write_rds(tripdata_2021_revised, 
          file ="Cleaned.Traffic.Dataset.rds", compress="bz2")


save(tripdata_2021_revised, file = "tripdata_2021_revised.RData")

#visualizations
#popular day of the week member vs casual

tripdata_2021_revised %>%
  group_by(member_casual, day_of_the_week) %>%
  summarise(total_riders =n(), average_ride_length = mean(ride_length)) %>%
  arrange(member_casual, day_of_the_week) %>%
  ggplot(aes(x= day_of_the_week, y= average_ride_length, fill= member_casual)) + 
  geom_col(position= 'dodge') +
           ggtitle("Popular day of the week")

#popular month in 2021

tripdata_2021_revised %>%
  group_by(month) %>%
  summarise(number_of_rides = n()) %>%
  arrange(-number_of_rides) %>%
  ggplot(aes(x= month, y= number_of_rides))+
  geom_col(position = 'dodge') +
  ggtitle("popular month in 2021")
