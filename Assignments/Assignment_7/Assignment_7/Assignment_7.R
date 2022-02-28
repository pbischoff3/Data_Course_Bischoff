library(tidyverse)
library(dplyr)

##Import the 4 related datasets found in the Data_Course/Data/flights/ directory. There should be:
#airlines.csv
#airports.csv
#jan_flights.csv
#Jan_snowfall.csv

airlines <- read_csv("../../Assignment_7/airlines.csv")
airports <- read_csv("../../Assignment_7/airports.csv")
jan_flights <- read.csv("../../Assignment_7/jan_flights.csv")
Jan_snowfall <- read.csv("../../Assignment_7/Jan_snowfall.csv")



##Combine the data sets appropriately to investigate whether departure delay was correlated with snowfall amount
##You will need to think carefully about column names


jan_flights$Date<-as.Date(with(jan_flights,paste(jan_flights$YEAR,jan_flights$MONTH,jan_flights$DAY,sep="-")),"%Y-%m-%d")

flights1 <- jan_flights %>% 
  select(c("Date","AIRLINE","ORIGIN_AIRPORT","DESTINATION_AIRPORT","DEPARTURE_DELAY","ARRIVAL_DELAY")) %>% 
  rename(iata = "ORIGIN_AIRPORT")

airport1 <- airports %>% 
  rename(iata = "IATA_CODE")

airport_snow <- merge(x=airport1,y=Jan_snowfall, by = "iata", all = TRUE)

all_three <- merge(x=flights1,y=airport_snow, by = c("Date","iata"), all= TRUE) 

all_three_1 <- all_three %>% 
  rename(airline_code = "AIRLINE")

airlines_1 <- airlines %>% 
  rename(airline_code = "IATA_CODE")

all_four <- merge(x=airlines_1,y=all_three_1, by = "airline_code")

##Plot average departure delays by state over time
state_over_time <- all_four %>% 
  ggplot(aes(x = Date, y = DEPARTURE_DELAY)) +
  facet_wrap(~STATE)+
  geom_line()

state_over_time


##Plot average departure delays by airline over time
airline_over_time <- all_four %>% 
  ggplot(aes(x = Date, y = DEPARTURE_DELAY)) +
  facet_wrap(~AIRLINE)+
  geom_line()

airline_over_time


##Plot effect of snowfall on departure and arrival delays
snowfall_departure <- all_four %>% 
  ggplot(aes(x = DEPARTURE_DELAY, y = snow_precip_cm))+
  geom_line()

snowfall_departure


snowfall_arrival <- all_four %>% 
  ggplot(aes(x = ARRIVAL_DELAY, y = snow_precip_cm))+
  geom_line()

snowfall_arrival
