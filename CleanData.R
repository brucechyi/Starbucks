library(dplyr)
library(data.table)
library(tidyr)

#setwd("/Users/bruce/Desktop/DataScienceBootcamp/shiny-examples/063-superzip-example_test")
raw_sbux <- fread('./starbucks_locations.csv')
uspop_file  <- fread('./population_us.csv')
worldpop_file <- fread('./population_world.csv')
ctycode_file <- fread('./country_codes.csv')

#remove rows without coordinates
raw_sbux <- raw_sbux[!is.na(raw_sbux$Latitude),]

# Select only the columns required
raw_sbux <- raw_sbux %>% 
  select(Country, State_code='State/Province', Longitude, Latitude)

# make a look table for country names
ctycode_file <- ctycode_file %>%
  select(Country_name = official_name_en, Country_code2 = "ISO3166-1-Alpha-2", Country_code3 = "ISO3166-1-Alpha-3")

# Fix missing country name
ctycode_file$Country_name[ctycode_file$Country_code2=='TW'] = 'Taiwan'

# clean world population file
worldpop_file <- worldpop_file %>% select(Country_code3="Country Code", Population="2016") %>%
  left_join(ctycode_file, by= c("Country_code3"))
# clean up rows with N/A's as country names because they are names with other descriptions
worldpop_file <- worldpop_file[!is.na(worldpop_file$Country_code2),] %>%
  select(Country_name, Country_code3, Country_code2, Population)
# get ride of any rows with "" as population
worldpop_file <- worldpop_file[!worldpop_file$Population=="",]
# Fix missing data for Taiwan
worldpop_file[nrow(worldpop_file) + 1,] = list("Taiwan","TWN","TW",23500000)


# Make statenamesdf as a lookup table
statenamesdf <- as.data.frame(state.name)
setnames(statenamesdf, "state.name", "State_name")
statenamesdf["code"] <- as.data.frame(state.abb)
statenamesdf <- statenamesdf %>% mutate_all(as.character)

### Make us_data file with count of store numbers and popoulation per state
us_data <- raw_sbux %>% 
  select(Country, State_code) %>%
  filter(Country=='US') %>%
  group_by(Country, State_code) %>% 
  summarise(StoreCount=n())

# join us_data with statenamedf to 
us_data <- us_data %>% left_join(statenamesdf, by=c("State_code" = "code"))

# Fix missing State_name
us_data$State_name[us_data$State_code=='DC'] = 'District of Columbia'

# Join us_data with uspop_file to include population per state
us_data <- us_data %>% 
  inner_join(uspop_file, by=c("State_name"="State")) %>%
  select(Country,State_name, State_code, State_store_count = StoreCount, State_population = "2018 Population" )

### Make a global file with store count 
global_data <- raw_sbux %>% group_by(Country) %>% summarise(Store_count=n())
global_data <- global_data %>% 
  left_join(ctycode_file, by=c("Country" = "Country_code2")) %>%
  select(Country_name, Country_code = Country, Store_count)

# Join to get population stats
global_data <- global_data %>% 
  left_join(worldpop_file, by= c("Country_code"="Country_code2", "Country_name")) %>%
  select(Country_name,Country_code,Store_count, Population)

###### Putting it together #####

# Join raw data with global data to get store count and population for all countries
all_data <- raw_sbux %>% left_join(global_data, by=c("Country"="Country_code"))

# update all State_code to "" where country is NOT United States
# this is done to avoid confusion in the Data source output in the UI
all_data[!all_data$Country=="US",]$State_code = ""

# Join with us_data to get store count and state-population for each state
all_data <- all_data %>% left_join(us_data, by=c("State_code")) %>% 
  select(Country_name, Country.x, State_name, State_code, 
         Store_count, Population, State_store_count, State_population, Longitude, Latitude )

cleantb <- all_data %>% 
  select(Country = Country_name,
         Code = Country.x,
         State = State_name,
         State_code,
         Country_Stores = Store_count,
         Country_Population = Population,
         State_Stores = State_store_count,
         State_population,
         Longitude,
         Latitude)

fwrite(cleantb, file="cleantable.csv")
