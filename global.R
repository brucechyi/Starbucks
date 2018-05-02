library(dplyr)
library(data.table)
library(tidyr)

cleantb = fread("./cleantable.csv")
countrynames_df <- as.data.frame(unique(cleantb %>% select(Country, Code) %>% group_by(Country,Code)))
statenames_df <- as.data.frame(unique(cleantb %>% filter(Code=='US') %>% select(State,State_code)))

cleantb <- cleantb %>% mutate(Stores_Per_Capita = ifelse(Code=='US', 
                                                 (State_Stores/State_population)*10000,
                                                 (Country_Stores/Country_Population)*10000))
