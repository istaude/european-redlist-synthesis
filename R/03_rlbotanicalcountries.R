source("R/00_preamble.R")


# MATCH THE RED LIST COUNTRIES BOTANICAL COUNTRIES ------------------------

# load data
redlist_clean <- read.csv("Data/data_outputs/redlist_clean.csv")
unique(redlist_clean$Country)


# regroup and reclassify RL countries according to botanical countries (which KEW uses)
# more info here: https://github.com/tdwg/wgsrpd
redlist_clean <- redlist_clean %>%
  mutate(botanical_countries_l3 = as.character(Country)) %>% 
  # Czechoslovakia
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Slovakia", "Czechoslovakia")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Czech Republic", "Czechoslovakia")) %>% 
  
  # Baltic states
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Estonia", "Baltic States")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Latvia", "Baltic States")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Lithuania", "Baltic States")) %>% 
  
  # Belgium
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Luxembourg", "Belgium")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Flanders", "Belgium")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Wallonia", "Belgium")) %>% 
  
  # Yugoslavia
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Slovenia", "Yugoslavia")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Bosnia and Herzegovina", "Yugoslavia")) %>%
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Montenegro", "Yugoslavia")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Croatia", "Yugoslavia")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Kosovo", "Yugoslavia")) %>% 
  
  # Great Britain
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="England", "Great Britain")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Wales", "Great Britain")) %>% 
  
  # Malta
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Malta", "Italy")) %>% 
  
  # Ukraine
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Moldova", "Ukraine")) 


# write csv
write.table(redlist_clean, "Data/data_outputs/redlist_clean.csv", row.names = FALSE, sep = ",")

