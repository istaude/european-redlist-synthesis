source("R/00_preamble.R")


# JOIN RED LIST SPECIES WITH KEW SPECIES ----------------------------------------------

# read species list
kew_sp <- fread("Data/kew/checklist_names.txt")

# load red list species
redlist_clean <- read.csv("Data/data_outputs/redlist_clean.csv")

# join with kew to find accepted plant name id, which is needed to find the species distribution
rl_kew <- left_join(redlist_clean %>% select(species) %>% distinct, 
                    kew_sp %>% select(taxon_name, accepted_plant_name_id, taxon_rank, taxon_status), 
                    by = c("species"="taxon_name") )

# some species have more than one accepted plant name id, often one is a synonym 
# (same species name but described by different author, which links to another species)
length(unique(rl_kew$species))
nrow(rl_kew)

# seperate species with more than 1 entry, for those that have more than 1, choose the Accepted entry
rl_kew <- bind_rows(
  rl_kew %>% 
    group_by(species) %>% 
    mutate(count = n()) %>% 
    filter(count == 1)
  ,
  rl_kew %>% 
    group_by(species) %>% 
    mutate(count = n()) %>% 
    filter(count > 1) %>% 
    filter(taxon_status == "Accepted")
) %>% filter(row_number()==1)



# c. 230 species are not found in KEW due to spelling differences, add manually
rl_kew_na <- rl_kew %>% filter(is.na(accepted_plant_name_id)) 

# load csv where species where individually checked with the powo database
rl_kew_na_solved <- read_csv("Data/data_outputs/rl_kew_na_solved.csv", 
                             locale = locale(encoding = "ISO-8859-1"))

rl_kew_na_solved <- left_join(rl_kew_na_solved, 
                    kew_sp %>% select(taxon_name, accepted_plant_name_id, taxon_rank, taxon_status), 
                    by = c("species_powo"="taxon_name") ) %>% 
  filter(taxon_status == "Accepted")

# delete the rows where the species has NA and add the rows with the right species name
rl_kew <- anti_join(rl_kew, rl_kew_na) %>% 
  mutate(species_powo = species) %>% 
  select(-count) %>% 
  bind_rows(rl_kew_na_solved) # species powo now contains the correct species according to KEW

# wherever there is a blank entry for accepted_plant_name_id, species is recognized by KEW but 
# there is no link in KEW to an accepted species and hence no distribution, 
# happens when species name is unplaced
rl_kew %>% filter(accepted_plant_name_id == "")
rl_kew <- rl_kew %>% filter(taxon_status != "Unplaced")

# linked for 7,011 species
nrow(rl_kew)
View(rl_kew)




# FIND RED LIST SPECIES DISTRIBUTIONS ----------------------------------------------

# read distribution data
kew_dis <- fread("Data/kew/checklist_distribution.txt")

# botanical countries e.g. Canary islands / Sicilly / Corse are actually included in the national RL of Spain / Italy / France
kew_dis <- kew_dis %>% 
mutate(botanical_countries_l3 = as.character(area)) %>% 

  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Corse", "France")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Kriti", "Greece")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Krym", "Ukraine")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="East Aegean Is.", "Greece")) %>% 
  
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Canary Is.", "Spain")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Baleares", "Spain")) %>%
  
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Sicilia", "Italy")) %>% 
  mutate(botanical_countries_l3=replace(botanical_countries_l3, botanical_countries_l3=="Sardegna", "Italy"))


# subset kew dis to include only rl species
rl_dis <- rl_kew %>% 
  select(species, species_powo, accepted_plant_name_id) %>% 
  rename(plant_name_id = accepted_plant_name_id) %>% 
  na.omit() %>% 
  left_join(kew_dis)


# some rl species have ids but have no distribution
rl_dis %>% filter(is.na(area))
rl_dis <- rl_dis %>% filter(!is.na(area))




# join with the countries in which species are threatened


# first confine redlist_clean to those species that have a kew plant name id and distribution
redlist_clean <- read.csv("Data/data_outputs/redlist_clean.csv")
redlist_clean <- rl_dis %>% 
  select(species, species_powo, plant_name_id) %>% 
  distinct %>% 
  left_join(redlist_clean) %>% 
  select(species, species_powo, plant_name_id, botanical_countries_l3, Threatened) %>% 
  distinct


# some species are red-listed in countries where they don't occur according to kew: Aquilegia transsilvanica / Callianthemum coriandrifolium 
# occur according to KEW only in Romania, but on Euro+Med it is also occurring in Ukraine, where they are mentioned as threatened on the rl
# bind these rows to the rl_dis data frame
rl_dis <- bind_rows(
  rl_dis %>% 
    select(species, species_powo, plant_name_id, botanical_countries_l3) %>% 
    distinct,
  redlist_clean %>% 
    select(species, species_powo, plant_name_id, botanical_countries_l3) %>% 
    distinct
) %>% distinct


# join the threatened column and convert to binary variable
rl_dis <- rl_dis %>% left_join(redlist_clean)
rl_dis <- rl_dis %>% mutate(Threatened = ifelse(is.na(Threatened), 0, 1) ) %>% 
  ungroup %>% 
  select(-species) %>% 
  rename(species = species_powo)

 

# some species are listed as extinct in KEW in a specific country, where the national red lists
# don't list this species anymore. This is for example the case for Bromos Bromus bromoideus.
# merge this info from kew with the national red lists.
rl_dis <- rl_dis %>% 
  left_join(kew_dis %>% 
              select(plant_name_id, botanical_countries_l3, extinct) %>% 
              filter(extinct == 1) %>% 
              distinct) %>% 
  mutate(extinct = ifelse(is.na(extinct), 0, 1) ) %>% 
  mutate(Threatened = Threatened + extinct) %>% 
  mutate(Threatened = ifelse(Threatened >= 1, 1, 0) ) %>% 
  select(-extinct) %>% 
  distinct




# WHICH RL SPECIES ENDEMIC TO EUROPE ---------------------------------------

# European botanical countries
eu_coun <- c("France" , "Spain" , "Italy" , "Greece" ,
             "Yugoslavia" , "Great Britain" , "Austria" ,
             "Belgium" , "Hungary" , "Switzerland" ,
             "Portugal" , "Albania" , "Bulgaria" ,
             "Romania" ,
             "Ukraine" , "Denmark" , "Czechoslovakia" ,
             "Germany" , "Poland" , "Finland" ,
             "Norway" , "Sweden" ,
             "Belarus" , "Baltic States" ,
             "Ireland" , "Netherlands" , "Cyprus")


eu_endemics <- rl_dis %>% 
  select(species, botanical_countries_l3) %>% 
  # new column innout: if country in europe -> europe
  mutate(innout = ifelse(botanical_countries_l3 %in% eu_coun, "europe", "not_europe")) %>% 
  #throw out area column 
  dplyr::select(-botanical_countries_l3) %>% 
  #keep only unique rows
  distinct %>% 
  group_by(species) %>% 
  # is the species only in or also outside of Europe 
  # keep species if it only occurs once (only europe or only not_europe)
  filter(n() == 1) %>% 
  select(-innout) %>% 
  mutate(europe_endemic = 1)

rl_dis <- full_join(rl_dis, eu_endemics)
rl_dis <- rl_dis %>% mutate(europe_endemic = ifelse(is.na(europe_endemic), 0, 1) )

# how many endemic red-listed species
rl_dis %>% filter(europe_endemic == 1) %>% select(species) %>% distinct %>% nrow

# write csv
write.csv(rl_dis, "Data/data_outputs/rlspecies_distribution.csv", row.names = FALSE, sep = ",")

