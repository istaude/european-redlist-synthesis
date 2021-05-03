source("R/00_preamble.R")



# WHICH SPECIES ARE LISTED MOST FREQUENTLY ON NATIONAL RLS ----------------
# read data
rl_dis <- fread("Data/data_outputs/rlspecies_distribution.csv")

rl_dis %>% 
  group_by(species) %>% 
  summarise(no_countries_threatened = sum(Threatened)) %>% 
  arrange(desc(no_countries_threatened)) 


# create data frame for Herminium monorchis, with area codes, for example figure
kew_dis <- fread("Data/kew/checklist_distribution.txt")

H_monorchis <- rl_dis %>% 
  filter(species == "Herminium monorchis") %>% 
  left_join(kew_dis %>% select(plant_name_id, area_code_l3, area), 
            by = c("plant_name_id", "botanical_countries_l3" = "area"))

# write csv
write.csv(H_monorchis, "Data/data_outputs/H_monorchis_distribution.csv", row.names = FALSE, sep = ",")







# CALCULATE THE % OF RANGE THREATENED FOR SPECIES ENDEMIC TO EUROPE -------
# read data
rl_dis <- fread("Data/data_outputs/rlspecies_distribution.csv")


# only endemic species
rl_dis_en <- rl_dis %>% filter(europe_endemic == 1)

# 3282 species endemic that occur on some red list
length(unique(rl_dis_en$species))


rl_dis_en <- rl_dis_en %>% 
  group_by(species) %>% 
  summarise(no_countries = n_distinct(botanical_countries_l3),
            no_countries_threatened = sum(Threatened)) %>% 
  mutate(percentage_threatened = no_countries_threatened / no_countries)


# how many species that are threatened in 100% of their range 
rl_dis_en %>% 
  filter(percentage_threatened == 1) %>% 
  select(species) %>% 
  distinct %>% 
  nrow

# write csv
write.csv(rl_dis_en, "Data/data_outputs/rlspecies_percentage_threatened.csv", row.names = FALSE, sep = ",")


# how many species that are threatened in 100% of their range are occurring in 1,2,3 etc. countries
rl_dis_en %>% 
  filter(percentage_threatened == 1) %>% 
  count(no_countries)


# find example species occurring in two countries only, there being threatened
rl_dis_en %>% 
  filter(percentage_threatened == 1) %>% 
  filter(no_countries == 2) %>% View


# create data frame for Achillea thracica, with area codes, for example figure 
A_thracica <- rl_dis %>% 
  filter(species == "Achillea thracica") %>% 
  left_join(kew_dis %>% select(plant_name_id, area_code_l3, area), 
            by = c("plant_name_id", "botanical_countries_l3" = "area"))

# write csv
write.csv(A_thracica, "Data/data_outputs/A_thracica_distribution.csv", row.names = FALSE, sep = ",")







# SUPP INFORMATION --------------------------------------------------------

# europe endemics, how many in 1,2,3... countries
rl_dis_en %>% count(no_countries) 



# which countries contribute most to the species threatened in 100% of their range
rl_dis_en %>% 
  filter(percentage_threatened == 1) %>% 
  select(species) %>% 
  left_join(rl_dis) %>%
  count(botanical_countries_l3) %>% 
  arrange(desc(n)) %>% 
  View


# which genera contribute most to the species threatened in 100% of their range
rl_dis_en %>% 
  filter(percentage_threatened == 1) %>% 
  mutate(genera = str_split_fixed(species, " ", 2)[,1]) %>% 
  count(genera) %>% 
  arrange(desc(n))






