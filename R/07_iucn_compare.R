source("R/00_preamble.R")



# CROSS RL SPECIES WITH IUCN GLOBAL ASSESSMENT ----------------------------

# read iucn data
iucn <-fread("Data/iucn_assessments.csv")

iucn <- iucn %>% 
  select(scientificName, redlistCategory) %>% 
  rename(species = scientificName)

# join with rl_dis
rl_dis_en <- fread("Data/data_outputs/rlspecies_percentage_threatened.csv")

#join endemic species with iucn assessment
iucn_rl <-left_join(rl_dis_en, iucn)

# write csv
write.csv(iucn_rl, "Data/data_outputs/iucn_rl.csv", row.names = FALSE, sep = ",")


# add interval column corresponding to percentage range threatened
iucn_rl_cross <- iucn_rl %>% 
  mutate(interval_threatened = cut(percentage_threatened, breaks = c(0, .25, .5, .75, 1))) %>% 
  group_by(interval_threatened) %>% 
  count(redlistCategory)


# iucn cross only for species 100% threatened in range
iucn_threat <- iucn_rl %>%
  filter(percentage_threatened == 1) %>% 
  select(-percentage_threatened) %>% 
  
  mutate(redlistCategory = replace_na(redlistCategory, "Not Evaluated")) %>% 
  mutate(redlistCategory = replace(redlistCategory, redlistCategory=="Extinct", "Extinct")) %>% 
  mutate(redlistCategory = replace(redlistCategory, redlistCategory=="Extinct in the Wild", "Extinct")) %>% 
  
  mutate(redlistCategory = replace(redlistCategory, redlistCategory=="Critically Endangered", "Threatened")) %>% 
  mutate(redlistCategory = replace(redlistCategory, redlistCategory=="Endangered", "Threatened")) %>% 
  mutate(redlistCategory = replace(redlistCategory, redlistCategory=="Vulnerable", "Threatened")) %>% 
  
  mutate(redlistCategory = replace(redlistCategory, redlistCategory=="Near Threatened", "Lower Risk")) %>% 
  mutate(redlistCategory = replace(redlistCategory, redlistCategory=="Least Concern", "Lower Risk")) %>% 
  
  mutate(redlistCategory = replace(redlistCategory, redlistCategory=="Data Deficient", "Data Deficient")) %>% 
  mutate(redlistCategory = replace(redlistCategory, redlistCategory=="Not Evaluated", "Not Evaluated")) %>% 
  
  group_by(redlistCategory) %>% 
  count(redlistCategory) %>% 
  ungroup() %>% 
  mutate(n=n/sum(n)*100) %>% 
  mutate(threat = "100% range threatened")

# write csv
write.csv(iucn_threat, "Data/data_outputs/iucn_threat.csv", row.names = FALSE, sep = ",")



# what overall percentage is not assessed
iucn_rl %>% filter(is.na(redlistCategory)) %>% nrow / nrow(iucn_rl) 
# 87% of the species endemic to europe and threatened in at least one country is not assessed
# of the species we classify as 100% threatened in their range

# how many of the 100% threatened species is not assessed
iucn_rl %>%
  filter(percentage_threatened == 1) %>% 
  filter(is.na(redlistCategory) | redlistCategory == "Data Deficient") %>% 
  nrow
1553/1842
