source("R/00_preamble.R")


# LOAD RED LIST DATA  -------------------------------------------------
redlist_data <- read_csv("Data/data_outputs/redlist_data.csv")

# create vector with species names as input
species <- unique(redlist_data$Species)

# harmonization code
df <- list()
for(i in 1:length(species)){
  
  temp <- get_gbifid_(species[i])[[1]] 
  if(nrow(temp)>=1){
    temp <- temp %>% 
      filter(kingdom == "Plantae") %>% 
      filter(phylum == "Tracheophyta") %>% 
      filter(!(rank == "genus"| rank == "family"| rank == "order"| rank == "phylum"))
  } else {temp <- temp} 
  
  
  if(nrow(temp) == 0){next} else{
    if(nrow(temp)>=1 & temp %>% filter(matchtype == "EXACT") %>% nrow >= 1){
      
      temp <-  temp %>% 
        filter(kingdom == "Plantae") %>% 
        filter(phylum == "Tracheophyta") %>% 
        filter(matchtype == "EXACT")
      
      if(temp %>% filter(confidence == 100) %>% nrow == 1) {
        
        temp <- temp %>% filter(confidence == 100) 
        
        temp2 <-  temp %>% 
          filter(row_number()== 1 ) %>% 
          select_if(~ !any(is.na(.)))
        
        if(length(temp2$specieskey) >= 1){
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname,
            speciesKey = ifelse(length(temp$specieskey) == 1, temp$specieskey, NA) ,
            species = ifelse(length(temp$species) == 1, temp$species, NA) , 
            phylum = temp$phylum,
            rank = temp$rank,
            status = temp$status)
        } else 
          
        {
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname[z],
            speciesKey = ifelse(temp$rank == "subspecies", NA, temp$usagekey[z]),
            species = ifelse(temp$rank == "subspecies", NA, temp$canonicalname[z]), 
            phylum = temp$phylum[z],
            rank = temp$rank[z],
            status = temp$status[z]) 
        }
        
      } else {
        
        hc <-c()
        
        for(k in 1:nrow(temp)) {
          hc[k] <- occ_count(temp$usagekey[k])
        }
        z <- first(which.max(hc))
        
        temp2 <-  temp %>% 
          filter(row_number()== z ) %>% 
          select_if(~ !any(is.na(.)))
        
        if(length(temp2$specieskey[z]) > 0){
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname[z],
            speciesKey = ifelse(length(temp$specieskey[z]) == 1, temp$specieskey[z], NA) ,
            species = ifelse(length(temp$species[z]) == 1, temp$species[z], NA) , 
            phylum = temp$phylum[z],
            rank = temp$rank[z],
            status = temp$status[z]) 
        } else {
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp2$scientificname[z],
            speciesKey = ifelse(temp2$rank == "subspecies", NA, temp2$usagekey[z]),
            species = ifelse(temp2$rank == "subspecies", NA, temp2$canonicalname[z]), 
            phylum = temp2$phylum[z],
            rank = temp2$rank[z],
            status = temp2$status[z]) 
        }
        
      }} else {
        
        if(nrow(temp)>=1){
          
          temp <-  temp %>% 
            filter(kingdom == "Plantae") %>% 
            filter(phylum == "Tracheophyta") %>% 
            filter(!(rank == "genus"| rank == "family"| rank == "order"| rank == "phylum")) %>% 
            filter(row_number()==1 )
          
          df[[i]] <- data.frame(
            Taxon = species[i],
            scientificname = temp$scientificname,
            speciesKey = ifelse(length(temp$specieskey) == 1, temp$specieskey, NA) ,
            species = ifelse(length(temp$species) == 1, temp$species, NA) , 
            phylum = temp$phylum,
            rank = temp$rank,
            status = temp$status)
        } else {
          next
        }
      }}
}


redlist_data_harmonized <- bind_rows(df)

# join with the original taxonomy
redlist_data_harmonized <- full_join(redlist_data, redlist_data_harmonized, by=c("Species" = "Taxon") )

# write csv
write.table(redlist_data_harmonized, "Data/data_outputs/redlist_data_harmonized.csv", row.names = FALSE, sep = ",")





# FILTER SUBSPECIES / NON-VASCULAR PLANTS / UNRECOGNIZED -------------------------------
# read csv
redlist_data_harmonized <- read.csv("Data/data_outputs/redlist_data_harmonized.csv")

redlist_clean <- redlist_data_harmonized %>% 
  # omit species with no data, these can be for example funghi, non-vascular plants etc.
  drop_na(species) %>% 
  # include only species that where identified as species, meaning exclude subspecies
  filter(rank == "species")

length(unique(redlist_clean$Species)) # 9661 species before harmonization  
length(unique(redlist_clean$species)) # 6907 species after harmonization


length(unique(redlist_clean$speciesKey)) # 6921 speciesKey
redlist_clean %>% 
  select(speciesKey, species) %>% 
  distinct() %>% 
  count(species) %>%
  arrange(desc(n)) %>% head(n = 20)
# an accepted species can have more speciesKeys when a submitted species name yields DOUBTFUL as status

# write csv
write.table(redlist_clean, "Data/data_outputs/redlist_clean.csv", row.names = FALSE, sep = ",")

