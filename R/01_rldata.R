source("R/00_preamble.R")
setwd("Data/national_redlists")

# LOAD NATIONAL RED LISTS -------------------------------------------------

# AUSTRIA
Austria_redlist <- read_excel("Austria_redlist.xlsx")
unique(Austria_redlist$Redlist_cat) # 0 = extirpated , 1 = CR, 2 = EN, 3 = VU, !r means that this can be regionally even worse
Austria_redlist <- Austria_redlist %>%  
  filter(!Redlist_cat == "-r") %>% 
  filter(!Redlist_cat == "4") %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Austria", nrow(.)))

#ALBANIA
Albania_redlist <-read_excel("Albania_redlist.xlsx")
unique(Albania_redlist$Category)
albania_threatened <- c("VU", "EN", "CR")
Albania_redlist <- Albania_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% albania_threatened) %>%
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Albania", nrow(.)))

#BELARUS
Belarus_redlist <-read_excel("Belarus_redlist.xlsx")
unique(Belarus_redlist$Category)
belarus_threatened <- c("VU", "CR", "EN")
Belarus_redlist <- Belarus_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% belarus_threatened) %>%
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Belarus", nrow(.)))

# BELGIUM (Flanders)
Belgium_redlist <- read.delim("Flanders_redlist.txt") %>% 
  filter(kingdom == "Plantae" & phylum == "Tracheophyta") %>% 
  left_join(read.delim("distribution.txt") %>% dplyr::select(id, threatStatus)) %>%
  dplyr::select(scientificName, threatStatus)
unique(Belgium_redlist$threatStatus)
belgium_threatened <-  c("EN", "RE", "CR", "VU")
Belgium_redlist <- Belgium_redlist %>% 
  dplyr::rename(Species = scientificName, Redlist_cat = threatStatus) %>% 
  filter(Redlist_cat %in% belgium_threatened) %>%
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Flanders", nrow(.)))

#BELGIUM (WALLONIA)
Belgiumw_redlist <-read_excel("Wallonia_redlist.xlsx")
Belgiumw_redlist <-select(Belgiumw_redlist,-c(`Nom FR`,Famille, Statut, Protection))
unique(Belgiumw_redlist$Category)
belgiumw_threatened <- c("EX", "CR", "EN", "VU")
Belgiumw_redlist <- Belgiumw_redlist %>% 
  dplyr::rename(Redlist_cat = Category, Species = Taxon) %>% 
  filter(Redlist_cat %in% belgiumw_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Wallonia", nrow(.)))

#BOSNIA & HERZEGOVINA
BH_redlist <-read_excel("BosnHerz_redlist.xlsx")
unique(BH_redlist$Category)
bh_threatened <- c("VU", "EN", "CR", "EX")
BH_redlist <- BH_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% bh_threatened) %>%
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Bosnia and Herzegovina", nrow(.)))

#BULGARIA
Bulgaria_redlist <-read_excel("Bulgaria_redlist.xlsx")
Bulgaria_redlist <-select(Bulgaria_redlist,-c(`English name`))
unique(Bulgaria_redlist$`Degree of threat`)
Bulgaria_redlist <- Bulgaria_redlist %>% 
  dplyr::rename(Redlist_cat = `Degree of threat`, Species = `Latin name`) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Bulgaria", nrow(.)))

#CROATIA
Croatia_redlist <-read_excel("Croatia_redlist.xlsx")
Croatia_redlist <-select(Croatia_redlist,-c("NAME"))
unique(Croatia_redlist$Category)
croatia_threatened <- c("EN", "CR", "RE", "VU", "EX")
Croatia_redlist <- Croatia_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% croatia_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Croatia", nrow(.)))

#CYPRUS
Cyprus_redlist <-read_excel("Cyprus_redlist.xlsx")
Cyprus_redlist <-select(Cyprus_redlist,-c("familyName", "genusName", "speciesName",
                                          "subspeciesName", "rankTitle", "redDataBookCategory"))
unique(Cyprus_redlist$Category)
cyprus_threatened <- c("CR", "EN", "RE", "RE?", "VU")
Cyprus_redlist <- Cyprus_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% cyprus_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Cyprus", nrow(.)))

# CZECH REP
Czech_redlist <- read_excel("Czech_redlist.xlsx")
unique(Czech_redlist$`Value [enum]`)
czech_threatened <-c("VU", "EN", "CR", "RE", "EX") 
Czech_redlist <- Czech_redlist %>% 
  dplyr::rename(Species = Taxon, Redlist_cat = `Value [enum]`) %>% 
  dplyr::select(Species, Redlist_cat) %>% 
  filter(Redlist_cat %in% czech_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Czech Republic", nrow(.)))

#DENMARK
Denmark_redlist <- read_excel("Denmark_redlist.xlsx")
Denmark_redlist <-select(Denmark_redlist,-c("id", "assessmentDate", "assessmentInfo.assessmentRound"
                                            , "speciesInformation.taxonID", "speciesInformation.scientificNameAuthorship"))
unique(Denmark_redlist$redlistCategory.category)
denmark_threatened <- c("EN", "CR", "RE", "VU")
Denmark_redlist <- Denmark_redlist %>% 
  dplyr::rename(Redlist_cat = redlistCategory.category, Species = speciesInformation.scientificName) %>% 
  filter(Redlist_cat %in% denmark_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Denmark", nrow(.)))

# ENGLAND
England_redlist <- read_excel("England_redlist.xlsx")
unique(England_redlist$Category) #WL waiting list
england_threatened <- c("CR", "EN", "EW", "EX", "RE", "VU")
England_redlist <- England_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  dplyr::select(Species, Redlist_cat) %>% 
  filter(Redlist_cat %in% england_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("England", nrow(.)))

#ESTONIA
Estonia_redlist <-read_excel("Estonia_redlist.xlsx")
Estonia_redlist <-select(Estonia_redlist,-c(`Common names`))
unique(Estonia_redlist$`Regional status`)
estonia_threatened <- c("CR", "VU", "RE")
Estonia_redlist <- Estonia_redlist %>% 
  dplyr::rename(Redlist_cat = `Regional status`) %>% 
  filter(Redlist_cat %in% estonia_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.)))%>% 
  mutate(Country = rep("Estonia", nrow(.)))

#FINLAND
Finland_redlist <-read_excel("Finland_redlist.xlsx")
Finland_redlist <-select(Finland_redlist,-c(ID, `Category 2019`, `Habitat types`, `Causes of threat`, `Threat factors`))
unique(Finland_redlist$Category)
finland_threatened <- c("VU", "EN", "RE", "CR")
Finland_redlist <- Finland_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% finland_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>%
  mutate(Country = rep("Finland", nrow(.)))

# FRANCE
France_redlist <- read_delim("France_redlist.csv", ";", escape_double = FALSE, trim_ws = TRUE)
unique(France_redlist$STATUT)
france.threatened <-c("EX" , "RE" , "CR*", "CR" , "EN" , "VU")
France_redlist <- France_redlist %>% 
  dplyr::rename(Redlist_cat = STATUT) %>% 
  dplyr::select(Species, Redlist_cat) %>% 
  filter(Redlist_cat %in% france.threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("France", nrow(.)))

# GERMANY
Germany_redlist <- read_excel("Germany_redlist.xlsx")
unique(Germany_redlist$`RL Kat.`)# 0 = extinct, 1 = CR, 2 = EN, 3 = VU, 
germany_threatened <- c("0", "1", "2", "3")
Germany_redlist <- Germany_redlist %>% 
  dplyr::rename(Species = Name, Redlist_cat = `RL Kat.`) %>% 
  dplyr::select(Species, Redlist_cat) %>% 
  filter(Redlist_cat %in% germany_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Germany", nrow(.)))

#Greece
Greece_redlist <- read_excel("Greece_redlist.xlsx")
unique(Greece_redlist$Category)
greece_threatened <- c("VU", "EN", "CR")
Greece_redlist <- Greece_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% greece_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Greece", nrow(.)))

# HUNGARY
Hungary_redlist <- read_excel("Hungary_redlist.xlsx")
Hungary_redlist <-select(Hungary_redlist,-c(Species.norm))
unique(Hungary_redlist$Category)# K = extinct, KV = CR, AV = EN
hungary_threatened <- c("CR", "EN", "EW", "EX", "VU")
Hungary_redlist <- Hungary_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% hungary_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Hungary", nrow(.))) %>% 
  mutate(Species = str_to_sentence(str_remove(Species, "[.]")))

# IRELAND
Ireland_redlist <- read_excel("Ireland_redlist.xlsx")
unique(Ireland_redlist$Redlist_cat)
ireland.threatened <- c("EN","RE", "CR", "VU")
Ireland_redlist <- Ireland_redlist %>% 
  filter(Redlist_cat %in% ireland.threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Ireland", nrow(.)))

#ITALY
Italy_redlist <-read_excel("Italy_redlist.xlsx")
Italy_redlist <-select(Italy_redlist, -Source)
unique(Italy_redlist$Category)
italy_threatened <-c("EX","EW", "RE", "CR", "CR(PE)", "CR (PEW)", "EN",  "VU")
Italy_redlist <- Italy_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% italy_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Italy", nrow(.)))

#KOSOVO
Kosovo_redlist <- read_excel("Kosovo_redlist.xlsx")
unique(Kosovo_redlist$Category)
kosovo_threatened <- c("EX", "EW", "CR", "EN", "VU")
Kosovo_redlist <- Kosovo_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% kosovo_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Kosovo", nrow(.)))

#LATVIA
Latvia_redlist <- read_excel("Latvia_redlist.xlsx")
Latvia_redlist <-select(Latvia_redlist,-c("Species.disc"))
Latvia_redlist[] <- lapply(Latvia_redlist, as.character)
unique(Latvia_redlist$Category)
latvia_threatened <- c("0", "1", "2") #possibly 3: are species, no threat of extinction as yet, although encountered in
#such a small number or in so limited areas and specified sites that they may probably disappear
Latvia_redlist <- Latvia_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% latvia_threatened) %>%
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Latvia", nrow(.)))

#LITHUANIA
Lithuania_redlist <- read_excel("Lithuania_redlist.xlsx")
Lithuania_redlist <-select(Lithuania_redlist,-c("Species.disc"))
unique(Lithuania_redlist$Category)
lithuania_threatened <- c("Ex", "E", "V")
Lithuania_redlist <- Lithuania_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% lithuania_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Lithuania", nrow(.)))

#LUXEMBOURG
Luxembourg_redlist <- read_delim("Luxembourg_redlist.csv", ",", escape_double = FALSE, trim_ws = TRUE)
Luxembourg_redlist <-select(Luxembourg_redlist,-c(Status, Criteria, Habitat))
unique(Luxembourg_redlist$Redlist_cat)
luxembourg_threatened <- c("CR", "VU", "RE", "EN")
Luxembourg_redlist <- Luxembourg_redlist %>% 
  filter(Redlist_cat %in% luxembourg_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Luxembourg", nrow(.)))

#MALTA
Malta_redlist <- read_excel("Malta_redlist.xlsx")
Malta_redlist <-select(Malta_redlist,-c("Range", "Page"))
unique(Malta_redlist$Category)
malta_threatened <- c("E", "E?", "X", "X?", "V")#r = rare, RR = very rare ?
Malta_redlist <- Malta_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% malta_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Malta", nrow(.)))

#MOLDOVA
Moldova_redlist <- read_excel("Moldova_redlist.xlsx")
unique(Moldova_redlist$Category)
moldova_threatened <- c("CR", "VU", "EN")
Moldova_redlist <- Moldova_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% moldova_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Moldova", nrow(.)))

#MONTENEGRO
Montenegro_redlist <- read_excel("Montenegro_redlist.xlsx")
unique(Montenegro_redlist$Category)
montenegro_threatened <- c("RE", "CR", "EN", "VU")
Montenegro_redlist <- Montenegro_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% montenegro_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Montenegro", nrow(.)))

# NETHERLANDS
Netherlands_redlist <- read_excel("Netherlands_redlist.xlsx")
unique(Netherlands_redlist$Redlist_cat)
netherlands.threatened <-c("EN","RE","VU","CR")
Netherlands_redlist <- Netherlands_redlist %>% 
  filter(Redlist_cat %in% netherlands.threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Netherlands", nrow(.)))

# NORWAY
Norway_redlist <- read_delim("Norway_redlist.csv", 
                             ";", quote = "\\\"", escape_double = FALSE, 
                             col_types = cols_only(`Vitenskapelig navn` = col_guess(), 
                                                   Kategori = col_guess()), 
                             locale = locale(encoding = "ISO-8859-1", asciify = TRUE), 
                             na = "NA", trim_ws = TRUE)
unique(Norway_redlist$Kategori)
norway_threatened <- c("RE","CR", "EN", "VU", "VUº")
Norway_redlist <- Norway_redlist %>% 
  dplyr::rename(Species = `Vitenskapelig navn`, Redlist_cat = Kategori) %>% 
  filter(Redlist_cat %in% norway_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Norway", nrow(.)))

# POLAND
Poland_redlist <- read_excel("Poland_redlist.xlsx")
unique(Poland_redlist$Redlist_cat)
poland_threatened <- c("EX", "EW", "RE", "CR", "EN", "VU", "REW")
Poland_redlist <- Poland_redlist %>% 
  filter(Redlist_cat %in% poland_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Poland", nrow(.)))

#PORTUGAL
Portugal_redlist <-read_excel("Portugal_redlist.xlsx")
Portugal_redlist <-select (Portugal_redlist,-c("TAXON", "CATEGORIA DE RISCO", "Categoria"))
unique(Portugal_redlist$`CATEGORIA E CRITERIOS`)
portugal_threatened <- c("CR", "VU", "EN", "EX", "RE")
Portugal_redlist <- Portugal_redlist %>% 
  dplyr::rename(Redlist_cat = `CATEGORIA E CRITERIOS`) %>% 
  filter(Redlist_cat %in% portugal_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Portugal", nrow(.)))

#ROMANIA
Romania_redlist <- read_excel("Romania_redlist.xlsx")
unique(Romania_redlist$Category)
romania_threatened <- c("VU", "CR", "EN", "CR/EW", "EX?", "VU/EN", "CR-DD",
                        "CR/VU", "CR- EX?", "EX")
Romania_redlist <- Romania_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% romania_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Romania", nrow(.)))

# SLOVAKIA
Slovakia_redlist <- read_csv("Slovakia_redlist.csv")
unique(Slovakia_redlist$IUCN)# CR(PE) means CR with probably regionally extinct
slovakia_threatened <- c("RE", "CR", "EN", "VU", "CR(PE)")
Slovakia_redlist <- Slovakia_redlist %>% 
  dplyr::rename(Redlist_cat = IUCN) %>% 
  filter(Redlist_cat %in% slovakia_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Slovakia", nrow(.)))

# SLOVENIA
Slovenia_redlist <- read_excel("Slovenia_redlist.xlsx")
unique(Slovenia_redlist$Redlist_cat)# threatened species in Slovenia are Ex, EX?, E, V
slovenia_threatened <- c("Ex", "Ex?", "E", "V")
Slovenia_redlist <- Slovenia_redlist %>% 
  filter(Redlist_cat %in% slovenia_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Slovenia", nrow(.)))

#SPAIN
Spain_redlist <- read_excel("Spain_redlist.xlsx")
unique(Spain_redlist$Redlist_cat)
spain_threatened <- c("VU", "CR", "EN", "RE")
Spain_redlist <- Spain_redlist %>% 
  filter(Redlist_cat %in% spain_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Spain", nrow(.)))

# SWEDEN
Sweden_redlist <- read_delim("Sweden_redlist.csv", 
                             ";", escape_double = FALSE, col_types = cols_only(`Scientific name` = col_guess(), 
                                                                               `Red List Category` = col_guess()), 
                             trim_ws = TRUE)
unique(Sweden_redlist$`Red List Category`)
sweden_treatened <- c("CR", "EN", "VU", "RE", "VU°")
Sweden_redlist <- Sweden_redlist %>% 
  dplyr::rename(Species = `Scientific name`, Redlist_cat = `Red List Category`) %>% 
  filter(Redlist_cat %in% sweden_treatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Sweden", nrow(.)))

# SWITZERLAND
Switzerland_redlist <- read_excel("Switzerland_redlist.xlsx", skip = 1)
unique(Switzerland_redlist$CH)
ch_treatened <- c("EX", "CR", "VU", "EN", "CR(PE)", "RE")
Switzerland_redlist <- Switzerland_redlist %>% 
  dplyr::rename(Species = `Scientific name`, Redlist_cat = CH) %>%
  dplyr::select(Species, Redlist_cat) %>% 
  filter(Redlist_cat %in% ch_treatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Switzerland", nrow(.)))

#UKRAINE
Ukraine_redlist <-read_excel("Ukraine_redlist.xlsx")
Ukraine_redlist <-select (Ukraine_redlist,-c("Category.uk", "page", "translation"))
unique(Ukraine_redlist$Category)
ukraine_threatened <- c("VU", "EX", "VU on border of area", "EW", "EN")
Ukraine_redlist <- Ukraine_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% ukraine_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Ukraine", nrow(.)))

#WALES
Wales_redlist <-read_excel("Wales_redlist.xlsx")
Wales_redlist <-select(Wales_redlist,-c("Species.orig"))
unique(Wales_redlist$Category)
wales_threatened <- c("EX", "VU", "RE", "EN", "CR", "EW")
Wales_redlist <- Wales_redlist %>% 
  dplyr::rename(Redlist_cat = Category) %>% 
  filter(Redlist_cat %in% wales_threatened) %>% 
  mutate(Threatened = rep("yes", nrow(.))) %>% 
  mutate(Country = rep("Wales", nrow(.)))

# JOIN ALL COUNTRIES ------------------------------------------------------
redlist_data <- bind_rows(
  list(
    Albania_redlist,
    Austria_redlist,
    Belarus_redlist,
    Belgium_redlist,
    Belgiumw_redlist,
    BH_redlist,
    Bulgaria_redlist,
    Croatia_redlist,
    Cyprus_redlist,
    Czech_redlist,
    Denmark_redlist,
    England_redlist,
    Estonia_redlist,
    Finland_redlist,
    France_redlist,
    Germany_redlist,
    Greece_redlist,
    Hungary_redlist,
    Ireland_redlist,
    Italy_redlist,
    Kosovo_redlist,
    Latvia_redlist,
    Luxembourg_redlist,
    Lithuania_redlist,
    Malta_redlist,
    Moldova_redlist,
    Montenegro_redlist,
    Netherlands_redlist,
    Norway_redlist,
    Poland_redlist,
    Portugal_redlist,
    Romania_redlist,
    Slovakia_redlist,
    Slovenia_redlist,
    Spain_redlist,
    Sweden_redlist,
    Switzerland_redlist,
    Ukraine_redlist,
    Wales_redlist))

# WRITE CSV  ------------------------------------------------------
setwd("/data/Ingmar/european_redlist_synthesis")
write.table(redlist_data, "Data/data_outputs/redlist_data.csv", row.names = FALSE, sep = ",")
