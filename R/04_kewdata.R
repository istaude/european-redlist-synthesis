source("R/00_preamble.R")

# DOWNLOAD KEW DATA -------------------------------------------------------
download.file(url='https://storage.googleapis.com/kew-dev-backup/world_checklist_names_and_distribution_feb_21.zip',
              destfile='Data/KEW.zip')

# unpack
zipF<- "Data/KEW.zip"
outDir<-"Data/kew"
unzip(zipF,exdir=outDir)

