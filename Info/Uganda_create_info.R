##################################################################
##################################################################
# This script is used to generate the info file for a given country
##################################################################
##################################################################
rm(list = ls())

library(readOGR)
library(dplyr)
################################################################
#########   set parameters
################################################################

# Files info (For those lines with ### xxx ### above, please fill in as commented)
country <- 'Uganda'

### please fill in the country abbreviation in all upper case of gadm files ### (e.g. fill in SEN for gadm36_SEN_3.shp)
gadm.abbrev <- "UGA"
doHIVAdj <- T

### please fill in the path to country shape files ####
poly.path <- paste0("shapeFiles/alt_shapeFiles")


##### explain how these may need to be changed
poly.layer.adm0 <- paste('gadm41', gadm.abbrev,'0', sep = "_") # specify the name of the national shape file
poly.layer.adm1 <- paste('UDHS Regions 2019') # specify the name of the admin1 shape file
poly.layer.adm2 <- paste('uganda_districts') # specify the name of the admin2 shape file


poly.label.adm1 <- "poly.adm1@data$Name"
poly.label.adm2 <- "poly.adm2@data$District"

##### Modify case of shapeFiles name variable
adm1 <- readOGR(dsn = paste0("R:/Project/STAB/",
                             country, "/", poly.path),
                layer = poly.layer.adm1)
shape <- readOGR(dsn = paste0("R:/Project/STAB/",
                              country, "/", poly.path),
                 layer = poly.layer.adm2)
shape <- spTransform(shape, CRS(proj4string(adm1)))
shape@data <- shape@data %>% 
  mutate(District = paste0(str_sub(District, 1, 1),
                           tolower(str_sub(District, 2)))) %>% 
  select(OBJECTID, District) %>% 
  as.data.frame()

writeOGR(shape, dsn = paste0("R:/Project/STAB/",
                             country, "/shapeFiles/alt_shapeFiles"),
         layer = poly.layer.adm2,
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE)
##################################################################
##################################################################
##################################################################

## setting rest of parameters using info from above
country.abbrev <- tolower(gadm.abbrev)           # lower the country gadm abbreviation 
beg.year <- 2000 # the first year of the interest
end.proj.year <- 2020 # last year we would like to project to 

info.name <- paste0(country, "_general_info.Rdata")

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

save.image(file = paste0(paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/"),'/Info/', info.name, sep=''))

