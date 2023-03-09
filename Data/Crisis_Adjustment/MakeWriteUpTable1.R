#' Checking Crisis Adj
#' @author Jessica Godwin

# Setup ####
rm(list = ls())

## Libraries ####
library(dplyr)
library(tidyr)
library(xtable)
library(readxl)

## Parameters ####
countries <- c("Guinea", "Liberia", "Haiti", "Myanmar", "Sierra Leone")
gadm.abbrevs <- c("GIN", "LBR", "HTI", "MMR", "SLE")
home.dir <- "C:/Users/jlg0003/jlgodwin/UN-Subnational-Estimates"

## Load data ####

### IGME ####
igme <- readxl::read_xlsx(paste0(home.dir,
                                 "/Data/Crisis_Adjustment/",
                                 "Crisis_Under5_deaths_2022.xlsx"))




## Country-specific ####

for(gadm.abbrev in gadm.abbrevs[-3]){
  load(paste0(home.dir, "/Data/Crisis_Adjustment/crisis_",
              gadm.abbrev, ".rda"))
}

# Get our countries' data ####

## Population ####

pop <- igme[igme$ISO3Code %in% gadm.abbrevs,
            c("Country", "Year", "Pop 0", "Pop 1-4")]
names(pop) <- c("country", "years", "nat_pop_0_1", "nat_pop_1_5")
pop$years <- as.integer(floor(pop$years))

## Crisis ####
crisis_igme <- igme[igme$Country %in%
             c("Guinea", "Liberia", "Haiti", "Sierra Leone", "Myanmar"),] %>%
  select(Country, Year, contains("Crisis"), contains("Pop")) %>% 
  group_by(Country, Year) %>%
  mutate(ed_1m0 = `Crisis d0`/`Pop 0`*1000,
         ed_1m4 = `Crisis d1-4`/`Pop 1-4`*1000) %>% 
  select(Country, Year, `Crisis d0`, `Crisis d1-4`, contains("ed_"))


crisis.tableList <- list()
for(country in countries){
    crisis.tableList[[country]] <- crisis_igme %>% 
      filter(Country == country) %>% 
      ungroup() %>% 
      select(-Country)
    names(crisis.tableList[[country]]) <- c("Year",
                                            "Deaths: 0-1",
                                            "Deaths: 1-5",
                                            "Rate: 0-1", "Rate: 1-5")
}
  
attr(crisis.tableList, "subheadings") <- names(crisis.tableList)

xtableList(crisis.tableList,
           digits = c(0,0,0,0,1,1),
           align = "cc|cccc", 
           caption = paste0("Numbers of deaths and mortality rate ",
                            "per 1000 individuals ",
                            "by country, year, and age group."))
