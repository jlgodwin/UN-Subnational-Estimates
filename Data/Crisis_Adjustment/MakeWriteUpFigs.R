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
gadm.abbrev <- gadm.abbrevs[5]
home.dir <- "C:/Users/jlg0003/jlgodwin/UN-Subnational-Estimates"

# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Sierra_Leone'
# Specify straification of final U5MR model (which was benchmarked)
mod_label <- c('strat_u5_bench','unstrat_u5_allsurveys_bench')[1]

data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info


## Functions ####
# Function for crisis-specific 5q0  from u1 and 1to4 deaths and population
# Input: df [data.frame; columns listed below]
# - ed_0_1: excess deaths age 0 years
# - ed_1_5: excess deaths age 1-4 years
# - pop_0_1: population age 0 years
# - pop_1_5: population age 1-4 years
get_ed_5q0 <- function (df, nax_1_5) {
  df <- df %>%
    mutate(ed_1m0 = ed_0_1 / pop_0_1,
           ed_4m1 = ed_1_5 / pop_1_5) %>%
    mutate(ed_1q0 = ed_1m0 / (1 + (1 - (0.3)) * ed_1m0),
           ed_4q1 = 4 * ed_4m1 / (1 + (4 - 4 * (nax_1_5)) * ed_4m1)) %>%
    mutate(ed_5q0 = 1 - (1 - ed_1q0) * (1 - ed_4q1))
  return(df)
}

## Load data ####

### IGME Crisis ####
igme <- readxl::read_xlsx(paste0(home.dir,
                                 "/Data/Crisis_Adjustment/",
                                 "Crisis_Under5_deaths_2022.xlsx"))


### IGME Estimates: Crisis-Adjusted ####

{
  setwd(paste0(home.dir,'/Data/IGME'))
  
  ## U5MR
  igme.ests.u5.raw <- read.csv('igme2022_u5.csv')
  igme.ests.u5 <- igme.ests.u5.raw[igme.ests.u5.raw$ISO.Code==gadm.abbrev,]
  igme.ests.u5 <- data.frame(t(igme.ests.u5[,10:ncol(igme.ests.u5)]))
  names(igme.ests.u5) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.u5$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.u5),'X')) - 0.5
  igme.ests.u5 <- igme.ests.u5[igme.ests.u5$year %in% 2000:2021,]
  rownames(igme.ests.u5) <- NULL
  igme.ests.u5$OBS_VALUE <- igme.ests.u5$OBS_VALUE/1000
  igme.ests.u5$LOWER_BOUND <- igme.ests.u5$LOWER_BOUND/1000
  igme.ests.u5$UPPER_BOUND <- igme.ests.u5$UPPER_BOUND/1000
  igme.ests.u5.crisis <- igme.ests.u5
  
  ## NMR
  igme.ests.nmr.raw <- read.csv('igme2022_nmr.csv')
  igme.ests.nmr <- igme.ests.nmr.raw[igme.ests.nmr.raw$iso==gadm.abbrev,]
  igme.ests.nmr <- data.frame(t(igme.ests.nmr[,10:ncol(igme.ests.nmr)]))
  names(igme.ests.nmr) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.nmr$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.nmr),'X')) - 0.5
  igme.ests.nmr <- igme.ests.nmr[igme.ests.nmr$year %in% 2000:2021,]
  rownames(igme.ests.nmr) <- NULL
  igme.ests.nmr$OBS_VALUE <- igme.ests.nmr$OBS_VALUE/1000
  igme.ests.nmr$LOWER_BOUND <- igme.ests.nmr$LOWER_BOUND/1000
  igme.ests.nmr$UPPER_BOUND <- igme.ests.nmr$UPPER_BOUND/1000
  igme.ests.nmr.crisis <- igme.ests.nmr
}

### IGME Estimates: No Crisis Adjustment ####
{
  setwd(paste0(home.dir,'/Data/IGME'))
  
  ## U5MR
  igme.ests.u5.raw <- read.csv('igme2022_u5_nocrisis.csv')
  igme.ests.u5 <- igme.ests.u5.raw[igme.ests.u5.raw$ISO.Code==gadm.abbrev,]
  igme.ests.u5 <- data.frame(t(igme.ests.u5[,10:ncol(igme.ests.u5)]))
  names(igme.ests.u5) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.u5$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.u5),'X')) - 0.5
  igme.ests.u5 <- igme.ests.u5[igme.ests.u5$year %in% 2000:2021,]
  rownames(igme.ests.u5) <- NULL
  igme.ests.u5$OBS_VALUE <- igme.ests.u5$OBS_VALUE/1000
  igme.ests.u5$LOWER_BOUND <- igme.ests.u5$LOWER_BOUND/1000
  igme.ests.u5$UPPER_BOUND <- igme.ests.u5$UPPER_BOUND/1000
  
  
  ## NMR
  igme.ests.nmr.raw <- read.csv('igme2022_nmr.csv')
  igme.ests.nmr <- igme.ests.nmr.raw[igme.ests.nmr.raw$iso==gadm.abbrev,]
  igme.ests.nmr <- data.frame(t(igme.ests.nmr[,10:ncol(igme.ests.nmr)]))
  names(igme.ests.nmr) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.nmr$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.nmr),'X')) - 0.5
  igme.ests.nmr <- igme.ests.nmr[igme.ests.nmr$year %in% 2000:2021,]
  rownames(igme.ests.nmr) <- NULL
  igme.ests.nmr$OBS_VALUE <- igme.ests.nmr$OBS_VALUE/1000
  igme.ests.nmr$LOWER_BOUND <- igme.ests.nmr$LOWER_BOUND/1000
  igme.ests.nmr$UPPER_BOUND <- igme.ests.nmr$UPPER_BOUND/1000
}

## Country-specific ####

### Prepared Crisis File ####
load(paste0(home.dir, "/Data/Crisis_Adjustment/crisis_",
              gadm.abbrev, ".rda"))

### Admin Pop Weights ####

load(paste0(home.dir,'/Data/Crisis_Adjustment/adm_weights/',
            tolower(gadm.abbrev), '_adm1_weights_u1.rda'))
load(paste0(home.dir,'/Data/Crisis_Adjustment/adm_weights/',
            tolower(gadm.abbrev), '_adm1_weights_u5.rda'))
load(paste0(home.dir,'/Data/Crisis_Adjustment/adm_weights/',
            tolower(gadm.abbrev), '_adm2_weights_u1.rda'))
load(paste0(home.dir,'/Data/Crisis_Adjustment/adm_weights/',
            tolower(gadm.abbrev), '_adm2_weights_u5.rda'))

# load admin1.names and admin2.names (map from area id to name)
load(paste0(data.dir,'/',poly.path,'/', country, '_Amat_Names.rda'))


###  BB8 Benched ####

# final admin1 and admin2 benchmarked u5mr without crisis adjustment
load(paste0(res.dir,"/Betabinomial/U5MR/",
            country, "_res_adm1_",
            mod_label,".rda"))
load(paste0(res.dir,"/Betabinomial/U5MR/",
            country, "_res_adm2_",
            mod_label, ".rda"))

# simplify names of objects above
res_adm1_u5 <- eval(str2lang(paste0('bb.res.adm1.',
                                    str_replace_all(mod_label,'_','.'),
                                    '$overall')))
res_adm2_u5 <- eval(str2lang(paste0('bb.res.adm2.',
                                    str_replace_all(mod_label,'_','.'),
                                    '$overall')))


# Admin 1 Crisis Adjustments ####

## Get Natl Pop ####
pop <- igme[igme$ISO3Code == gadm.abbrev, c("Country", "Year", "Pop 0", "Pop 1-4")]
names(pop) <- c("country", "years", "nat_pop_0_1", "nat_pop_1_5")
pop$years <- as.integer(floor(pop$years))

## Admin 1 Pop from weights ####
pop_adm1 <- merge(weight.adm1.u1, weight.adm1.u5, by = c("region", "years"))
names(pop_adm1) <- c("region", "years", "prop_0_1", "prop_1_5")
pop_adm1$level <- "admin1"
pop_adm1 <- merge(pop_adm1, admin1.names, by.x = "region", by.y = "Internal")
names(pop_adm1)[names(pop_adm1) == "GADM"] <- "gadm"
pop_adm1 <- merge(pop_adm1, pop, by = "years")

## Admin 2 Pop from weights ####
pop_adm2 <- merge(weight.adm2.u1, weight.adm2.u5, by = c("region", "years"))
names(pop_adm2) <- c("region", "years", "prop_0_1", "prop_1_5")
pop_adm2$level <- "admin2"
pop_adm2 <- merge(pop_adm2, admin2.names, by.x = "region", by.y = "Internal")
names(pop_adm2)[names(pop_adm2) == "GADM"] <- "gadm"
pop_adm2 <- merge(pop_adm2, pop, by = "years")

# combine and compute area-level population from weights and national pop
pop <- rbind(pop_adm1, pop_adm2)
pop$pop_0_1 <- pop$prop_0_1 * pop$nat_pop_0_1
pop$pop_1_5 <- pop$prop_1_5 * pop$nat_pop_1_5

# final columns: country, level (i.e. admin1, admin2), gadm (area name),
# region (area code), years, pop_0_1, pop_1_5
pop <- pop[, c("country", "level", "gadm", "region", "years", "pop_0_1", "pop_1_5")]

## Main code
# 1. merge all components together
# 2. crisis deaths and pop to crisis qx
# 3. add on to non-crisis results to get final u5mr

## Distribute Deaths ####
### admin1 u5mr ####
deaths_adm1 <- df_SLE %>% filter(level == "admin1")
pop_adm1 <- pop %>% filter(level == "admin1")
df <- merge(deaths_adm1, pop_adm1, by = c("gadm", "years"))
df <- get_ed_5q0(df, nax_1_5 = 0.4) # convert deaths to qx
df <- merge(df, admin1.names, by.x = "region", by.y = "Internal", all=T)
if (nrow(df[is.na(df$GADM) | is.na(df$gadm),]) > 0) {
  stop("Incomplete merge of Admin 1 location information.")
}
df <- df %>% select(region, years, ed_5q0)
res_adm1_u5_crisis <- merge(res_adm1_u5, df, by = c("region", "years"), all=T)
res_adm1_u5_crisis <- res_adm1_u5_crisis %>%
  mutate(ed_5q0 = ifelse(is.na(ed_5q0), 0, ed_5q0)) %>%
  mutate(median = median + ed_5q0, # final qx = non-crisis qx + crisis qx
         lower = lower + ed_5q0,
         upper = upper + ed_5q0) %>%
  dplyr::select(c(region,years,time,area,median,upper,lower,is.yearly))

### admin2 u5mr ####
deaths_adm2 <- df_SLE %>% filter(level == "admin2")
pop_adm2 <- pop %>% filter(level == "admin2")
df <- merge(deaths_adm2, pop_adm2, by = c("gadm", "years"))
df <- get_ed_5q0(df, nax_1_5 = 0.4) # convert deaths to qx
df <- df %>% select(region, years, ed_5q0)
res_adm2_u5_crisis <- merge(res_adm2_u5, df, by = c("region", "years"), all=T)
res_adm2_u5_crisis <- res_adm2_u5_crisis %>%
  mutate(ed_5q0 = ifelse(is.na(ed_5q0), 0, ed_5q0)) %>%
  mutate(median = median + ed_5q0, # final qx = non-crisis qx + crisis qx
         lower = lower + ed_5q0,
         upper = upper + ed_5q0) %>%
  dplyr::select(c(region,years,time,area,median,upper,lower,is.yearly))


# Aggregate to natl & compare ####

natl_ed_adm1_agg <- deaths_adm1 %>%
  left_join(pop_adm1,
            by = c("gadm", "years")) %>% 
  group_by(years) %>%
  summarise(ed_0_1 = sum(ed_0_1),
            ed_1_5 = sum(ed_1_5),
            pop_0_1 = sum(pop_0_1),
            pop_1_5 = sum(pop_1_5))

natl_ed_adm2_agg <- deaths_adm2 %>%
  left_join(pop_adm2,
            by = c("gadm", "years")) %>% 
  group_by(years) %>%
  summarise(ed_0_1 = sum(ed_0_1),
            ed_1_5 = sum(ed_1_5),
            pop_0_1 = sum(pop_0_1),
            pop_1_5 = sum(pop_1_5))

natl_adm1_agg_nocrisis <- res_adm1_u5 %>%
  mutate(years = as.numeric(as.character(years))) %>%
  left_join(pop_adm1,
            by = c("region", "years")) %>%
  arrange(years) %>%
  filter(years %in% 2014:2015) %>%
  mutate(pop = pop_0_1 + pop_1_5) %>%
  group_by(years) %>%
  summarise(median = sum(median*pop/sum(pop)),
            upper = sum(upper*pop/sum(pop)),
            lower = sum(lower*pop/sum(pop)))

natl_adm2_agg_nocrisis <- res_adm2_u5 %>%
  mutate(years = as.numeric(as.character(years))) %>%
  left_join(pop_adm2,
            by = c("region", "years")) %>%
  arrange(years) %>%
  filter(years %in% 2014:2015) %>%
  mutate(pop = pop_0_1 + pop_1_5) %>%
  group_by(years) %>%
  summarise(median = sum(median*pop/sum(pop)),
            upper = sum(upper*pop/sum(pop)),
            lower = sum(lower*pop/sum(pop)))

natl_adm1_agg <- res_adm1_u5_crisis %>%
  mutate(years = as.numeric(as.character(years))) %>%
  left_join(pop_adm1,
            by = c("region", "years")) %>%
  arrange(years) %>%
  filter(years %in% 2014:2015) %>%
  mutate(pop = pop_0_1 + pop_1_5) %>%
  group_by(years) %>%
  summarise(median = sum(median*pop/sum(pop)),
            upper = sum(upper*pop/sum(pop)),
            lower = sum(lower*pop/sum(pop)))

natl_adm2_agg <- res_adm2_u5_crisis %>%
  mutate(years = as.numeric(as.character(years))) %>%
  left_join(pop_adm2,
            by = c("region", "years")) %>%
  arrange(years) %>%
  filter(years %in% 2014:2015) %>%
  mutate(pop = pop_0_1 + pop_1_5) %>%
  group_by(years) %>%
  summarise(median = sum(median*pop/sum(pop)),
            upper = sum(upper*pop/sum(pop)),
            lower = sum(lower*pop/sum(pop)))

natl_igme_adm1_ourfn <- igme.ests.u5 %>% 
  filter(year %in% 2014:2015) %>%
  left_join(get_ed_5q0(natl_ed_adm1_agg, 0.4) %>%
              dplyr::select(years, ed_5q0),
            by = c("year" = "years")) %>%
  mutate(median = OBS_VALUE + ed_5q0,
         upper = UPPER_BOUND + ed_5q0,
         lower = LOWER_BOUND + ed_5q0)
natl_igme_adm2_ourfn <- igme.ests.u5 %>% 
  filter(year %in% 2014:2015) %>%
  left_join(get_ed_5q0(natl_ed_adm2_agg, 0.4) %>%
              dplyr::select(years, ed_5q0),
            by = c("year" = "years")) %>%
  mutate(median = OBS_VALUE + ed_5q0,
         upper = UPPER_BOUND + ed_5q0,
         lower = LOWER_BOUND + ed_5q0)

natl_igme_crisis <- igme.ests.u5.crisis %>% 
  filter(year %in% 2014:2015) %>% 
  mutate(median = OBS_VALUE,
         upper = UPPER_BOUND,
         lower = LOWER_BOUND)

natl_igme_nocrisis <- igme.ests.u5 %>% 
  filter(year %in% 2014:2015) %>%
  mutate(median = OBS_VALUE,
         upper = UPPER_BOUND,
         lower = LOWER_BOUND)

# Plot

pdf(paste0(res.dir,"CrisisCompare.pdf"),
    height = 5, width = 5)
{
  cols <- c("navy", "firebrick", "forestgreen",
                  "goldenrod", "black", "grey75")
                  par(lend = 1)
                  
                  plot(NA,
                       xlim = c(0, 3),
                       ylim = c(100, 250),
                       xlab = "",
                       ylab = "U5MR per 1000 births",
                       xaxt = "n")
                  axis(1, at = 1:2,
                       labels = c("Unadjusted",
                                  "Adjusted"))
                  points(c(.85, 1, 1.15),
                         c(natl_adm1_agg_nocrisis$median[2]*1000,
                           natl_adm2_agg_nocrisis$median[2]*1000,
                           natl_igme_nocrisis$median[2]*1000),
                         pch = 19,
                         col = alpha(cols[c(1,3,5)], 0.45))
                  segments(x0 = c(.85, 1, 1.15),
                           x1 = c(.85, 1, 1.15),
                           y0 = c(natl_adm1_agg_nocrisis$lower[2]*1000,
                                  natl_adm2_agg_nocrisis$lower[2]*1000,
                                  natl_igme_nocrisis$lower[2]*1000),
                           y1 = c(natl_adm1_agg_nocrisis$upper[2]*1000,
                                  natl_adm2_agg_nocrisis$upper[2]*1000,
                                  natl_igme_nocrisis$upper[2]*1000),
                           lwd = 2,
                           col = alpha(cols[c(1,3,5)], 0.45))
                  
                  points(seq(1.75, 2.25, length.out = 5),
                         c(natl_adm1_agg$median[2]*1000,
                           natl_adm2_agg$median[2]*1000,
                           natl_igme_adm1_ourfn$median[2]*1000,
                           natl_igme_adm2_ourfn$median[2]*1000,
                           natl_igme_crisis$median[2]*1000),
                         pch = 19,
                         col = cols[c(1,3,2,4,5)])
                  segments(x0 = seq(1.75, 2.25, length.out = 5),
                           x1 = seq(1.75, 2.25, length.out = 5),
                           y0 = c(natl_adm1_agg$lower[2]*1000,
                                  natl_adm2_agg$lower[2]*1000,
                                  natl_igme_adm1_ourfn$lower[2]*1000,
                                  natl_igme_adm2_ourfn$lower[2]*1000,
                                  natl_igme_crisis$lower[2]*1000),
                           y1 = c(natl_adm1_agg$upper[2]*1000,
                                  natl_adm2_agg$upper[2]*1000,
                                  natl_igme_adm1_ourfn$upper[2]*1000,
                                  natl_igme_adm2_ourfn$upper[2]*1000,
                                  natl_igme_crisis$upper[2]*1000),
                           lwd = 2,
                           col = cols[c(1,3,2,4,5)])
                  points(rep(1.45, 5),
                         1000*res_adm1_u5$median[res_adm1_u5$years == 2015],
                         pch = 1, lwd = 2, col = alpha("navy", .45))
                  points(rep(1.55, 5),
                         1000*res_adm1_u5_crisis$median[res_adm1_u5_crisis$years == 2015],
                         pch = 1, lwd = 2, col = alpha("navy", 1))
                  legend("topleft",
                         pch = c(1, 1, rep(19, 8)), bty = 'n', cex = 0.75,
                         col = c(alpha(cols[1], .45),
                                 "navy", alpha(cols[c(1,3,5)], 0.45),
                                 cols[c(1,3,2,4,5)]),
                         legend = c("BB8 Adm-1", "BB8 Admin-1: Crisis",
                                    "BB8 Adm-1 Agg",
                                    "BB8 Adm-2 Agg",
                                    "IGME",
                                    "BB8 Adm-1 Agg",
                                    "BB8 Adm-2 Agg",
                                    "IGME Adm-1 Death Agg",
                                    "IGME Adm-2 Death Agg",
                                    "IGME"))
}
dev.off()


## Look at m to q's
## for 5q0
mtoq_df <- natl_ed_adm1_agg %>% 
 transmute(m_01 = ed_0_1/pop_0_1,
           m_14 = ed_1_5/pop_1_5,
           m_05 = (ed_0_1 + ed_1_5)/(pop_0_1 + pop_1_5)) %>% 
  mutate(q_01 = m_01/(1 + (1 - 0.3)*m_01),
         q_14 = 4*m_14/(1 + (4 - 4*.4)*m_14),
         q_05_atonce = 5*m05/(1 + (5 - 5*.5)*m_05),
         q_05 = 1 - (1 - q_01)*(1 - q_14))
nax <- 5*seq(0,1,.001)

pdf(paste0(home.dir,
           "/Data/Crisis_Adjustment/MtoQ_IGME.pdf"),
    height =3.5, width = 7)
{
  plot(nax,
       (5*m05)/(1 + (5-nax)*m05),
       xlab = "5a0",
       ylab = "5q0", main = "n = 5",
       type = 'l', lwd = 2)
  plot(nax,
       (1*m05)/(1 + (1-nax)*m05),
       xlab = "5a0",
       ylab = "5q0", main = "n = 1",
       type = 'l', lwd = 2)
}
dev.off()