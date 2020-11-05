#UNFINISHED

# Profiles Indicators- Male & Female Life expectancy Inequalities


#source data from NRS - scotland (3 year agg), ca  ( 5 year agg)
#scotland or hb quintiles



###############################################.
## Packages/Filepaths ----
###############################################.

library(dplyr)    #data manipulations 
library(readr)    #reading in file


# Varies filepaths depending on if using server or not.
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"
  output_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Output Data/"
  data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"
} else {
  source_network <- "//stats/ScotPHO/Life Expectancy/Data/Source Data/NRS data/"
  output_network <- "//stats/ScotPHO/Life Expectancy/Data/Output Data/"
  data_folder <- "//stats/ScotPHO/Profiles/Data/"
}


##########################################################################################.
## Part 1 - Read in Life Expectancy estimates from NRS at Scotland, NHS Board and LA level ----
## Note these estimates are the official national statistics and are 3 year rolling averages.
## Figures orginally supplied by population & migration team at NRS but in future may be available online.
##########################################################################################. 


NRS_data <- read_csv(paste0(source_network,"20190517_NRS_ Life expectancy by SIMD - all years.csv")) %>%
  mutate(quintile=as.character(quintile)) %>%
  setNames(tolower(names(.))) #variables to lower case

NRS_data <- NRS_data %>%
  mutate(code="S00000001",
         quintile=(case_when(quintile=="0" ~ "Total",TRUE~quintile)),
         quint_type="sc_quin",
         year=as.numeric(substr(timeperiod,1,4))+1,
         def_period=paste0(gsub("-"," to ",timeperiod)," calendar years"),
         sex_grp=case_when(sex=="M"~1,sex=="F"~2, TRUE~3)) %>%
  rename(lowci=lci, upci=uci, rate=le, trend_axis=timeperiod) %>%
  select(sex_grp, year, code, quintile, quint_type,rate, lowci, upci,trend_axis, def_period)
  
  





#Saving file
saveRDS(data_shiny, file = paste0(data_folder, "Shiny Data/", filename, "_ineq.rds"))