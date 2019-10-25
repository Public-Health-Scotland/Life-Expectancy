# LE for small areas (e.h. Intermediate Zone or HSCP locality)
# 08/10/2019

# Program generates life expectancy at birth estimates for small areas such as 2011 intermediate zone
# geogrpahies

# Uses 85+ years as maximum age group (this is different to national stats which NRS/ONS and national level analysis use)
# 85+ years is still required to avoid issues where no deaths or population are registered within some smaller areas
# smaller georgraphies have smaller populations and death events - 90+ population not sufficient for many IZ to produce robust estimates

# Prior to 2018 ScotPHO imputed some deaths at IZ level - non-scottish resident deaths were apportioned across all IZ
# This imputation is no longer applied after investigation suggest it makes little difference/no to final figures & increases complexity of code.

## Part 1 - Extract deaths data from ISD deaths tables
## Part 2 - Read in Scotland Populations based on 2011 datazone ISD lookup
## Part 3 - Call life expectancy function - generates aggregated raw data file, life table and life expectancy at birth (LE0) RDS ouput files 

###############################################.
## Packages/Filepaths ----
###############################################.
library(dplyr)
library(odbc)     #connections to SMRA
library(foreign)  #read spss files in lookups folder
library(readr)    #reading in file
library(tidyr) #for melt - reshaping of data

#change depending on what version of RStudio are you using
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {  
  cl_out_pop <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/"
} else {
  cl_out_pop <- "//stats/linkage/output/lookups/Unicode/Populations/Estimates/"
}

source("Function_Life Expectancy (85+).R") #Life expectancy function where max age band is 85+ years (deliberate choise to use 85+ not 90+ because of small geographic units & small numbers)

# Function to age bands (<1 years, 1-4 years, then 5 year age bands)
# Note agebands not the same as those used in ScotPHO profile indicator production
create_agegroups <- function(df) {
  df <- df %>% mutate(age_grp = case_when( 
    age == 0 ~ 1,  age >= 1 & age <=4 ~ 2,  age >= 5 & age <=9 ~ 3,  
    age >= 10 & age <=14 ~ 4, age >= 15 & age <=19 ~ 5, age >= 20 & age <=24 ~ 6, 
    age >= 25 & age <=29 ~ 7, age >= 30 & age <=34 ~ 8, age >= 35 & age <=39 ~ 9, 
    age >= 40 & age <=44 ~ 10, age >= 45 & age <=49 ~ 11, age >= 50 & age <=54 ~ 12,
    age >= 55 & age <=59 ~ 13, age >= 60 & age <=64 ~ 14,age >= 65 & age <=69 ~ 15,
    age >= 70 & age <=74 ~ 16, age >= 75 & age <=79 ~ 17,age >= 80 & age <=84 ~ 18, 
    age >= 85 ~ 19
  ))
  }

###########################################################.
## Part 1 - Extract deaths data from NRS deaths view ----
###########################################################.

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Select data from deaths file - adjust years to select min and max year of registration needed
# Deaths for scottish residents are coded as (XS) non-residents codes as (non-res)
data_deaths_raw <- tbl_df(dbGetQuery(channel, statement=
 "SELECT year_of_registration year, age, sex sex_grp, POSTCODE pc7,
        CASE WHEN country_of_residence='XS'THEN 'XS'ELSE 'nonres' END as nonres 
  FROM ANALYSIS.GRO_DEATHS_C 
  WHERE year_of_registration between 2001 AND 2018")) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Check of numbers of non-residents - can compare to NRS published estimates.
table(data_deaths_raw$nonres, data_deaths_raw$year)

# Format and add age bands (<1 years, 1-4 years, then 5 year age bands)
# Recode unknown to male in line with NRS custom when calculating life expectancy
data_deaths_raw <- data_deaths_raw %>%
  create_agegroups() %>%
  mutate(sex_grp=recode(data_deaths_raw$sex_grp,"9"="1"))
  
# Read in geographic reference file.
postcode_lookup <- read_rds('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>%
  setNames(tolower(names(.)))  #variables to lower case

# Join geogrpaphy lookup and deaths data.
data_deaths_raw <- left_join(data_deaths_raw, postcode_lookup, "pc7") %>% 
  select(year, age_grp, sex_grp, datazone2011, intzone2011) 

# Frequencies of deaths where no match to DZ/IZ by year -
# Check earlier years don't have excessive non-matching deaths. Non matched should be the same for DZ and IZ
table(filter(data_deaths_raw, is.na(intzone2011))$year)
table(filter(data_deaths_raw, is.na(datazone2011))$year)

#aggregate IZ level deaths by year, age, sex for scottish residents only 
data_deaths_iz <- data_deaths_raw %>%
  subset(!(is.na(intzone2011))) %>%  #exclude non scottish residents
  group_by(year,age_grp,sex_grp,intzone2011) %>%
  summarise(deaths=n()) %>%
  ungroup() %>%
  rename(geography=intzone2011)

#aggregate HSCP locality level deaths by year, age, sex for scottish residents only 
dz_locality_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/DataZone11_HSCLocality_Lookup.rds")

# Join deaths data to DZ to Partnership geogrpaphy lookup.
data_deaths_locality <- left_join(data_deaths_raw, dz_locality_lookup, by ="datazone2011") 

data_deaths_locality <- data_deaths_locality %>%
  subset(!(is.na(hscp_locality))) %>%  #exclude non scottish residents
  group_by(year,age_grp,sex_grp,hscp_locality) %>%
  summarise(deaths=n()) %>%
  ungroup() %>%
  rename(geography=hscp_locality)

# Add HSCP locality and IZ deaths files
data_deaths_all <- bind_rows(data_deaths_iz, data_deaths_locality)

# Check deaths totals are the same for each geography type
xtabs(deaths~year+(substr(geography,1,3)), data_deaths_all)

# Save deaths file
saveRDS(data_deaths_all, file=paste0(temp_network,'data_deaths.rds'))

rm(data_deaths_raw, data_deaths_iz,data_deaths_locality,dz_locality_lookup) #optional cleaning 

###############################################.
## Part 2 - Read in Scotland Populations from ISD lookup ----
###############################################.

# Read in small area population lookup based on 2011 datazones for years prior to 2011.
# NRS back calculated these populations
data_pop1 <- readRDS(paste0(cl_out_pop,"DataZone2011_pop_est_2001_2010.rds"))%>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year>=2001) %>%
  mutate(sex_grp = case_when(sex=="M"~"1",sex=="F"~"2",TRUE~"other")) %>%
  select(-c(total_pop,sex, datazone2011name))

# Read in small area population lookup, select required data and recode to permit matching.
data_pop2 <- readRDS(paste0(cl_out_pop,"DataZone2011_pop_est_2011_2018.rds")) %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year>=2011) %>%
  mutate(sex_grp = case_when(sex=="M"~"1",sex=="F"~"2",TRUE~"other")) %>%
  select(-c(total_pop,sex, datazone2011name))

data_pop <- bind_rows(data_pop1,data_pop2)
rm(data_pop1,data_pop2)

# Reshape data to long format
data_pop <- data_pop %>% gather(age, pop, -c(year, sex_grp, intzone2011))

#Converting pop & age to numeric
data_pop$pop <- as.numeric(data_pop$pop)
data_pop$age <- as.numeric(gsub("age|plus", "", data_pop$age))

# Format and add age bands (<1 years, 1-4 years, then 5 year age bands)
data_pop <- data_pop %>%
  create_agegroups() %>%
  group_by (year,sex_grp, age_grp, datazone2011) %>%
  summarise(pop=sum(pop)) %>%
  ungroup()

# Read in dz to localities and izs lookup
dz_geo_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/DataZone11_All_Geographies_Lookup.rds")

# Join IZ pops data to Partnership geogrpaphy lookup.
data_pop <- left_join(data_pop, dz_geo_lookup, "datazone2011") 

# Aggregate populations for hscp localities
data_pop_locality <- data_pop %>%
group_by(year,age_grp,sex_grp,hscp_locality) %>%
  summarise(pop=sum(pop)) %>%
  ungroup() %>%
  rename(geography=hscp_locality)

# Aggregate populations for hscp localities
data_pop_iz <- data_pop %>%
  group_by(year,age_grp,sex_grp,intzone2011) %>%
  summarise(pop=sum(pop)) %>%
  ungroup() %>%
  rename(geography=intzone2011)

# Add HSCP locality and IZ pops files
data_pop_all <- bind_rows(data_pop_iz, data_pop_locality)

# Check pops totals are the same for each geography type
xtabs(pop~year+(substr(geography,1,3)), data_pop_all)

# Save population file
saveRDS(data_pop_all, file=paste0(temp_network,'data_pop.rds'))

rm(postcode_lookup, data_pop, data_pop_iz, data_pop_locality, dz_geo_lookup) #optional cleaning


##########################################################################################.
## Part 3 - Generating Life Expectancy Estimates ----
##########################################################################################.
# Running part 1 & 2 of this program generate a population & death file that are picked up & joined in the macro
# Call life expectancy function 

# Parameters:

# run_name - This token acts as an identifier for the output files generate,
#              if it is not changed it will over-write files generated in a previous run 
# fp_deaths     - filename identifying deaths data to use
# fp_pop        - filename identifying pop data to use
# fp_output     - Name of existing network where output files are saved to (for options see: \\nssstats01\ScotPHO\Life Expectancy\Data\Output Data\ ) 
# yearstart     - first calendar year of data to use in trend     
# yearend       - last calendar year of data to use in trend 
# time_agg      - number of years of data for aggegrated time periods (1 = single year, 2,3,4,5 etc)


LE_85_function(run_name="2001to2018 IZ&Locality LE(85+)_20191003",fp_deaths="data_deaths", 
               fp_pop="data_pop", fp_output="4_Intermediate Zone LE (annual)",   
               yearstart=2001, yearend=2018, time_agg=5)

# Function generates 3 output RDS files than can be used for checking or analysis
# Select & run the "run_name" & "fp_output" tokens above & the script below will 
# read in files you created with the function above.
run_name="2001to2018 IZ&Locality LE(85+)_20191003"
fp_output="4_Intermediate Zone LE (annual)"

raw_data <- readRDS(paste0(temp_network,run_name,"-le_raw.rds")) # raw data before le calculations
lifetable_data <- readRDS(paste0(output_network,fp_output,"/",run_name,"_full life table.rds")) #full life table
le0_data <- readRDS(paste0(output_network,fp_output,"/",run_name,"_life expectancy at birth.rds")) #life expectancy at birth

# Optional files can be saved as csv if required - not generally required
write_csv(le0_data, path = paste0(output_network,fp_output,"/",run_name,"_life expectancy at birth.csv"),
         col_names = TRUE)


##END
