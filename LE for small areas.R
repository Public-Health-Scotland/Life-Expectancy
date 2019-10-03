# LE for small areas (e.h. Intermediate Zone or HSCP locality)
# 03/10/2019

# Program generates life expectancy at birth estimates for small areas such as 2011 intermediate zone
# geogrpahies

# Uses 85+ years as maximum age group (this is different to national stats which NRS/ONS and national level analysis use)
# 85+ years is still required to avoid issues where no deaths or population are registered within some smaller areas

# Prior to 2018 ScotPHO imputed some deaths at IZ level - non-scottish resident deaths were apportioned across all IZ
# This imputation is no longer applied after investigation suggest it makes little difference/no to final figures & increases complexity of code.


## Part 1 - Extract deaths data from ISD deaths tables
## Part 2 - Read in Scotland Populations based on 2011 datazone ISD lookup
## Part 3 - Call life expectancy function - generates aggregated raw data file, life table and life expectancy at birth (LE0) RDS ouput files 


###############################################.
## Packages/Filepaths ----
###############################################.

library(odbc)     #connections to SMRA
library(dplyr)    #data manipulations 
library(foreign)  #read spss files in lookups folder
library(readr)    #reading in file
library(reshape2) #for melt - reshaping of data

server_desktop <- "server" #change depending on what version of RStudio are you using

if (server_desktop == "server") {
  cl_out_pop <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/"
  temp_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/temporary/"
  output_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Output Data/"
  
} else if (server_desktop == "desktop") {
  cl_out_pop <- "//stats/linkage/output/lookups/Unicode/Populations/Estimates/"
  temp_network <- "//stats/ScotPHO/Life Expectancy/Data/temporary/"
  output_network <- "//stats/ScotPHO/Life Expectancy/Data/Output Data/"
}

source("./Function_Life Expectancy (85+).R") #Life expectancy function where max age band is 85+ years


###########################################################.
## Part 1 - Extract deaths data from NRS deaths view ----
###########################################################.

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), pwd=.rs.askForPassword("SMRA Password:")))

# Select data from deaths file - adjust years to select min and max year of registration needed
# Deaths for scottish residents are coded as (XS) non-residents codes as (non-res)

data_deaths_raw <- tbl_df(dbGetQuery(channel, statement=
                                       "SELECT year_of_registration year, age, sex sex_grp, country_of_residence cor, POSTCODE pc7,DATE_OF_BIRTH dob,DATE_OF_DEATH dod,
                                     CASE WHEN country_of_residence='XS'THEN 'XS'ELSE 'nonres' END as nonres 
                                     FROM ANALYSIS.GRO_DEATHS_C 
                                     WHERE year_of_registration between '2001' AND '2018'")) %>%
  setNames(tolower(names(.)))  #variables to lower case


# Check of numbers of non-residents - can compare to NRS published estimates.
table(data_deaths_raw$nonres, data_deaths_raw$year)

# Format and add age bands (<1 years, 1-4 years, then 5 year age bands)
# Recode unknown to male
data_deaths_raw <- data_deaths_raw %>%
  mutate(age_grp = case_when(
    age == 0 ~ 1,  age >= 1 & age <=4 ~ 2,  age >= 5 & age <=9 ~ 3,  age >= 10 & age <=14 ~ 4,
    age >= 15 & age <=19 ~ 5, age >= 20 & age <=24 ~ 6, age >= 25 & age <=29 ~ 7, age >= 30 & age <=34 ~ 8,
    age >= 35 & age <=39 ~ 9, age >= 40 & age <=44 ~ 10, age >= 45 & age <=49 ~ 11, age >= 50 & age <=54 ~ 12,
    age >= 55 & age <=59 ~ 13,age >= 60 & age <=64 ~ 14,age >= 65 & age <=69 ~ 15,age >= 70 & age <=74 ~ 16,
    age >= 75 & age <=79 ~ 17,age >= 80 & age <=84 ~ 18, age >= 85 ~ 19)) %>%
  mutate(sex_grp=recode(data_deaths_raw$sex_grp,"9"="1"))

# Read in geographic reference file.
postcode_lookup <- read_rds('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2019_2.rds') %>%
  setNames(tolower(names(.)))  #variables to lower case

# Join geogrpaphy lookup and deaths data.
data_deaths_raw <- left_join(data_deaths_raw, postcode_lookup, "pc7") %>% 
  select(year, age_grp, sex_grp, intzone2011) 

# Non-residents will not have an IZ - recode missing intzones to "xx"
data_deaths_raw$intzone2011[is.na(data_deaths_raw$intzone2011)] <- "xx"

# Frequencies of deaths where no IZ matched by year - check that earlier years don't have excessive non-matched deaths
table(filter(data_deaths_raw, intzone2011 == 'xx')$year)

#aggregate deaths by year, age, sex for scottish residents only 
data_deaths <- data_deaths_raw %>%
  subset(intzone2011 != "xx") %>%  #exclude non scottish residents
  group_by(year,age_grp,sex_grp,intzone2011) %>%
  summarise(deaths=n()) %>%
  ungroup() %>%
  rename(geography=intzone2011)

saveRDS(data_deaths, file=paste0(temp_network,'data_deaths.rds'))

rm(data_deaths_raw) #optional cleaning 

###############################################.
## Part 2 - Read in Scotland Populations from ISD lookup ----
###############################################.

# Read in small area population lookup based on 2011 datazones for years prior to 2011.
# NRS back calculated these populations
data_pop1 <- readRDS(paste0(cl_out_pop,"IntZone2011_pop_est_2001_2010.rds"))%>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year>=2001) %>%
  mutate(sex_grp = case_when(sex=="M"~"1",sex=="F"~"2",TRUE~"other")) %>%
  select(-c(total_pop,sex, intzone2011name))

# Read in small area population lookup, select required data and recode to permit matching.
data_pop2 <- readRDS(paste0(cl_out_pop,"IntZone2011_pop_est_2011_2018.rds")) %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year>=2011) %>%
  mutate(sex_grp = case_when(sex=="M"~"1",sex=="F"~"2",TRUE~"other")) %>%
  select(-c(total_pop,sex, intzone2011name))

data_pop <- bind_rows(data_pop1,data_pop2)

# Reshape data to long format
data_pop <- data_pop %>% melt(id.vars = c("year", "sex_grp", "intzone2011"),
                              variable.name = "age", value.name = "pop")
#Converting age in numeric
data_pop$age <- as.numeric(gsub("age|plus", "", data_pop$age))

# Format and add age bands (<1 years, 1-4 years, then 5 year age bands)
data_pop <- data_pop %>%
  mutate(age_grp = case_when(
    age == 0 ~ 1,  age >= 1 & age <=4 ~ 2,  age >= 5 & age <=9 ~ 3,  age >= 10 & age <=14 ~ 4,
    age >= 15 & age <=19 ~ 5, age >= 20 & age <=24 ~ 6, age >= 25 & age <=29 ~ 7, age >= 30 & age <=34 ~ 8,
    age >= 35 & age <=39 ~ 9, age >= 40 & age <=44 ~ 10, age >= 45 & age <=49 ~ 11, age >= 50 & age <=54 ~ 12,
    age >= 55 & age <=59 ~ 13,age >= 60 & age <=64 ~ 14,age >= 65 & age <=69 ~ 15,age >= 70 & age <=74 ~ 16,
    age >= 75 & age <=79 ~ 17,age >= 80 & age <=84 ~ 18,age >= 85 ~ 19)) %>%
  group_by (year,sex_grp, age_grp, intzone2011) %>%
  summarise(pop=sum(pop)) %>%
  rename(geography=intzone2011)

saveRDS(data_pop, file=paste0(temp_network,'data_pop.rds'))

#check annual population totals look OK
data_check_pop <- data_pop %>%
  group_by (year) %>%
  summarise(tot_pop=sum(pop))

rm(postcode_lookup, data_check_pop, data_pop1, data_pop2) #optional cleaning


##########################################################################################.
## Part 3 - Optional step for HSCP & Locality LE ----

## This chunk of code can be used if you want to create life expectancy data for a geography that is built
## up from a number of intermediate zones such as HSCP Partnerships or localities.
## Uses the IZ to partnership lookup file used by ScotPHO profiles tool.
##########################################################################################.

IZtoPartnership_lookup <- readRDS("/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/IZtoPartnership_parent_lookup.rds") %>%
  rename(geography=intzone2011)

# Join IZ deaths data to Partnership geogrpaphy lookup.
data_deaths_hscp <- left_join(data_deaths, IZtoPartnership_lookup, "geography") 

# LE only to be calculated for localities but not Partnerships (partnerships are effectively council area - CA life expectancy is domain of NRS and calculated using 3 year time period)
data_deaths_locality <- data_deaths_hscp %>%
  group_by(year,age_grp,sex_grp,hscp_locality) %>%
  summarise(deaths=sum(deaths)) %>%
  rename(geography=hscp_locality) %>%
  ungroup() 

# Join IZ pops data to Partnership geogrpaphy lookup.
data_pop_hscp <- left_join(data_pop, IZtoPartnership_lookup, "geography") 

# LE only to be calculated for localities but not Partnerships (partnerships are effectively council area - CA life expectancy is domain of NRS and calculated using 3 year time period)
data_pop_locality <- data_pop_hscp %>%
  group_by(year,age_grp,sex_grp,hscp_locality) %>%
  summarise(pop=sum(pop)) %>%
  rename(geography=hscp_locality) %>%
  ungroup() 


# Add locality geographies to IZ deaths & population files & save files ready for running function

data_pop_all <- bind_rows(data_pop, data_pop_locality)
saveRDS(data_pop_all, file=paste0(temp_network,'data_pop.rds'))

data_deaths_all <- bind_rows(data_deaths, data_deaths_locality)
saveRDS(data_deaths_all, file=paste0(temp_network,'data_deaths.rds'))



##########################################################################################.
## Part 4 - Generating Life Expectancy Estimates ----
##########################################################################################.

# Running part 1 & 2 of this program generate a population & death file that are picked up & joined in the macro
# Call life expectancy function 

# Tokens:

# run_name - This token acts as an identifier for the output files generate,
#              if it is not changed it will over-write files generated in a previous run 
# fp_deaths     - filename identifying deaths data to use
# fp_pop        - filename identifying pop data to use
# fp_output     - Name of existing network where output files are saved to (for options see: \\nssstats01\ScotPHO\Life Expectancy\Data\Output Data\ ) 
# yearstart     - first calendar year of data to use in trend     
# yearend       - last calendar year of data to use in trend 
# time_agg      - number of years of data for aggegrated time periods (1 = single year, 2,3,4,5 etc)


LE_85_function(run_name="2001to2018 IZ&Locality LE(85+)_20191003",fp_deaths="data_deaths", fp_pop="data_pop",
             fp_output="4_Intermediate Zone LE (annual)",   yearstart=2001, yearend=2018, time_agg=5)


# Function generates 3 output RDS files than can be used for checking or analysis
# Select & run the "run_name" & "fp_output" tokens above & the script below will 
# read in files you created with the function above.


raw_data<- readRDS(paste0(temp_network,run_name,"-le_raw.rds")) # raw data before le calculations
lifetable_data <- readRDS(paste0(output_network,fp_output,"/",run_name,"_full life table.rds")) #full life table
le0_data<- readRDS(paste0(output_network,fp_output,"/",run_name,"_life expectancy at birth.rds")) #life expectancy at birth


# Optional files can be saved as csv if required - not generally required
write_csv(le0_data, path = paste0(output_network,fp_output,"/",run_name,"_life expectancy at birth.csv"),
         col_names = TRUE)


##END
