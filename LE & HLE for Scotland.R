## LE and HLE Scotland.R

## V Elliott
## March 2030

## Script can be used to generate Life Expectancy and Healthy Life Expectancy figure for males and females in Scotland.

## Program requires 3 sources of data (split by ageband and gender)
## 1 - Deaths (from SMRA deaths oracle table)
## 2 - Population (from ISD reference files)
## 3 - Self assessed health data (provided by SG SHoS and SHeS survey)
## 4 - Call to functions


###############################################.
## Packages and filepaths ----
###############################################.

source("1_functions for life expectancy.R")


####################################################################################.
## Part 1: Deaths Data ----
## Required for both LE (and optional HLE)
####################################################################################.

# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"),
                                      pwd=.rs.askForPassword("SMRA Password:")))


# Select data from deaths file - adjust years to select min and max year of registration needed
# Deaths for scottish residents are coded as (XS) non-residents codes as (non-res)

data_deaths <- tbl_df(dbGetQuery(channel, statement=
                                   "SELECT year_of_registration year, age, sex sex_grp, country_of_residence cor, POSTCODE pc7,DATE_OF_BIRTH dob,DATE_OF_DEATH dod,
                                 CASE WHEN country_of_residence='XS'THEN 'XS'ELSE 'nonres' END as nonres 
                                 FROM ANALYSIS.GRO_DEATHS_C 
                                 WHERE year_of_registration = '2017'")) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Format and add age bands (<1 years, 1-4 years, then 5 year age bands)
data_deaths <- data_deaths %>% 
  create_agegroups_90() %>%
  mutate(sex_grp=recode(data_deaths$sex_grp,"9"="1")) %>% #reassign any deaths with unknown gender to males - as NRS do 
  group_by(year, sex_grp,age_grp) %>%
  summarise(deaths=n()) %>%
  mutate(geography="Scotland")

# cross-tab for checking total deaths - do the numbers of deaths look as expected?
xtabs(data_deaths$deaths ~ data_deaths$sex_grp + data_deaths$year)

# Save deaths file
saveRDS(data_deaths, file=paste0(temp_network,'data_deaths.rds'))


####################################################################################.
## Part 2: Population Data ----
## Required for LE (and optional HLE)
####################################################################################.

data_pop  <- read_rds(paste0(cl_out_pop,'HB2019_pop_est_1981_2018.rds')) %>%
  setNames(tolower(names(.))) %>% #variables to lower case
  subset(year==2017)

data_pop <- data_pop %>%
  mutate(sex_grp=as.character(sex)) %>%
  create_agegroups_90() %>%
  group_by(year, sex_grp, age_grp) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(geography="Scotland") %>%
  ungroup()

#check total population - Are populations for all years 5.4 million ish 
xtabs(data_pop$pop~data_pop$year)

# Save population file
saveRDS(data_pop, file=paste0(temp_network,'data_pop.rds'))

####################################################################################.
## Part 3: Self-Assessed Health Data ----
## ONLY REQUIRED FOR HLE CALCULATION  - skip part 3 if only calculating LE
####################################################################################.
.
## SHeS SAH data (SAH for under 15s year olds)----

##Read in csv SHeS data & reformat.
shes_data <- read_csv(paste0(source_network,"Self Assessed Health Data/SHeS/","2018/AH2018-005 - Figures for healthy life expectancy stats (2017).csv")) %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  rename(sah=genhelf, year=syear, sex_grp=sex) %>%
  subset(age<=14) %>%    # SHeS data only used for those up to age of 14
  create_agegroups_90()

## sum count of respondents based on unweighted pop - this is used in calculation of HLE Standard error
shes_unweighted <- shes_data %>%
  mutate(sahunw = 1) %>%
  group_by(year, sex_grp, age_grp) %>%
  summarise(totsahunw=sum(sahunw))
            
## sum count of respondents based on weighted pop by health status
## SAH Health Status 1=very good & 2=good (grouped into healthy) 3=fair, 4=bad, 5=very bad (grouped into unhealhty)

shes_weighted <- shes_data %>%
  mutate(health_status = case_when(sah==1 ~ "healthy",sah==2~ "healthy", sah>=3 & sah<=5 ~"not_healthy", TRUE~"x")) %>% 
  group_by(year, sex_grp, age_grp, health_status) %>%
  summarise(weighted_pop=sum(cint17wt)) %>%
  spread(health_status, weighted_pop) %>%
  ungroup()

shes_data <- full_join(shes_weighted, shes_unweighted, by=c("year","sex_grp","age_grp"))
rm(shes_weighted,shes_unweighted)


## SHoS SAH data (SAH for >=16s year olds) ----

##Read in csv SHeS data & reformat.
shos_data <- read_csv(paste0(source_network,"Self Assessed Health Data/SHoS/","2018/data/SHoS 2017.csv")) %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  rename (sah=health, sex_grp=gender) %>%
  subset(sah>=1 & sah<=5) %>% #filter out any sah=6 which are 'don't know' responses
  subset(age>=16) %>%   # SHoS data only available for >=16 years - use the 16-19 year SHoS data to represent 15-19 yrs age group.
  create_agegroups_90()

## sum count of respondents based on unweighted pop - this is used in calculation of HLE Standard error
shos_unweighted <- shos_data %>%
  mutate(sahunw = 1) %>%
  group_by(year, sex_grp, age_grp) %>%
  summarise(totsahunw=sum(sahunw))

## sum count of respondents based on weighted pop by health status
## SAH Health Status 1=very good & 2=good (grouped into healthy) 3=fair, 4=bad, 5=very bad (grouped into unhealhty)

shos_weighted <- shos_data %>%
  mutate(health_status = case_when(sah==1 ~ "healthy",sah==2~ "healthy", sah>=3 & sah<=5 ~"not_healthy", TRUE~"x")) %>% 
  group_by(year, sex_grp, age_grp, health_status) %>%
  summarise(weighted_pop=sum(ind_wt)) %>%
  spread(health_status, weighted_pop) %>%
  ungroup()

shos_data <- full_join(shos_weighted, shos_unweighted, by=c("year","sex_grp","age_grp"))

rm(shos_weighted,shos_unweighted)

#### Join all SAH data ----

sah_data <- full_join(shes_data, shos_data, by=c("year","sex_grp","age_grp","healthy","not_healthy","totsahunw"))%>%
  mutate(totsah=healthy+not_healthy,  #provides weighted popultion totals.
         sex_grp=as.character(sex_grp),  #format variables to enable joining
         year=as.numeric(year))

# Save deaths file
saveRDS(sah_data, file=paste0(temp_network,'data_sah.rds'))

rm(shes_data,shos_data)


##########################################################################################.
## Part 4 - Generating Life (and optional Healthy Life) Expectancy Estimates ----
##########################################################################################.
# Running parts 1 & 2 of this program, if HLE is required then part 3 must also be run. 
# Then call function within this R project to generate LE.

# Parameters for function:

# hle       - Set to true to include HLE calculation    
# max_agegrp - Set to max age group used in calculating LE 85 or 90.
# run_name - This token acts as an identifier for the output files generate,
#              if it is not changed it will over-write files generated in a previous run 
# fp_deaths     - filename identifying deaths data to use
# fp_pop        - filename identifying pop data to use
# fp_output     - Name of existing network where output files are saved to (for options see: \\nssstats01\ScotPHO\Life Expectancy\Data\Output Data\ ) 
# yearstart     - first calendar year of data to use in trend     
# yearend       - last calendar year of data to use in trend 
# time_agg      - number of years of data for aggegrated time periods (1 = single year, 2,3,4,5 etc)


#call function excluding HLE (default)
LE_function(max_agegrp=90,run_name="Scotland LE(90+)_20200311",
            fp_deaths="data_deaths",fp_pop="data_pop", fp_output="1_Scotland HLE & LE (annual)",
            yearstart=2017, yearend=2017, time_agg=1)


#call function including HLE
LE_function(hle=TRUE, max_agegrp=90,run_name="Scotland HLE & LE(90+)_20200311",
            fp_deaths="data_deaths",fp_pop="data_pop", fp_output="1_Scotland HLE & LE (annual)",
            yearstart=2017, yearend=2017, time_agg=1)


##END
