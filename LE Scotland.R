# Life expectancy - Scotland trends


## Part 1 - Extract deaths data from NRS deaths view
## Part 2 - Read in Scotland Populations from ISD lookup
## Part 3 - Run life table macro - generates aggregated raw data file, life table and life expectancy at birth (LE0) RDS ouput files 
## Part 4 - Analysis of trends (optional)

###############################################.
## Packages/Filepaths ----
###############################################.
library(odbc)     #connections to SMRA
library(dplyr)    #data manipulations 
library(foreign)  #read spss files in lookups folder


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

source("./Function_Life Expectancy (90+).R") #Life expectancy function
source("./Function_Life Expectancy (85+).R") #Life expectancy function

###############################################.
## Part 1 - Extract deaths data from GRO view ----
###############################################.
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
                                 WHERE year_of_registration between '2009' AND '2017'")) %>%
  setNames(tolower(names(.)))  #variables to lower case


# Check numbers of non-residents - can compare to NRS published estimates if numbers look unusually high/low.
table(data_deaths$nonres)

# Format and add age bands (<1 years, 1-4 years, then 5 year age bands)
data_deaths <- data_deaths %>% 
  mutate(age_grp = case_when( 
    age == 0 ~ 1,  age >= 1 & age <=4 ~ 2,  age >= 5 & age <=9 ~ 3,  age >= 10 & age <=14 ~ 4,  
    age >= 15 & age <=19 ~ 5, age >= 20 & age <=24 ~ 6, age >= 25 & age <=29 ~ 7, age >= 30 & age <=34 ~ 8,
    age >= 35 & age <=39 ~ 9, age >= 40 & age <=44 ~ 10, age >= 45 & age <=49 ~ 11, age >= 50 & age <=54 ~ 12,
    age >= 55 & age <=59 ~ 13,age >= 60 & age <=64 ~ 14,age >= 65 & age <=69 ~ 15,age >= 70 & age <=74 ~ 16,
    age >= 75 & age <=79 ~ 17,age >= 80 & age <=84 ~ 18,age >= 85 & age <=89 ~ 19,age >= 90 ~ 20)) %>%
  mutate(sex_grp=recode(data_deaths$sex_grp,"9"="1")) %>% #reassign any deaths with unknown gender to males - as NRS do 
  group_by(year, sex_grp,age_grp) %>%
  summarise(deaths=n()) %>%
  mutate(geography="Scotland")

# cross-tab for checking total deaths - do the numbers of deaths look as expected?
xtabs(data_deaths$deaths ~ data_deaths$sex_grp + data_deaths$year)

#Published NRS estimates available here.
#https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/deaths-time-series-data

saveRDS(data_deaths, file=paste0(temp_network,'data_deaths.rds'))


###############################################.
## Part 2 - Read in Scotland Populations from ISD lookup ----
###############################################.


#Using health board population for Scotland population
data_pop <- read.spss(paste0(cl_out_pop, "HB2014_pop_est_1981_2017.sav"),
                      to.data.frame=TRUE, use.value.labels=FALSE) %>% 
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year>=2009)


data_pop <- data_pop %>%
  mutate(sex_grp=as.character(sex)) %>%
  mutate(age_grp = case_when( 
    age == 0 ~ 1,  age >= 1 & age <=4 ~ 2,  age >= 5 & age <=9 ~ 3,  age >= 10 & age <=14 ~ 4,  
    age >= 15 & age <=19 ~ 5, age >= 20 & age <=24 ~ 6, age >= 25 & age <=29 ~ 7, age >= 30 & age <=34 ~ 8,
    age >= 35 & age <=39 ~ 9, age >= 40 & age <=44 ~ 10, age >= 45 & age <=49 ~ 11, age >= 50 & age <=54 ~ 12,
    age >= 55 & age <=59 ~ 13,age >= 60 & age <=64 ~ 14,age >= 65 & age <=69 ~ 15,age >= 70 & age <=74 ~ 16,
    age >= 75 & age <=79 ~ 17,age >= 80 & age <=84 ~ 18,age >= 85 & age <=89 ~ 19,age >= 90 ~ 20)) %>%
  group_by(year, sex_grp, age_grp) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(geography="Scotland") %>%
  ungroup()

#check total population - Are populations for all years 5.4 million ish 
xtabs(data_pop$pop~data_pop$year)

saveRDS(data_pop, file=paste0(temp_network,'data_pop.rds'))


##########################################################################################.
## Part 3 - Generating Life Expectancy Estimates ----
##########################################################################################.

# Running part 1 & 2 of this program generate a population & death file that are opened, joined & manipulated in the LE function

# Tokens:

# run_name - This token acts as an identifier for the output files generate,
#              if it is not changed it will over-write files generated in a previous run 
# fp_deaths     - filename identifying deaths data to use
# fp_pop        - filename identifying pop data to use
# fp_output     - Name of folder to save any output files to (for options see: \\nssstats01\ScotPHO\Life Expectancy\Data\Output Data\ ) 
# yearstart     - first calendar year of data to use in trend     
# yearend       - last calendar year of data to use in trend 
# time_agg      - number of years of data for aggegrated time periods (1 = single year, 2,3,4,5 etc)



LE_90_function(run_name="2016to2017 LE Scotland single year(90+)_20190125",fp_deaths="data_deaths", fp_pop="data_pop",
            fp_output="Adhoc" , yearstart=2016, yearend=2017, time_agg=1)


# Macro should generate 3 output RDS files than can be used for checking or analysis
# Adjust the file paths below to open and inspect these files created by function

raw_data<- readRDS(paste0(temp_network,run_name,"-le_raw.rds"))
lifetable_data <- readRDS(paste0(output_network,fp_output,"/",run_name,"_full life table.rds")) #full life table
le0_data<- readRDS(paste0(output_network,fp_output,"/",run_name,"_life expectancy at birth.rds")) #life expectancy at birth


##########################################################################################.
## Part 4 - Analysis of trends ----
##########################################################################################.
##plot men mortality rate versus age
#plot(lifetable_data$age,ltable_m$mx, xlab="Ageband",ylab="Mortality rate" )
#plot(ltable_m$age,(log(ltable_m$mx,base = exp(1))), xlab="Ageband",ylab="Mortality rate")

#plot(ltable_m$age,ltable_m$ex, xlab="Age",ylab="Life expectancy",col="blue", ylim=c(0,80))
#points(ltable_f$age,ltable_f$ex, xlab="" , ylab="", col="red", ylim=c(0,80))





##END
