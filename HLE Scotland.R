## HLE Scotland.R

## V Elliott
## Jan 2019

## Script used to generate single year Healthy Life Expectancy figure for males and females in Scotland.

## Program requires 3 sources of data (split by ageband and gender)
## 1 - Population (from ISD reference files)
## 2 - Deaths (from SMRA deaths oracle table)
## 3 - Self assessed health data (provided by SG SHoS and SHeS survey)

## Agebands <1, 0-4, 5-9, 10-14 etc to 90+ years
## LE/HLE calculations assume 20 agebands - if maximum age band is not 90+ then LE & HLE calculations must be adapted.





###############################################.
## Packages and filepaths ----
###############################################.
library(odbc)     #connections to SMRA
library(dplyr)    #data manipulations 
library(readr)    #for opening csv
library(foreign)  #read spss files in lookups folder
library(tidyr)    #for spread function to rearrange coloumn to variable (SHeS)
library(RcppRoll) #for rolling sums

server_desktop <- "server" #change depending on what version of RStudio are you using

if (server_desktop == "server") {
  cl_out_pop <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/"
  source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/"
  output_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Output Data/"

} else if (server_desktop == "desktop") {
  cl_out_pop <- "//stats/linkage/output/lookups/Unicode/Populations/Estimates/"
  source_network <- "//stats/ScotPHO/Life Expectancy/Data/raw data/"
  output_network <- "//stats/ScotPHO/Life Expectancy/Data/Output Data/"
}


####################################################################################
## SHeS SAH data ----
####################################################################################

##Read in csv SHeS data & reformat.
shes_data <- read_csv(paste0(source_network,"Self Assessed Health Data/SHeS/","2018/AH2018-005 - Figures for healthy life expectancy stats (2017).csv")) %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  rename(sah=genhelf, year=syear, sex_grp=sex) %>%
  subset(age<=14) %>%   # SHeS data only used for those up to age of 14
  mutate(age_grp = case_when( 
    age == 0 ~ 1,  age >= 1 & age <=4 ~ 2,  age >= 5 & age <=9 ~ 3,  age >= 10 & age <=14 ~ 4,age >= 15 ~ 20))

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

####################################################################################
## SHoS SAH data ----
####################################################################################

##Read in csv SHeS data & reformat.
shos_data <- read_csv(paste0(source_network,"Self Assessed Health Data/SHoS/","2018/data/SHoS 2017.csv")) %>% 
  setNames(tolower(names(.))) %>% #variables to lower case
  rename (sah=health, sex_grp=gender) %>%
  subset(sah>=1 & sah<=5) %>% #filter out any sah=6 which are 'don't know' responses
  subset(age>=16) %>%   # SHoS data only available for >=16 years - use the 16-19 year SHoS data to represent 15-19 yrs age group.
  mutate(age_grp = case_when( 
    age >= 0 & age <=14 ~ 99, age >= 15 & age <=19 ~ 5, age >= 20 & age <=24 ~ 6, age >= 25 & age <=29 ~ 7, age >= 30 & age <=34 ~ 8,
    age >= 35 & age <=39 ~ 9, age >= 40 & age <=44 ~ 10, age >= 45 & age <=49 ~ 11, age >= 50 & age <=54 ~ 12,
    age >= 55 & age <=59 ~ 13,age >= 60 & age <=64 ~ 14,age >= 65 & age <=69 ~ 15,age >= 70 & age <=74 ~ 16,
    age >= 75 & age <=79 ~ 17,age >= 80 & age <=84 ~ 18,age >= 85 & age <=89 ~ 19,age >= 90 ~ 20))


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
  mutate(totsah=healthy+not_healthy)  #provides weighted popultion totals

rm(shes_data,shos_data)

####################################################################################
## Deaths Data ----
####################################################################################

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
  mutate(age_grp = case_when( 
    age == 0 ~ 1,  age >= 1 & age <=4 ~ 2,  age >= 5 & age <=9 ~ 3,  age >= 10 & age <=14 ~ 4,  
    age >= 15 & age <=19 ~ 5, age >= 20 & age <=24 ~ 6, age >= 25 & age <=29 ~ 7, age >= 30 & age <=34 ~ 8,
    age >= 35 & age <=39 ~ 9, age >= 40 & age <=44 ~ 10, age >= 45 & age <=49 ~ 11, age >= 50 & age <=54 ~ 12,
    age >= 55 & age <=59 ~ 13,age >= 60 & age <=64 ~ 14,age >= 65 & age <=69 ~ 15,age >= 70 & age <=74 ~ 16,
    age >= 75 & age <=79 ~ 17,age >= 80 & age <=84 ~ 18,age >= 85 & age <=89 ~ 19,age >= 90 ~ 20)) %>%
  mutate(sex_grp=recode(data_deaths$sex_grp,"9"="1")) %>% #reassign any deaths with unknown gender to males - as NRS do 
  group_by(year, sex_grp,age_grp) %>%
  summarise(deaths=n()) %>%
  mutate(level="Scotland")

# cross-tab for checking total deaths - do the numbers of deaths look as expected?
xtabs(data_deaths$deaths ~ data_deaths$sex_grp + data_deaths$year)


####################################################################################
## Population Data ----
####################################################################################

#Using health board population for Scotland population
data_pop <- read.spss(paste0(cl_out_pop, "HB2014_pop_est_1981_2017.sav"),
                      to.data.frame=TRUE, use.value.labels=FALSE) %>%
  setNames(tolower(names(.))) %>%  #variables to lower case
  subset(year==2017)

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
  mutate(level="Scotland") %>%
  ungroup()

#check total population - Are populations for all years 5.4 million ish 
xtabs(data_pop$pop~data_pop$year)


####################################################################################
## Join Pop, Deaths & SAH Data ----
####################################################################################

all_data <- left_join(data_pop, data_deaths, by=c("year","sex_grp","age_grp","level"))

# reformat some fields in sah file to allow join
sah_data <- sah_data %>%
  mutate(sex_grp=as.character(sex_grp),  #format variables to enable joining
         year=as.numeric(year))

all_data <- left_join(all_data, sah_data, by=c("year","sex_grp","age_grp")) %>%
  rename(totdeaths=deaths, totpop=pop)

rm(data_deaths, data_pop, sah_data) #remove files no longer needed

####################################################################################
## HLE Calculations ----
####################################################################################


# Read in raw data file including population, deaths and self-assessed health data from SHoS & SHeS.

# all_data_old <- read_csv(paste0(source_network,"HLE_Scot_2016_17.csv")) %>% 
#   setNames(tolower(names(.)))  #variables to lower case

# Set time aggregation for estimate (1 - single year, 2 years etc)
time_agg = 1

#create label for time period calculated - uses time_agg token supplied.
if(time_agg == 1){
  all_data <- all_data %>%
    mutate(time_period=paste0(as.character(year)," single year estimate"))
}else if(time_agg != 1){
  all_data <- all_data %>%
    mutate(time_period = paste0(as.character(year-(time_agg-1))," to ",as.character(year)," (",time_agg," year aggregate)"))
}


# Life Expectancy calulations
lifetable_data <- all_data %>%
  group_by(sex_grp, time_period, level) %>% 
  mutate(mx = totdeaths / totpop,              # death rate
         n = c(1,4,rep(5,17),2/mx[20]),        # age intervals (<1=1, 1-4 = 4, 5-85 = 5 years, 90+ = 2/Mx)
         ax = c(0.1, rep(0.5,19)) ,            # ax = Fraction of the age interval lived by those who die in the interval - chiang methodolgy sets ax to 0.1 for first age band and 0.5 for all others 
         qx = case_when(age_grp == 20 ~ 1,     # qx = Conditional probability individual entering age band will die
                        TRUE ~ n*mx/(1+n*(1-ax)*mx)),
         px= 1-qx,                             # px = Conditional probability individual entering age band will survive
         radix = 100000 ,                      # set radix (total imaginary cohort population)
         pre_Ix = cumprod(1-qx)*radix ,        # first step in calculating life table pop - second step (to calculate Ix) uses a lag function which only seems to work when lag value already defined.
         Ix= case_when(age_grp == 1 ~ radix,   # Ix =  Life table population (usually 100,000 at birth)
                       TRUE ~ lag(pre_Ix)),
         dx = Ix*qx,                           # dx =  Number of life table deaths
         Lx = case_when(age_grp == 20 ~ n*ax*dx,           # Lx = Total number of years lived during the time period. Last age group treated differently as this is wider/open ended age band
                        TRUE ~  n*(lead(Ix,1)+(ax*dx))),
         Tx = rev(cumsum(rev(Lx))),           # Total # years lived beyond entry age
         LEx =Tx/Ix,                          # Life expectancy
         var_qx=(n^2*mx*(1-ax*n*mx))/(totpop*(1+(1-ax)*n*mx)^3),  #variance of qx (prob of death) 1984 method
         se1=case_when(age_grp==20~0, TRUE ~ (Ix^2)*(((1-ax)*n+lead(LEx,1))^2)*var_qx), # deriving standard error - requires 2 steps (se1 & se2)
         se2=rev(cumsum(rev(se1))),
         var_ex=se2/(Ix^2),     # variance of life expectancy
         se=sqrt(var_ex),       # standard error of life expectancy
         lci=LEx-(1.96*se),     # 95% lower confidence interval
         uci=LEx+(1.96*se)) %>%     # 95% upper confidence interval
  ungroup ()


# Healthy life expectancy calculations
lifetable_data <- lifetable_data %>%
  group_by(sex_grp, time_period, level) %>% 
  mutate(p_notgood_x=not_healthy/totsah,                  # proportion of ageband with not good health
         goodhealth_Lx=(1-p_notgood_x)*Lx,                # person years lived in good health
         THx = rev(cumsum(rev(goodhealth_Lx))),           # total years lived as healthy from year x
         HLEx = THx/Ix,                                   # healthy life expectancy
         Nx= totsahunw,                                   # number in survey in age interval
         hle_ci1= p_notgood_x*(1-p_notgood_x)/Nx,         # stage1 in calculating HLE variance
         hle_ci2= Lx^2*hle_ci1,                           # stage2
         hle_3=rev(cumsum(rev(hle_ci2))),                 # stage3
         var_hle=hle_3/Ix^2,                              # variance for HLE
         se_hle=sqrt(var_hle),                            # standard error for HLE
         hle_lci=HLEx-(1.96*se_hle),                      # 95% lower confidence interval   
         hle_uci=HLEx+(1.96*se_hle))                      # 95% upper confidence interval


# Summary table for LE and HLE at birth
le0_data<-  lifetable_data %>% 
  group_by(level,time_period, sex_grp) %>%  #generate total populations & deaths to permit testing of conditions such as total pop above a certin size
  mutate(pop=sum(totpop), deaths=sum(totdeaths)) %>%
  ungroup() %>%
  subset(age_grp==1) %>% #generate table for life expectancy at birth with confidence intervals.
  select(level,time_period,sex_grp, pop, deaths,LEx, lci, uci,HLEx,hle_lci, hle_uci ) %>%
  arrange(level, sex_grp,time_period)


##END
