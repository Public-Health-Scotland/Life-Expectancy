## Script to prepare data downloaded from NRS website ##
## Created Feb 2023, Peter Menzies ## 

## Before running this script:
# 1. - Download the latest data from: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/life-expectancy-at-scotland-level
#    - Latest publication -> Data tables -> Download csv of Tables 3 & 4 (these are zip files)
# 2. Open the zip folder and copy the latest file to \\stats\ScotPHO\Life Expectancy\Data\Source Data\NRS data
# 3. Delete the zip folder.  
# 4. Update filenames below

### Dates/Filenames to update ###

latest_NRS_data <- "2019-2021"  #should be three years
new_year <- "19-21"           # should match the years of latest_NRS_data 
new_data_years <- "2019 to 2021" # should match the years of latest_NRS_data

previous_data_filename <- "2001 to 2020" # see latest file in "\\stats\ScotPHO\Life Expectancy\Data\Source Data" titled "NRS LE data with CI..."
new_data_filename <- "2001 to 2021" # should be same start year and 1 year after for end year


###############################################.
## 1. Packages, filepaths and lookups ----
###############################################.

library(odbc)     #connections to SMRA
library(readr)    #reading in file
library(tidyr)    #for melt - reshaping of data
library(dplyr)    #data manipulations 
library(readr)    #reading in file
library(janitor)


#change depending on what version of RStudio are you using
if (sessionInfo()$platform %in% c("x86_64-redhat-linux-gnu (64-bit)", "x86_64-pc-linux-gnu (64-bit)")) {
  cl_out_pop <- "/conf/linkage/output/lookups/Unicode/Populations/Estimates/"
  temp_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/temporary/"
  source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/"
  output_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Output Data/"
  shiny_network <- "/PHI_conf/ScotPHO/Profiles/Data/Data to be checked/"
  
} else {
  cl_out_pop <- "//stats/linkage/output/lookups/Unicode/Populations/Estimates/"
  temp_network <- "//stats/ScotPHO/Life Expectancy/Data/temporary/"
  source_network <- "//stats/ScotPHO/Life Expectancy/Data/Source Data/"
  output_network <- "//stats/ScotPHO/Life Expectancy/Data/Output Data/"
  shiny_network <- "//stats/ScotPHO/Profiles/Data/Data to be checked/"
}


geo_lookup <- read_csv("/conf/linkage/output/lookups/Unicode/Geography/DataZone2011/Datazone2011lookup.csv")

hb_lookup <- geo_lookup %>% 
  select(hb2019, hb2019name) %>% 
  rename(area = hb2019name,
         code = hb2019) %>% 
  unique()

ca_lookup <- geo_lookup %>% 
  select(LA_Name, CA2018) %>% 
  rename(area = LA_Name, 
         code = CA2018) %>% 
  unique()

#################################
### 2.  Read in new data      ###
#################################

# due to format of data, it is read in in multiple parts then bound together.

### COUNCIL AREAS ###

ca_le_1 <- read_csv(paste0(source_network, "NRS data/", "life-expectancy-", new_year, "-tab3_", latest_NRS_data, ".csv"),
                  skip = 1, n_max = 5, col_select = !1) %>% 
  remove_empty() %>% 
  t() %>% as_tibble() %>% 
   fill(V1, V2)

ca_le_2 <- read_csv(paste0(source_network, "NRS data/", "life-expectancy-", new_year, "-tab3_", latest_NRS_data, ".csv"),
                    skip = 26, n_max = 5, col_select = !1) %>% 
  remove_empty() %>% 
  t() %>% as_tibble() %>% 
  fill(V1, V2)

ca_le_3 <- read_csv(paste0(source_network, "NRS data/", "life-expectancy-", new_year, "-tab3_", latest_NRS_data, ".csv"),
                    skip = 51, n_max = 5, col_select = !1) %>% 
  remove_empty() %>% 
  t() %>% as_tibble() %>% 
  fill(V1, V2)
 
ca_le_4 <- read_csv(paste0(source_network, "NRS data/", "life-expectancy-", new_year, "-tab3_", latest_NRS_data, ".csv"),
                    skip = 76, n_max = 5, col_select = !1) %>% 
  remove_empty() %>% 
  t() %>% as_tibble() %>% 
  fill(V1, V2)

ca_le_5 <- read_csv(paste0(source_network, "NRS data/", "life-expectancy-", new_year, "-tab3_", latest_NRS_data, ".csv"),
                    skip = 101, n_max = 5, col_select = !1) %>% 
  remove_empty() %>% 
  t() %>%  as_tibble() %>% 
  fill(V1, V2)

ca_le_6 <- read_csv(paste0(source_network, "NRS data/", "life-expectancy-", new_year, "-tab3_", latest_NRS_data, ".csv"),
                    skip = 126, n_max = 5, col_select = !1) %>% 
  remove_empty() %>% 
  t() %>% as_tibble() %>% 
  fill(V1, V2)

ca_le_7 <- read_csv(paste0(source_network, "NRS data/", "life-expectancy-", new_year, "-tab3_", latest_NRS_data, ".csv"),
                    skip = 151, n_max = 5, col_select = 2:19) %>% 
  remove_empty() %>% 
  t() %>% as_tibble() %>% 
  fill(V1, V2)

all_ca_le <- rbind(ca_le_1, ca_le_2, ca_le_3, ca_le_4, ca_le_5, ca_le_6, ca_le_7) %>% 
  rename("area" = V1,
         "sex" = V2,
         "value_type" = V3,
         "value" = V4) %>% 
  filter(area != "Scotland1") %>%  # Scotland data is also in the HB file so this prevents duplicates
  left_join(ca_lookup, by = "area") %>% 
  pivot_wider(names_from = value_type,
              values_from = value)

rm(ca_le_1, ca_le_2, ca_le_3, ca_le_4, ca_le_5, ca_le_5, ca_le_6, ca_le_7)

### HEALTH BOARDS ###

hb_le_1 <- read_csv(paste0(source_network, "NRS data/", "life-expectancy-", new_year, "-tab4_", latest_NRS_data, ".csv"),
                    skip = 1, n_max = 5, col_select = !1) %>% 
  remove_empty() %>% 
  t() %>% as_tibble() %>% 
  fill(V1, V2)

hb_le_2 <- read_csv(paste0(source_network, "NRS data/", "life-expectancy-", new_year, "-tab4_", latest_NRS_data, ".csv"),
                    skip = 26, n_max = 5, col_select = !1) %>% 
  remove_empty() %>% 
  t() %>% as_tibble() %>% 
  fill(V1, V2)

hb_le_3 <- read_csv(paste0(source_network, "NRS data/", "life-expectancy-", new_year, "-tab4_", latest_NRS_data, ".csv"),
                    skip = 51, n_max = 5, col_select = !1) %>% 
  remove_empty() %>% 
  t() %>% as_tibble() %>% 
  fill(V1, V2)

all_hb_le <- rbind(hb_le_1, hb_le_2, hb_le_3) %>% 
  rename("area" = V1,
         "sex" = V2,
         "value_type" = V3,
         "value" = V4) %>% 
  left_join(hb_lookup, by = "area") %>% 
  mutate(code = case_when(area == "Scotland" ~ "S00000001",
                          T ~ code)) %>% 
  pivot_wider(names_from = value_type,
              values_from = value)
  

rm(hb_le_1, hb_le_2, hb_le_3)

##################################################
### 3. Prepare new data to macth old data      ###
##################################################

all_new_data <- all_hb_le %>% 
  rbind(all_ca_le) %>% 
  mutate(time_period = new_data_years,
         sex = recode(sex, 
                      "Males" = 1,
                      "Females" = 2)) %>% 
  rename("sex_grp" = "sex",
         "LEx" = "exo",
         "lci" = "lower CI",
         "uci" = "upper CI",
         "geography" = "area") %>% 
  select(time_period, sex_grp, LEx, lci, uci, code, geography)
  


#################################
### 4.  Read in old data      ###
#################################

old_data <- read_csv(paste0(source_network, "NRS LE data with CI ", previous_data_filename, ".csv"))

output <- old_data %>% 
  rbind(all_new_data)

write_csv(output, paste0(source_network, "NRS LE data with CI ", new_data_filename, ".csv"))


# END
