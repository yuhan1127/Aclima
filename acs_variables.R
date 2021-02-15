library(tidycensus)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)

library(sf)
library(sp)

# add CENSUS API key to your .Renviron
# cf. obtain your key first at http://api.census.gov/data/key_signup.html
census_api_key("328ef26ed80f8d26c52b7ff647b6f21b4dd5de80") # Yunbi's API key
readRenviron("~/.Renviron")
# Sys.getenv("CENSUS_API_KEY")

# load variables from 2015-2019 5-year ACS data
acs5_19_var <- load_variables(2019, "acs5", cache = TRUE)

# obtain total population data 1.656 billion
pop <- get_acs(geography = "block group",
               variables = "B01003_001",
               state = "CA", county = "Alameda", geometry = TRUE)

# obtain percent estimate
pop <- pop %>%
        mutate(percent = round(estimate/sum(estimate)*100, 1))

pop <- pop[order(pop$estimate),]

# range(pop$estimate) # 0 - 11,119
# head(pop,10) # There are 5 block groups whose population estimates are 0 

# obtain race/ethnicity data and feature geometry for the American Community Survey (ACS)
race_vars <- acs5_19_var[c(590:596,599),]

race_bg <- get_acs(geography = "block group",
                   variables = race_vars$name,
                   state = "CA", county = "Alameda", geometry = TRUE)

# obtain percent estimate
race_bg <- race_bg %>%
        group_by(NAME) %>%
        mutate(percent = round(estimate/sum(estimate)*100, 1))

# grouping: non-hispanic white alone, non-hispanic african american alone, non-hispanic asian alone,
########### non-hispanic other, hispanic
# follow the race categoreis used in reference - michelle and keita (2012)

white_bg <- race_bg %>% filter(variable == "B03002_003")
aa_bg <- race_bg %>% filter(variable == "B03002_004")
asian_bg <- race_bg %>% filter(variable == "B03002_006")
other_bg <- race_bg %>% filter(variable %in% c("B03002_005", "B03002_007", "B03002_008", "B03002_009"))
hisp_bg <- race_bg %>% filter(variable == "B03002_012")

# obtain summed percent estimate of non-hispanic other group
other_bg <- other_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))

# obtain *median* age data
med_age_bg <- get_acs(geography = "block group",
                      variables = c(med_age = "B01002_001"),
                      state = "CA", county = "Alameda", geometry = TRUE)

# obtain *categorical* age estimates data
age_vars <- acs5_19_var[c(3:25,27:49),]

cat_age_bg <- get_acs(geography = "block group",
                      variables = age_vars$name,
                      state = "CA", county = "Alameda", geometry = TRUE)

# obtain percent estimate
cat_age_bg <- cat_age_bg %>% group_by(NAME) %>%
        mutate(percent = round(estimate/sum(estimate)*100, 1))

# follow the age categoreis used in reference - michelle and keita (2012)
# 0-19, 20-64, 65+
###### Q. grouping - is the interval too large?
###### Q. median age vs. age categories
###### Q. Any suggestions on how to explore age variable?
cat_age19_bg <- cat_age_bg %>%
        filter(variable %in% c(age_vars$name[1:5], age_vars$name[24:28]))
cat_age64_bg <- cat_age_bg %>%
        filter(variable %in% c(age_vars$name[6:17], age_vars$name[29:40]))
cat_age65_bg <- cat_age_bg %>%
        filter(variable %in% c(age_vars$name[18:23], age_vars$name[41:46]))

cat_age19_bg <- cat_age19_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))
cat_age64_bg <- cat_age64_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))
cat_age65_bg <- cat_age65_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))

# Further investigation
cat_age24_bg <- cat_age_bg %>%
        filter(variable %in% c(age_vars$name[1:8], age_vars$name[24:31]))
cat_age44_bg <- cat_age_bg %>%
        filter(variable %in% c(age_vars$name[9:12], age_vars$name[32:35]))
cat_age45_64_bg <- cat_age_bg %>%
        filter(variable %in% c(age_vars$name[13:17], age_vars$name[36:40]))
cat_age65_bg <- cat_age_bg %>%
        filter(variable %in% c(age_vars$name[18:23], age_vars$name[41:46]))

cat_age24_bg <- cat_age24_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))
cat_age44_bg <- cat_age44_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))
cat_age45_64_bg <- cat_age45_64_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))
cat_age65_bg <- cat_age65_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))

# obtain educational attainment data for the population 25 years and over
edu_vars <- acs5_19_var[8156:8179,]

cat_edu_bg <- get_acs(geography = "block group",
                      variables = edu_vars$name,
                      state = "CA", county = "Alameda", geometry = TRUE)

# obtain percent estimate
cat_edu_bg <- cat_edu_bg %>% group_by(NAME) %>%
        mutate(percent = round(estimate/sum(estimate)*100, 1))

###### cf. Regular high school diploma vs. GED or alternative credential
###### cf. GED is an alternative to the US high school diploma

# in reference - Hajat et al. (2013), *for neighborhood SES index data,*
# Education was characterized as the percentage of persons with at least a high school degree 
# and the percentage with at least a Bachelor’s degree.

# obtain percent of person 25 or older with at least high school education
high_bg <- cat_edu_bg %>%
        filter(variable %in% edu_vars$name[16:24])

high_bg <- high_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))

# obtain percent of person 25 or older with at least a Bachelor's degree
bachelor_bg <- cat_edu_bg %>%
        filter(variable %in% edu_vars$name[21:24])

bachelor_bg <- bachelor_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))

# obtain *median* household income data
med_inc_bg <- get_acs(geography = "block group",
                      variables = c(med_inc = "B19013_001"),
                      state = "CA", county = "Alameda", geometry = TRUE)

# obtain categorical household income estimates data
inc_vars <- acs5_19_var[11083:11098,]

cat_inc_bg <- get_acs(geography = "block group",
                      variables = inc_vars$name,
                      state = "CA", county = "Alameda", geometry = TRUE)

cat_inc_bg <- cat_inc_bg %>% group_by(NAME) %>%
        mutate(percent = round(estimate/sum(estimate)*100, 1))

# obtain percent of households with $100,000 or more income in the past 12 months
# Median household income of Alameda County: 99406 (source: ACS 2015-2019)
hhinc_abv_bg <- cat_inc_bg %>%
        filter(variable %in% inc_vars$name[13:16])

hhinc_abv_bg <- hhinc_abv_bg %>% group_by(NAME) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))

# obtain unemployed among civilians 16 and over in the labor force
empst_bg <- get_acs(geography = "block group",
                    variables = c("B23025_004", "B23025_005"),
                    state = "CA", county = "Alameda", geometry = TRUE)

empst_bg <- empst_bg %>% group_by(NAME) %>%
        mutate(percent = round(estimate/sum(estimate)*100, 1))

unemp_bg <- empst_bg %>% filter(variable == "B23025_005")