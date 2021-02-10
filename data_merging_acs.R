
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
other_bg <- other_bg %>% group_by(GEOID) %>% 
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

cat_age24_bg <- cat_age24_bg %>% group_by(GEOID) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))
cat_age44_bg <- cat_age44_bg %>% group_by(GEOID) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))
cat_age45_64_bg <- cat_age45_64_bg %>% group_by(GEOID) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))
cat_age65_bg <- cat_age65_bg %>% group_by(GEOID) %>% 
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

high_bg <- high_bg %>% group_by(GEOID) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))

# obtain percent of person 25 or older with at least a Bachelor's degree
bachelor_bg <- cat_edu_bg %>%
        filter(variable %in% edu_vars$name[21:24])

bachelor_bg <- bachelor_bg %>% group_by(GEOID) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))

colnames(high_bg)[3]<-"edu_>high_per"
colnames(bachelor_bg)[3]<-"edu_>bachelor_per"
        
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

hhinc_abv_bg <- hhinc_abv_bg %>% group_by(GEOID) %>% 
        summarise(estimate = sum(estimate), percent = sum(percent))

# obtain unemployed among civilians 16 and over in the labor force
empst_bg <- get_acs(geography = "block group",
                    variables = c("B23025_004", "B23025_005"),
                    state = "CA", county = "Alameda", geometry = TRUE)

empst_bg <- empst_bg %>% group_by(GEOID) %>%
        mutate(percent = round(estimate/sum(estimate)*100, 1))

unemp_bg <- empst_bg %>% filter(variable == "B23025_005")


### Above code is credited to Yunbi 
### Yuhan changed group_by(NAME) to group_by(GEOID) for data merging below
### GEOID as id to merge different ACS dataset


# ACS Race: Category
colnames(white_bg)[7]<-"race_white_per"
colnames(aa_bg)[7]<-"race_aa_per"
colnames(asian_bg)[7]<-"race_asian_per"
colnames(other_bg)[3]<-"race_other_per"
colnames(hisp_bg)[7]<-"race_hisp_per"
data_race<-white_bg%>%inner_join(aa_bg,by="GEOID")%>%
        inner_join(asian_bg,by="GEOID")%>%
        inner_join(hisp_bg,by="GEOID")%>%
        inner_join(other_bg,by="GEOID")
data_race<-data_race%>%select(c("GEOID",starts_with("race")))


# ACS Age: Median + category (<24|24-44|45-64|>65)
colnames(med_age_bg)[4]<-"age_median"
colnames(cat_age24_bg)[3]<-"age_24_per"
colnames(cat_age44_bg)[3]<-"age_25_44_per"
colnames(cat_age45_64_bg)[3]<-"age_45_64_per"
colnames(cat_age65_bg)[3]<-"age_65_per"
data_age<-as.data.frame(med_age_bg)%>%
        inner_join(as.data.frame(cat_age24_bg),by="GEOID")%>%
        inner_join(as.data.frame(cat_age44_bg),by="GEOID")%>%
        inner_join(as.data.frame(cat_age45_64_bg),by="GEOID")%>%
        inner_join(as.data.frame(cat_age65_bg),by="GEOID")
data_age<-data_age%>%select(c("GEOID",starts_with("age")))    

# ACS Education: Catergory      
colnames(high_bg)[3]<-"edu_>high_per"
colnames(bachelor_bg)[3]<-"edu_>bachelor_per"
data_edu<-as.data.frame(high_bg)%>%
        inner_join(as.data.frame(bachelor_bg),by="GEOID")
data_edu<-data_edu%>%select(c("GEOID",starts_with("edu")))    


# ACS Income: Median + Category ($100,000 cut-off)
colnames(med_inc_bg)[4]<-"hhinc_median"
colnames(hhinc_abv_bg)[3]<-"hhinc_>100000_per"
data_hhinc<-as.data.frame(med_inc_bg)%>%
        inner_join(as.data.frame(hhinc_abv_bg),by="GEOID")
data_hhinc<-data_hhinc%>%select(c("GEOID",starts_with("hhinc")))

# ACS unemployment: Category
colnames(unemp_bg)[7] <- "unemp_per"
data_unemp<-unemp_bg
data_unemp<-data_unemp%>%select(c("GEOID","unemp_per","geometry"))

alameda<-data_race%>%
        inner_join(data_age,by="GEOID")%>%
        inner_join(data_edu,by="GEOID")%>%
        inner_join(data_hhinc,by="GEOID")%>%
        inner_join(data_unemp,by="GEOID")








