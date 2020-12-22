#### Preamble ####
# Purpose: The purpose of this code is to clean-up the 2015 GSS data obtained 
# from the U of T library. That data is available to U of T students, but it needs 
# to be put into a tidy format before it can be analysed. This code does that.
# The main issue is that the data are released with codes for variables, whereas,
# we want the variable. e.g. sex is 1 or 2, but we want sex is female or male. (This
# sounds trite in that case, but gets more difficult with more involved variables.)
# So we create a dictionary type dataset that has the variable names and their 
# possible values. In that we embed some R code that will do a replacement. We 
# then apply that dataset to the raw dataset. Finally we do all the usual cleaning.
# to the dataset. You will end up with a dataset called gss.csv.
# Authors: Rohan Alexander and Sam Caetano
# Contact: rohan.alexander@utoronto.ca
# Date: 7 October 2020
# License: MIT
# Pre-reqs: You need to have downloaded the data from the library. To do that: 
## 1. Go to: http://www.chass.utoronto.ca/
## 2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
## 3. Click SDA @ CHASS, should redirect to sign in. Sign in.
## 4. Continue in English (you're welcome to use the French, but we probably can't
## help you too much).
## 5. Crtl F GSS, click
## 6. Click "Data" on the one you want. We used 2015, but you may want a different 
## wave. In particular the General Social Survey on social identity (cycle 27), 
## 2013 has some variables on voter participation if you're into that sort of 
## thing. You're welcome to pick any year but this code applies to 2015.
## 7. Click download
## 8. Select CSV data file, data definitions for STATA (gross, but stick with it for now).
## 9. Can select all variables by clicking button next to green colored "All". Then continue.
## 10. Create the files, download and save
# Check: 
## You WILL need to change the raw data name. Search for .csv - line 41
## You may need to adjust the filepaths depending on your system. Search for: read_


#### Workspace set-up ####
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("AAgXjREA.csv")
dict <- read_lines("time_use_data_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("time_use_data_labels.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))

# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
time_use_data <- raw_data %>% 
  select(CASEID, 
         agegr10,
         hsdsizec,
         sex,
         prv,
         chh0014c,
         cxr0014c,
         gtu_110,
         gtu_130,
         sleepdur,
         persdur,
         pdwkdur,
         mealsdur,
         hswkdur,
         dur13,
         dur14,
         dur15,
         dur16,
         dur27,
         dur29,
         dur31,
         socprdur,
         soctcdur,
         sprtsdur,
         actldur,
         tvdur,
         dur61,
         dur62,
         tcs_120,
         tcs_160,
         slm_01,
         srh_110,
         srh_115,
         srs_10,
         esc1_01,
         incg1,
         dur50,
         dur51,
         durl300,
         durl305,
         chlddur,
         adltdur
  )

# Fix the names
time_use_data <- time_use_data %>% 
  clean_names() %>% 
  rename(age_group = agegr10,
         sex = sex,
         province = prv,
         num_children = chh0014c,
         hh_size = hsdsizec,
         work_hrs = pdwkdur,
         chores_dur = hswkdur,
         self_rated_health = srh_110,
         self_rated_mental_health = srh_115,
         life_satisfaction = slm_01,
         self_rated_stress = srs_10,
         income_respondent = incg1,
         feel_rushed = gtu_110,
         extra_time = gtu_130,
         sleepdur = sleepdur,
         personal_care = persdur,
         mealsdur = mealsdur,
         school_on_site = dur13,
         school_online = dur14,
         studydur = dur15,
         self_development = dur16,
         soc_in_person = socprdur,
         soc_tech = soctcdur,
         exercise = sprtsdur,
         tvdur = tvdur,
         musicdur = dur61,
         use_tech = dur62,
         workaholic = tcs_120,
         pressure = tcs_160,
         current_student = esc1_01,
         outdoor_sports = dur50,
         active_leisure = actldur,
         dur_at_home = durl300,
         dur_outside = durl305,
         children_care_dur = chlddur,
         adult_care_dur = adltdur
  ) 

#### Clean up ####
time_use_data <- time_use_data %>% 
  mutate_at(vars(age_group:current_student), 
            .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"
                                |.=="Not stated"|.=="Don't know", "NA", .))) 

time_use_data <- time_use_data %>% 
  filter(!(age_group %in% c(96,97,98,99))) %>%
  filter(!(sex %in% c(6,7,8,9))) %>%
  mutate(sex = ifelse(sex== 1, "M", "F")) %>%
  mutate(age_group = case_when(age_group == 1 ~ "15 to 24 years",
                               age_group == 2 ~ "25 to 34 years",
                               age_group == 3 ~ "35 to 44 years",
                               age_group == 4 ~ "45 to 54 years",
                               age_group == 5 ~ "55 to 64 years",
                               age_group == 6 ~ "65 to 74 years",
                               age_group == 7 ~ "75 years and older")) %>%
  mutate(province = case_when(province == 10 ~ "NL",
                              province == 11 ~ "PE",
                              province == 12 ~ "NS",
                              province == 13 ~ "NB",
                              province == 24 ~ "QC",
                              province == 35 ~ "ON",
                              province == 46 ~ "MB",
                              province == 47 ~ "SK",
                              province == 48 ~ "AB",
                              province == 59 ~ "BC",
                              province %in% c(96,97,98,99) ~ as.character(NA))) %>% 
  filter(!(hh_size %in% c(96, 97, 98, 99))) %>% 
  filter(!(work_hrs %in% c(9996,9997,9998,9999))) %>%
  mutate(self_rated_health = case_when(self_rated_health == 1 ~ 5,
                                       self_rated_health == 2 ~ 4,
                                       self_rated_health == 3 ~ 3,
                                       self_rated_health == 4 ~ 2,
                                       self_rated_health == 5 ~ 1,
                                       self_rated_health %in% c(6,7,8,9) ~ as.numeric(NA))) %>%
  mutate(self_rated_mental_health = case_when(self_rated_mental_health == 1 ~ 5,
                                              self_rated_mental_health == 2 ~ 4,
                                              self_rated_mental_health == 3 ~ 3,
                                              self_rated_mental_health == 4 ~ 2,
                                              self_rated_mental_health == 5 ~ 1,
                                              self_rated_mental_health %in% c(6,7,8,9) ~ as.numeric(NA))) %>%
  filter(!(life_satisfaction %in% c(96,97,98,99))) %>%
  mutate(self_rated_stress = case_when(self_rated_stress == 1 ~ 1,
                                       self_rated_stress == 2 ~ 2,
                                       self_rated_stress == 3 ~ 3,
                                       self_rated_stress == 4 ~ 4,
                                       self_rated_stress == 5 ~ 5,
                                       self_rated_stress %in% c(6,7,8,9) ~ as.numeric(NA))) %>%
  filter(!(sleepdur %in% c(9996,9997,9998,9999))) %>%
  filter(!(soc_in_person %in% c(9996,9997,9998,9999))) %>%
  filter(!(soc_tech %in% c(9996,9997,9998,9999))) %>%
  filter(!(exercise %in% c(9996,9997,9998,9999))) %>%
  filter(!(extra_time %in% c(96,97,98,99))) %>%
  filter(!(tvdur %in% c(9996,9997,9998,9999))) %>%
  filter(!(personal_care %in% c(9996,9997,9998,9999))) %>%
  filter(!(adult_care_dur %in% c(9996,9997,9998,9999))) %>%
  filter(!(children_care_dur %in% c(9996,9997,9998,9999))) %>%
  filter(!(dur_outside %in% c(9996,9997,9998,9999))) %>%
  filter(!(dur_at_home %in% c(9996,9997,9998,9999))) %>%
  filter(!(active_leisure %in% c(9996,9997,9998,9999))) %>%
  filter(!(outdoor_sports %in% c(9996,9997,9998,9999))) %>%
  filter(!(chores_dur %in% c(9996,9997,9998,9999))) %>%
  filter(!(workaholic %in% c(6,7,8,9))) %>%
  mutate(is_workaholic = ifelse(workaholic == 1, 1, 0))

socializing_data <- time_use_data %>%
  mutate(prefer_in_person = ifelse(soc_in_person>soc_tech,1,0)) %>%
  filter(!is.na(self_rated_health) & !is.na(self_rated_mental_health) & !is.na(self_rated_stress)) 
socializing_data <- socializing_data %>%
  mutate(id = c(1:nrow(socializing_data))) %>%
  select(id, age_group, sex, num_children, prefer_in_person, work_hrs, 
         outdoor_sports, chores_dur, sleepdur, dur_at_home, outdoor_sports, self_development, self_rated_mental_health)

write_csv(socializing_data, "time_use_survey.csv")
