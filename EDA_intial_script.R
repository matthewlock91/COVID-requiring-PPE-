library(MASS)
library(lubridate)
library(corrplot)
library(modelr)
#library(leaflet)
library(viridis)
library(tidyverse)
library(wesanderson)

# Aim ---------------------------------------------------------------------

#Task question: Which populations of clinicians and patients require protective equipment?
  
  
# Data Import -------------------------------------------------------------

## For the user: Specify folders to include (only add folders after those in vector)##

folders <- c('our_world_in_data',
             'einstein',
             'esri_covid-19/esri_covid-19')


file_path_start <- '../uncover/UNCOVER/'


for (i in 1:length(folders)) {

  if(i == 1) {
    tbl_list_names <- vector(mode = 'list', length = 0)
    }
  
file_path_folder <- paste0(file_path_start,folders[i],'/')

tbl_list_names_i <-
  list.files(path = file_path_folder, pattern = "*.csv") %>% 
  str_c(file_path_folder,.)

tbl_list_names <- append(tbl_list_names, tbl_list_names_i)
  
}

tbl_list <-
  tbl_list_names %>% 
  map(~read_csv(.))

#ONS UK data

deaths_UK_tbl <- read_csv('../deaths_by_trust_UK_20042020.csv')
clinical_staff_UK_tbl <- read_csv('../clinical_staff_by_trust_UK_Dec19.csv')

# EDA ---------------------------------------------------------------------

tbl_list_names

map(tbl_list[[4]],function(x) sum(is.na(x)))

#our_world_tbl
our_world_tbl <- tbl_list[[1]]

our_world_tbl %>%
  group_by(entity) %>% 
  mutate(total_cases_last_date = ifelse(date == max(date), total_confirmed_cases_of_covid_19_per_million_people_cases_per_million, 0)
         , max_cases = max(total_cases_last_date)
         ) %>% 
  ungroup() %>% 
  mutate(
    rank_cases = dense_rank(desc(max_cases))
  ) %>% 
  filter(rank_cases < 4 | entity %in% c('United Kingdom', 'Italy', 'Spain', 'South Korea', 'United States')) %>% 
  ggplot(aes(x = date
             , y = total_confirmed_cases_of_covid_19_per_million_people_cases_per_million
             , colour = fct_reorder2(entity, date, total_confirmed_cases_of_covid_19_per_million_people_cases_per_million)
             )) +
  geom_line() +
  guides(colour=guide_legend(title = 'Country')) + 
  scale_x_date(date_breaks = '2 weeks') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

 # labs(title = 'Placeholder', caption = 'data: our_world_in_data')

  # geom_bar(aes(y = total_covid_19_tests_per_million_people / 5, fill = entity)
  #          , stat = 'identity'
  #          , position = 'dodge'
  #          , alpha = 0.7
  #          ) +
  # scale_y_continuous(
  #   name = expression("total_confirmed_cases_of_covid_19_per_million_people_cases_per_million")
  #   , sec.axis = sec_axis(~ . * 5 , name = "total_covid_19_tests_per_million_people")
  # ) +
  #scale_y_log10()

our_world_total_cases_tbl <- tbl_list[[2]]

our_world_total_cases_tbl %>%
  group_by(entity) %>% 
  mutate(total_cases_last_date = ifelse(date == max(date), total_confirmed_cases_of_covid_19_cases, 0)
         , max_cases = max(total_cases_last_date)
  ) %>% 
  ungroup() %>% 
  filter(entity != 'World') %>% 
  mutate(
    rank_cases = dense_rank(desc(max_cases))
  ) %>% 
  filter(rank_cases < 7 | entity == 'United Kingdom') %>% 
  ggplot(aes(x = date
             , y = total_confirmed_cases_of_covid_19_cases
             , colour = fct_reorder2(entity, date, total_confirmed_cases_of_covid_19_cases)
  )) +
  geom_line() +
  guides(colour=guide_legend(title = 'Country'))

#einstein: diagnosis-of-covid-19-and-its-clinical-spectrum

einstein_clinical_tbl <- tbl_list[[5]]

glimpse(einstein_clinical_tbl)

#NA rows
NAs <- map_dbl(einstein_clinical_tbl, function(x) sum(is.na(x)) / nrow(einstein_clinical_tbl))
as_tibble(as.list(NAs)) %>% 
  pivot_longer(cols = everything(), values_to = 'NA_percentage')

einstein_clinical_tbl %>% 
  group_by(patient_age_quantile, patient_addmited_to_intensive_care_unit_1_yes_0_no ) %>% 
  mutate( total = n(),
          isCOVID = !(sars_cov_2_exam_result == 'negative'), #every patient has exam result
          prop_COVID = isCOVID / total) %>% 
  ggplot(aes(patient_age_quantile, patient_addmited_to_intensive_care_unit_1_yes_0_no, fill = prop_COVID )) +
  geom_tile() +
  scale_fill_viridis()

einstein_clinical_tbl %>% 
  group_by(patient_age_quantile, patient_addmited_to_intensive_care_unit_1_yes_0_no ) %>% 
  mutate( total = n(),
          isCOVID = !(sars_cov_2_exam_result == 'negative'), #every patient has exam result
          prop_COVID = isCOVID / total) %>% 
  ggplot(aes(patient_age_quantile, prop_COVID, fill = patient_addmited_to_intensive_care_unit_1_yes_0_no)) +
  geom_bar(stat = 'identity')

einstein_clinical_tbl %>% 
  ggplot(aes(sars_cov_2_exam_result))+
  geom_bar(aes(fill = patient_addmited_to_intensive_care_unit_1_yes_0_no) )


column_NAs <- map_dbl(einstein_clinical_tbl, function(x) sum(is.na(x)) / nrow(einstein_clinical_tbl))

names(column_NAs[column_NAs<0.90])

tbl <- einstein_clinical_tbl %>% 
  select(names(column_NAs[column_NAs<0.90])) %>% 
  filter(complete.cases(.))

glimpse(tbl)


#full  model
mod_data <- tbl %>% 
  mutate(y = !(sars_cov_2_exam_result == 'negative')) %>% 
  dplyr::select(c(2,4:20,25,'y')) 

mod_data %>% 
  mutate(y_num = as.numeric(y)) %>%
  select_if(., is.numeric) %>% 
  pairs()


mod_data %>% 
  mutate(y_num = as.numeric(y)) %>%
  select_if(., is.numeric) %>% 
  cor() %>% 
  corrplot(.,type = "upper", 
tl.col = "black", tl.srt = 45)

mod <- glm(y ~ ., data = mod_data[-c(2:4)], family = 'binomial')

#summary(mod)

#stepwise regression model
step_mod <- stepAIC(mod, direction = "both", 
                      trace = FALSE)

summary(step_mod)

#correlation between leukocytes and eosinophils
mod_data %>% 
  ggplot(aes(x = leukocytes, y = eosinophils)) +
  geom_point() +
  geom_smooth()

#leukocytes

tbl %>% 
  mutate(y = !(sars_cov_2_exam_result == 'negative')) %>% 
  ggplot(aes(x = leukocytes, y = y)) +
  geom_point()

avg_prop_COVID <- einstein_clinical_tbl %>% 
  summarise(sum(!(sars_cov_2_exam_result == 'negative')) / n()) %>% 
  unlist()

einstein_clinical_tbl %>% 
  mutate(y = !(sars_cov_2_exam_result == 'negative')
         , leukocytes_cut = cut(leukocytes, 10)
         ) %>% 
  group_by(leukocytes_cut) %>% 
  summarise(prop_COVID = mean(y)
            , leukocytes_count = n()
            ) %>% 
  mutate(row_num = row_number()) %>% 
  ggplot(aes(x = leukocytes_cut, y = prop_COVID)) +
  geom_bar(stat = 'identity', aes(fill = !is.na(leukocytes_cut))) +
  geom_ref_line(h = avg_prop_COVID, size = 1, colour = 'red') +
  theme(legend.position = 'none') +
  scale_fill_manual(values = c('orange', 'blue')) +
  geom_label(aes(x = leukocytes_cut, y = prop_COVID + 0.03, label = str_c('n = ', leukocytes_count))) +
  geom_text(aes(x = max(row_num + 1) / 2 , y = avg_prop_COVID + 0.02, label = 'Average proportion tested positive'), colour = 'red') +
  labs(title = 'Patients with lower leukocytes values have a higher chance of testing positive for COVID-19'
       , caption = 'Data from einstein/diagnosis-of-covid-19-and-its-clinical-spectrum.csv'
       , x = 'Leukocytes (binned into ranges)'
       , y = 'Proportion tested positive')


einstein_clinical_tbl %>% 
  mutate(y = !(sars_cov_2_exam_result == 'negative')
         , leukocytes_cut = cut(leukocytes, 7)
         , basophils_cut = cut(basophils, 7)
  ) %>% 
  group_by(leukocytes_cut, basophils_cut) %>% 
  summarise(prop_COVID = mean(y)
            , count = n()
  ) %>% 
  ggplot(aes(x = leukocytes_cut, y = basophils_cut, fill = prop_COVID)) +
  geom_tile() +
  scale_fill_viridis() +
  geom_label(aes(label = str_c(round(prop_COVID,2),'\n (n = ',count,')'))
             , fill = 'white')

einstein_clinical_tbl %>% 
  mutate(y = !(sars_cov_2_exam_result == 'negative')
         , leukocytes_cut = cut(leukocytes, 6)
         , patient_age_quantile_cut = cut(patient_age_quantile, 6)
  ) %>% 
  group_by(leukocytes_cut, patient_age_quantile_cut) %>% 
  summarise(prop_COVID = mean(y)
            , count = n()
  ) %>% 
  ggplot(aes(x = leukocytes_cut, y = patient_age_quantile_cut, fill = prop_COVID)) +
  geom_tile() +
  scale_fill_viridis() +
  geom_label(aes(label = str_c(round(prop_COVID,2),'\n (n = ',count,')'))
             , fill = 'white'
             , label.size = 0.1)


#types of leukocytes
#lymphocytes, basophils, eosinophils

# tbl_list[[1]] %>% 
#   ggplot(aes(region, critical_care_beds))+
#   geom_bar(stat = 'identity')


# long_lat <- tbl_list[[1]] %>% 
#   select('lat','long') %>% 
#   unique()
# 
# leaflet() %>% 
#   setView(lat = as.numeric(unlist(long_lat[,1][1])), lng = as.numeric(unlist(long_lat[,2][1])), zoom = 5) %>% 
#   addTiles() %>% 
#   addMarkers(lat = as.numeric(unlist(long_lat[,2][1])), lng = as.numeric(unlist(long_lat[,2][1])))

# Looking at 

hospital_beds_US_tbl <- tbl_list[[10]]
colnames(hospital_beds_US_tbl)


# date_cols <- names(deaths_UK_tbl[,str_detect(names(deaths_UK_tbl), "\\d{2}-...-\\d{2}")])
# deaths_UK_tbl %>% 
#   pivot_longer(cols = date_cols, names_to = 'date', values_to = 'deaths')

clinical_staff_UK_tbl2 <- clinical_staff_UK_tbl %>% 
  select(`X2`, 'Organisation', 'Professionally qualified clinical staff') %>% 
  mutate(
    Organisation = str_to_upper(Organisation),
    `Professionally qualified clinical staff` = as.double(str_replace(`Professionally qualified clinical staff`,',',''))
  ) %>% 
  rename(clinical_staff = `Professionally qualified clinical staff`)


#deaths but no hospital staff..[ISSUE]
deaths_UK_tbl %>% 
  anti_join(., clinical_staff_UK_tbl2, by = c('Name' = 'Organisation')) %>% 
  select(Name, Total) %>% 
  arrange(desc(Total)) %>% 
  print(n = Inf)

deaths_UK_tbl2 <- deaths_UK_tbl %>% 
  mutate(
    Name = str_to_upper(Name),
    Name = str_replace_all(Name, 'THE ', '')
  ) %>% 
  select(c(1:4,'Total')) %>% 
  rename(deaths = Total,
         NHS_England_Region = `NHS England Region`) 
  
deaths_UK_tbl2 %>% 
  anti_join(., clinical_staff_UK_tbl2, by = c('Name' = 'Organisation')) %>% 
  select(Name, deaths) %>% 
  arrange(desc(deaths)) %>% 
  print(n = Inf)

clinical_staff_UK_tbl2 %>% 
  anti_join(., deaths_UK_tbl2, by = c('Organisation' = 'Name')) %>% 
  arrange(desc(clinical_staff)) %>% 
  filter(!is.na(Organisation)) %>% 
  print(n = Inf)
  

deaths_UK_tbl2 %>% 
  inner_join(., clinical_staff_UK_tbl2, by = c('Name' = 'Organisation')) %>% 
  mutate(
    COVID_deaths_per_clinical_staff = deaths / clinical_staff
  ) %>% 
  arrange(desc(COVID_deaths_per_clinical_staff)) %>% 
  top_n(n = 20) %>% 
  ggplot(aes(fct_reorder(Name, COVID_deaths_per_clinical_staff), COVID_deaths_per_clinical_staff, fill = COVID_deaths_per_clinical_staff)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_bw() +
  scale_fill_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous"))

deaths_UK_tbl2 %>% 
  inner_join(., clinical_staff_UK_tbl2, by = c('Name' = 'Organisation')) %>%
  group_by(NHS_England_Region) %>% 
  summarise(COVID_deaths_per_clinical_staff = sum(deaths) / sum(clinical_staff)) %>% 
  ggplot(aes(fct_reorder(NHS_England_Region, desc(COVID_deaths_per_clinical_staff))
             , COVID_deaths_per_clinical_staff
             , fill = COVID_deaths_per_clinical_staff)) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  scale_fill_gradientn(colours = wes_palette("Zissou1", 100, type = "continuous")) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)
      #, text = element_text(size = 30)
      , plot.caption = element_text(hjust = 1, vjust = -0.5)
      , legend.position = 'none') +
  labs(title =  'Average deaths per staff by region'
       , subtitle = 'London is the most at risk UK region'
       , y = 'COVID deaths per clinical staff'
       , x = 'NHS Trust Region'
       , caption = 'Data source: NHS (digital.nhs.uk & england.nhs.uk)')


##Going back to log reg model - leukocytes and age quanitile
einstein_clinical_tbl %>% 
  group_by(sars_cov_2_exam_result, patient_age_quantile, leukocytes) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(leukocytes, patient_age_quantile)) +
  geom_point(aes(colour = sars_cov_2_exam_result, size = count)) +
  geom_point(data = filter(einstein_clinical_tbl, leukocytes > 3, patient_age_quantile < 5, sars_cov_2_exam_result == 'positive')
             , pch=1
             , fill=NA
             , size=5
             , colour="orange"
             , stroke=1) +
  scale_colour_manual(values = c(negative = "blue", positive = "red"))



  # filter(einstein_clinical_tbl, leukocytes > 3, patient_age_quantile < 5, sars_cov_2_exam_result == 'positive') %>% 
  # View()
  