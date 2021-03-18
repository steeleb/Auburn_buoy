# R version 3.6.3
# R Studio version 1.2.5042

source('libraries_lists_functions.R')

#collate all L1 buoy data 

#read in L1 data

buoy2013 <- read_csv('C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2013.csv',
                     col_types = 'Tnnnnnnnnnnnnnnnnnnnnn')
str(buoy2013)

buoy2014 <- read_csv('C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2014.csv',
                     col_types = 'Tnnnnnnnnnnnnnnnnnnnnn')
str(buoy2014)

buoy2015 <- read_csv('C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2015.csv',
                     col_types = 'Tnnnnnnnnnnnnnnnnnnnnn')
str(buoy2015)

buoy2016 <- read_csv('C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2016.csv',
                     col_types = 'Tnnnnnnnnnnnnnnnnnnnnn')
str(buoy2016)

buoy2017 <- read_csv('C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2017.csv',
                     col_types = 'Tnnnnnnnnnnnnnnnnnnnnn')
str(buoy2017)

buoy2018 <- read_csv('C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2018.csv',
                     col_types = 'Tnnnnnnnnnnnnnnnnnnnnn')
str(buoy2018)

buoy2019 <- read_csv('C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2019.csv',
                     col_types = 'Tnnnnnnnnnnnnnnnnnnnnncc')
str(buoy2019)


#collate
buoy_1319 <- full_join(buoy2013, buoy2014) %>% 
  full_join(., buoy2015) %>% 
  full_join(., buoy2016) %>% 
  full_join(., buoy2017) %>% 
  full_join(., buoy2018) %>% 
  full_join(., buoy2019) 

buoy_1319 %>% 
  mutate(datetime = as.character(datetime)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2013_2019_v29April2020.csv')
  
#create vertical dataset
buoy_1319_vert <- buoy_1319 %>% 
  select(-bat_v) %>% 
  gather(variable, value, -datetime, -do_1m_flag, -do_14.5m_flag)
unique(buoy_1319_vert$variable)

#recode variables for depth to parameter, depth, value, flag
buoy_1319_vert <- buoy_1319_vert %>% 
  mutate(depth_m = str_extract(variable, '_.*m'),
         depth_m = gsub('_C_', '', depth_m),
         depth_m = gsub('_sat_pct_', '', depth_m),
         depth_m = gsub('_ppm_', '', depth_m),
         depth_m = gsub('m', '', depth_m),
         depth_m = as.numeric(depth_m),
         unit = str_extract(variable, '_.*_'),
         unit = gsub('_', ' ', unit),
         parameter = str_extract(variable, '.*?_'),
         parameter = gsub('_', '', parameter))

unique(buoy_1319_vert$unit)
unique(buoy_1319_vert$parameter)

#add sensor column
buoy_1319_vert <- buoy_1319_vert %>% 
  mutate(sensor_class = case_when(grepl('do', parameter) ~ 'DO',
                                  TRUE ~ 'thermistor'),
         parameter = case_when(grepl('temp', parameter) ~ 'waterTemperature',
                               parameter == 'do' ~ 'dissolvedOxygen',
                               TRUE ~ parameter),
         unit = case_when(grepl('C', unit) ~ 'degreesCelsius',
                          grepl('sat', unit) ~ 'percentSaturation',
                          grepl('ppm', unit) ~ 'milligramsPerLiter',
                          TRUE ~ unit))

unique(buoy_1319_vert$unit)
unique(buoy_1319_vert$parameter)
unique(buoy_1319_vert$sensor_class)

#deal with flags
colnames(buoy_1319_vert)

buoy_1319_vert <- buoy_1319_vert %>% 
  mutate(flag = case_when(depth_m == 1 & sensor_class == 'DO' & !is.na(do_1m_flag) ~ do_1m_flag,
                          depth_m == 14.5 & sensor_class == 'DO' & !is.na(do_14.5m_flag) ~ do_14.5m_flag,
                          TRUE ~ '')) %>% 
  select(-do_1m_flag, -do_14.5m_flag) 

#write RMNA and incNA datasets
buoy_1319_vert %>% 
  mutate(datetime = as.character(datetime)) %>% 
  select(datetime, parameter, depth_m, value, unit, flag, sensor_class) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2013_2019_vert_v29April2020.csv')

buoy_1319_vert %>% 
  mutate(datetime = as.character(datetime)) %>% 
  select(datetime, parameter, depth_m, value, unit, flag, sensor_class) %>%
  filter(!is.na(value)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2013_2019_vert_rmna_v29April2020.csv')
