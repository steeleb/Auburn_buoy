# R version 4.1.3
# R Studio version 2022.02.2

source('libraries_lists_functions.R')

# point to directories
datadir  = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/'
dumpdir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/'
#collate all L1 buoy data 

#read in L1 data

## 2013 ----
buoy2013 <- read.csv(file.path(datadir, 'buoy_L1_2013.csv'),
                     col.names = c('waterTemperature_DO_degC_1m', 
                                   'oxygenDissolvedPercentOfSaturation_pct_1m',
                                   'oxygenDissolved_mgl_1m',
                                   'waterTemperature_DO_degC_14p5m', 
                                   'oxygenDissolvedPercentOfSaturation_pct_14p5m',
                                   'oxygenDissolved_mgl_14p5m',
                                   'waterTemperature_DO_degC_32m', 
                                   'oxygenDissolvedPercentOfSaturation_pct_32m',
                                   'oxygenDissolved_mgl_32m',
                                   'waterTemperature_degC_0p5m',
                                   'waterTemperature_degC_1m',
                                   'waterTemperature_degC_2m',
                                   'waterTemperature_degC_4m',
                                   'waterTemperature_degC_6m',
                                   'waterTemperature_degC_8m',
                                   'waterTemperature_degC_10m',
                                   'waterTemperature_degC_12m',
                                   'waterTemperature_degC_16m',
                                   'waterTemperature_degC_22m',
                                   'waterTemperature_degC_30m',
                                   'flag_do1',
                                   'flag_do14p5',
                                   'flag_do32',
                                   'datetime_EST')) %>% 
  mutate(datetime_EST = as.POSIXct(datetime_EST, tz = 'Etc/GMT+5')) %>% 
  select(datetime_EST, 
         waterTemperature_DO_degC_1m:oxygenDissolved_mgl_1m, flag_do1,
         waterTemperature_DO_degC_14p5m:oxygenDissolved_mgl_14p5m, flag_do14p5,
         waterTemperature_DO_degC_32m:oxygenDissolved_mgl_32m, flag_do32,
         waterTemperature_degC_0p5m:waterTemperature_degC_30m)
str(buoy2013)
write.csv(buoy2013, file.path(datadir, 'buoy_L1_CV_underwater_2013_v2022-06-01.csv'), row.names = F)

## 2014 ----
buoy2014 <-  read.csv(file.path(datadir, 'buoy_L1_2014.csv'),
                      col.names = c('waterTemperature_DO_degC_1m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_1m',
                                    'oxygenDissolved_mgl_1m',
                                    'waterTemperature_DO_degC_14p5m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_14p5m',
                                    'oxygenDissolved_mgl_14p5m',
                                    'waterTemperature_DO_degC_32m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_32m',
                                    'oxygenDissolved_mgl_32m',
                                    'waterTemperature_degC_0p5m',
                                    'waterTemperature_degC_1m',
                                    'waterTemperature_degC_2m',
                                    'waterTemperature_degC_4m',
                                    'waterTemperature_degC_6m',
                                    'waterTemperature_degC_8m',
                                    'waterTemperature_degC_10m',
                                    'waterTemperature_degC_12m',
                                    'waterTemperature_degC_16m',
                                    'waterTemperature_degC_22m',
                                    'waterTemperature_degC_30m',
                                    'flag_temp',
                                    'flag_do1',
                                    'flag_do14p5',
                                    'flag_do32',
                                    'datetime_EST')) %>% 
  mutate(datetime_EST = as.POSIXct(datetime_EST, tz = 'Etc/GMT+5')) %>% 
  select(datetime_EST, 
         waterTemperature_DO_degC_1m:oxygenDissolved_mgl_1m, flag_do1,
         waterTemperature_DO_degC_14p5m:oxygenDissolved_mgl_14p5m, flag_do14p5,
         waterTemperature_DO_degC_32m:oxygenDissolved_mgl_32m, flag_do32,
         waterTemperature_degC_0p5m:waterTemperature_degC_30m, flag_temp)
str(buoy2014)
write.csv(buoy2014, file.path(datadir, 'buoy_L1_CV_underwater_2014_v2022-06-01.csv'), row.names = F)

## 2015 ----
buoy2015 <-  read.csv(file.path(datadir, 'buoy_L1_2015.csv'),
                      col.names = c('waterTemperature_DO_degC_1m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_1m',
                                    'oxygenDissolved_mgl_1m',
                                    'waterTemperature_DO_degC_14p5m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_14p5m',
                                    'oxygenDissolved_mgl_14p5m',
                                    'waterTemperature_DO_degC_32m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_32m',
                                    'oxygenDissolved_mgl_32m',
                                    'waterTemperature_degC_0p5m',
                                    'waterTemperature_degC_1m',
                                    'waterTemperature_degC_2m',
                                    'waterTemperature_degC_4m',
                                    'waterTemperature_degC_6m',
                                    'waterTemperature_degC_8m',
                                    'waterTemperature_degC_10m',
                                    'waterTemperature_degC_12m',
                                    'waterTemperature_degC_16m',
                                    'waterTemperature_degC_22m',
                                    'waterTemperature_degC_30m',
                                    'flag_do1',
                                    'flag_do14p5',
                                    'flag_do32',
                                    'datetime_EST')) %>% 
  mutate(datetime_EST = as.POSIXct(datetime_EST, tz = 'Etc/GMT+5')) %>% 
  select(datetime_EST, 
         waterTemperature_DO_degC_1m:oxygenDissolved_mgl_1m, flag_do1,
         waterTemperature_DO_degC_14p5m:oxygenDissolved_mgl_14p5m, flag_do14p5,
         waterTemperature_DO_degC_32m:oxygenDissolved_mgl_32m, flag_do32,
         waterTemperature_degC_0p5m:waterTemperature_degC_30m)
str(buoy2015)
write.csv(buoy2015, file.path(datadir, 'buoy_L1_CV_underwater_2015_v2022-06-01.csv'), row.names = F)

## 2016 ----
buoy2016 <-  read.csv(file.path(datadir, 'buoy_L1_2016.csv'),
                      col.names = c('waterTemperature_DO_degC_1m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_1m',
                                    'oxygenDissolved_mgl_1m',
                                    'waterTemperature_DO_degC_14p5m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_14p5m',
                                    'oxygenDissolved_mgl_14p5m',
                                    'waterTemperature_DO_degC_32m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_32m',
                                    'oxygenDissolved_mgl_32m',
                                    'waterTemperature_degC_0p5m',
                                    'waterTemperature_degC_1m',
                                    'waterTemperature_degC_2m',
                                    'waterTemperature_degC_4m',
                                    'waterTemperature_degC_6m',
                                    'waterTemperature_degC_8m',
                                    'waterTemperature_degC_10m',
                                    'waterTemperature_degC_12m',
                                    'waterTemperature_degC_16m',
                                    'waterTemperature_degC_22m',
                                    'waterTemperature_degC_30m',
                                    'flag_do1',
                                    'flag_do14p5',
                                    'flag_do32',
                                    'datetime_EST')) %>% 
  mutate(datetime_EST = as.POSIXct(datetime_EST, tz = 'Etc/GMT+5')) %>% 
  select(datetime_EST, 
         waterTemperature_DO_degC_1m:oxygenDissolved_mgl_1m, flag_do1,
         waterTemperature_DO_degC_14p5m:oxygenDissolved_mgl_14p5m, flag_do14p5,
         waterTemperature_DO_degC_32m:oxygenDissolved_mgl_32m, flag_do32,
         waterTemperature_degC_0p5m:waterTemperature_degC_30m)
str(buoy2016)
write.csv(buoy2016, file.path(datadir, 'buoy_L1_CV_underwater_2016_v2022-06-01.csv'), row.names = F)

## 2017 ----
buoy2017 <-  read.csv(file.path(datadir, 'buoy_L1_2017.csv'),
                      col.names = c('oxygenDissolved_mgl_1m',
                                    'waterTemperature_DO_degC_1m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_1m',
                                    'waterTemperature_DO_degC_14p5m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_14p5m',
                                    'oxygenDissolved_mgl_14p5m',
                                    'oxygenDissolved_mgl_32m',
                                    'waterTemperature_DO_degC_32m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_32m',
                                    'waterTemperature_degC_0p5m',
                                    'waterTemperature_degC_1m',
                                    'waterTemperature_degC_2m',
                                    'waterTemperature_degC_4m',
                                    'waterTemperature_degC_6m',
                                    'waterTemperature_degC_8m',
                                    'waterTemperature_degC_10m',
                                    'waterTemperature_degC_12m',
                                    'waterTemperature_degC_16m',
                                    'waterTemperature_degC_22m',
                                    'waterTemperature_degC_30m',
                                    'flag_do1',
                                    'flag_do14p5',
                                    'flag_do32',
                                    'datetime_EST')) %>% 
  mutate(datetime_EST = as.POSIXct(datetime_EST, tz = 'Etc/GMT+5')) %>% 
  select(datetime_EST, 
         waterTemperature_DO_degC_1m,oxygenDissolvedPercentOfSaturation_pct_1m, oxygenDissolved_mgl_1m, flag_do1,
         waterTemperature_DO_degC_14p5m, oxygenDissolvedPercentOfSaturation_pct_14p5m, oxygenDissolved_mgl_14p5m, flag_do14p5,
         waterTemperature_DO_degC_32m, oxygenDissolvedPercentOfSaturation_pct_32m, oxygenDissolved_mgl_32m, flag_do32,
         waterTemperature_degC_0p5m:waterTemperature_degC_30m)
str(buoy2017)
write.csv(buoy2017, file.path(datadir, 'buoy_L1_CV_underwater_2017_v2022-06-01.csv'), row.names = F)

## 2018 ----
buoy2018 <-  read.csv(file.path(datadir, 'buoy_L1_2018.csv'),
                      col.names = c('oxygenDissolved_mgl_1m',
                                    'waterTemperature_DO_degC_1m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_1m',
                                    'waterTemperature_DO_degC_14p5m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_14p5m',
                                    'oxygenDissolved_mgl_14p5m',
                                    'oxygenDissolved_mgl_32m',
                                    'waterTemperature_DO_degC_32m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_32m',
                                    'waterTemperature_degC_0p5m',
                                    'waterTemperature_degC_1m',
                                    'waterTemperature_degC_2m',
                                    'waterTemperature_degC_4m',
                                    'waterTemperature_degC_6m',
                                    'waterTemperature_degC_8m',
                                    'waterTemperature_degC_10m',
                                    'waterTemperature_degC_12m',
                                    'waterTemperature_degC_16m',
                                    'waterTemperature_degC_22m',
                                    'waterTemperature_degC_30m',
                                    'flag_do1',
                                    'flag_do14p5',
                                    'flag_do32',
                                    'datetime_EST')) %>% 
  mutate(datetime_EST = as.POSIXct(datetime_EST, tz = 'Etc/GMT+5')) %>% 
  select(datetime_EST, 
         waterTemperature_DO_degC_1m,oxygenDissolvedPercentOfSaturation_pct_1m, oxygenDissolved_mgl_1m, flag_do1,
         waterTemperature_DO_degC_14p5m, oxygenDissolvedPercentOfSaturation_pct_14p5m, oxygenDissolved_mgl_14p5m, flag_do14p5,
         waterTemperature_DO_degC_32m, oxygenDissolvedPercentOfSaturation_pct_32m, oxygenDissolved_mgl_32m, flag_do32,
         waterTemperature_degC_0p5m:waterTemperature_degC_30m)
str(buoy2018)
write.csv(buoy2018, file.path(datadir, 'buoy_L1_CV_underwater_2018_v2022-06-01.csv'), row.names = F)

## 2019 ----
buoy2019 <-  read.csv(file.path(datadir, 'buoy_L1_2019.csv'),
                      col.names = c('oxygenDissolved_mgl_1m',
                                    'waterTemperature_DO_degC_1m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_1m',
                                    'waterTemperature_DO_degC_14p5m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_14p5m',
                                    'oxygenDissolved_mgl_14p5m',
                                    'oxygenDissolved_mgl_32m',
                                    'waterTemperature_DO_degC_32m', 
                                    'oxygenDissolvedPercentOfSaturation_pct_32m',
                                    'waterTemperature_degC_0p5m',
                                    'waterTemperature_degC_1m',
                                    'waterTemperature_degC_2m',
                                    'waterTemperature_degC_4m',
                                    'waterTemperature_degC_6m',
                                    'waterTemperature_degC_8m',
                                    'waterTemperature_degC_10m',
                                    'waterTemperature_degC_12m',
                                    'waterTemperature_degC_16m',
                                    'waterTemperature_degC_22m',
                                    'waterTemperature_degC_30m',
                                    'flag_do1',
                                    'flag_do14p5',
                                    'flag_do32',
                                    'flag_temp',
                                    'datetime_EST')) %>% 
  mutate(datetime_EST = as.POSIXct(datetime_EST, tz = 'Etc/GMT+5')) %>% 
  select(datetime_EST, 
         waterTemperature_DO_degC_1m,oxygenDissolvedPercentOfSaturation_pct_1m, oxygenDissolved_mgl_1m, flag_do1,
         waterTemperature_DO_degC_14p5m, oxygenDissolvedPercentOfSaturation_pct_14p5m, oxygenDissolved_mgl_14p5m, flag_do14p5,
         waterTemperature_DO_degC_32m, oxygenDissolvedPercentOfSaturation_pct_32m, oxygenDissolved_mgl_32m, flag_do32,
         waterTemperature_degC_0p5m:waterTemperature_degC_30m, flag_temp)
str(buoy2019)
write.csv(buoy2019, file.path(datadir, 'buoy_L1_CV_underwater_2019_v2022-06-01.csv'), row.names = F)

## 2020 ----
buoy2020 <- read.csv(file.path(datadir, 'buoy_L1_2020.csv'),
                     col.names = c('waterTemperature_DO_degC_1m', 
                                   'oxygenDissolvedPercentOfSaturation_pct_1m',
                                   'oxygenDissolved_mgl_1m',
                                   'waterTemperature_DO_degC_14p5m', 
                                   'oxygenDissolvedPercentOfSaturation_pct_14p5m',
                                   'oxygenDissolved_mgl_14p5m',
                                   'waterTemperature_DO_degC_32m', 
                                   'oxygenDissolvedPercentOfSaturation_pct_32m',
                                   'oxygenDissolved_mgl_32m',
                                   'waterTemperature_degC_0p5m',
                                   'waterTemperature_degC_1m',
                                   'waterTemperature_degC_2m',
                                   'waterTemperature_degC_4m',
                                   'waterTemperature_degC_6m',
                                   'waterTemperature_degC_8m',
                                   'waterTemperature_degC_10m',
                                   'waterTemperature_degC_12m',
                                   'waterTemperature_degC_16m',
                                   'waterTemperature_degC_22m',
                                   'waterTemperature_degC_30m',
                                   'windDirectionInstantaneous_deg',
                                   'windSpeedInstantaneous_mps',
                                   'windSpeedAverage_mps',
                                   'airTemperature_degC',
                                   'relativeHumidity_perc',
                                   'barometricPressure_mmHg',
                                   'precipitation_dailyCumulative_cm',
                                   'flag_do1',
                                   'flag_do14p5',
                                   'flag_do32',
                                   'precipition_cm',
                                   'datetime_EST')) %>% 
  mutate(datetime_EST = as.POSIXct(datetime_EST, tz = 'Etc/GMT+5')) 

#separate above and below water sensors
buoy2020_uw <- buoy2020 %>% 
  select(datetime_EST, 
         waterTemperature_DO_degC_1m:oxygenDissolved_mgl_1m, flag_do1,
         waterTemperature_DO_degC_14p5m:oxygenDissolved_mgl_14p5m, flag_do14p5,
         waterTemperature_DO_degC_32m:oxygenDissolved_mgl_32m, flag_do32,
         waterTemperature_degC_0p5m:waterTemperature_degC_30m)
str(buoy2020_uw)
write.csv(buoy2020_uw, file.path(datadir, 'buoy_L1_CV_underwater_2020_v2022-06-01.csv'), row.names = F)
buoy2020_met <- buoy2020 %>% 
  select(datetime_EST, 
         windDirectionInstantaneous_deg:barometricPressure_mmHg, precipition_cm)
str(buoy2020_met)
write.csv(buoy2020_met, file.path(datadir, 'buoy_L1_CV_met_2020_v2022-06-01.csv'), row.names = F)

## 2017 ----
buoy2021 <- read.csv(file.path(datadir, 'buoy_L1_2021.csv'),
                     col.names = c('oxygenDissolved_mgl_1m',
                                   'waterTemperature_DO_degC_1m', 
                                   'oxygenDissolvedPercentOfSaturation_pct_1m',
                                   'waterTemperature_DO_degC_14p5m', 
                                   'oxygenDissolvedPercentOfSaturation_pct_14p5m',
                                   'oxygenDissolved_mgl_14p5m',
                                   'oxygenDissolved_mgl_32m',
                                   'waterTemperature_DO_degC_32m', 
                                   'oxygenDissolvedPercentOfSaturation_pct_32m',
                                   'waterTemperature_degC_0p5m',
                                   'waterTemperature_degC_1m',
                                   'waterTemperature_degC_2m',
                                   'waterTemperature_degC_4m',
                                   'waterTemperature_degC_6m',
                                   'waterTemperature_degC_8m',
                                   'waterTemperature_degC_10m',
                                   'waterTemperature_degC_12m',
                                   'waterTemperature_degC_16m',
                                   'waterTemperature_degC_22m',
                                   'waterTemperature_degC_30m',
                                   'windDirectionInstantaneous_deg',
                                   'windSpeedInstantaneous_mps',
                                   'windSpeedAverage_mps',
                                   'airTemperature_degC',
                                   'relativeHumidity_perc',
                                   'barometricPressure_mmHg',
                                   'precipitation_dailyCumulative_cm',
                                   'flag_do1',
                                   'flag_do14p5',
                                   'flag_do32',
                                   'flag_met',
                                   'precipition_cm',
                                   'datetime_EST')) %>% 
  mutate(datetime_EST = as.POSIXct(datetime_EST, tz = 'Etc/GMT+5')) 

#separate above and below water sensors
buoy2021_uw <- buoy2021 %>% 
  select(datetime_EST, 
         waterTemperature_DO_degC_1m,oxygenDissolvedPercentOfSaturation_pct_1m, oxygenDissolved_mgl_1m, flag_do1,
         waterTemperature_DO_degC_14p5m, oxygenDissolvedPercentOfSaturation_pct_14p5m, oxygenDissolved_mgl_14p5m, flag_do14p5,
         waterTemperature_DO_degC_32m, oxygenDissolvedPercentOfSaturation_pct_32m, oxygenDissolved_mgl_32m, flag_do32,
         waterTemperature_degC_0p5m:waterTemperature_degC_30m)
str(buoy2021_uw)
write.csv(buoy2021_uw, file.path(datadir, 'buoy_L1_CV_underwater_2021_v2022-06-01.csv'), row.names = F)
buoy2021_met <- buoy2021 %>% 
  select(datetime_EST, 
         windDirectionInstantaneous_deg:barometricPressure_mmHg, precipition_cm,
         flag_met)
str(buoy2021_met)
write.csv(buoy2021_met, file.path(datadir, 'buoy_L1_CV_met_2021_v2022-06-01.csv'), row.names = F)
