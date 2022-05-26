# File: buoy_2020.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 30Nov2021                             #
# Purpose: to create a L1 dataset from the raw 2020   #
#         Auburn buoy data                            #
# R version: 3.6.1                                    #

# read in libraries, lists and functions
source('libraries_lists_functions.R')

#point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/2020/'
fig_dir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/programs/Auburn_buoy_GH/2020 cleaning graphs/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/'
visit_dir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/visit notes/2020/'

L0_2020 <- read_csv(file.path(data_dir, 'iChart_YSR_200428.csv'),
                    col_names = varnames2016,
                    col_types = 'cnnnnnnnnnnnnnnnnnnnnn',
                    skip=4) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='Etc/GMT+4', format='%m/%d/%Y %H:%M')) %>% 
  rename(datetime_instrument = datetime)
str(L0_2020)

L02020_met <- read_csv(file.path(data_dir,'may-nov2020_buoyweather.csv'),
                       col_names = met2020,
                       col_types = 'cnnnnnnnn',
                       skip = 4) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='Etc/GMT+4', format='%m-%d-%Y %H:%M')) %>%
  rename(datetime_instrument = datetime) %>% 
  select(-bat_v)

  
#do a quick check of number of obs per hour to see if DST is in play
dst_check <- L0_2020 %>% 
  mutate(date = format(datetime_instrument, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(n_obs = length(datetime_instrument))
view(dst_check)
#DST in 2020: 03/08/20; 11/01/20 look at those dates
dst_check <- L02020_met %>% 
  mutate(date = format(datetime_instrument, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(n_obs = length(datetime_instrument))

#join the buoy and met fils
L0_2020 <- full_join(L0_2020, L02020_met)

#read in visit data
visit <- read_xlsx(file.path(visit_dir, 'visit_deployment_history.xlsx'),
                   sheet = 'visits_from_samplinglog',
                   col_types = c('date', 'text', 'numeric', 'date', 'date', 'text', 'numeric', 'text', 'text')) %>% 
  filter(site == 8)

#round down/up by 10 mins
visit <- visit %>% 
  mutate(time_start = floor_date(time_start, '10 minutes'),
         time_end = case_when(!is.na(time_end) ~ ceiling_date(time_end, '10 minutes'),
                              TRUE ~ time_start + minutes(x = 10)),
         time_start = as.POSIXct(paste(date, format(time_start, '%H:%M'), sep = ' '), tz = 'Etc/GMT+4'),
         time_end = as.POSIXct(paste(date, format(time_end, '%H:%M'), sep = ''), tz = 'Etc/GMT+4')) 

#add flag info
visit <- visit %>% 
  mutate(maintenance_performed = case_when(is.na(maintenance_performed) & sensor_type == 'DO' ~ 'clean',
                                           TRUE ~ maintenance_performed))

#### initial cleaning ####

# recode NA values as NA ####
L1_2020 <- L0_2020 %>% 
  mutate_at(vars(dotemp_C_1m:temp_C_30m),
            ~ case_when(. < -99999.9 ~ NA_real_, # for -99999.99 and -100000 - had to do it this way because there were issues with -99999.99
                           TRUE ~ .))
str(L1_2020)

#check battery
ggplot(L1_2020, aes(x = datetime_instrument, y = bat_v)) +
  geom_point()

# recode values when battery below 8v
L1_2020 <- L1_2020 %>% 
  mutate_at(vars(dotemp_C_1m:temp_C_30m),
            ~ case_when(bat_v < 8 ~ NA_real_, 
                        TRUE ~ .)) 

#recode values from log
for(j in 1:nrow(visit)){
L1_2020 <- L1_2020 %>% 
  mutate_at(vars(dotemp_C_1m:temp_C_30m),
            ~ case_when(datetime_instrument >= visit$time_start[j] & datetime_instrument <= visit$time_end[j] ~ NA_real_,
                        TRUE ~ .))
}

L1_2020_vert <- L1_2020 %>%
  pivot_longer(names_to = 'sensor', 
               values_to = 'value', 
               -c(datetime_instrument, bat_v)) %>%
  mutate(depth = as.numeric(''),
         sensor_type = as.character(''),
         sensor_class = as.character(''),
         sensor_unit = as.character(''),
         depth = case_when(sensor == 'do_ppm_1m' ~ 1,
                           sensor == 'dotemp_C_1m' ~ 1,
                           sensor == 'do_sat_pct_1m' ~ 1,
                           sensor == 'do_ppm_14.5m' ~ 14.5,
                           sensor == 'dotemp_C_14.5m' ~ 14.5,
                           sensor == 'do_sat_pct_14.5m' ~ 14.5,
                           sensor == 'do_ppm_32m' ~ 32,
                           sensor == 'dotemp_C_32m' ~ 32,
                           sensor == 'do_sat_pct_32m' ~ 32,
                           sensor == 'temp_C_0.5m' ~ 0.5,
                           sensor == 'temp_C_1m' ~ 1,
                           sensor == 'temp_C_2m' ~ 2,
                           sensor == 'temp_C_4m' ~ 4,
                           sensor == 'temp_C_6m' ~ 6,
                           sensor == 'temp_C_8m' ~ 8,
                           sensor == 'temp_C_10m' ~ 10,
                           sensor == 'temp_C_12m' ~ 12,
                           sensor == 'temp_C_16m' ~ 16,
                           sensor == 'temp_C_22m' ~ 22,
                           sensor == 'temp_C_30m' ~ 30,
                           TRUE ~ NA_real_),
         sensor_type = case_when(grepl('do', sensor) ~ 'do probe',
                                 grepl('temp', sensor) ~ 'thermister'),
         sensor_class = case_when(grepl('dotemp', sensor) ~ 'temp',
                                  grepl('temp', sensor) ~ 'temp',
                                  TRUE ~ 'do'),
         sensor_unit = case_when(grepl('dotemp', sensor) ~ 'deg C',
                                 grepl('temp', sensor) ~ 'deg C',
                                 grepl('do_sat', sensor) ~ 'percent',
                                 grepl('do_ppm', sensor) ~ 'ppm',
                                 TRUE ~ sensor))

L1_2020_vert %>%
  ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
  facet_grid(sensor_type ~ ., scales='free_y') +
  geom_point() +
  labs(title = 'Buoy data 2020 - NAs, log recoded') +
  final_theme

#create flag columns
L1_2020 <- L1_2020 %>% 
  mutate(flag_do1 = '',
         flag_do14 = '',
         flag_do32 = '')

####L1 Cleaning####

#### THERMISTERS ----
buoy_therm_vert_L1 <- L1_2020 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#print monthly data
monthlist = c('05', '06', '07', '08', '09', '10', '11')
buoy_therm_vert_L1 <- buoy_therm_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))

for (i in 1: length(monthlist)){
  df = buoy_therm_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=variable)) +
    geom_point() +
    labs(title = paste0('2020-', monthlist[i], ' thermister data - NAs recoded'), 
                        y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2020-', monthlist[i], '_L0p5_therm.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}

#May 05 - buoy deployed
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2020-05-15', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2020-05-16', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2020 buoy deploy', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

deployment_time = as.POSIXct('2020-05-15 10:10', tz='Etc/GMT+4')

L1_2020 <- L1_2020 %>% 
  mutate_at(vars(all_of(therm), all_of(do), all_of(weather)),
            ~ case_when(datetime_instrument<deployment_time ~ NA_real_,
                           TRUE ~ .)) %>% 
  filter(datetime_instrument >= deployment_time) %>% 
  mutate(flag = case_when(datetime_instrument == deployment_time ~ 'D',
                          TRUE ~ ''))

buoy_therm_vert_L1 <- L1_2020 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2020-05-01', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2020-06-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2020, clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#August 4-5 mixing is real, from Tropical Storm Isias

#Nov artifacts from weather station meltdown
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2020-11-01', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2020-11-07', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2020 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#recode termp data from Nov 2 13:30 through Nov 4 AM
L1_2020 <- L1_2020 %>% 
  mutate_at(vars(all_of(therm), all_of(weather), all_of(do)),
            ~ case_when(datetime_instrument>=as.POSIXct('2020-11-02 13:30', tz='Etc/GMT+4') &
                             datetime_instrument < as.POSIXct('2020-11-04 00:00', tz= 'Etc/GMT+4')~ NA_real_,
                           TRUE ~ .))
buoy_therm_vert_L1 <- L1_2020 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#Nov artifacts from weather station meltdown
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2020-11-01', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2020-11-07', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2020 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#buoy removed Nov 22
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2020-11-22', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2020-11-23', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2020 therm', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

retrieval_time = as.POSIXct('2020-11-22 9:00', tz='Etc/GMT+4')

L1_2020 <- L1_2020 %>% 
  mutate_at(vars(all_of(therm), all_of(do), all_of(weather)),
            ~ case_when(datetime_instrument>= retrieval_time ~ NA_real_,
                           TRUE ~ .)) %>% 
  filter(datetime_instrument <= retrieval_time) %>% 
  mutate(flag = case_when(datetime_instrument == retrieval_time ~ 'R',
                          TRUE ~ flag))
  
buoy_therm_vert_L1 <- L1_2020 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2020-11-01', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2020-12-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2020 therm clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#print monthly L1 figs
buoy_therm_vert_L1 <- buoy_therm_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))

for (i in 1: length(monthlist)){
  df = buoy_therm_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=variable)) +
    geom_point() +
    labs(title = paste0('2020-', monthlist[i], ' thermister data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2020-', monthlist[i], '_L1_therm.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}

rm(buoy_therm_vert_L1)

#### do ####
buoy_do_vert_L1 <- L1_2020 %>% 
  select(datetime_instrument, all_of(do)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(do)),
         depth = case_when(variable == 'do_ppm_1m' ~ 1,
                           variable == 'dotemp_C_1m' ~ 1,
                           variable == 'do_sat_pct_1m' ~ 1,
                           variable == 'do_ppm_14.5m' ~ 14.5,
                           variable == 'dotemp_C_14.5m' ~ 14.5,
                           variable == 'do_sat_pct_14.5m' ~ 14.5,
                           variable == 'do_ppm_32m' ~ 32,
                           variable == 'dotemp_C_32m' ~ 32,
                           variable == 'do_sat_pct_32m' ~ 32),
         sensor = case_when(variable == 'do_ppm_1m' ~ 'do_ppm',
                            variable == 'dotemp_C_1m' ~ 'do_temp',
                            variable == 'do_sat_pct_1m' ~ 'do_sat',
                            variable == 'do_ppm_14.5m' ~ 'do_ppm',
                            variable == 'dotemp_C_14.5m' ~ 'do_temp',
                            variable == 'do_sat_pct_14.5m' ~ 'do_sat',
                            variable == 'do_ppm_32m' ~ 'do_ppm',
                            variable == 'dotemp_C_32m' ~ 'do_temp',
                            variable == 'do_sat_pct_32m' ~ 'do_sat'))

#plot monthly
buoy_do_vert_L1 <- buoy_do_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))
for(i in 1:length(monthlist)){
  df = buoy_do_vert_L1 %>% 
    filter(month == monthlist[i])
  plot = ggplot(df, aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
    geom_point() +
    facet_grid(sensor ~ ., scales='free_y') +
    labs(title = paste0('2020-', monthlist[i], ' do data - initial recoding'))+
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2020-', monthlist[i], '_L0p5_do.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}

#may deploy
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2020-05-15', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2020-05-16', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2020 deploy - do data - NAs recoded') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2020 <- L1_2020 %>% 
  mutate_at(vars(do),
            funs(case_when(datetime_instrument<deployment_time+minutes(10) ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2020 %>% 
  select(datetime_instrument, all_of(do)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(do)),
         depth = case_when(variable == 'do_ppm_1m' ~ 1,
                           variable == 'dotemp_C_1m' ~ 1,
                           variable == 'do_sat_pct_1m' ~ 1,
                           variable == 'do_ppm_14.5m' ~ 14.5,
                           variable == 'dotemp_C_14.5m' ~ 14.5,
                           variable == 'do_sat_pct_14.5m' ~ 14.5,
                           variable == 'do_ppm_32m' ~ 32,
                           variable == 'dotemp_C_32m' ~ 32,
                           variable == 'do_sat_pct_32m' ~ 32),
         sensor = case_when(variable == 'do_ppm_1m' ~ 'do_ppm',
                            variable == 'dotemp_C_1m' ~ 'do_temp',
                            variable == 'do_sat_pct_1m' ~ 'do_sat',
                            variable == 'do_ppm_14.5m' ~ 'do_ppm',
                            variable == 'dotemp_C_14.5m' ~ 'do_temp',
                            variable == 'do_sat_pct_14.5m' ~ 'do_sat',
                            variable == 'do_ppm_32m' ~ 'do_ppm',
                            variable == 'dotemp_C_32m' ~ 'do_temp',
                            variable == 'do_sat_pct_32m' ~ 'do_sat'))

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2020-05-15', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2020-05-16', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2020 deploy - do data - NAs recoded') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

#august 5 is isiah
#sept 23 is teddy

#plot monthly
buoy_do_vert_L1 <- buoy_do_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))
for(i in 1:length(monthlist)){
  df = buoy_do_vert_L1 %>% 
    filter(month == monthlist[i])
  plot = ggplot(df, aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
    geom_point() +
    facet_grid(sensor ~ ., scales='free_y') +
    labs(title = paste0('2020-', monthlist[i], ' do data - clean'))+
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2020-', monthlist[i], '_L1_do.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}


#add flags from visit log
visit_flag = visit %>% 
  mutate(flag_do1 = case_when(sensor_type == 'DO' & sensor_depth == 1 & maintenance_performed == 'clean' ~ 'w',
                                sensor_type == 'DO' & sensor_depth == 1 & maintenance_performed == 'calibrate' ~ 'c',
                                TRUE ~ NA_character_),
         flag_do14 = case_when(sensor_type == 'DO' & sensor_depth == 14.5 & maintenance_performed == 'clean' ~ 'w',
                                   sensor_type == 'DO' & sensor_depth == 14.5 & maintenance_performed == 'calibrate' ~ 'c',
                                   TRUE ~ NA_character_),
         flag_do32 = case_when(sensor_type == 'DO' & sensor_depth == 32 & maintenance_performed == 'clean' ~ 'w',
                                 sensor_type == 'DO' & sensor_depth == 32 & maintenance_performed == 'calibrate' ~ 'c',
                                 TRUE ~ NA_character_))

for (f in 1:nrow(visit_flag)){
  L1_2020 <- L1_2020 %>% 
    mutate(flag_do1 = case_when(datetime_instrument >= visit_flag$time_start[f] & 
                                    datetime_instrument <= visit_flag$time_end[f] &
                                    !is.na(visit_flag$flag_do1[f]) ~ visit_flag$flag_do1[f],
                                  TRUE ~ flag_do1),
           flag_do14 = case_when(datetime_instrument >= visit_flag$time_start[f] & 
                                    datetime_instrument <= visit_flag$time_end[f] &
                                    !is.na(visit_flag$flag_do14[f]) ~ visit_flag$flag_do14[f],
                                  TRUE ~ flag_do14),
           flag_do32 = case_when(datetime_instrument >= visit_flag$time_start[f] & 
                                       datetime_instrument <= visit_flag$time_end[f] &
                                       !is.na(visit_flag$flag_do32[f]) ~ visit_flag$flag_do32[f],
                                     TRUE ~ flag_do32))
}

# Weather data ----

#convert daily rain to rain in each 10mins
L1_2020$rain_cm = NA_real_

L1_2020 <- L1_2020 %>% 
  rownames_to_column()

L1_2020_rain <- L1_2020 %>% 
  select(rowname, datetime_instrument, daily_cum_rain_cm, rain_cm) %>% 
  filter(!is.na(daily_cum_rain_cm))
daylist <- unique(format(L1_2020_rain$datetime_instrument, '%Y-%m-%d'))

for (z in 1:length(daylist)) {
  df <- L1_2020_rain %>% 
    filter(format(L1_2020_rain$datetime_instrument, '%Y-%m-%d')==daylist[z]) 
  if(length(na.omit(df$daily_cum_rain_cm))>0){
    for (x in 1:nrow(df)){
      if (x == 1) {
        row_value = df$rowname[x]
        L1_2020_rain$rain_cm[L1_2020_rain$rowname == row_value] = df$daily_cum_rain_cm[x]
      } else {
        rain = df$daily_cum_rain_cm[x] - df$daily_cum_rain_cm[x-1]
        row_value = df$rowname[x]
        L1_2020_rain$rain_cm[L1_2020_rain$rowname == row_value] = rain
      }
    }
  } else {
    print(daylist[z])
    message('instrument offline') #if this prints, there is an issue, since all na were removed and daylist made from narm dataframe
  }
}

#select only rowname and rain_cm and join with L1 dataset
L1_2020_rain <- L1_2020_rain %>% 
  select(rowname, rain_cm)
L1_2020 <- L1_2020 %>% 
  select(-rain_cm) %>% 
  full_join(., L1_2020_rain) %>% 
  select(-rowname)

#reorient and look at monthly graphs
buoy_weather_vert_L1 <- L1_2020 %>% 
  select(datetime_instrument, all_of(weather_proc)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -datetime_instrument) %>% 
  mutate(month = format(datetime_instrument, '%m'))

for (i in 1: length(monthlist)){
  df = buoy_weather_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = paste0('2020-', monthlist[i], ' met data - NAs recoded')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') 
  print(plot)
  filename = paste0('2020-', monthlist[i], '_L0p5_met.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}

#late to mid may rain values ~ 2cm/hr are suspect - recode all to na
L1_2020 <- L1_2020 %>% 
  mutate(rain_cm = case_when(datetime_instrument >= as.Date('2020-05-20') &
                                  datetime_instrument < as.Date('2020-05-25') &
                                  rain_cm > 2 ~ NA_real_,
                                TRUE ~ rain_cm))

buoy_weather_vert_L1 <- L1_2020 %>% 
  select(datetime_instrument, all_of(weather_proc)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -datetime_instrument) %>% 
  mutate(month = format(datetime_instrument, '%m'))


for (i in 1: length(monthlist)){
  df = buoy_weather_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = paste0('2020-', monthlist[i], ' met data - initial QAQC')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') 
  print(plot)
  filename = paste0('2020-', monthlist[i], '_L1_met.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}


# save file ----
colnames(L1_2020)

L1_2020 %>% 
  mutate(datetime_EST = with_tz(datetime_instrument, tzone = 'Etc/GMT+5')) %>% 
  mutate(datetime_EST = as.character(datetime_EST)) %>% 
  select(-datetime_instrument, -bat_v, -flag) %>% 
  write_csv(., file.path(dump_dir, 'buoy_L1_2020.csv'))
