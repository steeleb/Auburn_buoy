# File: buoy_2021.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 30Nov2021                             #
# Purpose: to create a L1 dataset from the raw 2021   #
#         Auburn buoy data                            #
# R version: 3.6.1                                    #

# read in libraries, lists and functions
source('libraries_lists_functions.R')

#point to directories
data_dir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/2021/'
fig_dir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/programs/Auburn_buoy_GH/2021 cleaning graphs/'
dump_dir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/'
visit_dir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/visit notes/'

#timezones
buoy_tz = 'Etc/GMT+4' #yes, the file says it's in UTC -5, but it's incorrect and the visit times don't line up
visit_tz = 'America/New_York'

# read in buoy data ----
#data from this year come in 3 parts; there is some overlap.
L0_2021a <- read_csv(file.path(data_dir, 'April-June_AuburnBuoy_2021.csv'),
                    col_names = varnames2021,
                    col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnnnn',
                    skip=3) %>% 
  mutate(datetime = as.POSIXct(datetime, tz=buoy_tz, format='%m-%d-%Y %H:%M:%S')) %>% 
  rename(datetime_instrument = datetime)
head(L0_2021a)
tail(L0_2021a)
eor_2021a =last(L0_2021a$datetime_instrument)
L0_2021b <- read_csv(file.path(data_dir, 'July-Sept2021_AuburnBuoy.csv'),
                     col_names = varnames2021,
                     col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnnnn',
                     skip=3) %>% 
  mutate(datetime = as.POSIXct(datetime, tz=buoy_tz, format='%m-%d-%Y %H:%M:%S')) %>% 
  rename(datetime_instrument = datetime) %>% 
  filter(datetime_instrument > eor_2021a)
head(L0_2021b)
tail(L0_2021b)
eor_2021b =last(L0_2021b$datetime_instrument)
L0_2021c <- read_csv(file.path(data_dir, 'Oct-Nov2021_AuburnBuoy.csv'),
                     col_names = varnames2021,
                     col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnnnn',
                     skip=3) %>% 
  mutate(datetime = as.POSIXct(datetime, tz=buoy_tz, format='%m-%d-%Y %H:%M:%S')) %>% 
  rename(datetime_instrument = datetime) %>% 
  filter(datetime_instrument > eor_2021b)
head(L0_2021c)
tail(L0_2021c)

#join all pieces together
L0_2021 <- full_join(L0_2021a, L0_2021b) %>% 
  full_join(., L0_2021c)

rm(L0_2021a, L0_2021b, L0_2021c)

#do a quick check of number of obs per hour to see if DST is in play
dst_check <- L0_2021 %>% 
  mutate(date = format(datetime_instrument, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(n_obs = length(datetime_instrument))
view(dst_check)
#DST in 2021: 03/14/21; 11/07/21 look at those dates
#looks good

#look at battery
ggplot(L0_2021, aes(x = datetime_instrument, y = bat_v)) +
  geom_point()

#battery is okay!
L1_2021 <- L0_2021 %>% 
  select(-bat_v)

#read in visit data
visit <- read_xlsx(file.path(visit_dir, '2021_visit_summary.xlsx'),
                   sheet = 'BuoyVisits',
                   na = 'NA',
                   col_types = c('date', 'text', 'numeric', 'date', 'date', 'text', 'numeric', 'text', 'text')) %>% 
  filter(site == 8)

#drop conductivity probe visit info; not part of the buoy
visit <- visit %>% 
  filter(!grepl('cond', sensor_type))

#round down/up by 10 mins and store datetime 
visit <- visit %>% 
  mutate(time_start = floor_date(time_start, '10 minutes'),
         time_end = case_when(!is.na(time_end) ~ ceiling_date(time_end, '10 minutes'),
                              TRUE ~ time_start + minutes(x = 10)),
         time_start = as.POSIXct(paste(date, format(time_start, '%H:%M'), sep = ' '), tz = visit_tz),
         time_end = as.POSIXct(paste(date, format(time_end, '%H:%M'), sep = ''), tz = visit_tz)) %>% 
  mutate(time_start = with_tz(time_start, tz = buoy_tz),
         time_end = with_tz(time_end, tz = buoy_tz))

#recode with flags
visit <- visit %>% 
  mutate(flag = case_when(grepl('clean', maintenance_performed, ignore.case = T) ~ 'w',
                          (grepl('adj', maintenance_performed, ignore.case = T) | grepl('tight', maintenance_performed, ignore.case = T) & sensor_type == 'weather station' ~ 'n'),
                          grepl('pull', maintenance_performed, ignore.case = T) ~ 'R',
                          grepl('deploy', maintenance_performed, ignore.case = T) ~ 'D'))

#### initial cleaning ####

# recode NA values as NA ####
L1_2021 <- L1_2021 %>% 
  mutate_at(vars(do_ppm_1m:temp_C_30m),
            ~ case_when(. < -99999.9 ~ NA_real_, # for -99999.99 and -100000 - had to do it this way because there were issues with -99999.99
                           TRUE ~ .))
str(L1_2021)

#create flag columns
L1_2021 <- L1_2021 %>% 
  mutate(flag_do1 = '',
         flag_do14 = '',
         flag_do32 = '',
         flag_met = '')

#recode values from log and add flags
unique(visit$sensor_depth)

for(j in 1:nrow(visit)){
L1_2021 <- L1_2021 %>% 
  mutate_at(vars(all_of(do1m), all_of(do14m)),
            ~ case_when(visit$sensor_type[j] == 'DO probe' &
                          visit$sensor_depth[j] == 1 &
                          datetime_instrument >= visit$time_start[j] & 
                          datetime_instrument <= visit$time_end[j] ~ NA_real_,
                        TRUE ~ .)) %>% 
  mutate(flag_do1 = case_when(visit$sensor_type[j] == 'DO probe' &
                                  visit$sensor_depth[j] == 1 &
                                  datetime_instrument >= visit$time_start[j] & 
                                  datetime_instrument <= visit$time_end[j] ~ visit$flag[j],
                                TRUE ~ flag_do1)) %>% 
  mutate(flag_met = case_when(visit$sensor_type[j] == 'weather station' &
                                    datetime_instrument >= visit$time_start[j] & 
                                    datetime_instrument <= visit$time_end[j] &
                                    !is.na(visit$flag[j])~ visit$flag[j],
                                  TRUE ~ flag_met))
}

#recode data before D flag, after R flag
deployment <- visit$time_end[visit$flag == 'D' & visit$sensor_type == 'temp probe line']
removal <- visit$time_start[visit$flag == 'R' & visit$sensor_type == 'temp probe line']

L1_2021 <- L1_2021 %>% 
  mutate_at(vars(all_of(therm), all_of(weather), all_of(do)),
            ~ case_when(datetime_instrument >= removal ~ NA_real_,
                        datetime_instrument < deployment ~ NA_real_,
                        TRUE ~ .)) %>% 
  filter(datetime_instrument < removal & datetime_instrument > deployment)

####L1 Cleaning####

#### THERMISTERS ####
buoy_therm_vert_L1 <- L1_2021 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#print monthly data
monthlist = c('04', '05', '06', '07', '08', '09', '10', '11')
buoy_therm_vert_L1 <- buoy_therm_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))

for (i in 1: length(monthlist)){
  df = buoy_therm_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=variable)) +
    geom_point() +
    labs(title = paste0('2021-', monthlist[i], ' thermister data - NAs recoded'), 
                        y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2021-', monthlist[i], '_L0p5_therm.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}

#Apr 21 outlier in 8m
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2021-04-21', tz=buoy_tz) & datetime_instrument<as.POSIXct('2021-04-22', tz=buoy_tz))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2021 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

L1_2021 <- L1_2021 %>% 
  mutate(temp_C_8m = case_when(temp_C_8m > 7.75 &
                                 datetime_instrument>=as.POSIXct('2021-04-21', tz=buoy_tz) & 
                                 datetime_instrument<as.POSIXct('2021-04-22', tz=buoy_tz) ~ NA_real_,
                               TRUE ~ temp_C_8m))

buoy_therm_vert_L1 <- L1_2021 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2021-04-21', tz=buoy_tz) & datetime_instrument<as.POSIXct('2021-04-22', tz=buoy_tz))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2021 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#May 23 mixing - super windy in afternoon; this is real

#print monthly L1 figs
buoy_therm_vert_L1 <- buoy_therm_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))
for (i in 1: length(monthlist)){
  df = buoy_therm_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=variable)) +
    geom_point() +
    labs(title = paste0('2021-', monthlist[i], ' thermister data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2021-', monthlist[i], '_L1_therm.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}

rm(buoy_therm_vert_L1)

#### do ####
buoy_do_vert_L1 <- L1_2021 %>% 
  select(datetime_instrument, all_of(do)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime_instrument) %>% 
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
    labs(title = paste0('2021-', monthlist[i], ' do data - initial recoding'))+
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2021-', monthlist[i], '_L0p5_do.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}


#may 19 outlier in 1m
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2021-05-19', tz=buoy_tz) & 
                                         datetime_instrument<as.POSIXct('2021-05-20', tz=buoy_tz))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2021') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

may19 <- as.POSIXct('2021-05-19 8:20', tz = buoy_tz)

L1_2021 <- L1_2021 %>% 
  mutate_at(vars(all_of(do1m), all_of(do14m)),
            ~ case_when(datetime_instrument == may19 ~ NA_real_,
                        TRUE ~ .))
buoy_do_vert_L1 <- L1_2021 %>% 
  select(datetime_instrument, all_of(do)) %>% 
  pivot_longer(names_to = 'variable', values_to = 'value', -datetime_instrument) %>% 
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
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2021-05-19', tz=buoy_tz) & 
                                         datetime_instrument<as.POSIXct('2021-05-20', tz=buoy_tz))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2021') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

#plot monthly
buoy_do_vert_L1 <- buoy_do_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))
for(i in 1:length(monthlist)){
  df = buoy_do_vert_L1 %>% 
    filter(month == monthlist[i])
  plot = ggplot(df, aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
    geom_point() +
    facet_grid(sensor ~ ., scales='free_y') +
    labs(title = paste0('2021-', monthlist[i], ' do data - clean'))+
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2021-', monthlist[i], '_L1_do.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}

rm(buoy_do_vert_L1)

# Weather ####

#convert daily rain to rain in each 10mins
L1_2021$rain_cm = NA_real_

L1_2021 <- L1_2021 %>% 
  rownames_to_column()

L1_2021_rain <- L1_2021 %>% 
  select(rowname, datetime_instrument, daily_cum_rain_cm, rain_cm) %>% 
  filter(!is.na(daily_cum_rain_cm))
daylist <- unique(format(L1_2021_rain$datetime_instrument, '%Y-%m-%d'))

for (z in 1:length(daylist)) {
  df <- L1_2021_rain %>% 
    filter(format(L1_2021_rain$datetime_instrument, '%Y-%m-%d')==daylist[z]) 
  if(length(na.omit(df$daily_cum_rain_cm))>0){
    for (x in 1:nrow(df)){
      if (x == 1) {
        row_value = df$rowname[x]
        L1_2021_rain$rain_cm[L1_2021_rain$rowname == row_value] = df$daily_cum_rain_cm[x]
      } else {
        rain = df$daily_cum_rain_cm[x] - df$daily_cum_rain_cm[x-1]
        row_value = df$rowname[x]
        L1_2021_rain$rain_cm[L1_2021_rain$rowname == row_value] = rain
      }
    }
  } else {
    print(daylist[z])
    message('instrument offline') #if this prints, there is an issue, since all na were removed and daylist made from narm dataframe
  }
}

#look at the rain values for negatives
view(L1_2021_rain)

#all the negative rains are internal resets, recode to 0 (same as cum rain for that 10 min period)
L1_2021_rain <- L1_2021_rain %>% 
  mutate(rain_cm = case_when(rain_cm <0 ~ 0,
                             TRUE ~ rain_cm))

#select only rowname and rain_cm and join with L1 dataset
L1_2021_rain <- L1_2021_rain %>% 
  select(rowname, rain_cm)
L1_2021 <- L1_2021 %>% 
  select(-rain_cm) %>% 
  full_join(., L1_2021_rain) %>% 
  select(-rowname)

#reorient and look at monthly graphs
buoy_weather_vert_L1 <- L1_2021 %>% 
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
    labs(title = paste0('2021-', monthlist[i], ' met data - NAs recoded')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') 
  print(plot)
  filename = paste0('2021-', monthlist[i], '_L0p5_met.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}

# wind malfunctioning through early 2021-04-21
look_date <- as.Date('2021-04-21')
buoy_weather_vert_L1 %>% 
  filter(datetime_instrument >= look_date - days(1) & 
           datetime_instrument < look_date + days(1)) %>% 
  ggplot(., aes(x=datetime_instrument, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') 

apr21 <- as.POSIXct('2021-04-21 1:20', tz = buoy_tz)

L1_2021 <- L1_2021 %>% 
  mutate(flag_wind = case_when(datetime_instrument <= apr21 & !is.na(wind_sp_mps) ~ 'e',
                               TRUE ~ '')) %>% 
  mutate_at(vars(all_of(wind)),
            ~ case_when(datetime_instrument <= apr21 ~ NA_real_,
                        TRUE ~ .))

#reorient and look at monthly graphs
buoy_weather_vert_L1 <- L1_2021 %>% 
  select(datetime_instrument, all_of(weather_proc)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -datetime_instrument) %>% 
  mutate(month = format(datetime_instrument, '%m'))

buoy_weather_vert_L1 %>% 
  filter(datetime_instrument >= look_date - days(1) & 
           datetime_instrument < look_date + days(1)) %>% 
  ggplot(., aes(x=datetime_instrument, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') 

# wind malfunctioning 2021-05-05
look_date <- as.Date('2021-05-05')

buoy_weather_vert_L1 %>% 
  filter(datetime_instrument >= look_date & 
           datetime_instrument < look_date + days(1)) %>% 
  ggplot(., aes(x=datetime_instrument, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') 

L1_2021 <- L1_2021 %>% 
  mutate(flag_wind = case_when(datetime_instrument >= look_date & 
                                 datetime_instrument < look_date + days(1) &
                                 (wind_dir_deg == 0 | ave_wind_sp_mps == 0) ~ 'e',
                               TRUE ~ flag_wind)) %>% 
  mutate_at(vars(all_of(wind)),
            ~ case_when(datetime_instrument >= look_date & 
                          datetime_instrument < look_date + days(1) &
                          (wind_dir_deg == 0 | ave_wind_sp_mps == 0) ~ NA_real_,
                        TRUE ~ .))

#reorient and look at monthly graphs
buoy_weather_vert_L1 <- L1_2021 %>% 
  select(datetime_instrument, all_of(weather_proc)) %>% 
  pivot_longer(names_to = 'variable',
               values_to = 'value',
               -datetime_instrument) %>% 
  mutate(month = format(datetime_instrument, '%m'))

buoy_weather_vert_L1 %>% 
  filter(datetime_instrument >= look_date  & 
           datetime_instrument < look_date + days(1)) %>% 
  ggplot(., aes(x=datetime_instrument, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') 

for (i in 1: length(monthlist)){
  df = buoy_weather_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = paste0('2021-', monthlist[i], ' met data - initial QAQC')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') 
  print(plot)
  filename = paste0('2021-', monthlist[i], '_L1_met.png')
  ggsave(file.path(fig_dir, filename), device = 'png')
}


# save file -----
colnames(L1_2021)

#look at flags
unique(L1_2021$flag_do1)
unique(L1_2021$flag_do14)
unique(L1_2021$flag_do32)
unique(L1_2021$flag_met)
unique(L1_2021$flag_wind)

#combine the met and wind flags
L1_2021 <- L1_2021 %>% 
  mutate(flag_met = case_when(flag_met == '' ~ flag_wind,
                       flag_met != '' & flag_wind != '' ~ paste(flag_met, flag_wind, sep = ', '),
                       TRUE ~ flag_met)) %>% 
  select(-flag_wind)

L1_2021 %>% 
  mutate(datetime_EST = with_tz(datetime_instrument, tzone = 'Etc/GMT+5')) %>% 
  mutate(datetime_EST = as.character(datetime_EST)) %>% 
  select(-datetime_instrument) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Auburn buoy/data/L1 data/buoy_L1_2021.csv')
