# File: buoy_2016.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 22Jan2018                             #
# Purpose: to create a L1 dataset from the raw 2016   #
#         Auburn buoy data                            #

source('libraries_lists_functions.R')

#point to directories
figdir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/programs/Auburn_buoy/2016 cleaning graphs/'
datadir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/'
dumpdir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/'

L0_2016 <- read_csv(file.path(datadir, "Lake_Auburn -SDL500R-2016.csv"), 
                    col_names = varnames2016) %>% 
  mutate(datetime = as.POSIXct(strptime(datetime, "%m/%d/%Y %I:%M:%S %p", tz='Etc/GMT+4'))) %>% 
  filter(datetime>=as.POSIXct('2016-01-01', tz='Etc/GMT+4')) %>% 
  rename(datetime_instrument = datetime)
#warnings okay

#do a quick check of number of obs per hour to see if DST is in play
dst_check <- L0_2016 %>% 
  mutate(date = format(datetime_instrument, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(n_obs = length(datetime_instrument))
view(dst_check)
#DST in 2016: 03/13/16; 11/06/16 look at those dates

#look at battery
ggplot(L0_2016, aes(x = datetime_instrument, y = bat_v)) +
  geom_point()

#battery is fine
L1_2016 <- L0_2016 %>% 
  select(-bat_v)


#reshape and plot to see NA values
L0_2016_vert <- L1_2016 %>%
  pivot_longer(names_to = 'sensor', 
               values_to = 'value', 
               -datetime_instrument) %>%
  mutate(depth = as.numeric(''),
         sensor_type = as.character(''),
         sensor_class = as.character(''),
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
                                  TRUE ~ 'do'))

# L0_2016_vert %>%
#   ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Raw Buoy data 2016') +
#   final_theme

#recode NA values as NA
L1_2016 <- L1_2016 %>% 
  mutate_at(vars(dotemp_C_1m:temp_C_30m),
            funs(case_when(. <= -99999.99 ~ NA_real_, 
                           TRUE ~ .))) 

L1_2016_vert <- L1_2016 %>%
  pivot_longer(names_to = 'sensor', 
               values_to = 'value', 
               -datetime_instrument) %>%
  mutate(depth = as.numeric(''),
         sensor_type = as.character(''),
         sensor_class = as.character(''),
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
                                  TRUE ~ 'do'))

# L1_2016_vert %>%
#   ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Buoy data 2016 - NAs recoded') +
#   final_theme

####L1 data cleaning####
# THERMISTERS ----
buoy_therm_vert_L1 <- L1_2016 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
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
    labs(title = paste0('2016-', monthlist[i], ' thermister data - NAs recoded'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2016-', monthlist[i], '_L0p5_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}


 
#recode 0.5m thermister until 5/26 to remove noise
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2016-05-26', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2016-05-27', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2016 ', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  coord_cartesian(ylim=c(0,25)) +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
may26 <- as.POSIXct('2016-05-26 8:50', tz='Etc/GMT+4') 

L1_2016 <- L1_2016 %>% 
  mutate(temp_C_0.5m = case_when(datetime_instrument<may26 ~ NA_real_,
                           TRUE ~ temp_C_0.5m))
buoy_therm_vert_L1 <- L1_2016 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#May 12 buoy deployment
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2016-05-12', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2016-05-13', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2016 buoy deployment', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
deployment = as.POSIXct('2016-05-12 10:50', tz = 'Etc/GMT+4')

L1_2016 <- L1_2016 %>% 
  mutate_at(vars(all_of(therm), all_of(do)),
                 ~(case_when(datetime_instrument<deployment ~ NA_real_,
                                TRUE ~ .))) %>% 
  filter(datetime_instrument>=deployment)
            
buoy_therm_vert_L1 <- L1_2016 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))
            

#June 23 buoy visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2016-06-23', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2016-06-24', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'June 2016 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
jun23 = as.POSIXct('2016-06-23 10:40', tz='Etc/GMT+4')

L1_2016 <- L1_2016 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument== jun23~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2016 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))


#July 21 buoy visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2016-07-21', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2016-07-22', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2016 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
jul21 = as.POSIXct('2016-07-21 11:10', tz='Etc/GMT+4')
L1_2016 <- L1_2016 %>% 
  mutate_at(vars(all_of(therm),all_of(do32)),
            ~(case_when(datetime_instrument>= jul21  &
                             datetime_instrument<jul21+minutes(80)~ NA_real_,
                           TRUE ~ .))) 
buoy_therm_vert_L1 <- L1_2016 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#sept 8 buoy viit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2016-09-08', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2016-09-09', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Sept 2016 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

sep8 = as.POSIXct('2016-09-08 10:40', tz='Etc/GMT+4')

#sept 22 buoy visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2016-09-22', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2016-09-23', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Sept 2016 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
sep22 = as.POSIXct('2016-09-22 8:50', tz='Etc/GMT+4') 

L1_2016 <- L1_2016 %>% 
  mutate_at(vars(all_of(therm),all_of(do32)),
            ~(case_when(datetime_instrument>=sep8  &
                             datetime_instrument<sep8 + minutes(30)~ NA_real_,
                           datetime_instrument>=sep22 &
                             datetime_instrument<sep22 + minutes(20)~ NA_real_,
                           TRUE ~ .))) 
buoy_therm_vert_L1 <- L1_2016 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#buoy removal nov 17
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2016-11-17', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2016-11-18', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2016 buoy removal', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

#print monthly data
buoy_therm_vert_L1 <- buoy_therm_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))

for (i in 1: length(monthlist)){
  df = buoy_therm_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=variable)) +
    geom_point() +
    labs(title = paste0('2016-', monthlist[i], ' thermister data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2016-', monthlist[i], '_L1_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_therm_vert_L1)

#DO ----
buoy_do_vert_L1 <- L1_2016 %>% 
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
  mutate(month = format(datetime_instrument, '%m'))

for (i in 1: length(monthlist)){
  df = buoy_do_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
    geom_point() +
    facet_grid(sensor ~ ., scales='free_y') +
    final_theme +
    scale_color_colorblind()
  labs(title = paste0('2016-', monthlist[i], ' do data - NAs recoded')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2016-', monthlist[i], '_L0p5_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

#buoy deployed May 12
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2016-05-12', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2016-05-13', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2016 buoy deployed') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

# check may 26
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2016-05-26', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2016-05-27', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2016') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
may26do = as.POSIXct('2016-05-26 8:40', tz='Etc/GMT+4')

L1_2016 <- L1_2016 %>% 
  mutate_at(vars(dotemp_C_1m, dotemp_C_14.5m),
            ~(case_when(datetime_instrument<deployment + minutes(20) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_1m, do_ppm_14.5m, do_ppm_32m, do_sat_pct_1m, do_sat_pct_14.5m, do_sat_pct_32m),
            ~(case_when(datetime_instrument<deployment + hours(4) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, dotemp_C_1m, 
                 do_ppm_14.5m, do_sat_pct_14.5m, dotemp_C_14.5m),
            ~(case_when(datetime_instrument>= may26do&
                             datetime_instrument< may26do + hours(2) + minutes(20)~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2016 %>% 
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

#Jul 21 recalibration
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2016-07-21', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2016-07-22', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2016 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jul21do = as.POSIXct('2016-07-21 11:30', tz='Etc/GMT+4')
L1_2016 <- L1_2016 %>% 
  mutate_at(vars(dotemp_C_1m, dotemp_C_14.5m),
            ~case_when(datetime_instrument>=jul21do &
                         datetime_instrument <jul21do + hours(1)~NA_real_,
                       TRUE ~ .)) %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, 
                 do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>= jul21do&
                             datetime_instrument<jul21do+hours(4) + minutes(40) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime_instrument>=jul21 &
                             datetime_instrument<jul21do + hours(5) ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2016 %>% 
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

#do clean Aug 18
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2016-08-18', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2016-08-19', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2016 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
aug18do = as.POSIXct('2016-08-18 11:20', tz='Etc/GMT+4')
L1_2016 <- L1_2016 %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, 
                 dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
           ~(case_when(datetime_instrument==aug18do ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1 <- L1_2016 %>% 
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
 
#sept 22 do recalibration
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2016-09-22', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2016-09-23', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Sept 2016 do recalibration') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
sep22do = as.POSIXct('2016-09-22 9:10', tz='Etc/GMT+4')
L1_2016 <- L1_2016 %>% 
  mutate_at(vars(dotemp_C_1m, dotemp_C_14.5m),
            ~(case_when(datetime_instrument>=sep22do&
                             datetime_instrument<sep22do + minutes(30) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>=sep22 &
                             datetime_instrument<sep22+hours(4) +minutes(30) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, 
                 do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=sep22do &
                             datetime_instrument<sep22do + hours(4) + minutes(30) ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2016 %>% 
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


#Nov 17 buoy removed
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2016-11-17', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2016-11-18', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Nov 2016 buoy removed') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2016 <- L1_2016 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime_instrument>=removal-minutes(40) ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1 <- L1_2016 %>% 
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
  mutate(month = format(datetime_instrument, '%m'))
for (i in 1: length(monthlist)){
  df = buoy_do_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
    geom_point() +
    facet_grid(sensor ~ ., scales='free_y') +
    final_theme +
    scale_color_colorblind()
  labs(title = paste0('2016-', monthlist[i], ' do data - clean')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2016-', monthlist[i], '_L1_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_do_vert_L1, L0_2016_vert, L1_2016_vert)

# add flags ----
L1_2016 <- L1_2016 %>% 
  mutate(flag_do1 =case_when(datetime_instrument == jul21do ~ 'c',
                             datetime_instrument == sep22do ~ 'c',
                             TRUE ~ ''),
         flag_do14 =case_when(datetime_instrument == jul21do ~ 'c',
                              datetime_instrument == sep22do ~ 'c',
                              TRUE ~ ''),
         flag_do32 =case_when(datetime_instrument == jul21do ~ 'c',
                              datetime_instrument == sep22do ~ 'c',
                              TRUE ~ ''))

# save file ----

L1_2016 %>% 
  mutate(datetime_EST = with_tz(datetime_instrument, tzone = 'Etc/GMT+5')) %>% 
  mutate(datetime_EST = as.character(datetime_EST)) %>%
  select(-datetime_instrument) %>% 
  write_csv(., file.path(dumpdir, 'buoy_L1_2016.csv'))




