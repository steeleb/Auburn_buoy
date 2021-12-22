#######################################################
# File: buoy_2014.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 22Jan2018                             #
# Purpose: to create a L1 dataset from the raw 2014   #
#         Auburn buoy data                            #
# R version: 3.4.3                                    #
#######################################################

source('libraries_lists_functions.R')

#point to directories
figdir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/programs/Auburn_buoy_GH/2014 cleaning graphs/'
datadir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/'

L0_2014 <- read_csv(file.path(datadir, "Lake_Auburn-SDL500R-11-23-2014_00-33.csv"), 
                    skip=3, 
                    col_names = varnames2014) %>% 
  mutate(datetime = as.POSIXct(strptime(datetime, "%m/%d/%Y %H:%M", tz='Etc/GMT+4'))) %>% 
  rename(datetime_instrument = datetime)
#warnings okay

#do a quick check of number of obs per hour to see if DST is in play
dst_check <- L0_2014 %>% 
  mutate(date = format(datetime_instrument, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(n_obs = length(datetime_instrument))
view(dst_check)
#DST in 2014: 03/09/14; 11/02/14 look at those dates

#look at battery
ggplot(L0_2014, aes(x = datetime_instrument, y = bat_v)) +
  geom_point()

#battery is fine
L1_2014 <- L0_2014 %>% 
  select(-bat_v)

#reshape and plot to see NA values
L1_2014_vert <- L1_2014 %>%
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

L1_2014_vert %>%
  ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
  facet_grid(sensor_type ~ ., scales='free_y') +
  geom_point() +
  labs(title = 'Raw Buoy data 2014') +
  final_theme

#recode NA values as NA
L1_2014 <- L1_2014 %>% 
  mutate_at(vars(temp_C_0.5m:temp_C_30m),
            funs(case_when(. == -100000 ~ NA_real_, 
                           TRUE ~ .))) %>% 
  mutate_at(vars(dotemp_C_1m:do_ppm_32m),
            funs(case_when(. == 9998 ~ NA_real_,
                           TRUE ~ .)))

L1_2014_vert <- L1_2014 %>%
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
L1_2014_vert %>%
  ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
  facet_grid(sensor_type ~ ., scales='free_y') +
  geom_point() +
  labs(title = 'Buoy data 2014 - NAs recoded') +
  final_theme

#THERMISTERS----
buoy_therm_vert_L1 <- L1_2014 %>% 
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
    labs(title = paste0('2014-', monthlist[i], ' thermister data - NAs recoded'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2014-', monthlist[i], '_L0p5_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}


#May 23 buoy deployment
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-05-23', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-05-24', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2014 buoy deployment', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

deployment = as.POSIXct('2014-05-23 10:00', tz='Etc/GMT+4')

L1_2014 <- L1_2014 %>% 
  mutate_at(vars(all_of(therm), all_of(do)),
            ~(case_when(datetime_instrument<deployment +minutes(10) ~ NA_real_,
                           TRUE ~ .))) %>% 
  filter(datetime_instrument >= deployment)

buoy_therm_vert_L1 <- L1_2014 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-05-01', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-06-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2014 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

#flag for intermittent data Jun 8 - June 19
L1_2014 <- L1_2014 %>% 
  mutate(flag_temp = case_when(datetime_instrument >= as.Date('2013-06-08') &
                                 datetime_instrument <= as.Date('2013-06-19') ~ 'i',
                               TRUE ~ ''))

#june 19 visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-06-19', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-06-20', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'June 2014 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
jun19 = as.POSIXct('2014-06-19 9:30', tz='Etc/GMT+4')

L1_2014 <- L1_2014 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>=jun19-hours(1)  &
                             datetime_instrument<jun19+minutes(20)~ NA_real_,
                           TRUE ~ .)))

buoy_therm_vert_L1 <- L1_2014 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-06-01', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-07-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'June 2014 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

#July 30 buoy visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-07-30', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-07-31', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2014 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
jul30 = as.POSIXct('2014-07-30 9:10', tz='Etc/GMT+4')
  
L1_2014 <- L1_2014 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>= jul30 &
                             datetime_instrument< jul30+minutes(30)~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2014 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-07-01', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-08-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2014 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

#Aug 26 buoy visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-08-26', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-08-27', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug 2014 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
aug26 = as.POSIXct('2014-08-26 12:00', tz='Etc/GMT+4')

L1_2014 <- L1_2014 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>= aug26 &
                             datetime_instrument<aug26 +minutes(50)~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2014 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-08-01', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-09-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug 2014 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

#Sept 4 buoy visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-09-04', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-09-05', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Sept 2014 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
sep4 = as.POSIXct('2014-09-04 11:00', tz='Etc/GMT+4')
  
L1_2014 <- L1_2014 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>= sep4 &
                             datetime_instrument<sep4 +minutes(20)~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2014 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-09-01', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-10-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Sept 2014 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

#buoy removal nov 20
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-11-20', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-11-21', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2014 buoy removal', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
removal = as.POSIXct('2014-11-20 8:50', tz='Etc/GMT+4')

L1_2014 <- L1_2014 %>% 
  mutate_at(vars(all_of(therm), all_of(do)),
            ~(case_when(datetime_instrument>= removal~ NA_real_,
                           TRUE ~ .))) %>% 
  filter(datetime_instrument <= removal)
buoy_therm_vert_L1 <- L1_2014 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2014-11-01', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2014-12-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2014 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
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
    labs(title = paste0('2014-', monthlist[i], ' thermister data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2014-', monthlist[i], '_L1_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}


rm(buoy_therm_vert_L1)

# DO ----
buoy_do_vert_L1 <- L1_2014 %>% 
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
                            variable == 'do_sat_pct_32m' ~ 'do_sat')) %>% 
  mutate(month = format(datetime_instrument, '%m'))

#plot monthly
for (i in 1: length(monthlist)){
  df = buoy_do_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
    geom_point() +
    facet_grid(sensor ~ ., scales='free_y') +
    final_theme +
    scale_color_colorblind()
  labs(title = paste0('2014-', monthlist[i], ' do data - NAs recoded')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2014-', monthlist[i], '_L0p5_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

#buoy deployed May 23
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-05-23', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-05-24', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2014 buoy deployed') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2014 <- L1_2014 %>% 
  mutate_at(vars(do_ppm_1m, do_ppm_14.5m, do_ppm_32m, do_sat_pct_1m, do_sat_pct_14.5m, do_sat_pct_32m),
            ~(case_when(datetime_instrument<as.POSIXct('2014-05-23 14:00', tz='Etc/GMT+4') ~ NA_real_,
                           TRUE ~ .)))

buoy_do_vert_L1 <- L1_2014 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-05-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-06-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2014 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

# Jun 3 do clean
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-06-03', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-06-04', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2014 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun3do = as.POSIXct('2014-06-03 9:20', tz='Etc/GMT+4')

#jun 19 do recalibration
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-06-19', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-06-20', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2014 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun19do = as.POSIXct('2014-06-19 8:40', tz='Etc/GMT+4')

L1_2014 <- L1_2014 %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, 
                 dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m, 
                 dotemp_C_32m, do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>= jun3do&
                             datetime_instrument<jun3do+minutes(20)~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>=jun3do&
                             datetime_instrument<jun3do + hours(4) +minutes(20) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m, 
                 dotemp_C_32m, do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>= jun19do&
                             datetime_instrument<jun19do+minutes(70) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m, 
                 do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>=jun19do &
                             datetime_instrument<jun19do + hours(5) +minutes(10) ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2014 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-06-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-07-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2014 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#Jul 9 recalibration
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-07-09', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-07-10', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2014 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jul9do = as.POSIXct('2014-07-09 7:20', tz='Etc/GMT+4')
jul9 = as.POSIXct('2014-07-09 08:00', tz='Etc/GMT+4')

#Jul 30 recalibration
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-07-30', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-07-31', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2014 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jul30do = as.POSIXct('2014-07-30 8:40', tz='Etc/GMT+4')

L1_2014 <- L1_2014 %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, 
                 dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>= jul9do&
                             datetime_instrument<jul9do +minutes(40) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(dotemp_C_32m, do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>=jul9 &
                             datetime_instrument<jul9+minutes(30) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, 
                 do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=jul9do&
                             datetime_instrument<jul9do + hours(5) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>=jul9 &
                             datetime_instrument<jul9 + hours(4) +minutes(10) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, 
                 dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=jul30do &
                             datetime_instrument<jul30do +minutes(30) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, 
                 do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=jul30do &
                             datetime_instrument<jul30do + hours(5) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>=as.POSIXct('2014-07-30 9:40', tz='Etc/GMT+4') &
                             datetime_instrument<jul30 + hours(4) + minutes(30) ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2014 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-07-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-08-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2014 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#Aug do recalibration Aug 26
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-08-26', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-08-27', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2014 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
aug26do = as.POSIXct('2014-08-26 12:00', tz='Etc/GMT+4')
  
L1_2014 <- L1_2014 %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, 
                 dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m, 
                 dotemp_C_32m, do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>=aug26do &
                             datetime_instrument<aug26do + hours(1) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m, 
                 do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>=aug26do &
                             datetime_instrument<aug26do + hours(5) ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1 <- L1_2014 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-08-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-09-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2014 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#Sept 04 recalibration
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-09-04', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-09-05', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2014 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
sept4do = as.POSIXct('2014-09-04 10:40', tz='Etc/GMT+4')

L1_2014 <- L1_2014 %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, 
                 dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument >= sept4do&
                             datetime_instrument< sept4do + minutes(40) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m, 
                 do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>=sept4do &
                             datetime_instrument<sept4do + hours(5) ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1 <- L1_2014 %>% 
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

ggplot(subset(buoy_do_vert_L1_b, subset=(datetime_instrument >=as.POSIXct('2014-09-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-10-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Sept 2014 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#Nov 20 buoy removed
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-11-20', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-11-21', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Nov 2014 buoy removed') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()


L1_2014 <- L1_2014 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime_instrument>=removal-minutes(10) ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1 <- L1_2014 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2014-11-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2014-12-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Nov 2014 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

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
  labs(title = paste0('2014-', monthlist[i], ' do data - clean')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2014-', monthlist[i], '_1_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_do_vert_L1, L1_2014_vert)

#add flags ----
L1_2014 <- L1_2014 %>% 
  mutate(flag_do1 =case_when(datetime_instrument == jun19do ~ 'w',
                              datetime_instrument == jul9do  ~ 'c',
                              datetime_instrument == jul30do ~ 'c', 
                              datetime_instrument == aug26do ~ 'c',
                              datetime_instrument == sept4do ~ 'c',
                              TRUE ~ ''),
         flag_do14 =case_when(datetime_instrument == jun19do ~ 'w',
                             datetime_instrument == jul9do  ~ 'c',
                             datetime_instrument == jul30do ~ 'c', 
                             datetime_instrument == aug26do ~ 'c',
                             datetime_instrument == sept4do ~ 'c',
                             TRUE ~ ''),
         flag_do32 =case_when(datetime_instrument == jun19do ~ 'w',
                             datetime_instrument == jul9do  ~ 'c',
                             datetime_instrument == jul30do ~ 'c', 
                             datetime_instrument == aug26do ~ 'c',
                             datetime_instrument == sept4do ~ 'c',
                             TRUE ~ ''))
  
# save file ----
L1_2014 %>% 
  mutate(datetime_EST = with_tz(datetime_instrument, tzone = 'EST')) %>% 
  mutate(datetime_EST = as.character(datetime_EST)) %>% 
  select(-datetime_instrument) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2014.csv')



