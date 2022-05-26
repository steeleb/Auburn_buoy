# File: buoy_2017.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 22Jan2018                             #
# Purpose: to create a L1 dataset from the raw 2017   #
#         Auburn buoy data                            #

source('libraries_lists_functions.R')

#point to directories
figdir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/programs/Auburn_buoy/2017 cleaning graphs/'
datadir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/'
dumpdir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/'

L0_2017 <- read_csv(file.path(datadir, 'Lake_Auburn-SDL500R-11-16-2017_13-10.csv'),
                    col_names = varnames2017,
                    col_types = 'cnnnnnnnnnnnnnnnnnnnnn',
                    skip=3) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='Etc/GMT+4', format='%m/%d/%Y %H:%M')) %>% 
  rename(datetime_instrument = datetime)

#do a quick check of number of obs per hour to see if DST is in play
dst_check <- L0_2017 %>% 
  mutate(date = format(datetime_instrument, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(n_obs = length(datetime_instrument))
view(dst_check)
#DST in 2017: 03/12/17; 11/05/17 look at those dates

#look at battery
ggplot(L0_2017, aes(x = datetime_instrument, y = bat_v)) +
  geom_point()

#battery is fine
L1_2017 <- L0_2017 %>% 
  select(-bat_v)


L0_2017_vert <- L0_2017 %>%
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

# L0_2017_vert %>% 
#   ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Raw Buoy data 2017') +
#   final_theme

#recode NA values as NA
L1_2017 <- L0_2017 %>% 
  mutate_at(vars(do_ppm_1m:temp_C_30m),
            ~(case_when(. < -99999.9 ~ NA_real_, # for -99999.99 and -100000 - had to do it this way because there were issues with -99999.99
                           TRUE ~ .)))

L1_2017_vert <- L1_2017 %>%
  pivot_longer(names_to = 'sensor', 
               values_to = 'value', 
               -datetime_instrument) %>%
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

# L1_2017_vert %>% 
#   ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Buoy data 2017 - NAs recoded') +
#   final_theme


####L1 Cleaning####

#THERMISTERS ----
buoy_therm_vert_L1 <- L1_2017 %>% 
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
    labs(title = paste0('2017-', monthlist[i], ' thermister data - NAs recoded'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2017-', monthlist[i], '_L0p5_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}


#May 24 - buoy deployed
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2017-05-24', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2017-05-25', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2017 buoy deploy', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
deployment = as.POSIXct('2017-05-24 11:20', tz='Etc/GMT+4')

L1_2017 <- L1_2017 %>% 
  mutate_at(vars(all_of(therm), all_of(do)),
            ~(case_when(datetime_instrument<deployment ~ NA_real_,
                           TRUE ~ .))) %>% 
  filter(datetime_instrument >= deployment)
buoy_therm_vert_L1 <- L1_2017 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))


# Jul 14 - therm check and do clean
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2017-07-14', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2017-07-15', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2017 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
jul14 = as.POSIXct('2017-07-14 12:40', tz='Etc/GMT+4')

L1_2017 <- L1_2017 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>= jul14&
                             datetime_instrument<jul14 + hours(1) + minutes(30)~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2017 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#september 14th buoy visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2017-09-14', tz='Etc/GMT+4') & 
                                            datetime_instrument<as.POSIXct('2017-09-15', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'September 2017 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
sep14 = as.POSIXct('2017-09-14 10:10', tz='Etc/GMT+4')

L1_2017 <- L1_2017 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>= sep14&
                          datetime_instrument<sep14 +  minutes(20)~ NA_real_,
                        TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2017 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#buoy removed Nov 16
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2017-11-16', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2017-11-17', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2017 therm', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
removal = as.POSIXct('2017-11-16 7:00', tz='Etc/GMT+4')
L1_2017 <- L1_2017 %>% 
  mutate_at(vars(all_of(therm), all_of(do)),
            ~(case_when(datetime_instrument>removal ~ NA_real_,
                           TRUE ~ .))) %>% 
  filter(datetime_instrument < removal)
buoy_therm_vert_L1 <- L1_2017 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))


#print monthly data
buoy_therm_vert_L1 <- buoy_therm_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))

for (i in 1: length(monthlist)){
  df = buoy_therm_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=variable)) +
    geom_point() +
    labs(title = paste0('2017-', monthlist[i], ' thermister data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2017-', monthlist[i], '_L1_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_therm_vert_L1)

# do -----
buoy_do_vert_L1 <- L1_2017 %>% 
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
    labs(title = paste0('2017-', monthlist[i], ' do data - NAs recoded')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2017-', monthlist[i], '_L0p5_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}
 
#May 24 - buoy deployed
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2017-05-24', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2017-05-25', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2017 buoy deployment', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2017 <- L1_2017 %>% 
  mutate_at(vars(dotemp_C_1m, dotemp_C_14.5m),
            ~(case_when(datetime_instrument<deployment+minutes(10) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_1m, do_ppm_14.5m, do_ppm_32m, 
                 do_sat_pct_1m, do_sat_pct_14.5m, do_sat_pct_32m),
            ~(case_when(datetime_instrument<deployment+hours(4) ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2017 %>% 
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



#June 7 do clean
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2017-06-07', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2017-06-08', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2017 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun7do = as.POSIXct('2017-06-07 10:00', tz='Etc/GMT+4')
#June 22 do clean
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2017-06-22', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2017-06-23', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2017 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun22do = as.POSIXct('2017-06-22 10:40', tz='Etc/GMT+4')
L1_2017 <- L1_2017 %>% 
  mutate_at(vars(all_of(do1), all_of(do14)),
            ~(case_when(datetime_instrument== jun7do~ NA_real_,
                           datetime_instrument==jun22do~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2017 %>% 
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

#Jul 6 do clean
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2017-07-06', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2017-07-07', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Jul 2017 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jul6do =as.POSIXct('2017-07-06 10:30', tz='Etc/GMT+4')
#Jul 14 - therm check and do recalibration
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2017-07-14', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2017-07-15', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Jul 2017 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jul14do = as.POSIXct('2017-07-14 12:40', tz='Etc/GMT+4')
#Jul 19 do clean
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2017-07-19', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2017-07-20', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Jul 2017 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jul19do = as.POSIXct('2017-07-19 11:40', tz='Etc/GMT+4')

L1_2017 <- L1_2017 %>% 
  mutate_at(vars(all_of(do1), all_of(do14)),
            ~(case_when(datetime_instrument== jul6do~ NA_real_,
                           datetime_instrument==jul19do ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(dotemp_C_1m, dotemp_C_14.5m),
            ~(case_when(datetime_instrument>=jul14do &
                             datetime_instrument<jul14do + hours(1) + minutes(10)~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=jul14do &
                             datetime_instrument<jul14do + hours(5) +minutes(10)~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2017 %>% 
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


#Aug 24 do clean
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2017-08-24', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2017-08-25', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2017 do', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
aug24do = as.POSIXct('2017-08-24 10:30', tz='Etc/GMT+4')
L1_2017 <- L1_2017 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime_instrument>=aug24do & 
                             datetime_instrument<aug24do+minutes(40) ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2017 %>% 
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


#recode data < 8; add suspect flagto data beginning Nov 1
L1_2017 <- L1_2017 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m),
            ~case_when(datetime_instrument >= as.POSIXct('2017-11-01', tz = 'Etc/GMT+4') & do_ppm_1m < 9 ~ NA_real_, 
                       TRUE ~ .)) %>% 
  mutate(flag_do1 =case_when(is.na(do_ppm_1m) & datetime_instrument >= as.POSIXct('2017-11-01', tz = 'Etc/GMT+4') & datetime_instrument < removal ~ 'e',
                             !is.na(do_ppm_1m) & datetime_instrument >= as.POSIXct('2017-11-01', tz = 'Etc/GMT+4') & datetime_instrument < removal ~ 's',
                             TRUE ~ '')) %>% 
  mutate(do_sat_pct_1m = case_when(flag_do1 == 'e' ~ NA_real_, # for whatever reason, the mutate at isn't working
                                   TRUE ~ do_sat_pct_1m))
ggplot(subset(L1_2017,
              subset = datetime_instrument >= as.POSIXct('2017-11-01', tz = 'Etc/GMT+4')), 
       aes(x = datetime_instrument, y = do_ppm_1m, color = flag_do1)) +
  geom_point() + 
  geom_point(aes(y = do_sat_pct_1m)) +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour')

buoy_do_vert_L1 <- L1_2017 %>% 
  select(datetime_instrument, all_of(do), flag_do1) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -c(datetime_instrument, 
               flag_do1)) %>% 
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

#buoy removed Nov 16
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2017-11-16', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2017-11-17', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Nov 2017 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

#plot monthly
buoy_do_vert_L1 <- buoy_do_vert_L1 %>% 
  mutate(month = format(datetime_instrument, '%m'))
for (i in 1: length(monthlist)){
  df = buoy_do_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=as.factor(depth), shape = flag_do1)) +
    geom_point() +
    facet_grid(sensor ~ ., scales='free_y') +
    labs(title = paste0('2017-', monthlist[i], ' do data - clean')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2017-', monthlist[i], '_L1_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_do_vert_L1)

#add flags ----
L1_2017 <- L1_2017 %>% 
  mutate(flag_do1 =case_when(datetime_instrument == jun7do ~ 'w',
                             datetime_instrument == jun22do ~ 'w',
                             datetime_instrument == jul6do ~ 'w',
                             datetime_instrument == jul14do ~ 'c',
                             datetime_instrument == jul19do ~ 'w',
                             datetime_instrument == aug24do ~ 'w',
                             datetime_instrument == sep14 ~ 'w',
                             TRUE ~ flag_do1),
         flag_do14 =case_when(datetime_instrument == jun7do ~ 'w',
                              datetime_instrument == jun22do ~ 'w',
                              datetime_instrument == jul6do ~ 'w',
                              datetime_instrument == jul14do ~ 'c',
                              datetime_instrument == jul19do ~ 'w',
                              datetime_instrument == aug24do ~ 'w',
                              datetime_instrument == sep14 ~ 'w',
                              TRUE ~ ''),
         flag_do32 =case_when(datetime_instrument == jun7do ~ 'w',
                              datetime_instrument == jun22do ~ 'w',
                              datetime_instrument == jul6do ~ 'w',
                              datetime_instrument == jul14do ~ 'c',
                              datetime_instrument == jul19do ~ 'w',
                              datetime_instrument == aug24do ~ 'w',
                              datetime_instrument == sep14 ~ 'w',
                              TRUE ~ ''))

#1m do likely miscalibrated until jul14do visit
L1_2017 <- L1_2017 %>% 
  mutate(flag_do1 = case_when(datetime_instrument<jul14do & flag_do1 == '' ~ 'm',
                              datetime_instrument<jul14do & flag_do1 != '' ~ paste0(flag_do1, '; m'),
                              TRUE ~ flag_do1))
         
#odd behavior in 1m do in November
#need to follow around with HAE


#save file ----

L1_2017 %>% 
  mutate(datetime_EST = with_tz(datetime_instrument, tzone = 'Etc/GMT+5')) %>% 
  mutate(datetime_EST = as.character(datetime_EST)) %>% 
  select(-datetime_instrument) %>% 
  write_csv(., file.path(dumpdir, 'buoy_L1_2017.csv'))
