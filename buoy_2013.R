# File: buoy_2013.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 22Jan2018                             #
# Purpose: to create a L1 dataset from the raw 2013   #
#         Auburn buoy data                            #

source('libraries_lists_functions.R')

#point to directories
figdir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/programs/Auburn_buoy_GH/2013 cleaning graphs/'
datadir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/'


# July-Nov2013
L0_2013a <- read_xls(file.path(datadir, "BatesDataSonde_hae.xls"), 
                     sheet="raw data", 
                     skip=1, 
                     col_names=varnames2016) %>% 
  mutate(datetime = as.POSIXct(strptime(datetime, "%Y-%m-%d %H:%M:%S", tz='Etc/GMT+4'))) #stays in DST for whole year

# Nov2013
L0_2013b <- read_csv(file.path(datadir, "batescollege_11-04-2013_11-24-2013.csv"), 
                     skip=3, 
                     col_names = varnames2016) %>% 
  mutate(datetime = as.POSIXct(strptime(datetime, "%m/%d/%Y %H:%M", tz='Etc/GMT+4'))) %>%  #stays in DST for whole year
  filter(datetime>as.POSIXct("2013-11-17 01:50:00", tz='Etc/GMT+4'))
#warnings okay

#merge to a single L0 file
L0_2013 <- full_join(L0_2013a, L0_2013b) %>% 
  rename(datetime_instrument = datetime)

#do a quick check of number of obs per hour to see if DST is in play
dst_check <- L0_2013 %>% 
  mutate(date = format(datetime_instrument, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(n_obs = length(datetime_instrument))
view(dst_check)
#DST in 2014: 03/10/13; 11/03/13 look at/around those dates

#clean up work space
rm(L0_2013a, L0_2013b)

#look at battery
ggplot(L0_2013, aes(x = datetime_instrument, y = bat_v)) +
  geom_point()

#battery is fine
L1_2013 <- L0_2013 %>% 
  select(-bat_v)

#reshape and plot to see NA values
L1_2013_vert <- L1_2013 %>%
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

# L1_2013_vert %>%
#   ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Raw Buoy data 2013') +
#   final_theme

#recode NA values as NA
L1_2013 <- L1_2013 %>% 
  mutate_at(vars(all_of(therm)),
            ~ case_when(. == -100000 ~ NA_real_, 
                           TRUE ~ .))

L1_2013_vert <- L1_2013 %>%
  pivot_longer(names_to = 'sensor', 
               values_to = 'value', 
               -datetime_instrument) %>%
  mutate(depth = case_when(sensor == 'do_ppm_1m' ~ 1,
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

# L1_2013_vert %>%
#   ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Buoy data 2013 - NAs recoded') +
#   final_theme

#THERMISTERS ----
buoy_therm_vert_L1 <- L1_2013 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
         values_to = 'value', 
         -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#print monthly data
monthlist = c('07', '08', '09', '10', '11')
buoy_therm_vert_L1 <- buoy_therm_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))

for (i in 1: length(monthlist)){
  df = buoy_therm_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=variable)) +
    geom_point() +
    labs(title = paste0('2013-', monthlist[i], ' thermister data - NAs recoded'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2013-', monthlist[i], '_L0p5_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

# Jul 25 - buoy deployed
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2013-07-25', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2013-07-26', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2013 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

deployment = as.POSIXct('2013-07-25 10:00', tz='Etc/GMT+4')

L1_2013 <- L1_2013 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~ case_when(datetime_instrument< deployment~ NA_real_,
                           TRUE ~ .)) %>% 
  filter(datetime_instrument >= deployment)

buoy_therm_vert_L1 <- L1_2013 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
         values_to = 'value', 
         -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2013-07-01', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2013-08-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2013 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

# Aug 15 buoy visit
aug15 = as.POSIXct("2013-08-15 11:20:00", tz='Etc/GMT+4')

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2013-08-30', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2013-09-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug 2013 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

# Aug 30 buoy visit
aug30 = as.POSIXct("2013-08-30 9:30", tz='Etc/GMT+4')
  
L1_2013 <- L1_2013 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~ case_when(datetime_instrument>= aug15 &
                             datetime_instrument< aug15 +minutes(20) ~ NA_real_,
                           datetime_instrument>= aug30 &
                             datetime_instrument< aug30 + minutes(30) ~ NA_real_,
                           TRUE ~ .))

buoy_therm_vert_L1 <- L1_2013 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2013-08-01', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2013-09-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug 2013 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

# Oct 4 buoy visit
oct4 = as.POSIXct("2013-10-04 6:50", tz='Etc/GMT+4')
# Oct 18 buoy visit
oct18 = as.POSIXct("2013-10-18 7:50", tz='Etc/GMT+4')

L1_2013 <- L1_2013 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~ case_when(datetime_instrument>= oct4 &
                             datetime_instrument<oct4 + minutes(20) ~ NA_real_,
                           datetime_instrument>= oct18 &
                             datetime_instrument<oct18 +minutes(20) ~ NA_real_,
                           TRUE ~ .))

buoy_therm_vert_L1 <- L1_2013 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2013-10-01', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2013-11-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Oct 2013 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

removal = as.POSIXct("2013-11-21 8:00", tz='Etc/GMT+4')

L1_2013 <- L1_2013 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)), 
            ~ case_when(datetime_instrument>= removal ~ NA_real_,
                           TRUE ~ .)) %>% 
  filter(datetime_instrument <= removal)

buoy_therm_vert_L1 <- L1_2013 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', 
               -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm))) %>% 
  mutate(month = format(datetime_instrument, '%m'))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2013-11-01', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2013-12-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2013 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

#plot monthly
for (i in 1: length(monthlist)){
  df = buoy_therm_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=variable)) +
    geom_point() +
    labs(title = paste0('2013-', monthlist[i], ' thermister data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2013-', monthlist[i], '_L1_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_therm_vert_L1)

# DO ----
buoy_do_vert_L1 <- L1_2013 %>% 
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
    labs(title = paste0('2013-', monthlist[i], ' do data - NAs recoded')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2013-', monthlist[i], '_L0p5_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

# #buoy deployment July 25
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2013-07-25', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2013-07-26', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Jul 2013 buoy deployed') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

#equilibration time extends to +4h
L1_2013 <- L1_2013 %>% 
  mutate_at(vars(do_ppm_1m, do_ppm_14.5m, do_ppm_32m, do_sat_pct_1m, do_sat_pct_14.5m, do_sat_pct_32m),
            ~case_when(datetime_instrument<deployment +hours(4) ~ NA_real_,
                           TRUE ~ .))

buoy_do_vert_L1 <- L1_2013 %>% 
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


ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2013-07-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2013-08-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Jul 2013 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

# Aug 15 buoy visit
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2013-08-15', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2013-08-16', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2013 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
aug15do = as.POSIXct('2013-08-15 8:30', tz = 'Etc/GMT+4')

# Aug 30 buoy visit
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2013-08-30', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2013-08-31', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2013 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
aug30do = as.POSIXct('2013-08-30 8:30', tz = 'Etc/GMT+4')

L1_2013 <- L1_2013 %>%
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=aug15do &
                             datetime_instrument<aug15do+hours(1) ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate(dotemp_C_14.5m = case_when(datetime_instrument>=aug15do &
                                      datetime_instrument<aug15do+hours(2) ~ NA_real_,
                                    TRUE ~ dotemp_C_14.5m)) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=aug15do &
                             datetime_instrument<aug15do + hours(6) ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            ~ case_when(datetime_instrument >= aug15 &
                          datetime_instrument < aug15 + hours(2) ~ NA_real_, 
                        TRUE ~ .)) %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=aug30do &
                             datetime_instrument<aug30do + hours(1)+minutes(10) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(dotemp_C_32m = case_when(datetime_instrument>=aug30 &
                                    datetime_instrument<aug30 + minutes(10) ~ NA_real_,
                                  TRUE ~ dotemp_C_32m)) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=aug30do &
                             datetime_instrument<aug30do +hours(4) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            ~ case_when(datetime_instrument >= aug30 &
                          datetime_instrument < aug30 +hours(3) ~ NA_real_,
                        TRUE ~ .))

buoy_do_vert_L1 <- L1_2013 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2013-08-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2013-09-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2013 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

 
#Oct 4 buoy visit
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2013-10-04', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2013-10-05', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Oct 2013 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
oct4do = as.POSIXct('2013-10-04 06:40', tz='Etc/GMT+4')

#Oct 18 buoy visit
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2013-10-18', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2013-10-19', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Oct 2013 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
oct18do = as.POSIXct('2013-10-18 06:40', tz='Etc/GMT+4')
  
L1_2013 <- L1_2013 %>%
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime_instrument>=oct4do &
                             datetime_instrument <oct4do +minutes(30)~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime_instrument>=oct4do&
                             datetime_instrument<oct4do+hours(4)+minutes(30) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime_instrument>=oct4 &
                             datetime_instrument<oct4+hours(2) ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime_instrument==oct18do ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(dotemp_C_14.5m = case_when(datetime_instrument == oct18-minutes(10) ~ NA_real_,
                                    TRUE ~ dotemp_C_14.5m)) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime_instrument>=oct18do&
                             datetime_instrument<oct18do+hours(4) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime_instrument>=oct18 &
                             datetime_instrument<oct18+hours(2) ~ NA_real_,
                           TRUE ~ .))) 

buoy_do_vert_L1 <- L1_2013 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2013-10-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2013-11-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Oct 2013 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

# do sensor error Nov 21 and beyond
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2013-11-21', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2013-11-22', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Sept 2013 do data - NAs recoded') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2013 <- L1_2013 %>%
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime_instrument>=removal ~ NA_real_,
                           TRUE ~ .)))

buoy_do_vert_L1 <- L1_2013 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2013-11-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2013-12-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Nov 2013 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#plot monthly
for (i in 1: length(monthlist)){
  df = buoy_do_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
    geom_point() +
    facet_grid(sensor ~ ., scales='free_y') +
    final_theme +
    scale_color_colorblind()
  labs(title = paste0('2013-', monthlist[i], ' do data - clean')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2013-', monthlist[i], '_L1_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_do_vert_L1)

# add flags ----
L1_2013 <- L1_2013 %>% 
  mutate(flag_do1 = case_when(datetime_instrument == aug15do ~ 'w', 
                              datetime_instrument == aug30do ~ 'c',
                              datetime_instrument == oct4do  ~ 'w',
                              datetime_instrument == oct18do ~ 'w',
                              TRUE ~ ''),
         flag_do14 = case_when(datetime_instrument == aug15do ~ 'w', 
                              datetime_instrument == aug30do ~ 'c',
                              datetime_instrument == oct4do  ~ 'w',
                              datetime_instrument == oct18do ~ 'w',
                              TRUE ~ ''),
         flag_do32 = case_when(datetime_instrument == aug15do ~ 'w', 
                              datetime_instrument == aug30do ~ 'c',
                              datetime_instrument == oct4do  ~ 'w',
                              datetime_instrument == oct18do ~ 'w',
                              TRUE ~ ''))

#save file ----

L1_2013 %>% 
  mutate(datetime_EST = with_tz(datetime_instrument, tzone = 'EST')) %>% 
  mutate(datetime_instrument = as.character(datetime_instrument),
         datetime_EST = as.character(datetime_EST)) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2013.csv')
