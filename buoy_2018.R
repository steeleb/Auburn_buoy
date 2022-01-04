# File: buoy_2018.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 02Feb2019                             #
# Purpose: to create a L1 dataset from the raw 2018   #
#         Auburn buoy data                            #

source('libraries_lists_functions.R')

#point to directories
figdir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/programs/Auburn_buoy_GH/2018 cleaning graphs/'
datadir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/'

L0_2018 <- read_csv(file.path(datadir, '2018data.csv'),
                    col_names = varnames2018,
                    col_types = 'cnnnnnnnnnnnnnnnnnnnnn',
                    skip=4) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='Etc/GMT+4', format='%m/%d/%Y %I:%M:%S %p')) %>% 
  rename(datetime_instrument = datetime)
head(L0_2018)

#do a quick check of number of obs per hour to see if DST is in play
dst_check <- L0_2018 %>% 
  mutate(date = format(datetime_instrument, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(n_obs = length(datetime_instrument))
view(dst_check)
#DST in 2018: 03/11/18; 11/04/18 look at those dates

#look at battery
ggplot(L0_2018, aes(x = datetime_instrument, y = bat_v)) +
  geom_point()

#battery is fine
L0_2018 <- L0_2018 %>% 
  select(-bat_v)

#recode NA values as NA
L1_2018 <- L0_2018 %>% 
  mutate_at(vars(do_ppm_1m:temp_C_30m),
            ~(case_when(. < -99999.9 ~ NA_real_, # for -99999.99 and -100000 - had to do it this way because there were issues with -99999.99
                           TRUE ~ .)))

L1_2018_vert <- L1_2018 %>%
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

# L1_2018_vert %>%
#   ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Buoy data 2018 - NAs recoded') +
#   final_theme

####L1 Cleaning####

#THERMISTERS ----
buoy_therm_vert_L1 <- L1_2018 %>% 
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
    labs(title = paste0('2018-', monthlist[i], ' thermister data - NAs recoded'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2018-', monthlist[i], '_L0p5_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

#12m is wrong. thought it was in farenheit... not the case. tried a bunch of different values, but they end up wonky somewhere in the record. recoding at place of jump

#may 11, when jump occurs
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2018-05-11', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2018-05-12', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2018', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#Look at data 20:00-22:00
test <- subset(L1_2018, subset=(datetime_instrument>=as.POSIXct('2018-05-11 20:00', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2018-05-11 22:00', tz='Etc/GMT+4')))

#recode to NA
L1_2018 <- L1_2018 %>% 
  mutate(temp_C_12m = case_when(datetime_instrument>=as.POSIXct('2018-05-11 21:00', tz='Etc/GMT+4') ~ NA_real_,
                                TRUE ~ temp_C_12m))
buoy_therm_vert_L1 <- L1_2018 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2018-05-11', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2018-05-12', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2018', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#May 10 - buoy deployed
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2018-05-10', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2018-05-11', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2018 buoy deploy', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

deployment = as.POSIXct('2018-05-10 9:00', tz='Etc/GMT+4')

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(all_of(therm), all_of(do)),
            ~(case_when(datetime_instrument< deployment ~ NA_real_,
                           TRUE ~ .))) %>% 
  filter(datetime_instrument >= deployment)
buoy_therm_vert_L1 <- L1_2018 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2018-05-10', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2018-05-11', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2018 buoy deploy', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#buoy visit Aug 29
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2018-08-29', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2018-08-30', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug 2018 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

aug29 = as.POSIXct('2018-08-29 11:20', tz='Etc/GMT+4')

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>= aug29 & datetime_instrument < aug29 + minutes(70)~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2018 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2018-08-29', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2018-08-30', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug 2018 buoy visit - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#buoy removed Nov 15
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2018-11-15', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2018-11-16', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2018 therm', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

removal = as.POSIXct('2018-11-15 10:40', tz='Etc/GMT+4')
L1_2018 <- L1_2018 %>% 
  mutate_at(vars(all_of(therm), all_of(do)),
            ~(case_when(datetime_instrument> removal ~ NA_real_,
                           TRUE ~ .))) %>% 
  filter(datetime_instrument <= removal)
buoy_therm_vert_L1 <- L1_2018 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#print monthly graphs
buoy_therm_vert_L1 <- buoy_therm_vert_L1 %>% 
  mutate(month = as.character(format(datetime_instrument, '%m')))

for (i in 1: length(monthlist)){
  df = buoy_therm_vert_L1 %>% 
    filter(month == monthlist[i])
  plot <- ggplot(df, aes(x=datetime_instrument, y=value, color=variable)) +
    geom_point() +
    labs(title = paste0('2018-', monthlist[i], ' thermister data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2018-', monthlist[i], '_L1_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_therm_vert_L1, df, test)

#### do ----
buoy_do_vert_L1 <- L1_2018 %>% 
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
    labs(title = paste0('2018-', monthlist[i], ' do data - NAs recoded')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2018-', monthlist[i], '_L0p5_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

#May 9 - buoy deployed, May 10 corrected
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-05-10', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2018-05-11', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2018 buoy deployment', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument<deployment + hours(1) ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2018 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-05-10', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2018-05-11', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2018 buoy deployment', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()


#may 20 stray point in 14.5
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-05-20', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2018-05-21', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2018 do data', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(all_of(do14)),
            ~(case_when(datetime_instrument==as.POSIXct('2018-05-20 20:40', tz='Etc/GMT+4') ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1 <- L1_2018 %>% 
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
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-05-20', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2018-05-21', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2018 do data', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

#strays on jun 6 in 14.5; jun 12 in 1; and jun20 in 1

#June 6
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-06-06', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2018-06-07', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2018 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun6do <- as.POSIXct('2018-06-06 10:40', tz='Etc/GMT+4')

#June 12
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-06-12', tz='Etc/GMT+4') &
                                           datetime_instrument < as.POSIXct('2018-06-13', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2018 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun12do =as.POSIXct('2018-06-12 07:40', tz='Etc/GMT+4')
  
#June 20
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-06-20', tz='Etc/GMT+4') &
                                           datetime_instrument < as.POSIXct('2018-06-21', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2018 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun20do = as.POSIXct('2018-06-20 10:50', tz='Etc/GMT+4')

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>= jun6do & datetime_instrument < jun6do + hours(1) + minutes(40) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(dotemp_C_14.5m = case_when(datetime_instrument==jun6do ~ NA_real_,
                                    TRUE ~ dotemp_C_14.5m)) %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m),
            ~(case_when(datetime_instrument==jun12do ~ NA_real_,
                            TRUE ~ .))) %>% 
  mutate_at(vars(all_of(do1), all_of(do14)),
            ~(case_when(datetime_instrument == jun20do ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2018 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-06-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2018-07-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2018 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#Aug 08 do recalibration
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-08-08', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2018-08-09', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2018 do', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
aug8do <- as.POSIXct('2018-08-08 11:00', tz='Etc/GMT+4')

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, dotemp_C_1m),
            ~(case_when(datetime_instrument>=aug8do & 
                             datetime_instrument<aug8do + minutes(30) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(dotemp_C_14.5m = case_when(datetime_instrument >= aug8do &
                             datetime_instrument < aug8do+hours(1) ~ NA_real_,
                             TRUE ~ dotemp_C_14.5m)) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument >= aug8do &
                             datetime_instrument < aug8do + hours(2) + minutes(30) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(all_of(do32)),
            ~case_when(datetime_instrument == aug8do ~ NA_real_,
                       TRUE ~ .))
buoy_do_vert_L1 <- L1_2018 %>% 
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
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-08-08', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2018-08-09', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2018 do', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

#aug29 recal?
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-08-29', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2018-08-30', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2018', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

aug29do = as.POSIXct('2018-08-29 11:10', tz='Etc/GMT+4')

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m),
            funs(case_when(datetime_instrument>=aug29do & 
                             datetime_instrument<aug29do + minutes(40) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(dotemp_C_14.5m =case_when(datetime_instrument >= aug29do&
                             datetime_instrument < aug29do + hours(1) + minutes(40) ~ NA_real_,
                           TRUE ~ dotemp_C_14.5m)) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime_instrument >= aug29do &
                             datetime_instrument < aug29do + hours(3) ~ NA_real_,
                           TRUE ~ .))) 
              
buoy_do_vert_L1 <- L1_2018 %>% 
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
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2018-08-29', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2018-08-30', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2018', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
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
    labs(title = paste0('2018-', monthlist[i], ' do data - clean')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2018-', monthlist[i], '_L1_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_do_vert_L1)

# add flags ----
L1_2018 <- L1_2018 %>% 
  mutate(flag_do1 =case_when(datetime_instrument == jun6do ~ 'w',
                             datetime_instrument == aug8do ~ 'c',
                             datetime_instrument == aug29do ~ 'c',
                             TRUE ~ ''),
         flag_do14 =case_when(datetime_instrument == jun6do ~ 'w',
                              datetime_instrument == aug8do ~ 'c',
                              TRUE ~ ''),
         flag_do32 =case_when(datetime_instrument == aug29 ~ 'c',
                              TRUE ~ ''))

# save file ----
L1_2018 %>% 
  mutate(datetime_EST = with_tz(datetime_instrument, tzone = 'EST')) %>% 
  mutate(datetime_EST = as.character(datetime_EST)) %>% 
  select(-datetime_instrument) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Auburn buoy/data/L1 data/buoy_L1_2018.csv')
