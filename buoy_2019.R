# File: buoy_2019R                                    #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 29Jan2020                             #
# Purpose: to create a L1 dataset from the raw 2019   #
#         Auburn buoy data                            #

source('libraries_lists_functions.R')

#point to directories
figdir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/programs/Auburn_buoy_GH/2019 cleaning graphs/'
datadir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/'

L0_2019 <- read_csv(file.path(datadir, 'Lake_Auburn-SDL500R-11-25-2019_05-00.csv'),
                    col_names = varnames2018,
                    col_types = 'cnnnnnnnnnnnnnnnnnnnnn',
                    skip=4) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='Etc/GMT+4', format='%m/%d/%Y %H:%M')) %>% 
  rename(datetime_instrument = datetime)
str(L0_2019)

#do a quick check of number of obs per hour to see if DST is in play
dst_check <- L0_2019 %>% 
  mutate(date = format(datetime_instrument, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(n_obs = length(datetime_instrument))
view(dst_check)
#DST in 2019: 03/10/19; 11/03/19 look at those dates

#look at battery
ggplot(L0_2019, aes(x = datetime_instrument, y = bat_v)) +
  geom_point()

#battery is fine
L1_2019 <- L0_2019 %>% 
  select(-bat_v)

#recode NA values as NA
L1_2019 <- L0_2019 %>% 
  mutate_at(vars(do_ppm_1m:temp_C_30m),
            ~(case_when(. < -99999.9 ~ NA_real_, # for -99999.99 and -100000 - had to do it this way because there were issues with -99999.99
                           TRUE ~ .)))
str(L1_2019)

L1_2019_vert <- L1_2019 %>%
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

# L1_2019_vert %>%
#   ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Buoy data 2019 - NAs recoded') +
#   final_theme


####L1 Cleaning####

#### THERMISTERS ----
buoy_therm_vert_L1 <- L1_2019 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#some errant points early on in 12m thermistor - per conversation with Holly, will recode all to NA until it consistently functions in July.
L1_2019 <- L1_2019 %>% 
  mutate(temp_C_12m = case_when(datetime_instrument < as.Date('2019-07-01') ~ NA_real_,
                                TRUE ~ temp_C_12m))
buoy_therm_vert_L1 <- L1_2019 %>% 
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
    labs(title = paste0('2019-', monthlist[i], ' thermister data - NAs recoded'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2019-', monthlist[i], '_L0p5_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

#May 09 - buoy deployed
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-05-09', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-05-10', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2019 buoy deploy', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
deployment = as.POSIXct('2019-05-09 12:30', tz='Etc/GMT+4')

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(all_of(therm), all_of(do)),
            ~(case_when(datetime_instrument< deployment ~ NA_real_,
                           TRUE ~ .))) %>% 
  filter(datetime_instrument >= deployment)
buoy_therm_vert_L1 <- L1_2019 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#buoy visit Jun 19
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-06-19', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-06-20', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'June 2019 buoy visit - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
jun19 = as.POSIXct('2019-06-19 11:00', tz='Etc/GMT+4')

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(all_of(therm),all_of(do32)),
            ~(case_when(datetime_instrument>= jun19&
                             datetime_instrument < jun19 +minutes(40) ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2019 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#July 31 visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-07-31', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-08-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2019 buoy visit - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
jul31 = as.POSIXct('2019-07-31 13:30', tz='Etc/GMT+4')

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>=jul31 &
                             datetime_instrument < jul31 + minutes(20) ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2019 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

#buoy removed Nov 17
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-11-17', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-11-18', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2019 therm', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

removal = as.POSIXct('2019-11-17 9:40', tz='Etc/GMT+4')

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(all_of(therm), all_of(do)),
            ~(case_when(datetime_instrument>= removal~ NA_real_,
                           TRUE ~ .))) %>% 
  filter(datetime_instrument <=removal)
buoy_therm_vert_L1 <- L1_2019 %>% 
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
    labs(title = paste0('2019-', monthlist[i], ' thermister data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2019-', monthlist[i], '_L1_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_therm_vert_L1)

#### do ####
buoy_do_vert_L1 <- L1_2019 %>% 
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
    labs(title = paste0('2019-', monthlist[i], ' do data - NAs recoded')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2019-', monthlist[i], '_L0p5_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

#may 9 deploy
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-05-09', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-05-10', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2019 deploy - do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_14.5m, do_ppm_32m, do_sat_pct_14.5m, do_sat_pct_32m),
            ~(case_when(datetime_instrument<deployment + hours(1) ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2019 %>% 
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

#June 5 errant at 1m
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-06-05', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-06-06', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun5do = as.POSIXct('2019-06-05 11:20', tz='Etc/GMT+4')
L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m),
            ~(case_when(datetime_instrument == jun5do  ~ NA_real_,
                           TRUE ~ .)))
#odd behavior at 1m 6-14
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-06-14 12:00', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-06-15 12:00', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
#see if this is a wind event - not a wind event. recode until visit on june 19.
jun14do = as.POSIXct('2019-06-14 21:30', tz='Etc/GMT+4')
L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m),
            ~(case_when(datetime_instrument >= jun14do &
                             datetime_instrument < jun19 ~ NA_real_,
                           TRUE ~ .))) 

#june 25, drop in do at 14.5 errant from there until replacement. flag all data prior.
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-06-25', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-06-26', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun25.14fail = as.POSIXct('2019-06-25 16:30', tz='Etc/GMT+4')
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-08-28', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-08-29', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
aug28do = as.POSIXct('2019-08-28 11:10', tz='Etc/GMT+4')
L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=jun25.14fail &
                             datetime_instrument < aug28do + minutes(40) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(flag_do14 = case_when(datetime_instrument<jun25.14fail &
                                     datetime_instrument >= deployment~ 'DO unit fails on June 25, apply data with caution',
                                   TRUE ~ ''))
#odd point on 26th at 1m
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-06-26', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-06-27', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun26do = as.POSIXct('2019-06-26 11:20', tz='Etc/GMT+4')
L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m),
            ~(case_when(datetime_instrument== jun26do~ NA_real_,
                           TRUE ~ .))) 

buoy_do_vert_L1 <- L1_2019 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-06-01', tz='Etc/GMT+4') & 
                                         datetime_instrument<as.POSIXct('2019-07-01', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#jul 18 1m recal
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-07-18', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-07-19', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jul18do = as.POSIXct('2019-07-18 14:00', tz='Etc/GMT+4')

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, dotemp_C_1m, dotemp_C_14.5m),
            ~(case_when(datetime_instrument==jul18do ~ NA_real_,
                           TRUE ~ .)))
#jul 31
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-07-31', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-08-01', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jul31do = as.POSIXct('2019-07-31 12:10', tz='Etc/GMT+4')
L1_2019 <- L1_2019 %>% 
  mutate(dotemp_C_14.5m = case_when(datetime_instrument>= jul31do &
                                      datetime_instrument < aug28do + minutes(30) ~ NA_real_,
                                    TRUE ~ dotemp_C_14.5m)) 

buoy_do_vert_L1 <- L1_2019 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-07-01', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-08-01', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()


#aug 21 
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-08-21', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-08-22', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'August 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
aug21do = as.POSIXct('2019-08-21 11:10', tz = 'Etc/GMT+4')
L1_2019 <- L1_2019 %>% 
  mutate_at(vars(all_of(do1)),
            ~(case_when(datetime_instrument==aug21do ~ NA_real_,
                        TRUE ~ .))) 

#14.5 replaced on august 28
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-08-28', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-08-29', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'August 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
aug28do

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(all_of(do1)),
            ~(case_when(datetime_instrument>=aug28do &
                             datetime_instrument < aug28do +minutes(20)~ NA_real_,
                           TRUE ~ .)))  %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            ~(case_when(datetime_instrument>=aug28do &
                          datetime_instrument <aug28do + minutes(30) ~ NA_real_,
                        TRUE ~ .)))
buoy_do_vert_L1 <- L1_2019 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-08-01', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-09-01', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'August 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#oct 16
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-10-16', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-10-17', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Oct 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
oct16do = as.POSIXct('2019-10-16 11:10', tz = 'Etc/GMT+4')

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(all_of(do1), all_of(do14)),
            ~(case_when(datetime_instrument==oct16do~ NA_real_,
                        TRUE ~ .))) 
buoy_do_vert_L1 <- L1_2019 %>% 
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
#buoy move nov 17
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument>=as.POSIXct('2019-11-17', tz='Etc/GMT+4') & datetime_instrument<as.POSIXct('2019-11-18', tz='Etc/GMT+4'))), 
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Nov 2019 do data - clean', y='temp (deg C)') +
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
    labs(title = paste0('2019-', monthlist[i], ' do data - clean')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_colorblind()
  print(plot)
  filename = paste0('2019-', monthlist[i], '_L1_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_do_vert_L1)


# add flags ----
colnames(L1_2019)
L1_2019 <- L1_2019 %>% 
  mutate(flag_do1 =case_when(datetime_instrument == jun19 ~ 'w',
                             datetime_instrument == jun26do ~ 'w',
                             datetime_instrument == jul18do ~ 'pc',
                             datetime_instrument == jul31do ~ 'w',
                             datetime_instrument == aug21do ~ 'pw',
                             datetime_instrument == aug28do ~ 'w',
                             datetime_instrument == oct16do ~ 'w',
                             TRUE ~ ''),
       flag_do14 =case_when(datetime_instrument == jun25.14fail ~ 'f',
                            datetime_instrument == aug28do ~ 'r',
                            datetime_instrument == oct16do ~ 'w',
                            TRUE ~ flag_do14),
       flag_do32 =case_when(datetime_instrument == jul31 ~ 'w',
                            TRUE ~ ''),
       flag_temp = case_when(datetime_instrument == jun19 ~ '12r',
                             TRUE ~ ''))


# save file ----

L1_2019 %>% 
  mutate(datetime_EST = with_tz(datetime_instrument, tzone = 'EST')) %>% 
  mutate(datetime_EST = as.character(datetime_EST)) %>% 
  select(-datetime_instrument) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Auburn buoy/data/L1 data/buoy_L1_2019.csv')
