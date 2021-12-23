#######################################################
# File: buoy_2015.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 22Jan2018                             #
# Purpose: to create a L1 dataset from the raw 2015   #
#         Auburn buoy data                            #
# R version: 3.4.3                                    #
#######################################################

source('libraries_lists_functions.R')

#point to directories
figdir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/programs/Auburn_buoy_GH/2015 cleaning graphs/'
datadir = 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/'


L0_2015 <- read_csv(file.path(datadir, "Lake_Auburn-SDL500R-11-20-2015_00-27.csv"), 
                    skip=3, 
                    col_names = varnames2015) %>% 
  mutate(datetime = as.POSIXct(strptime(datetime, "%m/%d/%Y %H:%M", tz='Etc/GMT+4'))) %>% 
  rename(datetime_instrument = datetime)
#warnings okay

#do a quick check of number of obs per hour to see if DST is in play
dst_check <- L0_2015 %>% 
  mutate(date = format(datetime_instrument, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(n_obs = length(datetime_instrument))
view(dst_check)
#DST in 2015: 03/08/15; 11/01/15 look at those dates

#look at battery
ggplot(L0_2015, aes(x = datetime_instrument, y = bat_v)) +
  geom_point()

#battery is fine
L0_2015 <- L0_2015 %>% 
  select(-bat_v)


#reshape and plot to see NA values
L0_2015_vert <- L0_2015 %>%
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

# L0_2015_vert %>%
#   ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Raw Buoy data 2015') +
#   final_theme

#recode NA values as NA
L1_2015 <- L0_2015 %>% 
  mutate_at(vars(temp_C_0.5m:temp_C_30m),
            ~(case_when(. == -100000 ~ NA_real_, 
                           TRUE ~ .))) 

L1_2015_vert <- L1_2015 %>%
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

# L1_2015_vert %>%
#   ggplot(aes(x=datetime_instrument, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Buoy data 2015 - NAs recoded') +
#   final_theme

####L1 data cleaning####

# THERMISTERS ----

buoy_therm_vert_L1 <- L1_2015 %>% 
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
    labs(title = paste0('2015-', monthlist[i], ' thermister data - NAs recoded'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2015-', monthlist[i], '_L0p5_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}


#May 7 buoy deployment
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2015-05-07', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2015-05-08', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2015 buoy deployment', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

deployment = as.POSIXct('2015-05-07 10:10', tz='Etc/GMT+4')

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(all_of(therm),all_of(do)),
            funs(case_when(datetime_instrument<deployment ~ NA_real_,
                           TRUE ~ .))) %>% 
  filter(datetime_instrument >= deployment)

buoy_therm_vert_L1 <- L1_2015 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2015-05-01', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2015-06-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2015 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

#June 18 buoy visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2015-06-18', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2015-06-19', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'June 2015 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))

jun18 = as.POSIXct('2015-06-18 10:30', tz='Etc/GMT+4')

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument== jun18~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2015 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))


#July 2 buoy visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2015-07-02', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2015-07-03', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2015 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
jul2 = as.POSIXct('2015-07-02 9:20', tz='Etc/GMT+4')

#July 16
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2015-07-16', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2015-07-17', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2015 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
jul16 = as.POSIXct('2015-07-16 10:20', tz='Etc/GMT+4')
  
#July 23
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2015-07-23', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2015-07-24', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2015 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
jul23 = as.POSIXct('2015-07-23 10:30', tz='Etc/GMT+4')

#July 30
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2015-07-30', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2015-07-31', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2015 buoy visit', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
jul30 = as.POSIXct('2015-07-30 8:50', tz='Etc/GMT+4')

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>=jul2  &
                             datetime_instrument<jul2+minutes(40)~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument==jul16 ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>=jul23 &
                             datetime_instrument<jul23+minutes(20) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(all_of(therm), all_of(do32)),
            ~(case_when(datetime_instrument>=jul30 &
                             datetime_instrument<jul30 +minutes(70) ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2015 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2015-07-01', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2015-08-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2015 clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
 
#buoy removal nov 18
ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2015-11-18', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2015-11-19', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2015 buoy removal', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
                              "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
                              "#f5ee89", "#005180", "#0081cc"))
removal = as.POSIXct('2015-11-18 8:00', tz='Etc/GMT+4')

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime_instrument>=removal ~ NA_real_,
                           TRUE ~ .))) %>% 
  filter(datetime_instrument <= removal)
buoy_therm_vert_L1 <- L1_2015 %>% 
  select(datetime_instrument, all_of(therm)) %>% 
  pivot_longer(names_to = 'variable', 
               values_to = 'value', -datetime_instrument) %>% 
  mutate(variable = factor(variable, c(therm)))


ggplot(subset(buoy_therm_vert_L1, subset=(datetime_instrument>=as.POSIXct('2015-11-01', tz='Etc/GMT+4') &
                                            datetime_instrument<as.POSIXct('2015-12-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2015 clean', y='temp (deg C)') +
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
    labs(title = paste0('2015-', monthlist[i], ' thermister data - clean'), 
         y='temp (deg C)') +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2015-', monthlist[i], '_L1_therm.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_therm_vert_L1)

# DO ----
buoy_do_vert_L1 <- L1_2015 %>% 
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
  labs(title = paste0('2015-', monthlist[i], ' do data - NAs recoded')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
  filename = paste0('2015-', monthlist[i], '_L0p5_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

#buoy deployed May 07
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-05-07', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-05-08', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2015 buoy deployed') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
deployment

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(do_ppm_1m, do_ppm_14.5m, do_ppm_32m, 
                 do_sat_pct_1m, do_sat_pct_14.5m, do_sat_pct_32m),
            ~(case_when(datetime_instrument<deployment + hours(4) ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2015 %>% 
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

# Jun 4 do clean
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-06-04', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-06-05', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2015 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun4do = as.POSIXct('2015-06-04 10:10', tz='Etc/GMT+4')

#jun 18 cleaning
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-06-18', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-06-19', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2015 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun18do = jun18

#jun 25 cleaning
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-06-25', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-06-26', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2015 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jun25do = as.POSIXct('2015-06-25 9:10', tz='Etc/GMT+4')

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(dotemp_C_1m, dotemp_C_14.5m, do_ppm_1m, do_ppm_14.5m, do_sat_pct_1m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument==jun4do ~ NA_real_,
                           datetime_instrument>=jun18do &
                             datetime_instrument<jun18do + minutes(20) ~ NA_real_,
                           datetime_instrument==jun25do ~ NA_real_,
                           TRUE ~ .))) 
  
buoy_do_vert_L1 <- L1_2015 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-06-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-07-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2015 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()
 
#Jul 2 recalibration
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-07-02', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-07-03', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2015 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jul2do = as.POSIXct('2015-07-02 8:20', tz='Etc/GMT+4')
  
#Jul 30 recalibration
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-07-30', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-07-31', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2015 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
jul30do = as.POSIXct('2015-07-30 8:50', tz='Etc/GMT+4')

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, 
                 dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=jul2do &
                             datetime_instrument<jul2do +minutes(70) ~ NA_real_,
                           datetime_instrument>=jul30do &
                             datetime_instrument<jul30do+minutes(70) ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m),
            ~(case_when(datetime_instrument>=jul2do &
                          datetime_instrument<jul2do +hours(4) + minutes(30) ~ NA_real_,
                        TRUE ~ .))) %>%
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=jul2do &
                          datetime_instrument<jul2do +hours(4) + minutes(30) ~ NA_real_,
                        TRUE ~ .))) %>%
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument>=jul2do &
                             datetime_instrument<jul2do + hours(5)~ NA_real_,
                           datetime_instrument>=jul30do&
                             datetime_instrument<jul30do+hours(5) +minutes(10) ~ NA_real_,
                           TRUE ~ .)))  %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m),
            ~case_when(datetime_instrument>=jul2&
                         datetime_instrument<jul2+hours(5) ~ NA_real_,
                       datetime_instrument>=jul30do&
                         datetime_instrument<jul30do+hours(5)+minutes(10) ~ NA_real_,
                       TRUE ~ .))
buoy_do_vert_L1 <- L1_2015 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-07-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-08-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2015 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#do clean Aug 28
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-08-28', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-08-29', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2015 buoy visit') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
aug28do = as.POSIXct('2015-08-28 12:00', tz='Etc/GMT+4')
  
L1_2015 <- L1_2015 %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            ~(case_when(datetime_instrument==aug28do ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1 <- L1_2015 %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-08-01', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-09-01', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Aug 2015 clean') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#Nov 18 buoy removed
ggplot(subset(buoy_do_vert_L1, subset=(datetime_instrument >=as.POSIXct('2015-11-18', tz='Etc/GMT+4') &
                                         datetime_instrument < as.POSIXct('2015-11-19', tz='Etc/GMT+4'))),
       aes(x=datetime_instrument, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Nov 2015 buoy removed') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(all_of(do)),
            ~(case_when(datetime_instrument>=removal - minutes(10) ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1 <- L1_2015 %>% 
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
    labs(title = paste0('2015-', monthlist[i], ' do data - clean')) +
    final_theme +
    scale_x_datetime(date_minor_breaks = '1 day') +
    scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                                "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
  print(plot)
    filename = paste0('2015-', monthlist[i], '_L1_do.png')
  ggsave(file.path(figdir, filename), device = 'png')
}

rm(buoy_do_vert_L1, L0_2015_vert, L1_2015_vert)

# add flags ----
L1_2015 <- L1_2015 %>% 
  mutate(flag_do1 =case_when(datetime_instrument == jun4do ~ 'w',
                             datetime_instrument == jul2do ~ 'c',
                             datetime_instrument == jul30do~ 'c',
                             datetime_instrument == aug28do~ 'w',
                             TRUE ~ ''),
         flag_do14 =case_when(datetime_instrument == jun4do ~ 'w',
                              datetime_instrument == jul2do ~ 'c',
                              datetime_instrument == jul30do~ 'c',
                              datetime_instrument == aug28do~ 'w',
                              TRUE ~ ''),
         flag_do32 =case_when(datetime_instrument == jul2 ~ 'c',
                              datetime_instrument == jul30 ~ 'c',
                              TRUE ~ ''))

#save files ----

L1_2015 %>% 
  mutate(datetime_EST = with_tz(datetime_instrument, tzone = 'EST')) %>% 
  mutate(datetime_EST = as.character(datetime_EST)) %>% 
  select(-datetime_instrument) %>% 
  write_csv(., 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2015.csv')



