#######################################################
# File: buoy_2019R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 29Jan2020                             #
# Purpose: to create a L1 dataset from the raw 2019   #
#         Auburn buoy data                            #
# R version: 3.6.1                                    #
#######################################################


L0_2019 <- read_csv('C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/Lake_Auburn-SDL500R-11-25-2019_05-00.csv',
                    col_names = varnames2018,
                    col_types = 'cnnnnnnnnnnnnnnnnnnnnn',
                    skip=4) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC', format='%m/%d/%Y %H:%M'))

str(L0_2019)

#recode NA values as NA
L1_2019 <- L0_2019 %>% 
  mutate_at(vars(do_ppm_1m:temp_C_30m),
            funs(case_when(. < -99999.9 ~ NA_real_, # for -99999.99 and -100000 - had to do it this way because there were issues with -99999.99
                           TRUE ~ .)))
str(L1_2019)

L1_2019_vert <- L1_2019 %>%
  select(-bat_v) %>%
  gather(sensor, value, -datetime) %>%
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

L1_2019_vert %>%
  ggplot(aes(x=datetime, y=value, color=depth)) +
  facet_grid(sensor_type ~ ., scales='free_y') +
  geom_point() +
  labs(title = 'Buoy data 2019 - NAs recoded') +
  final_theme


####L1 Cleaning####

#### THERMISTERS ####
buoy_therm_vert_L1 <- L1_2019 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(buoy_therm_vert_L1, aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = '2019 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#some errant points early on in 12m thermistor

ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-05-01', tz='UTC') & datetime<as.POSIXct('2019-06-01', tz='UTC'))),
              aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2019 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#May 09 - buoy deployed
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-05-09', tz='UTC') & datetime<as.POSIXct('2019-05-10', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2019 buoy deploy', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime<as.POSIXct('2019-05-09 12:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2019 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

#errant 12m stating on may 11-may 22
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-05-11', tz='UTC') & datetime<as.POSIXct('2019-05-12', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2019 12m probe erro', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-05-22', tz='UTC') & datetime<as.POSIXct('2019-05-23', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2019 12m probe erro', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#errant 12m may 27 - 
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-05-27', tz='UTC') & datetime<as.POSIXct('2019-05-28', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2019 12m probe erro', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-06-01', tz='UTC') & datetime<as.POSIXct('2019-06-02', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'June 2019 12m probe erro', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

L1_2019 <- L1_2019 %>% 
  mutate(temp_C_12m = case_when(datetime>=as.POSIXct('2019-05-11 3:10', tz='UTC') &
                                  datetime<as.POSIXct('2019-05-22 5:00', tz='UTC') ~ NA_real_,
                                datetime>=as.POSIXct('2019-05-27 11:50', tz='UTC') &
                                  datetime<as.POSIXct('2019-06-01 19:40', tz='UTC') ~ NA_real_,
                                TRUE ~ temp_C_12m))
buoy_therm_vert_L1 <- L1_2019 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-05-01', tz='UTC') & datetime<as.POSIXct('2019-06-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'May 2019, clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))


#June
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-06-01', tz='UTC') & datetime<as.POSIXct('2019-07-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'June 2019 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#buoy visit Jun 19
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-06-19', tz='UTC') & datetime<as.POSIXct('2019-06-20', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'June 2019 buoy visit - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct('2019-06-19 11:00', tz='UTC') &
                             datetime < as.POSIXct('2019-06-19 11:40', tz='UTC')~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2019 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-06-01', tz='UTC') & datetime<as.POSIXct('2019-07-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'June 2019 thermister data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#July
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-07-01', tz='UTC') & datetime<as.POSIXct('2019-08-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2019 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#July 31 visit
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-07-31', tz='UTC') & datetime<as.POSIXct('2019-08-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2019 buoy visit - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct('2019-07-31 13:30', tz='UTC') &
                             datetime < as.POSIXct('2019-07-31 13:50', tz='UTC')~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2019 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-07-01', tz='UTC') & datetime<as.POSIXct('2019-08-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'July 2019 thermister data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#August
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-08-01', tz='UTC') & datetime<as.POSIXct('2019-09-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Aug 2019 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#September
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-09-01', tz='UTC') & datetime<as.POSIXct('2019-10-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Sept 2019 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#Oct
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-10-01', tz='UTC') & datetime<as.POSIXct('2019-11-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Oct 2019 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#Nov
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-11-01', tz='UTC') & datetime<as.POSIXct('2019-12-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2019 thermister data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

#buoy removed Nov 17
ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-11-17', tz='UTC') & datetime<as.POSIXct('2019-11-18', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2019 therm', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct('2019-11-17 9:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1 <- L1_2019 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2019-11-01', tz='UTC') & datetime<as.POSIXct('2019-12-01', tz='UTC'))),
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = 'Nov 2019 therm clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

ggplot(buoy_therm_vert_L1,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = '2019 therm clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

rm(buoy_therm_vert_L1)

#### do ####
buoy_do_vert_L1 <- L1_2019 %>% 
  select(datetime, do) %>% 
  gather(variable, value, -datetime) %>% 
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

ggplot(buoy_do_vert_L1, aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = '2019 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()

#may
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-05-01', tz='UTC') & datetime<as.POSIXct('2019-06-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2019 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#may 9 deploy
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-05-09', tz='UTC') & datetime<as.POSIXct('2019-05-10', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2019 deploy - do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()


L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do),
            funs(case_when(datetime<as.POSIXct('2019-05-09 12:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime >= as.POSIXct('2019-05-09 12:30', tz='UTC') &
                             datetime < as.POSIXct('2019-05-09 13:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2019 %>% 
  select(datetime, do) %>% 
  gather(variable, value, -datetime) %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-05-01', tz='UTC') & datetime<as.POSIXct('2019-06-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'May 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-06-01', tz='UTC') & datetime<as.POSIXct('2019-07-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#June 5 errat at 1m
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-06-05', tz='UTC') & datetime<as.POSIXct('2019-06-06', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()
L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m),
            funs(case_when(datetime == as.POSIXct('2019-06-05 11:20', tz='UTC')  ~ NA_real_,
                           TRUE ~ .)))
#odd behavior at 1m 6-14
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-06-14 12:00', tz='UTC') & datetime<as.POSIXct('2019-06-15 12:00', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

#june 19
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-06-19', tz='UTC') & datetime<as.POSIXct('2019-06-20', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m, dotemp_C_32m),
            funs(case_when(datetime>=as.POSIXct('2019-06-19 11:00', tz='UTC') &
                             datetime < as.POSIXct('2019-06-19 11:40', tz='UTC')~ NA_real_,
                           TRUE ~ .))) 

#odd point on 26th at 1m
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-06-26', tz='UTC') & datetime<as.POSIXct('2019-06-27', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m),
            funs(case_when(datetime==as.POSIXct('2019-06-26 11:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 


buoy_do_vert_L1 <- L1_2019 %>% 
  select(datetime, do) %>% 
  gather(variable, value, -datetime) %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-06-01', tz='UTC') & datetime<as.POSIXct('2019-07-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'June 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#july
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-07-01', tz='UTC') & datetime<as.POSIXct('2019-08-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#jul 18 1m
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-07-18', tz='UTC') & datetime<as.POSIXct('2019-07-19', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()


L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, dotemp_C_1m),
            funs(case_when(datetime==as.POSIXct('2019-07-18 14:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))

#jul 31
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-07-31', tz='UTC') & datetime<as.POSIXct('2019-08-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

#14.5 replaced on august 28 - but has some errant data between.
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-08-28', tz='UTC') & datetime<as.POSIXct('2019-08-29', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'August 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()


L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m, dotemp_C_14.5m),
            funs(case_when(datetime>=as.POSIXct('2019-07-31 12:10', tz='UTC') &
                             datetime < as.POSIXct('2019-08-28 11:50', tz='UTC')~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_32m, do_sat_pct_32m, dotemp_C_32m),
            funs(case_when(datetime>=as.POSIXct('2019-07-31 13:30', tz='UTC') &
                             datetime < as.POSIXct('2019-07-31 14:00', tz='UTC')~ NA_real_,
                           TRUE ~ .)))

buoy_do_vert_L1 <- L1_2019 %>% 
  select(datetime, do) %>% 
  gather(variable, value, -datetime) %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-07-01', tz='UTC') & datetime<as.POSIXct('2019-08-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'July 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#august
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-08-01', tz='UTC') & datetime<as.POSIXct('2019-09-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'August 2019 do data - NA removed, L0.5', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#aug 28 14m replacement
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-08-28', tz='UTC') & datetime<as.POSIXct('2019-08-29', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'August 2019 do data - NA removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()


L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, dotemp_C_1m),
            funs(case_when(datetime>=as.POSIXct('2019-08-28 11:10', tz='UTC') &
                             datetime < as.POSIXct('2019-08-28 11:30', tz='UTC')~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1 <- L1_2019 %>% 
  select(datetime, do) %>% 
  gather(variable, value, -datetime) %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-08-01', tz='UTC') & datetime<as.POSIXct('2019-09-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'August 2019 do data - clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#september
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-09-01', tz='UTC') & datetime<as.POSIXct('2019-10-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'September 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#october
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-10-01', tz='UTC') & datetime<as.POSIXct('2019-11-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'October 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#november
ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-11-01', tz='UTC') & datetime<as.POSIXct('2019-12-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'November 2019 do data - NAs removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-11-17', tz='UTC') & datetime<as.POSIXct('2019-11-18', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'November 2019 do data - NA removed', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

#buoy move nov 17
L1_2019 <- L1_2019 %>% 
  mutate_at(vars(do),
            funs(case_when(datetime>=as.POSIXct('2019-11-17 9:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1 <- L1_2019 %>% 
  select(datetime, do) %>% 
  gather(variable, value, -datetime) %>% 
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

ggplot(subset(buoy_do_vert_L1, subset=(datetime>=as.POSIXct('2019-11-01', tz='UTC') & datetime<as.POSIXct('2019-12-01', tz='UTC'))), 
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'November 2019 do data - CLEAN', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()


ggplot(buoy_do_vert_L1, aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = '2019 do data - CLEAN', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()

write_csv(L1_2019, 'C:/Users/steeleb/Dropbox/Lake Auburn buoy/data/L1 data/buoy_L1_2019.csv')
