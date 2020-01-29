#######################################################
# File: buoy_2018.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 02Feb2019                             #
# Purpose: to create a L1 dataset from the raw 2018   #
#         Auburn buoy data                            #
# R version: 3.4.3                                    #
#######################################################


L0_2018 <- read_csv('C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/2018data.csv',
                    col_names = varnames2018,
                    col_types = 'cnnnnnnnnnnnnnnnnnnnnn',
                    skip=4) %>% 
  mutate(datetime = as.POSIXct(datetime, tz='UTC', format='%m/%d/%Y %I:%M:%S %p'))

str(L0_2018)

# L0_2018_vert <- L0_2018 %>%
#   select(-bat_v) %>%
#   gather(sensor, value, -datetime) %>%
#   mutate(depth = as.numeric(''),
#          sensor_type = as.character(''),
#          sensor_class = as.character(''),
#          depth = case_when(sensor == 'do_ppm_1m' ~ 1,
#                            sensor == 'dotemp_C_1m' ~ 1,
#                            sensor == 'do_sat_pct_1m' ~ 1,
#                            sensor == 'do_ppm_14.5m' ~ 14.5,
#                            sensor == 'dotemp_C_14.5m' ~ 14.5,
#                            sensor == 'do_sat_pct_14.5m' ~ 14.5,
#                            sensor == 'do_ppm_32m' ~ 32,
#                            sensor == 'dotemp_C_32m' ~ 32,
#                            sensor == 'do_sat_pct_32m' ~ 32,
#                            sensor == 'temp_C_0.5m' ~ 0.5,
#                            sensor == 'temp_C_1m' ~ 1,
#                            sensor == 'temp_C_2m' ~ 2,
#                            sensor == 'temp_C_4m' ~ 4,
#                            sensor == 'temp_C_6m' ~ 6,
#                            sensor == 'temp_C_8m' ~ 8,
#                            sensor == 'temp_C_10m' ~ 10,
#                            sensor == 'temp_C_12m' ~ 12,
#                            sensor == 'temp_C_16m' ~ 16,
#                            sensor == 'temp_C_22m' ~ 22,
#                            sensor == 'temp_C_30m' ~ 30,
#                            TRUE ~ NA_real_),
#          sensor_type = case_when(grepl('do', sensor) ~ 'do probe',
#                                  grepl('temp', sensor) ~ 'thermister'),
#          sensor_class = case_when(grepl('dotemp', sensor) ~ 'temp',
#                                   grepl('temp', sensor) ~ 'temp',
#                                   TRUE ~ 'do'))
# 
# L0_2018_vert %>%
#   ggplot(aes(x=datetime, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Raw Buoy data 2018') +
#   final_theme

#recode NA values as NA
L1_2018 <- L0_2018 %>% 
  mutate_at(vars(do_ppm_1m:temp_C_30m),
            funs(case_when(. < -99999.9 ~ NA_real_, # for -99999.99 and -100000 - had to do it this way because there were issues with -99999.99
                           TRUE ~ .)))

# L1_2018_vert <- L1_2018 %>%
#   select(-bat_v) %>%
#   gather(sensor, value, -datetime) %>%
#   mutate(depth = as.numeric(''),
#          sensor_type = as.character(''),
#          sensor_class = as.character(''),
#          sensor_unit = as.character(''),
#          depth = case_when(sensor == 'do_ppm_1m' ~ 1,
#                            sensor == 'dotemp_C_1m' ~ 1,
#                            sensor == 'do_sat_pct_1m' ~ 1,
#                            sensor == 'do_ppm_14.5m' ~ 14.5,
#                            sensor == 'dotemp_C_14.5m' ~ 14.5,
#                            sensor == 'do_sat_pct_14.5m' ~ 14.5,
#                            sensor == 'do_ppm_32m' ~ 32,
#                            sensor == 'dotemp_C_32m' ~ 32,
#                            sensor == 'do_sat_pct_32m' ~ 32,
#                            sensor == 'temp_C_0.5m' ~ 0.5,
#                            sensor == 'temp_C_1m' ~ 1,
#                            sensor == 'temp_C_2m' ~ 2,
#                            sensor == 'temp_C_4m' ~ 4,
#                            sensor == 'temp_C_6m' ~ 6,
#                            sensor == 'temp_C_8m' ~ 8,
#                            sensor == 'temp_C_10m' ~ 10,
#                            sensor == 'temp_C_12m' ~ 12,
#                            sensor == 'temp_C_16m' ~ 16,
#                            sensor == 'temp_C_22m' ~ 22,
#                            sensor == 'temp_C_30m' ~ 30,
#                            TRUE ~ NA_real_),
#          sensor_type = case_when(grepl('do', sensor) ~ 'do probe',
#                                  grepl('temp', sensor) ~ 'thermister'),
#          sensor_class = case_when(grepl('dotemp', sensor) ~ 'temp',
#                                   grepl('temp', sensor) ~ 'temp',
#                                   TRUE ~ 'do'),
#          sensor_unit = case_when(grepl('dotemp', sensor) ~ 'deg C',
#                                  grepl('temp', sensor) ~ 'deg C',
#                                  grepl('do_sat', sensor) ~ 'percent',
#                                  grepl('do_ppm', sensor) ~ 'ppm',
#                                  TRUE ~ sensor))
# 
# L1_2018_vert %>%
#   ggplot(aes(x=datetime, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Buoy data 2018 - NAs recoded') +
#   final_theme


####L1 Cleaning####

#THERMISTERS
buoy_therm_vert_L1 <- L1_2018 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(buoy_therm_vert_L1, aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = '2018 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
#12m is wrong. thought it was in farenheit, but that's not correct either.
L1_2018 <- L1_2018 %>% 
  mutate(temp_C_12m = NA_real_)
buoy_therm_vert_L1_b <- L1_2018 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

ggplot(buoy_therm_vert_L1_b, aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = '2018 thermister data - 12m recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))


# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-05-01', tz='UTC') & datetime<as.POSIXct('2018-06-01', tz='UTC'))),
#               aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'May 2018 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
# #May 09 - buoy deployed
# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-05-10', tz='UTC') & datetime<as.POSIXct('2018-05-11', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'May 2018 buoy deploy', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime<as.POSIXct('2018-05-10 9:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2018 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-05-01', tz='UTC') & datetime<as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'May 2018, clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-06-01', tz='UTC') & datetime<as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'June 2018 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-07-01', tz='UTC') & datetime<as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'July 2018 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-08-01', tz='UTC') & datetime<as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug 2018 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
# #buoy visit Aug 29
# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-08-29', tz='UTC') & datetime<as.POSIXct('2018-08-30', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug 2018 buoy visit', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct('2018-08-29 11:20', tz='UTC')  & datetime < as.POSIXct('2018-08-29 12:30')~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2018 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-08-29', tz='UTC') & datetime<as.POSIXct('2018-08-30', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug 2018 buoy visit - clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-08-01', tz='UTC') & datetime<as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug 2018 thermister data - l1', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
# 
# 
# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-09-01', tz='UTC') & datetime<as.POSIXct('2018-10-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Sept 2018 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-10-01', tz='UTC') & datetime<as.POSIXct('2018-11-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2018 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-11-01', tz='UTC') & datetime<as.POSIXct('2018-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2018 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))
# 
# #buoy removed Nov 15
# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-11-15', tz='UTC') & datetime<as.POSIXct('2018-11-16', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2018 therm', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>as.POSIXct('2018-11-15 10:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2018 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2018-11-01', tz='UTC') & datetime<as.POSIXct('2018-12-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2018 therm clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
#                               "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

ggplot(buoy_therm_vert_L1_b,
       aes(x=datetime, y=value, color=variable)) +
  geom_point() +
  labs(title = '2018 therm clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#a5b8f3",
                              "#00664b", "#00e639", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc"))

rm(buoy_therm_vert_L1, buoy_therm_vert_L1_b)

#do
buoy_do_vert_L1 <- L1_2018 %>% 
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
  labs(title = '2018 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()

# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2018-05-01', tz='UTC') &
#                                          datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#               aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'May 2018 do data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# #May 9 - buoy deployed, May 10 corrected
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2018-05-10', tz='UTC') &
#                                          datetime < as.POSIXct('2018-05-11', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'May 2018 buoy deployment', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, dotemp_C_32m, do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime<as.POSIXct('2018-05-10 9:00', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime<as.POSIXct('2018-05-10 10:00', tz='UTC') ~ NA_real_,
                                    TRUE ~ .)))
buoy_do_vert_L1_b <- L1_2018 %>% 
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

# #may 20 stray point in 14.5
# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2018-05-20', tz='UTC') &
#                                          datetime < as.POSIXct('2018-05-21', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'May 2018 do data', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime==as.POSIXct('2018-05-20 20:40', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1_b <- L1_2018 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2018-05-01', tz='UTC') &
#                                          datetime < as.POSIXct('2018-06-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'May 2018, clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2018-06-01', tz='UTC') &
#                                          datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'June 2018 do data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()

#strays  on jun 6 in 14.5 and jun20 in do

# #June 6 
# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2018-06-06', tz='UTC') &
#                                          datetime < as.POSIXct('2018-06-07', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'June 2018 do data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()
# 
# #June 20
# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2018-06-20', tz='UTC') &
#                                            datetime < as.POSIXct('2018-06-21', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'June 2018 do data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime>=as.POSIXct('2018-06-06 10:40', tz='UTC') & datetime < as.POSIXct('2018-06-06 12:20', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(dotemp_C_14.5m = case_when(datetime==as.POSIXct('2018-06-06 10:40', tz='UTC') ~ NA_real_,
                                    TRUE ~ dotemp_C_14.5m)) %>% 
  mutate_at(vars(do),
            funs(case_when(datetime == as.POSIXct('2018-06-20 10:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1_b <- L1_2018 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2018-06-01', tz='UTC') &
#                                          datetime < as.POSIXct('2018-07-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'June 2018 clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2018-07-01', tz='UTC') &
#                                          datetime < as.POSIXct('2018-08-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'July 2018 do data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2018-08-01', tz='UTC') &
#                                          datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Aug 2018 do data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# #Aug 08 do clean
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2018-08-08', tz='UTC') &
#                                          datetime < as.POSIXct('2018-08-09', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Aug 2018 do', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, dotemp_C_1m),
            funs(case_when(datetime>=as.POSIXct('2018-08-08 11:00', tz='UTC') & 
                             datetime<as.POSIXct('2018-08-08 11:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(dotemp_C_14.5m = case_when(datetime >= as.POSIXct('2018-08-08 11:00', tz='UTC') &
                             datetime < as.POSIXct('2018-08-08 12:00', tz='UTC') ~ NA_real_,
                             TRUE ~ dotemp_C_14.5m)) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime >= as.POSIXct('2018-08-08 11:00', tz='UTC') &
                             datetime < as.POSIXct('2018-08-08 13:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1_b <- L1_2018 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2018-08-29', tz='UTC') &
#                                          datetime < as.POSIXct('2018-08-30', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Aug 2018', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, dotemp_C_1m),
            funs(case_when(datetime>=as.POSIXct('2018-08-29 11:10', tz='UTC') & 
                             datetime<as.POSIXct('2018-08-29 11:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(dotemp_C_14.5m =case_when(datetime >= as.POSIXct('2018-08-29 11:10', tz='UTC') &
                             datetime < as.POSIXct('2018-08-29 12:50', tz='UTC') ~ NA_real_,
                           TRUE ~ dotemp_C_14.5m)) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime >= as.POSIXct('2018-08-29 11:10', tz='UTC') &
                             datetime < as.POSIXct('2018-08-29 14:10', tz='UTC') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(dotemp_C_32m, do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime >= as.POSIXct('2018-08-29 11:10', tz='UTC') &
                             datetime < as.POSIXct('2018-08-29 12:30', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
              
buoy_do_vert_L1_b <- L1_2018 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2018-08-01', tz='UTC') &
#                                            datetime < as.POSIXct('2018-09-01', tz='UTC'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Aug 2018 clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()

ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2018-09-01', tz='UTC') &
                                         datetime < as.POSIXct('2018-10-01', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Sept 2018 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2018-10-01', tz='UTC') &
                                         datetime < as.POSIXct('2018-11-01', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Oct 2018 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2018-11-01', tz='UTC') &
                                         datetime < as.POSIXct('2018-12-01', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Nov 2018 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

#buoy removed Nov 15
ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2018-11-15', tz='UTC') &
                                         datetime < as.POSIXct('2018-11-16', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Nov 2018 do data - NAs recoded', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 hour') +
  scale_color_colorblind()

L1_2018 <- L1_2018 %>% 
  mutate_at(vars(do),
            funs(case_when(datetime>=as.POSIXct('2018-11-15 10:50', tz='UTC') ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1_b <- L1_2018 %>% 
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

ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2018-11-01', tz='UTC') &
                                         datetime < as.POSIXct('2018-12-01', tz='UTC'))),
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = 'Nov 2018 do clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 day') +
  scale_color_colorblind()

ggplot(buoy_do_vert_L1_b,
       aes(x=datetime, y=value, color=as.factor(depth))) +
  geom_point() +
  facet_grid(sensor ~ ., scales='free_y') +
  labs(title = '2018 do data clean', y='temp (deg C)') +
  final_theme +
  scale_x_datetime(date_minor_breaks = '1 month') +
  scale_color_colorblind()

rm(buoy_do_vert_L1, buoy_do_vert_L1_b)

write_csv(L1_2018, 'C:/Users/steeleb/Dropbox/Lake Auburn buoy/data/L1 data/buoy_L1_2018.csv')
