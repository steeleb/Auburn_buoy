#######################################################
# File: buoy_2015.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 22Jan2018                             #
# Purpose: to create a L1 dataset from the raw 2015   #
#         Auburn buoy data                            #
# R version: 3.4.3                                    #
#######################################################

L0_2015 <- read_csv("C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/Lake_Auburn-SDL500R-11-20-2015_00-27.csv", 
                    skip=3, 
                    col_names = varnames2015) %>% 
  mutate(datetime = as.POSIXct(strptime(datetime, "%m/%d/%Y %H:%M", tz='EST')))

#warnings okay

#reshape and plot to see NA values
L0_2015_vert <- L0_2015 %>%
  select(-bat_v) %>%
  gather(sensor, value, -datetime) %>%
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
#   ggplot(aes(x=datetime, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Raw Buoy data 2015') +
#   final_theme

#recode NA values as NA
L1_2015 <- L0_2015 %>% 
  mutate_at(vars(temp_C_0.5m:temp_C_30m),
            funs(case_when(. == -100000 ~ NA_real_, 
                           TRUE ~ .))) 

L1_2015_vert <- L1_2015 %>%
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

# L1_2015_vert %>%
#   ggplot(aes(x=datetime, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Buoy data 2015 - NAs recoded') +
#   final_theme

####L1 data cleaning####
#THERMISTERS
buoy_therm_vert_L1 <- L1_2015 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(buoy_therm_vert_L1,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = '2015 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-05-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-06-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'May 2015 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# #May 7 buoy deployment
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-05-07', tz='EST') & 
#                                             datetime<as.POSIXct('2015-05-08', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'May 2015 buoy deployment', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime<as.POSIXct('2015-05-07 10:10', tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2015 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2015-05-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-06-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'May 2015 clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-06-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-07-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'June 2015 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# #June 18 buoy visit
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-06-18', tz='EST') & 
#                                             datetime<as.POSIXct('2015-06-19', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'June 2015 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime==as.POSIXct('2015-06-18 10:30', tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2015 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2015-06-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-07-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'June 2015 clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-07-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-08-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'July 2015 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# #July 2 buoy visit
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-07-02', tz='EST') & 
#                                             datetime<as.POSIXct('2015-07-03', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'July 2015 buoy visit', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# #July 16
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-07-16', tz='EST') & 
#                                             datetime<as.POSIXct('2015-07-17', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'July 2015 buoy visit', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# #July 23
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-07-23', tz='EST') & 
#                                             datetime<as.POSIXct('2015-07-24', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'July 2015 buoy visit', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# #July 30
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-07-30', tz='EST') & 
#                                             datetime<as.POSIXct('2015-07-31', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'July 2015 buoy visit', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct('2015-07-02 9:20', tz='EST')  &
                             datetime<as.POSIXct('2015-07-02 10:00', tz='EST')~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime==as.POSIXct('2015-07-16 10:20', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct('2015-07-23 10:30', tz='EST') &
                             datetime<as.POSIXct('2015-07-23 10:50', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct('2015-07-30 8:50', tz='EST') &
                             datetime<as.POSIXct('2015-07-30 10:10', tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2015 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2015-07-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-08-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'July 2015 clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-08-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-09-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug 2015 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-09-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-10-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Sept 2015 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-10-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-11-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2015 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-11-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-12-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2015 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# #buoy removal nov 18
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2015-11-18', tz='EST') & 
#                                             datetime<as.POSIXct('2015-11-19', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2015 buoy removal', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct('2015-11-18 8:00', tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2015 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2015-11-01', tz='EST') & 
#                                             datetime<as.POSIXct('2015-12-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2015 clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(buoy_therm_vert_L1_b, 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = '2015 temp clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))

rm(buoy_therm_vert_L1, buoy_therm_vert_L1_b)

#DO
buoy_do_vert_L1 <- L1_2015 %>% 
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

# ggplot(buoy_do_vert_L1, aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = '2015 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-05-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-06-01', tz='EST'))),
#               aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'May 2015 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# #buoy deployed May 07
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-05-07', tz='EST') &
#                                          datetime < as.POSIXct('2015-05-08', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'May 2015 buoy deployed') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(dotemp_C_1m, dotemp_C_14.5m, dotemp_C_32m),
            funs(case_when(datetime<as.POSIXct('2015-05-07 10:10', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_1m, do_ppm_14.5m, do_ppm_32m, do_sat_pct_1m, do_sat_pct_14.5m, do_sat_pct_32m),
            funs(case_when(datetime<as.POSIXct('2015-05-07 14:00', tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1_b <- L1_2015 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2015-05-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-06-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'May 2015 clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-06-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-07-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'June 2015 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# # Jun 4 do clean
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-06-04', tz='EST') &
#                                          datetime < as.POSIXct('2015-06-05', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'June 2015 buoy visit') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()
# 
# #jun 19 cleaning
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-06-18', tz='EST') &
#                                          datetime < as.POSIXct('2015-06-19', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'June 2015 buoy visit') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()
# 
# #jun 25 cleaning
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-06-25', tz='EST') &
#                                          datetime < as.POSIXct('2015-06-26', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'June 2015 buoy visit') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(dotemp_C_1m, dotemp_C_14.5m, do_ppm_1m, do_ppm_14.5m, do_sat_pct_1m, do_sat_pct_14.5m),
            funs(case_when(datetime==as.POSIXct('2015-06-04 10:10', tz='EST') ~ NA_real_,
                           datetime>=as.POSIXct('2015-06-18 10:30', tz='EST') &
                             datetime<as.POSIXct('2015-06-18 10:50', tz='EST') ~ NA_real_,
                           datetime==as.POSIXct('2015-06-25 9:10', tz='EST') ~ NA_real_,
                           TRUE ~ .))) 
  
buoy_do_vert_L1_b <- L1_2015 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2015-06-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-07-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'June 2015 clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-07-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-08-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'July 2015 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# #Jul 2 recalibration
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-07-02', tz='EST') &
#                                          datetime < as.POSIXct('2015-07-03', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'July 2015 buoy visit') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()
# #Jul 30 recalibration
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-07-30', tz='EST') &
#                                          datetime < as.POSIXct('2015-07-31', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'July 2015 buoy visit') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()


L1_2015 <- L1_2015 %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime>=as.POSIXct('2015-07-02 8:20', tz='EST') &
                             datetime<as.POSIXct('2015-07-02 9:20', tz='EST') ~ NA_real_,
                           datetime>=as.POSIXct('2015-07-30 8:50', tz='EST') &
                             datetime<as.POSIXct('2015-07-30 10:00', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(dotemp_C_32m, do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime>=as.POSIXct('2015-07-02 9:20', tz='EST') &
                             datetime<as.POSIXct('2015-07-02 9:50', tz='EST') ~ NA_real_,
                           datetime>=as.POSIXct('2015-07-30 9:00', tz='EST') &
                             datetime<as.POSIXct('2015-07-30 10:00', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime>=as.POSIXct('2015-07-02', tz='EST') &
                             datetime<as.POSIXct('2015-07-02', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate_at(vars(do_ppm_1m, do_sat_pct_1m, do_ppm_14.5m, do_sat_pct_14.5m, do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime>=as.POSIXct('2015-07-02 9:20', tz='EST') &
                             datetime<as.POSIXct('2015-07-02 13:50', tz='EST') ~ NA_real_,
                           datetime>=as.POSIXct('2015-07-30 10:00', tz='EST') &
                             datetime<as.POSIXct('2015-07-30 14:00', tz='EST') ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1_b <- L1_2015 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2015-07-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-08-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'July 2015 clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-08-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-09-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Aug 2015 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# #do clean Aug 28
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-08-28', tz='EST') &
#                                          datetime < as.POSIXct('2015-08-29', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'July 2015 buoy visit') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime==as.POSIXct('2015-08-28 12:00', tz='EST') ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1_b <- L1_2015 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2015-08-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-09-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Aug 2015 clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-09-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-10-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Sept 2015 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-10-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-11-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Oct 2015 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-11-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-12-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Nov 2015 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# #Nov 18 buoy removed
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2015-11-18', tz='EST') &
#                                          datetime < as.POSIXct('2015-11-19', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Nov 2015 buoy removed') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2015 <- L1_2015 %>% 
  mutate_at(vars(do),
            funs(case_when(datetime>=as.POSIXct('2015-11-18 8:00', tz='EST') ~ NA_real_,
                           TRUE ~ .))) 
buoy_do_vert_L1_b <- L1_2015 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2015-11-01', tz='EST') &
#                                          datetime < as.POSIXct('2015-12-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Nov 2015 clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(buoy_do_vert_L1_b,
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = '2015 do data - clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_colorblind()

rm(buoy_do_vert_L1, buoy_do_vert_L1_b, L0_2015_vert, L1_2015_vert)

# write_csv(L1_2015, 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2015.csv')



