#######################################################
# File: buoy_2013.R                                   #
# Written by: B. Steele (steeleb@caryinstitute.org)   #
# File created: 22Jan2018                             #
# Purpose: to create a L1 dataset from the raw 2013   #
#         Auburn buoy data                            #
# R version: 3.4.3                                    #
#######################################################

# July-Nov2013
L0_2013a <- read_xls("C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/BatesDataSonde_hae.xls", 
                     sheet="raw data", 
                     skip=1, 
                     col_names=varnames2016) %>% 
  mutate(datetime = as.POSIXct(strptime(datetime, "%Y-%m-%d %H:%M:%S", tz='EST')))

# Nov2013
L0_2013b <- read_csv("C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/raw_data/buoy/raw_files/batescollege_11-04-2013_11-24-2013.csv", 
                     skip=3, 
                     col_names = varnames2016) %>% 
  mutate(datetime = as.POSIXct(strptime(datetime, "%m/%d/%Y %H:%M", tz='EST'))) %>% 
  filter(datetime>as.POSIXct("2013-11-17 01:50:00", tz='EST'))
#warnings okay

#merge to a single L0 file
L0_2013 <- full_join(L0_2013a, L0_2013b)

#clean up work space
rm(L0_2013a, L0_2013b)

#reshape and plot to see NA values
L0_2013_vert <- L0_2013 %>%
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

# L0_2013_vert %>%
#   ggplot(aes(x=datetime, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Raw Buoy data 2013') +
#   final_theme

#recode NA values as NA
L1_2013 <- L0_2013 %>% 
  mutate_at(vars(temp_C_0.5m:temp_C_30m),
            funs(case_when(. == -100000 ~ NA_real_, 
                           TRUE ~ .)))

L1_2013_vert <- L1_2013 %>%
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

# L1_2013_vert %>%
#   ggplot(aes(x=datetime, y=value, color=depth)) +
#   facet_grid(sensor_type ~ ., scales='free_y') +
#   geom_point() +
#   labs(title = 'Buoy data 2013 - NAs recoded') +
#   final_theme

#THERMISTERS
buoy_therm_vert_L1 <- L1_2013 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(buoy_therm_vert_L1, 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = '2013 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-07-01', tz='EST') & datetime<as.POSIXct('2013-08-01', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'July 2013 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# # Jul 25 - buoy deployed
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-07-25', tz='EST') & datetime<as.POSIXct('2013-07-26', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'July 2013 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))

L1_2013 <- L1_2013 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime<as.POSIXct('2013-07-25 10:00', tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2013 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2013-07-01', tz='EST') & datetime<as.POSIXct('2013-08-01', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'July 2013 clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-08-01', tz='EST') & datetime<as.POSIXct('2013-09-01', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug 2013 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# # Aug 15 buoy visit
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-08-15', tz='EST') & datetime<as.POSIXct('2013-08-16', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug 2013 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))
# # Aug 30 buoy visit
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-08-30', tz='EST') & datetime<as.POSIXct('2013-08-31', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Aug 2013 thermister data - NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))

L1_2013 <- L1_2013 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct("2013-08-15 11:20:00", tz='EST') &
                             datetime<as.POSIXct("2013-08-15 11:40:00", tz='EST') ~ NA_real_,
                           datetime>=as.POSIXct("2013-08-30 8:50", tz='EST') &
                             datetime<as.POSIXct("2013-08-30 11:10", tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2013 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-09-01', tz='EST') & datetime<as.POSIXct('2013-10-01', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Sept 2013 clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-10-01', tz='EST') & datetime<as.POSIXct('2013-11-01', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2013 NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# # Oct 4 buoy visit
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-10-04', tz='EST') & datetime<as.POSIXct('2013-10-05', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2013 buoy visit', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# # Oct 18 buoy visit
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-10-18', tz='EST') & datetime<as.POSIXct('2013-10-19', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2013 buoy visit', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))

L1_2013 <- L1_2013 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct("2013-10-04 6:50", tz='EST') &
                             datetime<as.POSIXct("2013-10-04 7:10", tz='EST') ~ NA_real_,
                           datetime>=as.POSIXct("2013-10-18 7:50", tz='EST') &
                             datetime<as.POSIXct("2013-10-18 8:10", tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2013 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2013-10-01', tz='EST') & datetime<as.POSIXct('2013-11-01', tz='EST'))), 
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Oct 2013 clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", 
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711", 
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-11-01', tz='EST') & datetime<as.POSIXct('2013-12-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2013 NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# #buoy removed Nov 21
# ggplot(subset(buoy_therm_vert_L1, subset=(datetime>=as.POSIXct('2013-11-21', tz='EST') & datetime<as.POSIXct('2013-11-22', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2013 NAs recoded', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))

L1_2013 <- L1_2013 %>% 
  mutate_at(vars(therm),
            funs(case_when(datetime>=as.POSIXct("2013-11-21 8:00", tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_therm_vert_L1_b <- L1_2013 %>% 
  select(datetime, therm) %>% 
  gather(variable, value, -datetime) %>% 
  mutate(variable = factor(variable, c(therm)))

# ggplot(subset(buoy_therm_vert_L1_b, subset=(datetime>=as.POSIXct('2013-11-01', tz='EST') & datetime<as.POSIXct('2013-12-01', tz='EST'))),
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = 'Nov 2013 clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))
# 
# ggplot(buoy_therm_vert_L1_b,
#        aes(x=datetime, y=value, color=variable)) +
#   geom_point() +
#   labs(title = '2013 thermisters clean', y='temp (deg C)') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5",
#                               "#a5b8f3", "#00664b", "#00e639", "#8d840c", "#d4c711",
#                               "#f5ee89", "#005180", "#0081cc"))

rm(buoy_therm_vert_L1, buoy_therm_vert_L1_b)

#DO
buoy_do_vert_L1 <- L1_2013 %>% 
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
#   labs(title = '2013 do data - NAs recoded') +
#   final_theme +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-07-01', tz='EST') &
#                                          datetime < as.POSIXct('2013-08-01', tz='EST'))),
#               aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Jul 2013 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# #buoy deployment July 25
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-07-25', tz='EST') &
#                                          datetime < as.POSIXct('2013-07-26', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Jul 2013 buoy deployed') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2013 <- L1_2013 %>% 
  mutate_at(vars(dotemp_C_1m, dotemp_C_14.5m, dotemp_C_32m),
            funs(case_when(datetime<as.POSIXct('2013-07-25 9:50', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_1m, do_ppm_14.5m, do_ppm_32m, do_sat_pct_1m, do_sat_pct_14.5m, do_sat_pct_32m),
            funs(case_when(datetime<as.POSIXct('2013-07-25 14:00', tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1_b <- L1_2013 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2013-07-01', tz='EST') &
#                                          datetime < as.POSIXct('2013-08-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Jul 2013 clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-08-01', tz='EST') &
#                                          datetime < as.POSIXct('2013-09-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Aug 2013 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# # Aug 15 buoy visit
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-08-15', tz='EST') &
#                                          datetime < as.POSIXct('2013-08-16', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Aug 2013 buoy visit') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()
# # Aug 30 buoy visit
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-08-30', tz='EST') &
#                                          datetime < as.POSIXct('2013-08-31', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Aug 2013 buoy visit') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2013 <- L1_2013 %>%
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime>=as.POSIXct('2013-08-15 8:40', tz='EST') &
                             datetime<as.POSIXct('2013-08-15 10:00', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate_at(vars(dotemp_C_32m, do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime==as.POSIXct('2013-08-15 11:20', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime>=as.POSIXct('2013-08-15 10:00', tz='EST') &
                             datetime<as.POSIXct('2013-08-15 14:00', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>%
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, do_ppm_14.5m, do_sat_pct_14.5m, dotemp_C_32m, do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime>=as.POSIXct('2013-08-30 8:39', tz='EST') &
                             datetime<as.POSIXct('2013-08-30 9:50', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate(dotemp_C_32m = case_when(datetime>=as.POSIXct('2013-08-30 9:50', tz='EST') &
                                    datetime<as.POSIXct('2013-08-30 11:10', tz='EST') ~ NA_real_,
                                  TRUE ~ dotemp_C_32m)) %>% 
  mutate_at(vars(do_ppm_1m, do_ppm_14.5m, do_ppm_32m, do_sat_pct_1m, do_sat_pct_14.5m, do_sat_pct_32m),
            funs(case_when(datetime>=as.POSIXct('2013-08-30 8:40', tz='EST') &
                             datetime<as.POSIXct('2013-08-30 9:50', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m, do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime>=as.POSIXct('2013-08-30 9:50', tz='EST') &
                             datetime<as.POSIXct('2013-08-30 12:30', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime>=as.POSIXct('2013-08-30 12:30', tz='EST') &
                             datetime<as.POSIXct('2013-08-30 13:40', tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1_b <- L1_2013 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2013-08-01', tz='EST') &
#                                          datetime < as.POSIXct('2013-09-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Aug 2013 clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-09-01', tz='EST') &
#                                          datetime < as.POSIXct('2013-10-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Sept 2013 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-10-01', tz='EST') &
#                                          datetime < as.POSIXct('2013-11-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Oct 2013 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# #Oct 4 buoy visit
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-10-04', tz='EST') &
#                                          datetime < as.POSIXct('2013-10-05', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Oct 2013 buoy visit') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()
# 
# #Oct 18 buoy visit
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-10-18', tz='EST') &
#                                          datetime < as.POSIXct('2013-10-19', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Oct 2013 buoy visit') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2013 <- L1_2013 %>%
  mutate_at(vars(dotemp_C_1m, do_ppm_1m, do_sat_pct_1m, dotemp_C_14.5m, dotemp_C_32m),
            funs(case_when(datetime>=as.POSIXct('2013-10-04 6:40', tz='EST') &
                             datetime<as.POSIXct('2013-10-04 7:10', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m, do_ppm_32m, do_sat_pct_32m),
            funs(case_when(datetime>=as.POSIXct('2013-10-04 6:50', tz='EST') &
                             datetime<as.POSIXct('2013-10-04 8:40', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(do_ppm_14.5m, do_sat_pct_14.5m),
            funs(case_when(datetime>=as.POSIXct('2013-10-04 8:40', tz='EST') &
                             datetime<as.POSIXct('2013-10-04 11:00', tz='EST') ~ NA_real_,
                           TRUE ~ .))) %>% 
  mutate_at(vars(dotemp_C_1m, dotemp_C_14.5m, dotemp_C_32m),
            funs(case_when(datetime>=as.POSIXct('2013-10-18 7:40', tz='EST') &
                           datetime<as.POSIXct('2013-10-18 8:10', tz='EST') ~ NA_real_,
                         TRUE ~ .)))
buoy_do_vert_L1_b <- L1_2013 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2013-10-01', tz='EST') &
#                                          datetime < as.POSIXct('2013-11-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Oct 2013 clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-11-01', tz='EST') &
#                                          datetime < as.POSIXct('2013-12-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Nov 2013 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# # do sensor error Nov 21 and beyond
# ggplot(subset(buoy_do_vert_L1, subset=(datetime >=as.POSIXct('2013-11-21', tz='EST') &
#                                          datetime < as.POSIXct('2013-11-22', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Sept 2013 do data - NAs recoded') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 hour') +
#   scale_color_colorblind()

L1_2013 <- L1_2013 %>%
  mutate_at(vars(do),
            funs(case_when(datetime>=as.POSIXct('2013-11-21 8:00', tz='EST') ~ NA_real_,
                           TRUE ~ .)))
buoy_do_vert_L1_b <- L1_2013 %>% 
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

# ggplot(subset(buoy_do_vert_L1_b, subset=(datetime >=as.POSIXct('2013-11-01', tz='EST') &
#                                          datetime < as.POSIXct('2013-12-01', tz='EST'))),
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = 'Nov 2013 clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 day') +
#   scale_color_colorblind()
# 
# ggplot(buoy_do_vert_L1_b,
#        aes(x=datetime, y=value, color=as.factor(depth))) +
#   geom_point() +
#   facet_grid(sensor ~ ., scales='free_y') +
#   labs(title = '2013 do clean') +
#   final_theme +
#   scale_x_datetime(date_minor_breaks = '1 month') +
#   scale_color_colorblind()

rm(buoy_do_vert_L1, buoy_do_vert_L1_b, L0_2013_vert, L1_2013_vert)

# write_csv(L1_2013, 'C:/Users/steeleb/Dropbox/Lake Auburn Buoy/data/L1 data/buoy_L1_2013.csv')
