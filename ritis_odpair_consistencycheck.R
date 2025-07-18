# read data and merge together

readodpair = function(path, filename) {
  # Skip first two lines
  df <- read.csv(paste0(path, filename, '.csv'), skip = 2, header = T)
  if (any(colnames(df) == 'Trips')) {
    if (typeof(df$Trips) == 'character') {
      df$Trips = as.numeric(gsub(',','',df$Trips))
    }
  }

  return(df)
}

readgeojson = function(path, jsonname) {
  library(sf)
  # Read the GeoJSON file
  geo_data <- st_read(paste0(path, '\\', jsonname, '.geojson'))
  return(geo_data)
}

tottrips = function(odpair, filter, o_sel, d_sel) {
  if (typeof(odpair$Trips) == 'character') {
    odpair$Trips = as.numeric(gsub(",", "", odpair$Trips))
  }
  if (filter == 0) {
    trips = sum(odpair$Trips)
  }
  else if (filter == 1) {
    trips = sum(odpair$Trips[which(odpair$Origin.Source == o_sel & odpair$Destination.Source == d_sel)])
  }
  return(trips)
}


### this file specifies the matching of npmrds and roadlink
library('arcgisbinding')
library('sf')
library(dplyr)
library(tidyr)
library('openxlsx')
library(geosphere)
library('DBI')
library('odbc')
library('ggplot2')
library(leaflet)

setwd('C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment\\20250715_triptable\\querydata_2019')

## daily workday for march and may
march2019_nopath = list(
  Monday = readodpair('','NCTCOG_TAZ_201903_Monday_6-10_LDV_NoPath-pairs'),
  Tuesday = readodpair('','NCTCOG_TAZ_201903_Tuesday_6-10_LDV_NoPath-pairs'),
  Wednesday = readodpair('','NCTCOG_TAZ_201903_Wednesday_6-10_LDV_NoPath-pairs'),
  Thursday = readodpair('','NCTCOG_TAZ_201903_Thursday_6-10_LDV_NoPath-pairs'),
  Friday = readodpair('','NCTCOG_TAZ_201903_Friday_6-10_LDV_NoPath-pairs'),
)

# five Fri, four Mon-Thu
march2019_nopath$Totaltrip = data.frame(DOW = c("1.Mon",'2.Tue','3.Wed','4.Thu','5.Fri'),
                                        Tottrip = c(tottrips(march2019_nopath$Monday, filter = 0)/4,
                                                    tottrips(march2019_nopath$Tuesday, filter = 0)/4,
                                                    tottrips(march2019_nopath$Wednesday, filter = 0)/4,
                                                    tottrips(march2019_nopath$Thursday, filter = 0)/4,
                                                    tottrips(march2019_nopath$Friday, filter = 0)/5))

ggplot(march2019_nopath$Totaltrip, aes(x = DOW, y = Tottrip)) + geom_col(fill = 'white') + theme_black()


may2019_nopath = list(
  Monday = readodpair('', 'NCTCOG_TAZ_201905_Monday_6-10_LDV_NoPath-pairs'),
  Tuesday = readodpair('','NCTCOG_TAZ_201905_Tuesday_6-10_LDV_NoPath-pairs'),
  Wednesday = readodpair('','NCTCOG_TAZ_201905_Wednesday_6-10_LDV_NoPath-pairs'),
  Thursday = readodpair('','NCTCOG_TAZ_201905_Thursday_6-10_LDV_NoPath-pairs'),
  Friday = readodpair('','NCTCOG_TAZ_201905_Friday_6-10_LDV_NoPath-pairs')
)

# five Wed-Friday, four Mon, Tue
may2019_nopath$Totaltrip = data.frame(DOW = c("1.Mon",'2.Tue','3.Wed','4.Thu','5.Fri'), 
                                      Tottrip = c(tottrips(may2019_nopath$Monday, filter = 0)/4,
                                                  tottrips(may2019_nopath$Tuesday, filter = 0)/4,
                                                  tottrips(may2019_nopath$Wednesday, filter = 0)/5,
                                                  tottrips(may2019_nopath$Thursday, filter = 0)/5,
                                                  tottrips(may2019_nopath$Friday, filter = 0)/5))

ggplot(may2019_nopath$Totaltrip, aes(x = DOW, y = Tottrip)) + geom_col(fill = 'white') + theme_black()

## monthly workday
weekday2019_nopath = list(
  April = readodpair('','NCTCOG_TAZ_201904_Workday_6-10_LDV_NoPath-pairs'),
  May = readodpair('','NCTCOG_TAZ_201905_Workday_6-10_LDV_NoPath-pairs'),
  September = readodpair('','NCTCOG_TAZ_201909_Workday_6-10_LDV_NoPath-pairs'),
  October = readodpair('', 'NCTCOG_TAZ_201910_Workday_6-10_LDV_NoPath-pairs')
)

weekday2019_nopath$Totaltrip = data.frame(Mon = c('April','May','September','October'),
                                          Tottrip = c(tottrips(weekday2019_nopath$April, filter = 0),
                                                      tottrips(weekday2019_nopath$May, filter = 0),
                                                      tottrips(weekday2019_nopath$September, filter = 0),
                                                      tottrips(weekday2019_nopath$October, filter = 0)))

weekday2019_nopath$Totaltrip_II = data.frame(Mon = c('April','May','September','October'),
                                             Tottrip = c(tottrips(weekday2019_nopath$April, filter = 1, 'Internal','Internal'),
                                                         tottrips(weekday2019_nopath$May, filter = 1, 'Internal','Internal'),
                                                         tottrips(weekday2019_nopath$September, filter = 1, 'Internal','Internal'),
                                                         tottrips(weekday2019_nopath$October, filter = 1, 'Internal','Internal')))

ggplot(weekday2019_nopath$Totaltrip, aes(x = Mon, y = Tottrip)) + geom_col(fill = 'white') + theme_black()

## why april has much less data than other months
# check number of zones. <-- much less internal
length(unique(weekday2019_nopath$April$Origin.ID))
length(unique(weekday2019_nopath$May$Origin.ID))
length(unique(weekday2019_nopath$April$Destination.ID))
length(unique(weekday2019_nopath$May$Destination.ID))

nrow(weekday2019_nopath$April)
nrow(weekday2019_nopath$May)

mean(weekday2019_nopath$April$Median.Trip.Distance..mi., na.rm = T)
mean(weekday2019_nopath$May$Median.Trip.Distance..mi., na.rm = T)

nrow(weekday2019_nopath$April[which(weekday2019_nopath$April$Origin.Source == 'External' |
                                      weekday2019_nopath$April$Destination.Source == 'External'),])
nrow(weekday2019_nopath$May[which(weekday2019_nopath$May$Origin.Source == 'External' |
                                    weekday2019_nopath$May$Destination.Source == 'External'),])

nrow(weekday2019_nopath$April[which(weekday2019_nopath$April$Origin.Source == 'Internal' &
                                      weekday2019_nopath$April$Destination.Source == 'Internal'),])
nrow(weekday2019_nopath$May[which(weekday2019_nopath$May$Origin.Source == 'Internal' &
                                    weekday2019_nopath$May$Destination.Source == 'Internal'),])

(tottrips(weekday2019_nopath$April, filter = 1, o_sel = 'Internal', d_sel = 'Internal') - 
    tottrips(weekday2019_nopath$May, filter = 1, o_sel = 'Internal', d_sel = 'Internal'))/
  tottrips(weekday2019_nopath$May, filter = 1, o_sel = 'Internal', d_sel = 'Internal')

(tottrips(weekday2019_nopath$April, filter = 1, o_sel = 'Internal', d_sel = 'External') - 
    tottrips(weekday2019_nopath$May, filter = 1, o_sel = 'Internal', d_sel = 'External'))/
  tottrips(weekday2019_nopath$May, filter = 1, o_sel = 'Internal', d_sel = 'External')

(tottrips(weekday2019_nopath$April, filter = 1, d_sel = 'Internal', o_sel = 'External') - 
    tottrips(weekday2019_nopath$May, filter = 1, d_sel = 'Internal', o_sel = 'External'))/
  tottrips(weekday2019_nopath$May, filter = 1, d_sel = 'Internal', o_sel = 'External')

################################# compare internal trip consistency in three months (discard April) ###########
od_internal_month = data.frame(OD = paste(weekday2019_nopath$May$Origin.ID[which(weekday2019_nopath$May$Origin.Source == 'Internal' &
                                                                                   weekday2019_nopath$May$Destination.Source == 'Internal')],
                                          weekday2019_nopath$May$Destination.ID[which(weekday2019_nopath$May$Origin.Source == 'Internal' &
                                                                                        weekday2019_nopath$May$Destination.Source == 'Internal')]), 
                               trips_may = 0, trips_sep = 0, trips_oct = 0)
weekday2019_nopath$May$OD = paste(weekday2019_nopath$May$Origin.ID, weekday2019_nopath$May$Destination.ID)
weekday2019_nopath$September$OD = paste(weekday2019_nopath$September$Origin.ID, weekday2019_nopath$September$Destination.ID)
weekday2019_nopath$October$OD = paste(weekday2019_nopath$October$Origin.ID, weekday2019_nopath$October$Destination.ID)

od_internal_month <- od_internal_month %>%
  left_join(weekday2019_nopath$May %>%
              select(OD, Trips), by = 'OD') %>%
  mutate(trips_may = Trips) %>%
  select(-Trips)

od_internal_month <- od_internal_month %>%
  left_join(weekday2019_nopath$September %>%
              select(OD, Trips), by = 'OD') %>%
  mutate(trips_sep = Trips) %>%
  select(-Trips)

od_internal_month <- od_internal_month %>%
  left_join(weekday2019_nopath$October %>%
              select(OD, Trips), by = 'OD') %>%
  mutate(trips_oct = Trips) %>%
  select(-Trips)

for (i in 2:4) {
  od_internal_month[,i] = as.numeric(gsub(",", "", od_internal_month[,i]))
}

########################### plot ################################
# Fit linear model with intercept = 0
model <- lm(trips_may ~ 0 + trips_sep, data = od_internal_month)
r2 <- summary(model)$r.squared
slope <- coef(model)[["trips_sep"]]

# Create label for R² and slope
label_text <- paste0("slope = ", round(slope, 3), "\nR² = ", round(r2, 3))

ggplot(od_internal_month, aes(x = trips_sep, y = trips_may)) + geom_point(color = 'white') +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed", size = 1) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = label_text, hjust = 1.1, vjust = -1.1, size = 5, color = 'white') +
  theme_minimal()  + theme_black()# +
# coord_cartesian(xlim = c(0, 310), ylim = c(0, 310))

# Fit linear model with intercept = 0
model <- lm(trips_oct ~ 0 + trips_sep, data = od_internal_month)
r2 <- summary(model)$r.squared
slope <- coef(model)[["trips_sep"]]

# Create label for R² and slope
label_text <- paste0("slope = ", round(slope, 3), "\nR² = ", round(r2, 3))

ggplot(od_internal_month, aes(x = trips_sep, y = trips_oct)) + geom_point(color = 'white') +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed", size = 1) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = label_text, hjust = 1.1, vjust = -1.1, size = 5, color = 'white') +
  theme_minimal()  + theme_black()# +
# coord_cartesian(xlim = c(0, 310), ylim = c(0, 310))


########################### compare consistency with TAFT model #################
od_taft = read.xlsx('OD_TAFT_2019.xlsx', colNames = T, rowNames = T)
origin_id = rownames(od_taft)
destination_id = colnames(od_taft)
od_taft = as.matrix(od_taft)
od_taft = matrix(as.numeric(od_taft), nrow = nrow(od_taft), ncol = ncol(od_taft))
od_taft_long <- as.data.frame(as.table(od_taft))

# Rename columns
colnames(od_taft_long) <- c("Origin", "Destination", "Trips")
od_taft_long$OD = paste(od_taft_long$Origin, od_taft_long$Destination)
od_internal_month$taft = 0
od_internal_month <- od_internal_month %>%
  left_join(od_taft_long %>%
              select(OD, Trips), by = 'OD') %>%
  mutate(taft = Trips) %>%
  select(-Trips)

# Fit linear model with intercept = 0
model <- lm(taft ~ 0 + trips_sep/30, data = od_internal_month[which(!is.na(as.numeric(od_internal_month$taft))),])
r2 <- summary(model)$r.squared
slope <- coef(model)[["trips_sep"]]

# Create label for R² and slope
label_text <- paste0("slope = ", round(slope, 3), "\nR² = ", round(r2, 3))

ggplot(od_internal_month[which(!is.na(as.numeric(od_internal_month$taft))),], aes(x = trips_sep/30, y = as.numeric(taft))) + geom_point(color = 'white') +
  # geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed", size = 1) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = label_text, hjust = 1.1, vjust = -1.1, size = 5, color = 'white') +
  theme_minimal()  + theme_black()# +
# coord_cartesian(xlim = c(0, 310), ylim = c(0, 310))

##### compare production & attraction
prod = data.frame(origin = as.numeric(origin_id),
                  taft = rowSums(od_taft, na.rm = T))
attr = data.frame(destination = as.numeric(destination_id),
                  taft = colSums(od_taft, na.rm = T))

prod_data <- weekday2019_nopath$September %>%
  group_by(Origin.ID) %>%
  summarise(september = sum(Trips, na.rm = TRUE))

attr_data <- weekday2019_nopath$September %>%
  group_by(Destination.ID) %>%
  summarise(september = sum(Trips, na.rm = TRUE))

prod <- prod %>%
  left_join(prod_data, by = c('origin' = 'Origin.ID'))
# prod$september = prod$september/30
attr <- attr %>%
  left_join(attr_data, by = c('destination'  = 'Destination.ID'))
# attr$september = attr$september / 30

#### plot prod & attr
# Fit linear model with intercept = 0
model <- lm(taft ~ 0 + september, data = prod)
r2 <- summary(model)$r.squared
slope <- coef(model)[["september"]]

# Create label for R² and slope
label_text <- paste0("slope = ", round(slope, 3), "\nR² = ", round(r2, 3))

ggplot(prod, aes(x = september, y = taft)) + geom_point(color = 'white') +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed", size = 1) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = label_text, hjust = 1.1, vjust = -1.1, size = 5, color = 'white') +
  theme_minimal()  + theme_black()# +

# Fit linear model with intercept = 0
model <- lm(taft ~ 0 + september, data = attr)
r2 <- summary(model)$r.squared
slope <- coef(model)[["september"]]

# Create label for R² and slope
label_text <- paste0("slope = ", round(slope, 3), "\nR² = ", round(r2, 3))

ggplot(attr, aes(x = september, y = taft)) + geom_point(color = 'white') +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed", size = 1) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = label_text, hjust = 1.1, vjust = -1.1, size = 5, color = 'white') +
  theme_minimal()  + theme_black()# +


#################################### test #######################################
df = readodpair('','NCTCOG_TAZ_201903_Thursday_6-10_LDV-pairs')
df_nopath = readodpair('','NCTCOG_TAZ_201903_Thursday_6-10_LDV_NoPath-pairs')

tottrips(odpair = df, filter = 0)
tottrips(odpair = df_nopath, filter = 0)

o_sel = 'Internal'; d_sel = 'External'
tottrips(odpair = df, filter = 1, o_sel, d_sel ) # /(tottrips(odpair = df, filter = 0) - tottrips(odpair = df, filter = 1, o_sel = 'External', d_sel = 'External'))
tottrips(odpair = df_nopath, filter = 1, o_sel, d_sel) #/tottrips(odpair = df_nopath, filter = 0)


############################ compare w.path or w.o.path #########################
# o_internal = unique(df$Origin.ID[which(df$Origin.Source == 'Internal')])
# d_internal = unique(df$Destination.ID[which(df$Destination.Source == 'Internal')])

# od_internal = data.frame(cbind(apply(expand.grid(o_internal, d_internal), 1, paste0)), 0, 0)

od_internal = data.frame(cbind(paste(df_nopath$Origin.ID[which(df_nopath$Origin.Source == 'Internal')],
                                     df_nopath$Destination.ID[which(df_nopath$Destination.Source == 'Internal')]), 0, 0))

colnames(od_internal) = c('OD_ID','trips_path','trips_nopath')
df$OD_ID = paste(df$Origin.ID, df$Destination.ID)
df_nopath$OD_ID = paste(df_nopath$Origin.ID, df_nopath$Destination.ID)

od_internal = od_internal %>%
  left_join(df %>%
              select(OD_ID, Trips), by = 'OD_ID')
od_internal$trips_path = od_internal$Trips
od_internal = od_internal[,c(1,2,3)]

od_internal = od_internal %>%
  left_join(df_nopath %>%
              select(OD_ID, Trips), by = 'OD_ID')
od_internal$trips_nopath = od_internal$Trips
od_internal = od_internal[,c(1,2,3)]
od_internal = od_internal[which(!is.na(od_internal$trips_nopath)),]
################################# plot ##########################################
# Fit linear model with intercept = 0
model <- lm(trips_path ~ 0 + trips_nopath, data = od_internal)
r2 <- summary(model)$r.squared
slope <- coef(model)[["trips_nopath"]]

# Create label for R² and slope
label_text <- paste0("slope = ", round(slope, 3), "\nR² = ", round(r2, 3))

ggplot(od_internal, aes(x = trips_nopath, y = trips_path)) + geom_point(color = 'white') +
  geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed", size = 1) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = label_text, hjust = 1.1, vjust = -1.1, size = 5, color = 'white') +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 310), ylim = c(0, 310)) + theme_black()

