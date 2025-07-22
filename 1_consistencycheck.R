# read data and merge together

### load files
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

################################################# Pathway & w.o Pathway option check #######################
## daily workday for may
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


################################# Compare internal trip consistency in three months (discard April) ###########
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

ggplot(weekday2019_nopath$Totaltrip, aes(x = Mon, y = Tottrip)) + geom_col(fill = 'white') + theme_black()

#### get all unique ODs from Internal-Internal
weekday2019_nopath$May$OD = paste(weekday2019_nopath$May$Origin.Area, weekday2019_nopath$May$Destination.Area, sep = ',')
weekday2019_nopath$September$OD = paste(weekday2019_nopath$September$Origin.Area, weekday2019_nopath$September$Destination.Area, sep = ',')
weekday2019_nopath$October$OD = paste(weekday2019_nopath$October$Origin.Area, weekday2019_nopath$October$Destination.Area, sep = ',')

od_data = data.frame(rbind(cbind(weekday2019_nopath$May$OD, weekday2019_nopath$May$Origin.Source, weekday2019_nopath$May$Destination.Source), 
                cbind(weekday2019_nopath$September$OD, weekday2019_nopath$September$Origin.Source, weekday2019_nopath$September$Destination.Source),
                cbind(weekday2019_nopath$October$OD, weekday2019_nopath$October$Origin.Source, weekday2019_nopath$October$Destination.Source)))
colnames(od_data) = c('OD','Origin.Source','Destination.Source')
od_data = od_data[which(!duplicated(od_data$OD)),]

## determine if the OD is internal or external
od_data = od_data[which(od_data$Origin.Source == 'Internal' & od_data$Destination.Source == 'Internal'),]

## for all unique ODs, get trips from each month by left-join
od_internal_month = data.frame(OD = od_data$OD, trips_may = 0, trips_sep = 0, trips_oct = 0)

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

## append to the list
weekday2019_nopath$OD_05_09_10 = od_internal_month

## split OD to origin and destination
weekday2019_nopath$OD_05_09_10 <- weekday2019_nopath$OD_05_09_10 %>%
  separate(OD, into = c("Origin.ID", "Destination.ID"), sep = ",", remove = F)

## calculate monthly average of three months
weekday2019_nopath$OD_05_09_10$trips_avg = rowMeans(x = weekday2019_nopath$OD_05_09_10[,c(4,5,6)], na.rm = T)
weekday2019_nopath$OD_05_09_10$trips_sum = rowSums(x = weekday2019_nopath$OD_05_09_10[,c(4,5,6)], na.rm = T)
## clean data
rm(od_internal_month, od_data)

###plot 
pairwiseplot(data = weekday2019_nopath$OD_05_09_10, x = 'trips_sep', y = 'trips_may')
pairwiseplot(data = weekday2019_nopath$OD_05_09_10, x = 'trips_sep', y = 'trips_oct')

##### .--. intrazonal trip proportion ######
sum(weekday2019_nopath$OD_05_09_10$trips_avg[which(weekday2019_nopath$OD_05_09_10$Origin.ID == weekday2019_nopath$OD_05_09_10$Destination.ID)])/
sum(weekday2019_nopath$OD_05_09_10$trips_avg) # 12%

##### summarise all the internal trips and internal trip rows
nrow(weekday2019_nopath$OD_05_09_10)
sum(weekday2019_nopath$OD_05_09_10$trips_avg / 30) / 0.08 

#### .--. trip length distribution #########
# check 2_tripdistributioncheck.R

########################### Compare consistency with TAFT model #################
datapath = 'C:\\Users\\rtu\\OneDrive - The North Central Texas Council of Governments\\Documents\\0_ModelDataDevelopment\\20250715_triptable\\data_process'
od_taft = read.xlsx(file.path(datapath,'OD_TAFT_2019.xlsx'), colNames = T, rowNames = T)

# get unique OD id
origin_id = rownames(od_taft)
destination_id = colnames(od_taft)

# convert square matrix to long matrix
od_taft = as.matrix(od_taft)
od_taft_long <- as.data.frame(as.table(od_taft))
colnames(od_taft_long) <- c("Origin.TAZ", "Destination.TAZ", "Trips")
# convert to numbers, -- as NA
od_taft_long$Trips = as.numeric(od_taft_long$Trips)

# replace od_taz with od_id (explain: in taft OD, the ID is TAZ not real ID)
od_taft_long$Origin.TAZ = as.character(od_taft_long$Origin.TAZ)
taz_2026$TAZ = as.character(taz_2026$TAZ)
od_taft_long <- od_taft_long %>%
  left_join(taz_2026 %>%
              select(ID, TAZ), by = c('Origin.TAZ' = 'TAZ')) %>%
  mutate(Origin.ID = ID) %>%
  select(-ID)

od_taft_long <- od_taft_long %>%
  left_join(taz_2026 %>%
              select(ID, TAZ), by = c('Destination.TAZ' = 'TAZ')) %>%
  mutate(Destination.ID = ID) %>%
  select(-ID)

# od_taft_long$geometry.x = NULL; od_taft_long$geometry.y = NULL

# get unique OD pairs of taft
od_taft_long$OD = paste(od_taft_long$Origin.ID, od_taft_long$Destination.ID, sep = ',')
weekday2019_nopath$OD_05_09_10$taft = 0
weekday2019_nopath$OD_05_09_10 <- weekday2019_nopath$OD_05_09_10 %>%
  left_join(od_taft_long %>%
              select(OD, Trips), by = 'OD') %>%
  mutate(taft = Trips) %>%
  select(-Trips)

# Fit linear model with intercept = 0
pairwiseplot(data = weekday2019_nopath$OD_05_09_10[which(!is.na(as.numeric(weekday2019_nopath$OD_05_09_10$taft))),], x = 'trips_avg', y = 'taft')

######## .--. compare production & attraction ########
# od_taft_n = matrix(as.numeric(od_taft), nrow = nrow(od_taft), ncol = ncol(od_taft))
prod_model = od_taft_long %>%
  group_by(Origin.ID) %>%
  summarise(trips = sum(Trips, na.rm = TRUE))
attr_model = od_taft_long %>%
  group_by(Destination.ID) %>%
  summarise(trips = sum(Trips, na.rm = TRUE))

prod_data <- weekday2019_nopath$OD_05_09_10 %>%
  group_by(Origin.ID) %>%
  summarise(trips = sum(trips_avg, na.rm = TRUE))
prod_data$Origin.ID = as.numeric(prod_data$Origin.ID)

attr_data <- weekday2019_nopath$OD_05_09_10 %>%
  group_by(Destination.ID) %>%
  summarise(trips = sum(trips_avg, na.rm = TRUE))
attr_data$Destination.ID = as.numeric(attr_data$Destination.ID)

prod_model <- prod_model %>%
  left_join(prod_data, by = c('Origin.ID' = 'Origin.ID'))
attr_model <- attr_model %>%
  left_join(attr_data, by = c('Destination.ID'  = 'Destination.ID'))
colnames(prod_model) = c('Origin.ID','TAFT','Data')
colnames(attr_model) = c('Destination.ID','TAFT','Data')

## plot prod & attr
# Fit linear model with intercept = 0
pairwiseplot(prod_model, x = 'Data', y = 'TAFT')
pairwiseplot(attr_model, x = 'Data', y = 'TAFT')


rm(prod, attr, prod_data, attr_data, od_taft_n)

############## .--. proportion of taft trips sharing the same OD as data ########
sum(weekday2019_nopath$OD_05_09_10$taft, na.rm = T)/sum(od_taft_long$Trips, na.rm = T) ## 69.49%


#################################### @@@@@@@@@@@ #######################################
#################################### @@@@@@@@@@@ #######################################
#################################### comment out #######################################
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
pairwiseplot(data = od_internal, x = 'trips_nopath', y = 'trips_path')

#################### why april has much less data than other months ###############
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