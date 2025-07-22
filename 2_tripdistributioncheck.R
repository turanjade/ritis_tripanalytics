# check trip duration and trip length distribution

####### trip duration distribution, 5min as an interval #################
library('lubridate')
# categorize travel time to each 5 minutes
weekday2019_nopath$May$Average.Travel.Time.5min = time_to_period(weekday2019_nopath$May$Average.Travel.Time, 5*60)
weekday2019_nopath$September$Average.Travel.Time.5min = time_to_period(weekday2019_nopath$September$Average.Travel.Time, 5*60)
weekday2019_nopath$October$Average.Travel.Time.5min = time_to_period(weekday2019_nopath$October$Average.Travel.Time, 5*60)

# summarize and plot distribution
May_traveltimedist = weekday2019_nopath$May %>%
  group_by(Average.Travel.Time.5min) %>%
  summarise(trips = sum(Trips, na.rm = TRUE)) %>%
  mutate(
    freq = trips / sum(trips)
  )

Sep_traveltimedist = weekday2019_nopath$September %>%
  group_by(Average.Travel.Time.5min) %>%
  summarise(trips = sum(Trips, na.rm = TRUE)) %>%
  mutate(
    freq = trips / sum(trips)
  )

Oct_traveltimedist = weekday2019_nopath$October %>%
  group_by(Average.Travel.Time.5min) %>%
  summarise(trips = sum(Trips, na.rm = TRUE)) %>%
  mutate(
    freq = trips / sum(trips)
  )

ggplot(May_traveltimedist, aes(x = Average.Travel.Time.5min, y = freq)) + geom_col(fill = 'white') + 
  xlab('travel time in May (minutes)') + ylab('Freq %') + theme_black()
ggplot(Sep_traveltimedist, aes(x = Average.Travel.Time.5min, y = freq)) + geom_col(fill = 'white') + 
  xlab('travel time in September (minutes)') + ylab('Freq %') + theme_black()
ggplot(Oct_traveltimedist, aes(x = Average.Travel.Time.5min, y = freq)) + geom_col(fill = 'white') + 
  xlab('travel time in October (minutes)') + ylab('Freq %') + theme_black()

######## trip length distribution, 5 miles as an interval ##################
weekday2019_nopath$May$Median.Trip.Distance..mi.5 = ceiling(weekday2019_nopath$May$Median.Trip.Distance..mi. / 5) * 5
weekday2019_nopath$September$Median.Trip.Distance..mi.5 = ceiling(weekday2019_nopath$September$Median.Trip.Distance..mi. / 5) * 5
weekday2019_nopath$October$Median.Trip.Distance..mi.5 = ceiling(weekday2019_nopath$October$Median.Trip.Distance..mi. / 5) * 5

# summarize and plot distribution
May_travellendist = weekday2019_nopath$May %>%
  group_by(Median.Trip.Distance..mi.5) %>%
  summarise(trips = sum(Trips, na.rm = TRUE)) %>%
  mutate(
    freq = trips / sum(trips)
  )
Sep_travellendist = weekday2019_nopath$September %>%
  group_by(Median.Trip.Distance..mi.5) %>%
  summarise(trips = sum(Trips, na.rm = TRUE)) %>%
  mutate(
    freq = trips / sum(trips)
  )
Oct_travellendist = weekday2019_nopath$October %>%
  group_by(Median.Trip.Distance..mi.5) %>%
  summarise(trips = sum(Trips, na.rm = TRUE)) %>%
  mutate(
    freq = trips / sum(trips)
  )

ggplot(May_travellendist, aes(x = Median.Trip.Distance..mi.5, y = freq)) + geom_col(fill = 'white') + 
  xlab('travel distance in May (miles)') + ylab('Freq %') + theme_black()
ggplot(Sep_travellendist, aes(x = Median.Trip.Distance..mi.5, y = freq)) + geom_col(fill = 'white') + 
  xlab('travel distance in September (miles)') + ylab('Freq %') + theme_black()
ggplot(Oct_travellendist, aes(x = Median.Trip.Distance..mi.5, y = freq)) + geom_col(fill = 'white') + 
  xlab('travel distance in October (miles)') + ylab('Freq %') + theme_black()

##### .--. intrazonal trip length ##### 
# check Sep for example
intrazonal_traveltimedist = weekday2019_nopath$September[which(weekday2019_nopath$September$Origin.Area == weekday2019_nopath$September$Destination.Area),] %>%
  group_by(Average.Travel.Time.5min) %>%
  summarise(trips = sum(Trips, na.rm = TRUE)) %>%
  mutate(
    freq = trips / sum(trips)
  )
colnames(intrazonal_traveltimedist)[1] = 'Time.5Min'; intrazonal_traveltimedist$Group = 'Intrazonal'
ggplot(rbind(intrazonal_traveltimedist, Sep_traveltimedist), aes(x = Time.5Min, y = freq, fill = Group)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 2), width = 2) +  # bar width
  # scale_x_discrete(expand = expansion(add = 0.2)) +  # wider spacing between categories
  theme_black()

intrazonal_travellendist = weekday2019_nopath$September[which(weekday2019_nopath$September$Origin.Area == weekday2019_nopath$September$Destination.Area),] %>%
  group_by(Median.Trip.Distance..mi.5) %>%
  summarise(trips = sum(Trips, na.rm = TRUE)) %>%
  mutate(
    freq = trips / sum(trips)
  )
colnames(intrazonal_travellendist)[1] = 'Len.5mile'; intrazonal_travellendist$Group = 'Intrazonal'
ggplot(rbind(intrazonal_travellendist, Sep_travellendist), aes(x = Len.5mile, y = freq, fill = Group)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 2), width = 2) +  # bar width
  # scale_x_discrete(expand = expansion(add = 0.2)) +  # wider spacing between categories
  theme_black()

############ read TAFT trip length distribution and plot them into one figure #####
triplentime_taft = read.csv(file.path(datapath, 'TAFT_AMTripLenTime.csv'), header = F)
colnames(triplentime_taft) = c('Origin','Destination','Time.Min','Len.Mile')

triplentime_taft$Time.5min = ceiling(triplentime_taft$Time.Min / 5) * 5
triplentime_taft$Len.5Mile = ceiling(triplentime_taft$Len.Mile / 5) * 5

triplentime_taft$Origin = as.character(triplentime_taft$Origin); triplentime_taft$Destination = as.character(triplentime_taft$Destination)
triplentime_taft = left_join(triplentime_taft, od_taft_long,
                             by = c('Origin' = 'Origin.TAZ', 'Destination' = 'Destination.TAZ'))

Taft_traveltimedist = triplentime_taft %>%
  group_by(Time.5min) %>%
  summarise(trips = sum(Trips, na.rm = T)) %>%
  mutate(
    freq = trips/sum(trips)
  )
Taft_travellendist = triplentime_taft %>%
  group_by(Len.5Mile) %>%
  summarise(trips = sum(Trips, na.rm = T)) %>%
  mutate(
    freq = trips/sum(trips)
)

Taft_traveltimedist$Group = 'TAFT'; Sep_traveltimedist$Group = 'Sep_data'
colnames(Taft_traveltimedist)[1] = 'Time.5Min'; colnames(Sep_traveltimedist)[1] = 'Time.5Min'
traveltimedist = rbind(Taft_traveltimedist, Sep_traveltimedist)
ggplot(traveltimedist, aes(x = Time.5Min, y = freq, fill = Group)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 2), width = 2) +  # bar width
  # scale_x_discrete(expand = expansion(add = 0.2)) +  # wider spacing between categories
  theme_black()

Taft_travellendist$Group = 'TAFT'; Sep_travellendist$Group = 'Sep_data'
colnames(Taft_travellendist)[1] = 'Len.5mile'; colnames(Sep_travellendist)[1] = 'Len.5mile'
travellendist = rbind(Taft_travellendist, Sep_travellendist)
ggplot(travellendist, aes(x = Len.5mile, y = freq, fill = Group)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 2), width = 2) +  # bar width
  # scale_x_discrete(expand = expansion(add = 0.2)) +  # wider spacing between categories
  theme_black()

###### .--. use TAFT skim to draw distribution, not real travel time ###########
triplentime_taft$OD = paste(triplentime_taft$Origin.ID, triplentime_taft$Destination.ID, sep = ',')
triplentime_taft = triplentime_taft %>%
  left_join(weekday2019_nopath$OD_05_09_10 %>%
              select(OD, trips_sum), by = 'OD') %>%
  mutate(trips_data = trips_sum) %>%
  select(-trips_sum)

Data_traveltimedist = triplentime_taft %>%
  group_by(Time.5min) %>%
  summarise(trips = sum(trips_data, na.rm = T)) %>%
  mutate(freq = trips/sum(trips))
Data_travellendist = triplentime_taft %>%
  group_by(Len.5Mile) %>%
  summarise(trips = sum(trips_data, na.rm = T)) %>%
  mutate(freq = trips/sum(trips))

Taft_traveltimedist$Group = 'TAFT'; Data_traveltimedist$Group = 'Data'
colnames(Taft_traveltimedist)[1] = 'Time.5Min'; colnames(Data_traveltimedist)[1] = 'Time.5Min'
traveltimedist = rbind(Taft_traveltimedist, Data_traveltimedist)
ggplot(traveltimedist, aes(x = Time.5Min, y = freq, fill = Group)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 2), width = 2) +  # bar width
  # scale_x_discrete(expand = expansion(add = 0.2)) +  # wider spacing between categories
  theme_black()

Taft_travellendist$Group = 'TAFT'; Data_travellendist$Group = 'Data'
colnames(Taft_travellendist)[1] = 'Len.5mile'; colnames(Data_travellendist)[1] = 'Len.5mile'
travellendist = rbind(Taft_travellendist, Data_travellendist)
ggplot(travellendist, aes(x = Len.5mile, y = freq, fill = Group)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 2), width = 2) +  # bar width
  # scale_x_discrete(expand = expansion(add = 0.2)) +  # wider spacing between categories
  theme_black()
