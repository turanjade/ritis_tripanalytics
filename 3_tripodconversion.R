# this file convert data OD to TransCAD OD file input format 
library(tidyr)
library(dplyr)

### first, replace OD ID with TAZ ID
taz_2026$ID = as.character(taz_2026$ID)
weekday2019_nopath$May = weekday2019_nopath$May %>%
  left_join(taz_2026 %>%
              select(ID, TAZ), by = c('Origin.Area' = 'ID'))%>%
  mutate(Origin.TAZ = TAZ) %>%
  select(-TAZ)
weekday2019_nopath$May = weekday2019_nopath$May %>%
  left_join(taz_2026 %>%
              select(ID, TAZ), by = c('Destination.Area' = 'ID'))%>%
  mutate(Destination.TAZ = TAZ) %>%
  select(-TAZ)

weekday2019_nopath$September = weekday2019_nopath$September %>%
  left_join(taz_2026 %>%
              select(ID, TAZ), by = c('Origin.Area' = 'ID'))%>%
  mutate(Origin.TAZ = TAZ) %>%
  select(-TAZ)
weekday2019_nopath$September = weekday2019_nopath$September %>%
  left_join(taz_2026 %>%
              select(ID, TAZ), by = c('Destination.Area' = 'ID'))%>%
  mutate(Destination.TAZ = TAZ) %>%
  select(-TAZ)

weekday2019_nopath$OD_05_09_10 = weekday2019_nopath$OD_05_09_10 %>%
  left_join(taz_2026 %>%
              select(ID, TAZ), by = c('Origin.ID' = 'ID'))%>%
  mutate(Origin.TAZ = TAZ) %>%
  select(-TAZ)
weekday2019_nopath$OD_05_09_10 = weekday2019_nopath$OD_05_09_10 %>%
  left_join(taz_2026 %>%
              select(ID, TAZ), by = c('Destination.ID' = 'ID'))%>%
  mutate(Destination.TAZ = TAZ) %>%
  select(-TAZ)

### second, convert September to TAFT format
weekday2019_nopath$September$Trips = as.numeric(weekday2019_nopath$September$Trips)
sep_2019_ODmat <- acast(weekday2019_nopath$September, Origin.TAZ ~ Destination.TAZ, value.var = "Trips", fun.aggregate = sum, fill = 0) # cannot be NA
write.csv(sep_2019_ODmat, file.path(datapath, 'Sep_2019_ODmat_fromRITIS.csv'), row.names = T, col.names = T)

sep_2019_ODlong = data.frame(Origin = weekday2019_nopath$September$Origin.TAZ, 
                             Destination = weekday2019_nopath$September$Destination.TAZ,
                             Trips = weekday2019_nopath$September$Trips)
write.csv(sep_2019_ODlong, file.path(datapath, 'Sep_2019_ODlong_fromRITIS.csv'), row.names = F, col.names = T)

may_sep_oct_2019_ODlong = data.frame(Origin = weekday2019_nopath$OD_05_09_10$Origin.TAZ, 
                                     Destination = weekday2019_nopath$OD_05_09_10$Destination.TAZ, 
                                     Trips = weekday2019_nopath$OD_05_09_10$trips_sum)
may_sep_oct_2019_ODlong = may_sep_oct_2019_ODlong[which(!is.na(may_sep_oct_2019_ODlong$Origin) & !is.na(may_sep_oct_2019_ODlong$Destination)),]
may_sep_oct_2019_ODlong$Origin = as.double(may_sep_oct_2019_ODlong$Origin)
may_sep_oct_2019_ODlong$Destination = as.double(may_sep_oct_2019_ODlong$Destination)
write.csv(may_sep_oct_2019_ODlong, file.path(datapath, 'May_Sep_Oct_sum_2019_ODlong_fromRITIS.csv'), row.names = F, col.names = T)


