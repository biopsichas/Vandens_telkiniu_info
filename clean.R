################################################
## Paruošiame duomenis reikalingus funkcijoms ##
################################################

source('load.R')

##Paruošiame GIS duomenis
wb_rivers_sf <- wb_rivers_sf %>% 
  select(WBriver_code) %>% 
  mutate(WBriver_code = as.character(sub("^", "LT", WBriver_code))) %>% 
  rename(wb_code = WBriver_code) %>% 
  st_transform(crs = 4326)

wb_lakes_sf <- wb_lakes_sf %>% 
  select(MS_CD) %>% 
  rename(wb_code = MS_CD) %>% 
  mutate(wb_code = as.character(wb_code)) %>% 
  st_transform(crs = 4326)

st_crs(wb_basins_sf) <- 3346
wb_basins_sf <- wb_basins_sf %>% 
  filter(baseino_tipas == 'P') %>% 
  select(-baseino_tipas) %>% 
  st_transform(crs = 4326)

##Apdorojame upių moniutoringo duomenis
mon_data$Date <- ymd(mon_data$Date)
mon_data[, 3:13] <- suppressWarnings(as.numeric(unlist(mon_data[, 3:13])))
mon_data$StationID <-  sub("^", "R", mon_data[grepl("^[0-9]", mon_data$StationID), "StationID"])
mon_data$Year <- lubridate::year(mon_data$Date)

##Sujungiame upių ir ežerų duomenis
lakes <- bind_rows(lakes1, lakes2) %>%
  rename(StationID = st_kodas,
         Date = data,
         DO = o2,
         BOD7 = bds7,
         NH4.N = nh4_n,
         NO3.N = no3_n,
         N.total = n_bendras,
         PO4.P = po4_p,
         P.total = p_bendras)
lakes$Year <- lubridate::year(lakes$Date)

mon_data <- bind_rows(mon_data, lakes) %>% 
  left_join(wb_to_mst, by = c("StationID"="st_kodas"))

rm(lakes1, lakes2, lakes)

