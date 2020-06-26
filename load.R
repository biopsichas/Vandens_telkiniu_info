##############################################################
## Pakrauname į atminti reikalingas bibliotekas ir duomenis ##
##############################################################

library(shiny)
library(leaflet)
library(shinythemes)
library(tidyverse)
library(readxl)
library(sf)
library(lubridate)
library(plotly)

data_folder <- "./input/"

##Pakrauname telkinių kodus ir pavadinimis 
wb_names <- readRDS(paste0(data_folder, "wb_names.rds")) %>% 
  filter(nchar(`VT kodas`) == 11) %>% 
  distinct() 

##Pakrauname upių vandens telkinius su GIS informacija
wb_rivers_sf <- sf::st_read(dsn = paste0(data_folder,"GIS/wb.gdb"), layer = "WB_line_150420", quiet = TRUE)

##Pakrauname ežerų vandens telkinius su GIS informacija
wb_lakes_sf <- sf::st_read(dsn = paste0(data_folder,"GIS/wb.gdb"), layer = "WB_Lakes_150420", quiet = TRUE)

##Pakrauname vandens telkinių baseinų ribų GIS informacija
wb_basins_sf <- sf::st_read(dsn = paste0(data_folder,"GIS/wb_basins.gdb"), layer = "vandens_telkiniu_baseinai", quiet = TRUE)

##Pakrauname sąryšį tarp vandens telkinių ir monitoringo vietų
wb_to_mst <- readRDS(paste0(data_folder, "wb_mst.rds"))

##Pakrauname upių monitoringo duomenis
mon_data <- read.table(paste0(data_folder, "WQObs.txt"), header = TRUE, sep="\t", stringsAsFactors = FALSE)[-1, ] 

##Pakrauname ežerų monitoringo duomenis
lakes1 <- readRDS(paste0(data_folder, "ezerai_1993_2009.rds")) %>% 
  select(st_kodas, data, o2, bds7, nh4_n, no3_n, n_bendras, po4_p, p_bendras)
lakes2 <- readRDS(paste0(data_folder, "ezerai_2010_2018.rds")) %>% 
  select(st_kodas, data, o2, bds7, nh4_n, no3_n, n_bendras, po4_p, p_bendras)
lakes_type <- read_excel(paste0(data_folder, "ezeru_informacija.xlsx"), sheet = 1) %>% 
  select(vt_kodas, tipas) %>% 
  rename(l_type = tipas)

##Pakrauname GIS informacija naudota modelyje
basins_s <- sf::st_read(dsn = paste0(data_folder,"GIS/model_network/basins_s.shp"), layer = "basins_s", quiet = TRUE)
rivers_s <- sf::st_read(dsn = paste0(data_folder,"GIS/model_network/rivers_s.shp"), layer = "rivers_s", quiet = TRUE)

##Pakrauname klimato kaitos poveikio modeliavimo išvesties rezultatus
model_data_LT <- readRDS(paste0(data_folder, "model_data_LT.rds")) 