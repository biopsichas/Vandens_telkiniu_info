#####################################################
## Pagrindinės funkcijos pateikti rezultatus appse ##
#####################################################

##Funkcija paruošti vandens telkinių žemėlapius
wb_map <- function(wb_type, wb_code1){
  gc <- leaflet() %>% 
    addTiles (group = "OSM")%>% 
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%  
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI ortofoto") %>% 
    addLayersControl(
      baseGroups = c("CartoDB (numatytas)", "OSM", "ESRI ortofoto"),
      options = layersControlOptions(collapsed = TRUE))
  if (wb_type == "U"){
    sel_rivers <- wb_rivers_sf %>% 
      filter(wb_code == wb_code1)
    gc <- gc %>%
      addPolylines(data = sel_rivers)
  } else if (wb_type == "E"){
    sel_lakes <- wb_lakes_sf %>% 
      filter(wb_code == wb_code1)
    gc <- gc %>%
      addPolygons(data = sel_lakes)
  }
  return(gc)
}

##Funkcija paruošti vandens telkinių baseinų žemėlapius
wb_basin <- function(wb_code1){
  gc <- leaflet() %>% 
    addTiles (group = "OSM")%>% 
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%  
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI ortofoto") %>% 
    addLayersControl(
      baseGroups = c("CartoDB (numatytas)", "OSM", "ESRI ortofoto"),
      options = layersControlOptions(collapsed = TRUE))
  ##Parenkame baseina pateikti žėmėlapyje
    sel_basin <- wb_basins_sf %>% 
      filter(vt_kodas == wb_code1)
  gc <- gc %>% 
    addPolygons(data = sel_basin, fillColor = "transparent")
  
  ##Pridedame parinktą vandens telkinį
  if (sel_basin$vt_tipas == "R"){
    sel_rivers <- wb_rivers_sf %>% 
      filter(wb_code == wb_code1)
    gc <- gc %>%
      addPolylines(data = sel_rivers, color = "red")
  } else if (sel_basin$vt_tipas == "L"){
    sel_lakes <- wb_lakes_sf %>% 
      filter(wb_code == wb_code1)
    gc <- gc %>%
      addPolygons(data = sel_lakes, color = "red")
  }
  return(gc)
}

##Funkcija paruošti vandens telkinių baseinų plotą
basin_area_txt <- function(wb_code1){
  sel_basin <- wb_basins_sf %>% 
    filter(vt_kodas == wb_code1)
  st_geometry(sel_basin) <- NULL
  basin_area <- sel_basin %>% 
    select(Shape_Area) %>% 
    mutate(Shape_Area = Shape_Area/1000000)
  return(as.character(round(basin_area,2)))
}

##Funkcija pateikti koncentracijas vandens telkiniuose iš monitoringo duomenų
nlabels_conc <- c("BDS7", "Istirpes deguonis", "NH4-N", "NO3-N", "Bendras azotas", "PO4-P", "Bendras fosforas")
nlevels_conc <- c("BOD7", "DO", "NH4.N", "NO3.N", "N.total", "PO4.P", "P.total")

plot_mconc <- function(mst_code){
  figure_param <- function(param){
    if (param %in% c("BOD7", "DO")){
      y_name <- "Koncentracija 02 mg/l"
    } else {
      y_name <- "Koncentracija mg/l"
    }
    fig <- conc_plot %>% 
      filter(PARAM == param) %>%
      plot_ly(x = ~Year,y = ~VALUE, color = ~PARAM_f, colors = "Dark2", type = "box")
    gs <- good_status %>% filter(PARAM == param)
    if (is.na(gs$Z)){
      fig <- fig %>% layout(xaxis = list(title = "Metai", showgrid = TRUE),
                            yaxis = list(title = y_name, showgrid = TRUE, rangemode = 'tozero'))
    }else{
      fig <- fig %>% layout(shapes=list(type='line', x0= 1995, x1= 2019, y0=gs$Z, y1=gs$Z, line=list(color="red", width=2)),
                          xaxis = list(title = "Metai", showgrid = TRUE),
                          yaxis = list(title = y_name, showgrid = TRUE, rangemode = 'tozero'))
    }
    
    return(fig)
  }
  conc_plot <- mon_data %>% 
    filter(StationID == mst_code) 
  if (dim(conc_plot)[1] != 0) {
    wb_code <- as.character(unique(conc_plot$vt_kodas))
    conc_plot <- conc_plot %>%
      select(-c(Flow, SS, NO2.N, N.mineral, N.Org, P.Org, Date, vt_kodas)) %>%
      gather(PARAM, VALUE, -Year, -StationID) %>%
      mutate(PARAM = as.factor(PARAM))
    conc_plot$PARAM_f = factor(conc_plot$PARAM, levels=nlevels_conc, labels = nlabels_conc)
    if (startsWith(as.character(conc_plot[1,1]), "R")){
      conc_levels <- c(3.3, 7.5, 0.2, 2.3, 3, 0.09, 0.14)
      good_status <- data.frame(PARAM = nlevels_conc, Z = conc_levels)
    }else if((startsWith(as.character(conc_plot[1,1]), "L"))){
      if (lakes_type$l_type[lakes_type$vt_kodas == wb_code] == 1){
        conc_levels <- c(4.2, NA, NA, NA, 2, NA, 0.06)
      }else{
        conc_levels <- c(3.2, NA, NA, NA, 2, NA, 0.05)
      }
      good_status <- data.frame(PARAM = nlevels_conc, Z = conc_levels)
    }
    
    good_status$PARAM_f = factor(good_status$PARAM, levels=nlevels_conc, labels=nlabels_conc)
    fig <- subplot(figure_param("BOD7"), figure_param("DO"), figure_param("NH4.N"), 
                   figure_param("NO3.N"), figure_param("N.total"), figure_param("PO4.P"), 
                   figure_param("P.total"), nrows = 3, shareX = TRUE)
    return(fig)
  } else {
    gc <- ggplot() + theme_void() + annotate("text", x = 1, y = 1, label = paste("Telkiniui negalima suformuoti  \n vandens kokybes monitoringo duomenu \n grafiko."), color = "gray40", size = 6, fontface = 2) +
      theme(panel.border = element_rect(colour = "gray60", fill=NA, size = 1))
    return(ggplotly(gc))
  }
}

##Function to make map for concentration or precentage of change
get_status_maps <- function(basin_sf, river_data_sf, par, to_show = "s"){ ##s for status or concentration and p for percantage of change map
  river_data_sf <- river_data_sf %>% 
    filter(param == par)
  sc <- unique(river_data_sf$sc)
  if (startsWith(par, "FLOW") & to_show == "s"){
    to_show = "p"
  }
  if (to_show == "s"){
    if (par == "TOTN"){ ##Water quality marks for status categories
      bins <- c(0, 2, 3, 6, 12, Inf)
    } else if (par == "NO3_OUT"){
      bins <- c(0, 1.3, 2.3, 4.5, 10, Inf)
    } else if (par == "NH4_OUT"){
      bins <- c(0, 0.1, 0.2, 0.6, 1.5, Inf)
    } else if (par == "MINP_OUT"){
      bins <- c(0, 0.05, 0.09, 0.18, 0.4, Inf)
    } else if (par == "TOTP"){
      bins <- c(0, 0.1, 0.14, 0.23, 0.47, Inf)
    } else if (par == "CBOD_OUT"){
      bins <- c(0, 2.3, 3.3, 5, 7, Inf)
    }
    labels <- c("Labai gera", "Gera", "Vidutine", "Bloga", "Labai bloga")
    pal <- colorBin(c("blue","forestgreen","gold","orange","red"), domain = river_data_sf$conc_mg_l, bins = bins)
    variable <- "conc_mg_l"
    units_text <- "mg/l"
    name_text <- "Koncentracija"
    name_text_short <- "konc."
    legend_text <- "bukle"
  } else if (to_show == "p"){
    if (startsWith(par, "FLOW")){
      bins <- c(-Inf, -15, -10, -5, 5, 10, 15, Inf)
      labels <- c("<-15%", "-15--10%", "-10--5%", "-5-5%", "5-10%", "10-15%", ">15%")
      pal <- colorBin(c("red", "orange", "gold", "palegreen", "springgreen", "deepskyblue", "blue"), 
                      domain = river_data_sf$conc_mg_l, bins = bins)
      units_text <- "m3/s"
      name_text <- "Debitas"
      name_text_short <- "deb."
      legend_text <- "debito skirtumas"
    } else {
      bins <- c(-Inf, -50, -25, -10, 10, 25, 50, 75, 100, Inf)
      labels <- c("<-50%", "-50--25%", "-25--10%", "-10-10%", "10-25%", "25-50%", "50-75%", "75-100", ">100%")
      pal <- colorBin(c("blue","deepskyblue", "springgreen", "palegreen","gold","orange","red", "red3", "red4"), 
                      domain = river_data_sf$conc_mg_l, bins = bins)
      units_text <- "mg/l"
      name_text <- "Koncentracija"
      name_text_short <- "konc."
      legend_text <- "konc. skirtumas"
    }
    variable <- "prc_change"
  }
  gc <- basin_sf %>% 
    leaflet() %>% 
    addTiles (group = "OSM")%>% 
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%  
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI ortofoto") %>% 
    addPolygons(color = "black", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0,
                popup = ~paste("Grupe:", basin_sf$Subbasin, "<br>",
                               "Modelis:", basin_sf$Setup_name, "<br>",
                               "Modelio pabaseinis:", basin_sf$GRIDCODE,"<br>",
                               "Modelio pabaseinio plotas:", round(basin_sf$area, 1), "km2", "<br>"),
                group = "Baseinai") 
  
  for (s in sc){
    river_data_sf_sc <- river_data_sf %>% 
      filter(sc == s)
    gc <- gc %>%
      addPolylines(data = river_data_sf_sc, color = ~pal(river_data_sf_sc[[variable]]), opacity = 1,
                   popup = ~paste("Scenarijus:", river_data_sf_sc$sc, "<br>",
                                  "Parametras:", river_data_sf_sc$param, "<br>",
                                  name_text, river_data_sf_sc$conc_mg_l, units_text,"<br>",
                                  "Skirtumas nuo bazinio sc.:", river_data_sf_sc$prc_change, "%", "<br>"),
                   label = ~paste(par, name_text_short, ":", river_data_sf_sc$conc_mg_l, units_text,
                                  "sk. nuo bazinio sc.:", river_data_sf_sc$prc_change, "%"),
                   group = s)
  }
  sc_to_hide <- str_remove(sc, "baseline")
  gc <- gc %>% hideGroup(sc_to_hide)
  
  gc <- gc %>% addLegend(data = river_data_sf, position = "bottomleft", pal = pal, values = ~river_data_sf[[variable]], opacity = 0.8, 
                         labFormat = function(type, cuts, p) {paste0(labels)},
                         title = ~paste("Parametras:", par, "<br>",
                                        "Vandens telkiniu", "<br>",
                                        legend_text, "<br>"))
  
  gc <- gc %>% addLayersControl(
    baseGroups = c("CartoDB (numatytas)", "OSM", "ESRI ortofoto"),
    overlayGroups = c("Baseinai", sc), 
    options = layersControlOptions(collapsed = TRUE))
  
  gc
}

##Function for preparing sf input for all LT map
get_LT_sf_data <- function(df){
  st_geometry(basins_s) <- NULL
  df <- df %>% 
    left_join(basins_s, by = c("Subbasin", "Setup_name", "reach" = "GRIDCODE")) %>% 
    select(-Id, -area, -cachto)
  rivers_s <- rivers_s %>% 
    select(Catch) %>% 
    left_join(df, by = c("Catch" = "cach_id"))
  return(rivers_s)
}