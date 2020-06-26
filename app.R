source("load.R")
source("clean.R")
source("function.R")

##Naršyklės dalis
ui <- navbarPage("Vandens telkinių informacija",
                 theme = shinytheme("cerulean"),
                 tabPanel("Pradžia",
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("wb_type" , "Vandens telkinio tipas", choices = c("Ežeras/tvenkinys", "Upė/kanalas")),
                                      selectInput("wb_name", "Vandens telkinio pavadinimas", choices = NULL),
                                      selectInput("wb_code", "Vandens telkinio kodas", choices = NULL)
                                          ),
                                  mainPanel(
                                      tags$style(type = "text/css", "#wb_map {height: calc(100vh - 80px) !important;}"),
                                      leafletOutput("wb_map")
                                      )
                              )
                          ),
                 tabPanel("VT baseinas", 
                          sidebarLayout(
                              sidebarPanel(
                                  textOutput("wb_basin_area")
                                  ), 
                              mainPanel(
                                  tags$style(type = "text/css", "#wb_basin_map {height: calc(100vh - 80px) !important;}"),
                                  leafletOutput("wb_basin_map")
                                  )
                              )
                          ),
                 tabPanel("Monitoringo duomenys", 
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("wb_mst" , "Monitoringo vieta", choices = NULL),
                              ), 
                              mainPanel(
                                  tags$style(type = "text/css", "#mcplot {height: calc(100vh - 80px) !important;}"),
                                  plotlyOutput("mcplot")
                              )
                          )
                 ),
                 tabPanel("Klimato scenarijai", 
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("cl_par" , "Pasirinkite parametrą", choices = c("Debitas" = "FLOW_OUT", 
                                                                                              "Amonis" = "NH4_OUT", 
                                                                                              "Nitratai" = "NO3_OUT", 
                                                                                              "Bendrasis azotas" = "TOTN", 
                                                                                              "Fosfatai" = "MINP_OUT", 
                                                                                              "Bendrasis fosforas" = "TOTP"), 
                                              selected = "TOTN"),
                                  selectInput("cl_map_type" , "Pasirinkite žemėlapio tipą", choices = c("Būklės žemėlapis" = "s",
                                                                                                   "Procentinio skirtumo žemėlapis" = "p"), 
                                              selected = "s")
                              ), 
                              mainPanel(
                                  tags$style(type = "text/css", "#climate_map {height: calc(100vh - 80px) !important;}"),
                                  leafletOutput("climate_map")
                              )
                          )
                 ),
                 
                 navbarMenu("More",
                            tabPanel("Sub-Component A"),
                            tabPanel("Sub-Component B"))
)

##Serverio dalis
server <- function(session, input, output) {
    observeEvent(input$wb_type, {
        wb_type <- substring(input$wb_type, 1, 1)
        x <- unique(wb_names$Pavadinimas[wb_names$tipas==wb_type])
        updateSelectInput(session, "wb_name", "Vandens telkinio pavadinimas",
                          choices = sort(na.omit(x)))
    })

    observeEvent(input$wb_name, {
        x <- unique(wb_names$`VT kodas`[wb_names$Pavadinimas==input$wb_name])
        updateSelectInput(session, "wb_code", "Monitoringo vieta",
                          choices = sort(na.omit(x)))
    })
    
    output$wb_map <- renderLeaflet({
        wb_map(substring(input$wb_type, 1, 1), input$wb_code)
    })
    
    output$wb_basin_map <- renderLeaflet({
        wb_basin(input$wb_code)
    })
    
    output$wb_basin_area <- renderText({
        paste("Vandens telkinio baseino plotas yra", basin_area_txt(input$wb_code), "km2")
    })
    
    observeEvent(input$wb_code, {
        x <- unique(wb_to_mst$st_kodas[wb_to_mst$vt_kodas==input$wb_code])
        if (identical(x, character(0))){
            x <- "Nėra duomenų"
        }
        updateSelectInput(session, "wb_mst", "Monitoringo vieta",
                          choices = sort(na.omit(x)))
    })
    
    output$mcplot <- renderPlotly({
        plot_mconc(input$wb_mst)
    })
    
    output$climate_map <- renderLeaflet({
        get_status_maps(basins_s, get_LT_sf_data(model_data_LT), input$cl_par, input$cl_map_type)
    })
    
}

shinyApp(ui = ui, server = server)