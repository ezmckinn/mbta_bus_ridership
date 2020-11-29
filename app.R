library(dplyr)
library(tidycensus)
library(purrr)
library(sf)
library(leaflet)
library(jsonlite)
library(httr)
library(tidytransit)
library(ggplot2)
library(plotly)
library(tidyverse)
library(rio)
library(shiny)

##GET DATA

route_list <- read.csv("./data_for_viz/route_list.csv", header = TRUE)

time_periods <- read.csv("./data_for_viz/time_periods.csv", header = TRUE)

#Bus Stops
stops <- st_read("./data_for_viz/bus_stops.geojson")

#Rail Stops
t_stops_sf <- st_read("./data_for_viz/t_stops.geojson")

#Bus Line Geometries
lines <- st_read("./data_for_viz/bus_lines.geojson")

#Rail Line Geometries
rail_lines <- st_read("./data_for_viz/rail_lines.geojson")

#Rail Line Pallettes
rail_pal <- colorFactor(rail_lines$route_color[0:21], rail_lines$route_id[0:21], ordered = TRUE, reverse = FALSE) #set palette for rail lines based on color in column

#Get human readable trip name
trips <- read.csv("./data_for_viz/trips.csv")

#Get overall reliability summary
rel_overall <- read.csv("./data_for_viz/rel_overall_clean.csv") 

riders_full <- read.csv("./data_for_viz/MBTA_Bus_Ridership.csv")

#Reliability data
rel_all_routes <- read.csv("./data_for_viz/rel_all_routes.csv")

key_lines <- st_read("./data_for_viz/key_lines.geojson")

##CREATE ICON##

subwayIcon <- makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/64/MBTA.svg/600px-MBTA.svg.png",
    iconWidth = 10, iconHeight = 10,
    iconAnchorX = 0, iconAnchorY = 0
)

server <- function(input, output) {
    
   # output$ui <- renderUI({
    #    
     #   if (is.null(input$service))
     #       return()
     #   
     #   switch(input$service,
     #          "Weekend" = selectInput("period", label = "Time Period",
     #                                  choices = c("AM_PEAK","PM_PEAK","EARLY_AM","VERY_EARLY_MORNING","MIDDAY_BASE","MIDDAY_SCHOOL","EVENING","LATE_EVENING","NIGHT"), selected = "AM_PEAK"),
     #          "Saturday" = selectInput("period", label = "Time Period",
     #                                   choices = "OFF_PEAK", selected = "OFF_PEAK"),
     #          "Sunday" = selectInput("period", label = "Time Period",
     #                                 choices = "OFF_PEAK", selected = "OFF_PEAK")
     #   )
    #})
    
    ##SELECT INPUTS##
    route_id_select <- reactive({input$route}) 
    
    period_select <- reactive({input$period})
    
    day_type_select <- reactive({switch(input$service,
                                        "Saturday" = "saturday",
                                        "Weekday" = "weekday",
                                        "Sunday" = "sunday"
    )}) 
    
    var_select <- reactive({switch(input$variable,
                                   "Boardings" = "average_ons",
                                   "Alightings" = "average_offs",
                                   "Passenger Load" = "average_load")
    })
    
    dir_select <- reactive({switch(input$direction,
                                   "A" = 0,
                                   "B" = 1)
    })
    
    ##CLEAN RIDERSHIP DATA BASED ON INPUTS ## 
    
    values <- reactiveValues(
        riders = NULL
    )
    
    observe({ values$riders <- riders_full %>%  filter(time_period_name == period_select(),
                                                       route_id == input$route,
                                                       direction_id == dir_select()) })
    
    ## BUILD BASE MAP
    
    rider_viz <- reactive ({ values$riders %>% select(c("route_id","direction_id","day_type_name","time_period_name","stop_id",all_of(var_select()),"num_trips","ObjectId")) %>% 
        rename_at(vars(starts_with('average')), funs(paste0('var'))) %>% #rename selected variable to standard column so leaflet can read it
        left_join(stops, by = "stop_id") %>%
        left_join(trips, by = c("route_id","direction_id")) })
    
    map_lines <- reactive({ lines %>% filter(route_id == route_id_select()) })
    
    var_txt <- reactive ({ if_else(var_select() == "average_ons", 'Avg. Boardings',
                       if_else(var_select() == "average_offs", "Avg. Alightings", "Avg. Load")) })
    
    default_pal <-  colorBin("Greens", domain = riders_full$average_ons[riders_full$route_id == '1'], bins = 5) 
    
    output$rider_map <- renderLeaflet({
        leaflet(map_lines()) %>% ## 
            setView(lng = -71.056651, lat = 42.350461, zoom = 13) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter, options = providerTileOptions(opacity = 0.8, noWrap = TRUE), group = "tiles") 
    }) 
    
    observeEvent(c(input$route,
                 input$variable,
                 input$direction,
                 input$period), {
        leafletProxy('rider_map') %>% 
                #addMarkers(lat = t_stops_sf$stop_lat, lng = t_stops_sf$stop_lon, icon = subwayIcon, group = 'T Stations') %>% 
                addPolylines(data = key_lines, color = "yellow", weight = 3, group = 'Key Bus Lines') # %>%
                #addPolylines(data = rail_lines, weight = 2, color = ~rail_pal(route_id), popup = paste(rail_lines$route_id), group = 'Rail Lines') 
            
            })
    
    ##UPDATE MAP BASED ON DATA
    
    observe({
            
        input$route
        input$variable
        input$direction
        input$period
        
            index <- nrow(rider_viz())/2
            
        #SET UP PALETTES
        
            if(input$variable == "Boardings") {color_selected = "Greens"}
            if(input$variable == "Alightings") {color_selected = "Reds"}
            if(input$variable == "Passenger Load") {color_selected = "Blues"}
            
            rider_pal <-  colorBin(color_selected, domain = rider_viz()$var, bins = 5) 
        
        ## ADD & UPDATE DATA ## 
        leafletProxy('rider_map') %>% 
            clearGroup(c("Selected Lines","Selected Stops")) %>%
            clearControls() %>%
            setView(lat = rider_viz()$stop_lat[index], lng = rider_viz()$stop_lon[index], zoom = 13) %>%
            addPolylines(data = map_lines(), color = "white", weight = 3, group = 'Selected Lines') %>%
            addCircleMarkers(data = rider_viz(), lng = rider_viz()$stop_lon, lat = rider_viz()$stop_lat, radius = rider_viz()$var, fillColor = ~rider_pal(rider_viz()$var), 
                                 color = ~rider_pal(rider_viz()$var), fillOpacity = 0.5, 
                                 popup = paste0("Stop: ", rider_viz()$stop_name, "<br>", var_txt(), " ", rider_viz()$var, "<br>", "Destination: ", rider_viz()$trip_headsign), 
                                 options = popupOptions(autoPan = TRUE, riseOnHover = TRUE), group = 'Selected Stops') %>%
            addLegend(data = rider_viz(), position = "bottomleft", pal = rider_pal, values = rider_viz()$var, opacity = 0.6, title = var_txt(), group = "Legend") %>% 
            addLayersControl(
                baseGroups = c("tiles"),
                overlayGroups = c('Selected Lines','Selected Stops','Key Bus Lines'),
                options = layersControlOptions(collapsed = TRUE),
                position = "topleft") %>%
            hideGroup('Key Bus Lines')
         
        })
    
    ## BUILD VISUALS ##

    observeEvent(c(input$route, input$direction, input$variable, input$period, input$go), {
        
        rider_chart <- 
            
            values$riders %>% select(c("ObjectId", "route_id","stop_id","stop_sequence","stop_name","average_ons","average_offs")) %>% 
            pivot_longer(cols = c("average_ons","average_offs"), names_to = "type", values_to = "count") 
        
        rider_load <-  values$riders %>%
            select(c("ObjectId", "route_id","stop_id","stop_sequence","stop_name","average_load")) %>% 
            pivot_longer(cols = "average_load", names_to = "type", values_to = "count") 
        
        output$profile <- renderPlot({
            ggplot(rider_chart, (aes(x = stop_sequence, y=count, text = stop_name, fill=type))) +
                geom_bar(position = "stack", stat="identity") +
                geom_step(data = rider_load, color = "#70dcf0", size = 0.8, group = 1) +
                scale_fill_manual(values = c("#70dcf0","#f768a1","#66c2a4"), labels = c("Pass. Load", "Alightings","Boardings")) +
                theme(legend.title = element_blank(), panel.background = element_blank()) +
                xlab("Stop Sequence") +
                ylab("Count")
        })
        
    })
    
     #      
    observeEvent(c(input$route, input$direction, input$variable, input$period, input$go), {
        
        ##CLEAN RELIABILITY DATA ##
        
        rel_route <- rel_all_routes %>% 
            filter(gtfs_route_id == input$route) %>%
            select(gtfs_route_id, on_time_rate, peak_offpeak_ind, service_date)
        
        rel_chart <- rel_route %>% group_by(peak_offpeak_ind) %>%
            rename("rte_on_time_rate" = on_time_rate) %>% 
            left_join(rel_overall, by = c("peak_offpeak_ind","service_date")) %>% 
            rename("route" = rte_on_time_rate, "overall" = on_time_rate) 
        
        rel_chart <- rel_chart %>% pivot_longer(
            cols = c("route","overall"), 
            names_to = "type", 
            values_to = "on_time_rate") %>%
            mutate(type = as.factor(type))
        
        ##BUILD RELIABILITY BOX PLOT
        
        output$boxplot <- renderPlot (
            ggplot(rel_chart, aes(fill = type, color = type, x = type, y = on_time_rate)) + 
                geom_boxplot(position="dodge", alpha=0.5) + 
                ylab("On Time %") +
                theme(legend.title = element_blank()) +
                xlab(element_blank())
        )
        
    })
    
    
        
}

ui <- navbarPage("MBTA Bus Ridership Explorer", id="nav",
                 
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css")
                                  
                              ),
                              
                              leafletOutput('rider_map', width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 65, left = "auto", right = 20, bottom = "auto",
                                            width = "25%", height = "auto",
                                            
                                            fluidRow(
                                                
                                                column(width = 12,
                                                p(h4("Instructions")),
                                                p(em("Change map data with these menus, toggle layers via the stack on the top left, and update the plots with the button below. Click on stops to see details."))
                                                ),
                                            ),
                                            
                                            fluidRow(
                                                column(width = 6,
                                                       selectInput("route", "Route", choices = route_list$route_id, selected = "1")),
                                                column(width = 6,
                                                       selectInput("period", "Period", choices = time_periods$time_period_name[time_periods$time_period_name != "OFF_PEAK"], selected = "AM_PEAK")
                                                       )
                                            ),
                                            
                                            fluidRow(
                                                column(width = 6,
                                                       selectInput("direction", "Direction", choices = c("A", "B"), selected = "A")),
                                                       #selectizeInput("service", "Service", choices = c("Weekday"), selected = "Weekday")),
                                                column(width = 6, 
                                                       selectInput("variable", "Variable", choices = c("Boardings","Alightings","Passenger Load"), selected = "Boardings"))
                                                
                                            )
                                            
                              ),
                              
                              #absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              #              draggable = TRUE, top = 420, left = "auto", right = 20, bottom = "auto",
                              #              width = 300, height = "auto",
                              #              h4("About this Route")
                              #              ),
                              
                              fixedPanel(id = "controls", class = "panel panel-default", #fixed = TRUE,
                                         draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 190,
                                         width = "25%", height = "170px",
                                         
                                         h4(strong("Route Profile")),
                                         plotOutput("profile", height = "80%", width = "100%")
                                         
                              ),
                              
                              fixedPanel(id = "controls", class = "panel panel-default", #fixed = TRUE,
                                         draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 10,
                                         width = "25%", height = "170px",
                                         
                                         h4(strong("Reliability Comparison")),
                                         plotOutput("boxplot", height = "80%", width = "100%")
                                         
                              )
                              
                          )
                 )
)        

# Run the application 
shinyApp(ui = ui, server = server)
