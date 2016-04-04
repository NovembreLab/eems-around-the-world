library(rworldmap)
library(leaflet)

map_main_UI <- function(id, config) {
    ns <- NS(id)

    conditionalPanel(
        condition = "input.plot_selector.indexOf('map') != -1",
            absolutePanel(leafletOutput(ns("leaflet_map"), width='100%'),
            draggable=T, style='background:red; padding:20px;', width="50%")
    )
}

map_options_UI <- function(id, config) {
    ns <- NS(id)

    selectInput(ns("map_point_size"), 
        label = "Choose how points should be sizes",
        choices = c("Location uncertainty", "Sample size"),
        selected = config$size)
}

map_plot <- function(input, output, session, dataset) {
    config <- reactiveValues(size='Location uncertainty')
    
    observeEvent(input$map_point_size,{
        if(config$size != input$map_point_size){
            config$size <- input$map_point_size
        }
    })

    m = getMap('low')
    output$leaflet_map <-  renderLeaflet({
        x = dataset()
        print(x$sample_size)
        if(config$size == 'Location uncertainty'){
        map_leaflet = leaflet(m) %>% addTiles() %>%
            addCircleMarkers(lat=x$latitude, lng=x$longitude,
                color=x$color, radius=(x$accuracy/20+8),
                label = paste(x$name, ";Accuracy:", x$accuracy)
            )                                                              
        } else {
        map_leaflet = leaflet(m) %>% addTiles() %>%
            addCircleMarkers(lat=x$latitude, lng=x$longitude,
                color=x$color, radius=(x$sample_size+10),
                label = paste(x$name, "; Sample Size: ", x$sample_size)
            )                                                              
        }
    })
    return(config)
}

map_args <- "get_pop_data"
