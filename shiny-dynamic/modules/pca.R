library(leaflet)

pc2d_main_UI <- function(id, config) {
    ns <- NS(id)

    conditionalPanel(
        condition = "input.plot_selector.indexOf('pc2d') != -1",
	absolutePanel(leafletOutput(ns("leaflet_pc2d"), width='100%', height='100%'),
	div(sprintf("PC%s vs PC%s", 1, 2)),
       	draggable=T, style='background:blue; padding:20px;', left="50%",
	width="50%", height="1000px")
)}

pc2d_options_UI <- function(id, config) {
    ns <- NS(id)
    list(
	sliderInput(ns("which_pc2d_1"),
	    label = "Choose a PC to display",
	    min = 1, max = 20, value=config$pc2d_PC1, step=1
	),
	sliderInput(ns("which_pc2d_2"), 
	    label = "Choose a PC to display",
	    min = 1, max = 20, value=config$pc2d_PC2, step=1
	)
    )
}

pc2d_plot <- function(input, output, session, dataset, coloring) {
    config <- reactiveValues(
	pc2d_PC1 = 1, pc2d_PC2 = 2
	)

    observeEvent(input$which_pc2d_1,{
	 if(config$pc2d_PC1 != input$which_pc2d_1){
	     config$pc2d_PC1 <- input$which_pc2d_1}
    })

    observeEvent(input$which_pc2d_2,{
	 if(config$pc2d_PC2 != input$which_pc2d_2){
	     config$pc2d_PC2 <- input$which_pc2d_2}
    })

    output$leaflet_pc2d = renderLeaflet({
	x = dataset()
	PC1 = paste0("PC", config$pc2d_PC1)
	PC2 = paste0("PC", config$pc2d_PC2)
	if(coloring() == 'Source'){
	    pc2d_leaflet =leaflet(x) %>%
	    addCircleMarkers(lng = x[,PC1], lat = x[,PC2],
			     label = ~wasDerivedFrom, color= ~color)
	} else {
	    pc2d_leaflet =leaflet(x) %>%
	    addCircleMarkers(lng = x[,PC1], lat = x[,PC2],
			     label = ~name, color= ~color)
	    }
	pc2d_leaflet
    })
    return(config)
}

pc2d_args <-  c("get_dataset", "reactive(input$coloring)")
