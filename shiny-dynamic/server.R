library(shiny)
source("helpers.R")



#! Simple Server to add custom plot modules to a shiny plot,
#! Assuming they follow a template with three functions:
#! 1. %s_main_UI(id, config) displays the plot, including CSS
#! 2. %s_options_UI(id, config) is a set of configs
#! 3. %s_plot(input, output, session, ...) is the main module function,
#!, describing observers and renderers

shinyServer(  function(input, output) {

    config <- list()


    get_dataset <- reactive({
	print("reload_data")
        s <- input$which_data
        data <- load.data(sprintf("pca/flash_%s_dim20.pc", s),
                          sprintf("subset/%s.fam", s),
                          sprintf("subset/%s.indiv_meta", s),
                          sprintf("pgs/gvar.pop_display"))
        data <- merge(data, read.csv(sprintf('subset/%s.pop_geo', s)))
        print(input$coloring)
        if(input$coloring == 'Source'){
            data$color <- get_cols_source(data)
        } else{
            data$color <- get_cols_wrap(data)
        }

        data
    })

    get_pop_data <- reactive({
	data <- get_dataset()
	columns <- c('popLabel', 'abbrev', 'latitude', 'longitude', 'wasDerivedFrom',
		  'accuracy', 'name', 'color')
	pop_data <- unique(data[,columns])
	pop_data$sample_size <- aggregate(data$sampleId, data[,columns],
					  length)$x
	pop_data
    })


    output$options <- renderUI({
	mod = input$which_options
	modules$options_ui[[mod]](mod, config[[mod]])
    })


    output$element <- renderUI({
	lapply(modules$names, function(mod){
	    isolate(if(mod %in% input$plot_selector){
		    modules$main_ui[[mod]](mod, config[[mod]])
	    })
	})
    })


    feval <- function(sss)eval(parse(text=sss))

    if(T){
    config <- sapply(modules$names, simplify=F, USE.NAMES=T,
	function(mod){
	    do.call(callModule, c(lapply(modules$args[[mod]], feval),
				  module=modules$plot[[mod]],
				  id=mod))
    })
    }

    if(F){
    config[['map']] <- callModule(module=map_plot,
				  id='map',
				  get_pop_data)
    config[['pc2d']] <- callModule(module=pc2d_plot,
				   id='pc2d',
				   get_dataset, reactive(input$coloring))
    }
})


