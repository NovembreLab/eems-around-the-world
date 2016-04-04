source("scripts/ggpca.R")

print(getwd())

pca1d_main_UI <- function(id, config) {
    ns <- NS(id)

    conditionalPanel(
        condition = "input.plot_selector.indexOf('pca1d') != -1",
	absolutePanel(plotOutput(ns("gg_pc1d"), brush=ns("test_brush"), width='100%', height='100%'),
       	draggable=F, style='background:purple; padding:20px;', left="00%",
	width="50%", height="500px")
)}

pca1d_options_UI <- function(id, config) {
    ns <- NS(id)
    v <- if(is.null(config)){1} else{config$PC}
    print(v)
    list(
	sliderInput(ns("which_PC"),
	    label = "Choose a PC to display",
	    min = 1, max = 20, value=v, step=1
	),
         textOutput(ns("test_sel"))
    )
}

pca1d_plot <- function(input, output, session, dataset) {
    config <- reactiveValues(
	PC = 1
	)

    observeEvent(input$which_PC,{
	 if(config$PC != input$which_PC){
	     config$PC <- input$which_PC}
         x <<- dataset()
    })

    observeEvent(input$test_brush, {
                 x = dataset()
                 x$abbrev <- factor(x$abbrev)
                 b <- input$test_brush
                 PC <- paste0("PC", config$PC)
                 in_x <- b$xmin < as.numeric(factor(x$abbrev)) & as.numeric((x$abbrev)) <= b$xmax
                 in_y <- b$ymin < x[[PC]] & x[[PC]] <= b$ymax
                 pts <- x[in_x & in_y,]
                 x$selected_ <- in_x & in_y
        output$test_sel <- renderText(paste0(pts))
    })


    output$gg_pc1d <- renderPlot({
        x = dataset()

        cmap = unique(x[,c('abbrev', 'color')])
        col <- list(scale_color_manual(name=cmap$abbrev, values=cmap$color),
                    scale_fill_manual(name=cmap$abbrev, values=cmap$color));
        makePC(x, as.integer(config$PC), col)
    })
    return(config)
}

pca1d_args <-  c("get_dataset")
