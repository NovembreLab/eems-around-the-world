library(shiny)
library(jsonlite)

subsets <- names(jsonlite::fromJSON("config/subset.json")$subset)


plot_types = modules$from_readable_name
print(c("loaded types:", plot_types))

shinyUI(fluidPage(
    titlePanel("PCA viz experiments"),

    sidebarLayout(
	sidebarPanel(
	    helpText("Visualize PCA. The three plots created are:
                 1. Map of samples (with colors the same as in other plots)
                 2. PC X vs PC Y (PCs chosen from sliders)
                 3. Violinplot of PCX (first slider). 
                 "),

	    checkboxGroupInput("plot_selector", 
		label="pick which plots to display",
		choices=plot_types,
		selected=plot_types
	    ),

	    selectInput("which_data", 
		label = "Choose a Dataset to display",
		choices=subsets[-1], selected='oceania'
	    ),
	    radioButtons("coloring", 
		label = "How should data be colored?",
		choices=c('Population', 'Source'),
		selected='Population'
	    ),
	    selectInput("which_options", 
		label = "Choose a Plot for which to display options",
		choices=plot_types, selected=plot_types[1]
	    ),
	    uiOutput("options"),
	    textOutput("selecttest"),
	width=3),
	mainPanel(
        #plotOutput("plot"),

        uiOutput("element")
	    #leafletOutput("map_leaflet"),
	    #plotOutput("map"),
	    #leafletOutput("pc2d_leaflet"),
	    #plotOutput("pc1d")
	)
    )
))
