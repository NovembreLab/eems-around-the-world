source("modules/map.R")
source("modules/pca.R")
source("modules/pca1D.R")


setup_modules <- function(){
    modules <- list()
    modules$options_ui <- list()
    modules$main_ui <- list()
    modules$plot <- list()
    modules$call <- list()
    modules$from_readable_name <- list()
    modules
}

add_plot_module <- function(modules, module_name, readable_name, args_str){
    modules$options_ui[[module_name]] <- match.fun(sprintf("%s_options_UI", module_name))
    modules$main_ui[[module_name]] <- match.fun(sprintf("%s_main_UI", module_name))
    modules$plot[[module_name]] <- match.fun(sprintf("%s_plot", module_name))
    modules$from_readable_name[[readable_name]] <- module_name
    modules$readable_names[[module_name]] <- readable_name
    modules$names <- c(modules$names, module_name)
    modules$args[[module_name]] <- args_str
    return(modules)
}

modules <- setup_modules()
modules <- add_plot_module(modules,  "pc2d", "PCA", pc2d_args)
modules <- add_plot_module(modules,  "map", "Map", map_args)
modules <- add_plot_module(modules,  "pca1d", "1D PCA (vioplot)", pca1d_args)

modules <<- modules
