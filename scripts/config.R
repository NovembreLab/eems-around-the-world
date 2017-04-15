get_config <- function(snakemake,  plotname,
		       name=snakemake@wildcards$name){
    config <- snakemake@config$plot[['__default__']]

    if (plotname %in% names(snakemake@config$plot)){
        plot_config <- snakemake@config$plot[[plotname]]
        print(plot_config[['__default__']])
	for(flag in names(plot_config[['__default__']])){
	    config[[flag]] <- plot_config[['__default__']][[flag]]
	}
    }

    if(name %in% names(plot_config)){
	for(flag in names(plot_config[[name]])){
	    config[[flag]] <- plot_config[[name]][[flag]]
	}
    }
    config
}

