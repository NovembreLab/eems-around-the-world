update_config <- function(config, full_config, id){
    new_cfg <- full_config[[id]]
    if('copy' %in% names(new_cfg)){
	    copy_from_id <- new_cfg[['copy']]
        print(sprintf("copy from %s", copy_from_id))
	    config <- update_config(config, full_config, copy_from_id)
    }

    for(flag in names(new_cfg)){
        config[[flag]] <- new_cfg[[flag]]
    }

    return(config)
}

get_config <- function(snakemake,  plotname,
		       name=snakemake@wildcards$name){
    config <- snakemake@config$plot[['__default__']]

    if (plotname %in% names(snakemake@config$plot)){
        plot_config <- snakemake@config$plot[[plotname]]
        print(plot_config[['__default__']])
	config <- update_config(config, plot_config, '__default__')
    }

    if(name %in% names(plot_config)){
	config <- update_config(config, plot_config, name)
    }
    return(config)
}

