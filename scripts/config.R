update_config <- function(config, full_config, id){
    new_cfg <- full_config[[id]]
    if('copy' %in% names(new_cfg)){
	    copy_from_id <- new_cfg[['copy']]
        print(sprintf("copy from %s", copy_from_id))
	    config <- update_config(config, full_config, copy_from_id)
    }

    config <- modifyList(config, new_cfg)
    return(config)
}

get_config <- function(snakemake,  plotname,
		       name=snakemake@wildcards$name){
    print('---;')

    #get global default
    config <- snakemake@config$plot[['__default__']]


    #check plot$copy_from whether we start with another template - this is
    #recursive
    copied_default <- F
    if("__copy_from__" %in%  names(snakemake@config$plot)){
        copy_from <- snakemake@config$plot[['__copy_from__']]
        if(name %in% names(copy_from)){
            config0 <- get_config(snakemake, plotname, copy_from[[name]])
            config <- modifyList(config, config0)
            copied_default <- T
        }
    }

    print(names(config))
    print(copied_default)


    #if we have any fields relevant for this plot
    if (plotname %in% names(snakemake@config$plot)){
        plot_config <- snakemake@config$plot[[plotname]]
        if(!copied_default){
            config <- update_config(config, plot_config, '__default__')
        }
    }

    if(name %in% names(plot_config)){
        config <- update_config(config, plot_config, name)
    }
    print("####################################")
    print("#####          CFG               ###")
    print(config)
    print("####################################")
    return(config)
}

