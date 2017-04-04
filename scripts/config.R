get_config <- function(snakemake,  plotname,
		       name=snakemake@wildcards$name){
    all_cfg <- snakemake@config$plot[[plotname]]
    config <- all_cfg[['__default__']]
    if(name %in% names(all_cfg)){
	for(flag in names(all_cfg[[name]])){
	    config[[flag]] <- all_cfg[[name]][[flag]]
	}
    }
    config
}

