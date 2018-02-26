require(conStruct)
True = T
rstan_options(auto_write = TRUE)
options(mc.cores = snakemake@threads)

K <- as.integer(snakemake@wildcards$K)

outname <- sprintf("construct/%s/K%s", snakemake@wildcards$name, K)

x <- conStruct(spatial=True, 
	  K=K,
	  freqs = readRDS(snakemake@input$mat_rds)/2,
	  coords = readRDS(snakemake@input$coord_rds),
	  geoDist = readRDS(snakemake@input$dist_rds),
	  prefix=outname,
	  n.chains=2,
	  n.iter = K* 2000,
	  make.figs = TRUE,
	  save.files = TRUE)
saveRDS(x, paste0(outname, ".rds"))
