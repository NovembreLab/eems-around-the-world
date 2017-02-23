CLS_BATCH="sbatch -t {cluster.time} --mem {cluster.mem} -o {cluster.output} -e {cluster.err} --cpus-per-task {cluster.cpus-per-task}"
CLS_MODULE="module load  gcc/6.1 geos gdal boost/1.61"
CLS_ENV="module load python/3.3 Anaconda3; source activate all2 "
#CLS_PARS="$CLS_ENV; $CLS_MODULE ; $CLS_BATCH "
CLS_PARS="$CLS_ENV; $CLS_MODULE ; $CLS_BATCH --partition jnovembre"
#CLS_PARS="$CLS_ENV; $CLS_MODULE ; $CLS_BATCH --partition=\"jnovembre,amd,sandyb,westmere\""

snakemake -p -j 65 --cluster-config config/cluster.json --cluster "$CLS_PARS" $*
