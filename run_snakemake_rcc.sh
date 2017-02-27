CLS_BATCH="sbatch -t {cluster.time} --mem {cluster.mem} -o {cluster.output} -e {cluster.err} --cpus-per-task {cluster.cpus-per-task}"
CLS_MODULE="module load  boost/1.62.0 gcc/6.1 geos gdal "
CLS_ENV="module load Anaconda3; source activate all2 "
#CLS_PARS="$CLS_ENV; $CLS_MODULE ; $CLS_BATCH "
CLS_PARS="$CLS_ENV; $CLS_MODULE ; $CLS_BATCH --partition jnovembre"
#CLS_PARS="$CLS_ENV; $CLS_MODULE ; $CLS_BATCH --partition sandyb"

snakemake -p -j 65 --cluster-config config/cluster.json --cluster "$CLS_PARS" $*
