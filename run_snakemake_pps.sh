snakemake -j110 --cluster-config config/cluster.json --cluster "qsub -l h_vmem={cluster.mem} -o {cluster.output} -e {cluster.err} -cwd -V  -l h=!bigmem02\&!spudling11 -S /bin/bash" $*
