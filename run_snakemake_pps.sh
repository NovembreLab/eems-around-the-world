snakemake -j400 --cluster-config config/cluster.json --cluster "qsub -l h_vmem={cluster.mem} -o {cluster.output} -e {cluster.err} -cwd -V  -l h=!spudling9\&!spudling11\&!spudling12 -S /bin/bash" $*
