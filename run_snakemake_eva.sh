snakemake -w 30 --jobname "{rule}_{jobid}" -j800 \
    --cluster-config config/cluster.yaml \
    --cluster "qsub -q all.q -l h_vmem={cluster.mem} -o autosnake/ -e autosnake/ -cwd -V -S /bin/bash" $*  

