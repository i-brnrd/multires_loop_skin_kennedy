#!/bin/bash
#SBATCH -J loop_var_res_kenned2
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=2
#SBATCH -p parallel


source /opt/rh/devtoolset-8/enable
export PATH=/gpfs1/apps/software/devts8/bin:$PATH
export LD_LIBRARY_PATH=/gpfs1/apps/software/devts8/lib:$LD_LIBRARY_PATH
#export FC=gfortran-4.8
export OMP_NUM_THREADS=1
ulimit -s unlimited



./install.sh -n 2
  
