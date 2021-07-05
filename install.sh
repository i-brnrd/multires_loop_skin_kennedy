#!/bin/bash

function showhelp
{

  echo 'Usage: ./install.sh [option] [option]...'
  echo 'Compile and run mcgrid.'
  echo 
  echo '   -h, --help            Shows this dialog.'
  echo '   -n, --cores           Compiles and run code on n cores. Default is 1 core.'
  echo '   -d, --debug           Compiles and runs code with debug flags on n core.'
  echo '   -m, --make            Compiles the code with warning enabled.'

}

function makebuild
{
  if [ "$comp" = 'gnu' ];then
      string="FCOMP=mpifort"
    elif [ "$comp" = 'intel' ];then
      string="FCOMP=mpiifort"
  fi

  if [ "$debug" = 1 ];then
    make clean && make debug $string
elif [ "$SLURM_NPROCS" = 0 ];then
    make clean && make build $string
else
    if [ "$comp" = 'gnu' ];then
      make clean && make $string
    elif [ "$comp" = 'intel' ];then
      make clean && make $string
    fi
fi
}

function createdirs
{
  if [ ! -d "data" ]; then
      mkdir "data"
      mkdir "data/jmean"
      mkdir "data/im"
      mkdir "data/deposit"
  fi

  if [ ! -d "build" ]; then
     mkdir "build"
  fi
  cd build
  ndirec="$(pwd)"
  cd ..
  if [ ! -d "bin" ]; then
     mkdir "bin"
  fi
  cd bin
  bdirc="$(pwd)"
  cd ..
  cd src
}

function run
{
  for i in *; do
     if [ "${i}" != "${i%.mod}" ];then
        cp "${i}" "$ndirec"
     fi
     if [ "${i}" != "${i%.o}" ];then
        mv "${i}" "$ndirec"
     fi
  done


  if [ "$SLURM_NPROCS" = "0" ]; then #just make code
      exit 0
  fi
  echo $(pwd)
  mv mcgrid "$bdirc" && echo " "&& echo "*****Install complete*****" && echo " "

  clear
  cd ../bin

  if [ "$SLURM_NPROCS" = "1" ]; then
      ./mcgrid
  else
    if [ $comp = 'gnu' ];then
       /gpfs1/apps/software/devts8/bin/mpirun -n $SLURM_NPROCS ./mcgrid
     # /usr/local/bin/mpirun -n $SLURM_NPROCS ./mcgrid
     # /opt/openmpi/bin/mpirun -n $SLURM_NPROCS ./mcgrid
    elif [ $comp = 'intel' ];then
      mpirun -n $SLURM_NPROCS ./mcgrid
    fi
  fi
}

#defaults
SLURM_NPROCS=1
debug=0
help=0
comp="gnu" 

set -e

createdirs

while [ "$1" != "" ]; do
    case $1 in
        -n | --cores )          SLURM_NPROCS=$2
                                ;;
        -c | --comp )           comp=$2
                                ;;
        -h | --help )           showhelp
                                exit
                                ;;
        -m | --make )           SLURM_NPROCS=0
                                makebuild
                                exit
                                ;;
        -d | --debug )          debug=1
                                ;;
    esac
    shift
done

makebuild
run
