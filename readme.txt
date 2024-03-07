mpirun -n 4 ./prog.exe                                                  ! 4 processeurs (on peut remplacer 4 par un autre nombre)
mpiexec --allow-run-as-root --use-hwthread-cpus ./prog.exe            ! tous les processeurs

mpif90 -c main.f90
mpif90 main.o -o main
mpiexec -n 4 ./main

mpiexec --oversubscribe -n 7 allreduce