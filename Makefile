############################ -*- Mode: Makefile -*- ###########################
## Makefile --- Cours MPI : TP8 : Equation de poisson
##
## Auteur          : Isabelle Dupays (CNRS/IDRIS) <Isabelle.Dupays@idris.fr>
## 
###############################################################################
SHELL := /bin/bash
# Compilateur et options de compilation et de link
include arch/make_inc

OBJS = type_params.o parallel.o calcul_poisson.o poisson.o
OBJS1 = read.o
OBJS2 = calcul_exact.o
OBJS3 = type_params.o parallel.o calcul_poisson.o poisson_non_bloquant.o

# Règle implicite de compilation
.SUFFIXES: .o .f90
%.o : %.f90
	$(CF95) -c  $(FFLAGS) $<

default: poisson

all: poisson read calcul_exact

poisson: $(OBJS)
	$(CF95) -o $@ $(OBJS) $(FFLAGS)

poisson_non_bloquant: $(OBJS3)
	$(CF95) -o $@ $(OBJS3) $(FFLAGS)

read: $(OBJS1)
	$(CF95) -o $@ $(OBJS1) $(FFLAGS)

calcul_exact: $(OBJS2)
	$(CF95) -o $@ $(OBJS2) $(FFLAGS)

fort.11: read donnees.dat
	$(EXEC_TP8_VERIF) ./read

.PHONY: exe clean verification

exe donnees.dat: poisson poisson.data
	rm -f donnees.dat
	/usr/bin/mpiexec mpiexec --allow-run-as-root --use-hwthread-cpus ./poisson
#SBATCH	sbatch poisson.slurm

verification: fort.11 calcul_exact
	$(EXEC_TP8_VERIF) ./calcul_exact

clean:
	rm -f $(OBJS) $(OBJS1) $(OBJS2) $(OBJS3) poisson poisson_non_bloquant calcul_exact read *.mod core donnees.dat fort.10 fort.11 *.mod
