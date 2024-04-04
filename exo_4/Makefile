############################ -*- Mode: Makefile -*- ###########################
## Makefile --- Cours MPI : TP4 : Type de données derivées (type_transpose)
##
## Auteur          : Isabelle Dupays (CNRS/IDRIS) <Isabelle.Dupays@idris.fr>
## 
###############################################################################

# Compilateurs, options de compilation et d'édition de liens
include ../arch/make_inc

OBJS = transpose.o

# Règle implicite de compilation
.SUFFIXES: .o .f90
%.o : %.f90
	$(CF95) -c $(FFLAGS) $<

all : transpose

transpose: $(OBJS)
	$(CF95) -o $@ $(OBJS) $(LDFLAGS_TP4)

.PHONY: clean exe

exe : transpose
	/usr/bin/mpiexec -n 2 ./transpose
#SBATCH	sbatch transpose.slurm

clean :
	rm -f $(OBJS) transpose core
