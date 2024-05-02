FC = gfortran
OPT = -g #-O0 -fbounds-check

OBJ = type_params.o poisson_notmpi.o subroutines_notmpi.o calc_matriciel.o
EXE = prog.exe

prog:	$(OBJ)
	$(FC) $(OPT) $(OBJ) -o $(EXE)

type_params.o:	type_params.f90
	$(FC) $(OPT) type_params.f90 -c	

poisson_notmpi.o :	poisson_notmpi.f90
	$(FC) $(OPT) poisson_notmpi.f90 -c

subroutines_notmpi.o :	subroutines_notmpi.f90
	$(FC) $(OPT) subroutines_notmpi.f90 -c

clean :
	/bin/rm -f $(OBJ) $(EXE) $(MODS) *.mod