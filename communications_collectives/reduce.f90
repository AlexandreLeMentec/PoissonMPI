program reduce
    use mpi_f08
    implicit none
    integer     :: nb_procs, rang, valeur, somme

call MPI_INIT()
call MPI_Comm_size(MPI_COMM_WORLD, nb_procs)
call MPI_Comm_rank(MPI_COMM_WORLD, rang)

if (rang == 0) then 
    valeur =1000
else
    valeur = rang
endif

call MPI_reduce(valeur,somme,1,MPI_INTEGER, MPI_sum, 0, MPI_COMM_WORLD)

if (rang==0) then
    print*, 'Moi, processus 0, ma valeur de la somme globale est', somme
endif

call MPI_Finalize()
end program reduce