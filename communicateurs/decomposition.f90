program decomposition
    use mpi_f08
    implicit none

    integer                  :: rang_ds_topo, nb_procs
    TYPE(MPI_Comm)           :: comm_2D
    integer, dimension(4)    :: voisin
    integer,parameter        :: N=1, E=2, S=3, W=4
    integer, parameter       :: ndims = 2
    integer, dimension(ndmis)  :: dims, coords
    logical, dimension(ndims)  :: periods
    logical                 :: reorganisation

    call MPI_INIT()

    call MPI_COMM_SIZE(MPI_COMM_WORLD, nb_procs)

    ! Connaitre le nombre de processus suivant x et y
    dims(:) = 0

    call MPI_DIMS_CREATE(nb_procs, ndmis, dims)
    
    ! Creation grille 2D periodique en y
    periods(1) = .false.
    periods(2) = .true.
    reorganisation = .false.

    call MPI_CART_CREATE(MPI_COMM_WORLD, ndims, dims, periods, reorganisation, comm_2D)

    ! Connaitre mes coordonnees dans la topologie
    call MPI_Comm_rank(comm_2D, rang_ds_topo)
    call MPI_CART_COORDS(comm_2D, rang_ds_topo, ndims, coords)

    ! Recherche de mes voisins Ouest et Est
    call MPI_CART_SHIFT(comm_2D, 0,1, voisin(W), voisin(E))

    ! Recherche de mes voisins Sud et Nord
    call MPI_CART_SHIFT(comm_2D, 0,1, voisin(S), voisin(N))

    call MPI_FINALIZE()

end program decomposition
