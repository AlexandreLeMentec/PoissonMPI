program CommCartSub
    use mpi_f08
    implicit none

    TYPE(MPI_Comm)          :: Comm2D, Comm1D
    integer                 :: rang
    integer, parameter      :: NDim2D = 2
    integer, dimension(NDim2D)    :: Dim2D, Coord2D
    logical, dimension(NDim2D)    :: Periode, conserve_dims
    logical                       :: ReOrdonne
    integer, parameter            :: m=4
    real, dimension(m)            :: V = 0.
    real                          :: W=0.

    call MPI_INIT()

    ! Creation de la grille 2D initiale
    Dim2D(1) =4
    Dim2D(2) = 3
    Periode(:) = .false.
    ReOrdonne = .false.

    call MPI_CART_CREATE(MPI_COMM_WORLD, NDim2D, Dim2D, Periode, ReOrdonne, Comm2D)
    call MPI_Comm_rank(Comm2D, rang)
    call MPI_CART_COORDS(Comm2d, rang, NDim2D, Coord2D)

    ! Initialisation du vecteur V
    if (Coord2D(1) == 1) V(:) = real(rang)
    
    ! Chaque ligne de la grille doit etre une topologie cartesienne 1D
    conserve_dims(1) = .true.
    conserve_dims(2) = .false.

    ! Subdivision de la grille cartesienne 2D
    call MPI_CART_SUB(Comm2D, conserve_dims, Comm1D)

    ! Les processus de la colonne 2 distribuent le vecteur V aux processus de leur ligne
    call MPI_SCATTER(V,1,MPI_REAL,W,1,MPI_REAL,1,Comm1D)

    print'("Rang : ",I2, "; Coordonnees : ("I1,",",I1,") ; W = ",F2.0)', & 
    rang, Coord2D(1), Coord2D(2), MPI_COMM_WORLD

    call MPI_FINALIZE()
end program CommCartSub