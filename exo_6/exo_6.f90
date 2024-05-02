program exo_6
    use MPI_F08
  
    implicit none
  
    TYPE(MPI_Comm)                 :: CommCart2D,CommCart1D
    integer, parameter             :: NDimCart2D=2
    integer, dimension(NDimCart2D) :: DimCart2D,CoordCart2D
    logical, dimension(NDimCart2D) :: Periode
    logical                        :: Reordonne
    integer                        :: nb_procs,rang,i
    integer, parameter             :: m=4
    real, dimension(m)             :: V
    real                           :: W
  
    call MPI_INIT()
    call MPI_COMM_SIZE( MPI_COMM_WORLD, nb_procs)
  
    !*** Caractéristiques de la topologie cartésienne 2D
    DimCart2D(1) = 4
    DimCart2D(2) = 2
    if (DimCart2D(1)*DimCart2D(2) /= nb_procs) then
      ! On arrete l'execution du code
      print *, "Ce n'est pas le nombre de processeurs nécessaire !"
      call MPI_ABORT(MPI_COMM_WORLD,1)
    end if
  
    Periode(:)   = .false.
    ReOrdonne    = .false.
  
    !*** Création du communicateur CommCart2D (topologie cartésienne 2D)
    call MPI_CART_CREATE(MPI_COMM_WORLD, NDimCart2D, DimCart2D, Periode, &
                         Reordonne, CommCart2D)
  
    call MPI_COMM_RANK(CommCart2D, rang)
    call MPI_CART_COORDS(CommCart2D, rang, NDimCart2D, CoordCart2D)
  
    !*** Initialisation du vecteur V et du scalaire W
    V(:) = 0.
    W = 0.
    if (CoordCart2D(1) == 1) V(:) = (/ (real(i), i=1,m) /)
  
    !*** Subdivision de la grille 2D à l'aide de MPI_COMM_SPLIT.
    call MPI_COMM_SPLIT(CommCart2D, CoordCart2D(2), rang, CommCart1D)
  
    !*** Les processus de la 2eme colonne diffusent sélectivement
    !*** le vecteur V aux processus de leur ligne
    call MPI_SCATTER(V,1,MPI_REAL,W,1,MPI_REAL,1,CommCart1D)
  
    print '("Rang : ",I2," ; Coordonnees : (",I1,",",I1,") ; W = ",F2.0)', &
          rang,CoordCart2D(1),CoordCart2D(2),W
  
    !*** Destruction des communicateurs
    call MPI_COMM_FREE(CommCart1D)
    call MPI_COMM_FREE(CommCart2D)
  
    call MPI_FINALIZE()
  
  end program exo_6