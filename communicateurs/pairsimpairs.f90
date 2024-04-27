program PairsImpairs
    use mpi_f08
    implicit none

    integer, parameter  :: m=16
    integer             :: clef
    TYPE(MPI_Comm)      :: CommPairsImpairs
    integer             :: rang_dans_monde
    real, dimension(m)  :: a 

    call MPI_INIT()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang_dans_monde)

    ! Initialisation du vecteur A
    a(:) = 0.
    if (rang_dans_monde ==2) a(:) =2.
    if (rang_dans_monde ==3) a(:)=5.

    clef = rang_dans_monde
    if (rang_dans_monde ==2 .OR. rang_dans_monde == 5) then
        clef = -1
    end if

    ! Creation des communicateurs pair et impaire en leur donnant une meme denomination
    call MPI_COMM_SPLIT(MPI_COMM_WORLD, mod(rang_dans_monde,2), clef, CommPairsImpairs)

    ! Diffusion du message par le processus 0 de chaque communicateur aux processus
    ! de son groupe
    call MPI_BCAST(a,m,MPI_REAL, 0 , CommPairsImpairs)

    ! Destruction des communicateurs
    call MPI_COMM_FREE(CommPairsImpairs)
    call MPI_FINALIZE()
end program PairsImpairs
