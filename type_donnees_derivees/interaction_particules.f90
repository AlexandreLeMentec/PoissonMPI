program interaction_particules

    use mpi_f08
    implicit none

    integer, parameter      :: n=1000, etiquette=100
    TYPE(mpi_status)        :: statut
    integer                 :: rang, i
    TYPE(mpi_datatype)      :: type_particule, temp
    TYPE(mpi_datatype), dimension(4) :: types
    integer, dimension(4)       :: longueurs_blocs
    integer(kind=MPI_ADDRESS_KIND), dimension(5)     :: deplacements, adresses
    integer(kind=MPI_ADDRESS_KIND)      :: lb, extent

    type Particule
        character(len=5)        :: categorie
        integer                 :: masse
        real, dimension(3)      :: coords
        logical                 :: classe
    end type Particule
    type(Particule), dimension(n)   :: p, temp_p
    
    ! Initialisation du MPI
    call MPI_init( )
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    ! Construction du type de données
    types = (/ MPI_CHARACTER, MPI_INTEGER, MPI_REAL, MPI_LOGICAL/)
    longueurs_blocs = (/5,1,3,1/)
    call MPI_GET_ADDRESS(p(1), adresses(1))
    call MPI_GET_ADDRESS(p(1)%categorie , adresses(2))
    call MPI_GET_ADDRESS(p(1)%masse , adresses(3))
    call MPI_GET_ADDRESS(p(1)%coords , adresses(4))
    call MPI_GET_ADDRESS(p(1)%classe , adresses(5))

    ! Calcul des déplacements relatifs à l'adresse de départ
    do i =1,4
        deplacements(i) = MPI_AINT_DIFF(adresses(i+1), adresses(1))
    end do
    call MPI_TYPE_CREATE_STRUCT(4,longueurs_blocs, deplacements, types, temp)
    call MPI_GET_ADDRESS(p(2), adresses(2))
    lb = 0
    extent = MPI_AINT_DIFF(adresses(2),adresses(1))
    call MPI_TYPE_CREATE_RESIZED(temp, lb, extent, type_particule)

    ! Validation du type structure
    call MPI_TYPE_COMMIT(type_particule)

    ! Initialisation des particules pour chaque processus
    
    ! Envoi des particules de 0 vers 1
    if (rang == 0) then
        call MPI_SEND(p(1), n, type_particule,1,etiquette, MPI_COMM_WORLD)
    else
        call MPI_RECV(temp_p(1), n, type_particule, 0, etiquette, MPI_COMM_WORLD, &
        statut)
    endif

    ! Liberation du type
    call MPI_TYPE_FREE(type_particule)
    call MPI_FINALIZE()

end program interaction_particules
