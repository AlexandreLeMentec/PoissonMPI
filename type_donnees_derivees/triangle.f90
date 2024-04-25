program triangle
    use mpi_f08
    implicit none

    integer, parameter          :: n=8, etiquette = 100
    real, dimension(n,n)        :: a
    type(mpi_status)            :: statut
    integer                     :: i 
    integer                     :: rang
    type(mpi_datatype)          :: type_triangle
    integer, dimension(n)       :: longueurs_blocs, deplacements

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)

    ! Initialisation de la matrice sur chaque processus
    a(:,:) =reshape ((/ (sign(i,-rang), i=1, n*n) /), (/n,n/))

    ! Création du type matrice triangulaire sup pour le processus 0
    ! et du type matrice triangulaire inf pour le processus 1

    if (rang==0) then
        longueurs_blocs(:) = (/ (i-1, i=1, n)/)
        deplacements(:) = (/ (n*(i-1), i=1, n)/)
    else
        longueurs_blocs(:) = (/ (n-i, i=1, n)/)
        deplacements(:) = (/ (n*(i-1)+i, i=1, n)/)
    end if

    call MPI_TYPE_INDEXED(n, longueurs_blocs, deplacements, MPI_REAL, type_triangle)
    call MPI_TYPE_COMMIT(type_triangle)

    ! Permutation des matrices triangulaires sup et inf
    call MPI_SENDRECV_REPLACE(a, 1, type_triangle, mod(rang+1,2), etiquette,mod(rang+1,2), &
     etiquette, MPI_COMM_WORLD, statut)

    ! Liberation du type triangle
    call MPI_TYPE_FREE(type_triangle)
    call MPI_FINALIZE()

    print *, "Processus ", rang
    do i=1,n
        print *, a(i,:)
    end do

end program triangle