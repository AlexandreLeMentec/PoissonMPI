program ex_fence
    use mpi_f08
    implicit none

    integer, parameter :: assert = 0
    integer :: code, rang, taille_reel, i, nb_elements, cible, m=4, n=4
    type(mpi_win) :: win
    integer (kind= MPI_ADDRESS_KIND) :: deplacement, dim_win
    real(kind= kind(1.d0)), dimension(:), allocatable :: win_local, tab

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)
    call MPI_Type_size(MPI_DOUBLE_PRECISION, taille_reel)

    if (rang == 0) then
        n=0
        allocate(tab(m))
    endif

    allocate(win_local(n))
    dim_win = taille_reel*N
    
    call MPI_WIN_CREATE(win_local, dim_win, taille_reel, MPI_INFO_NULL, & 
        MPI_COMM_WORLD, win)

    if (rang ==0) then
        tab(:) = (/ (i, i=1,m)/)
    else 
        win_local(:) = 0.0
    end if

    call MPI_WIN_FENCE(assert,win)
    if (rang == 0) then
        cible = 1 ; nb_elements = 2; deplacement = 1
        call MPI_PUT(tab, nb_elements, MPI_DOUBLE_PRECISION, cible, deplacement, & 
            nb_elements, MPI_DOUBLE_PRECISION, win)

    end if 

    call MPI_WIN_FENCE(assert,win)
    if (rang == 0 ) then
        tab(m) = sum(tab(1:m-1))
    else
        win_local(n) = sum(win_local (1 :n-1))
    endif

    call MPI_WIN_FENCE(assert,win)
    if (rang == 0) then 
        nb_elements =1 ; deplacement = m-1 
        call MPI_GET(tab, nb_elements, MPI_DOUBLE_PRECISION, cible, deplacement, & 
            nb_elements, MPI_DOUBLE_PRECISION, win)
    endif
    
    call mpi_finalize()
end program ex_fence