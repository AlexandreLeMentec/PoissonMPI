program bsendrecv
    ! Faire un buffer pour éviter les communications bloquantes
    use mpi_f08
    use, INTRINSIC :: ISO_C_BINDING

    ! iso_c_binding is a standard intrinsic module which defines &
    ! named constants, types, and procedures for interoperability between C & Fortran.

    implicit none
    integer        ::  rang, valeur, num_proc, taille, surcout, & 
                        taille_buf
    integer, parameter  :: etiquette =110, nb_elt=1, nb_msg = 1
    integer, dimension(:), allocatable  :: buffer
    type(C_PTR) :: p 

    ! c_ptr :  Convert C into Fortran pointer ; CPTR, scalar of the type C_PTR
    ! derived type provided by the ISO C Binding that represents a C pointer. 
    ! It allows Fortran code to interact with C code that uses pointers.

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    call MPI_Type_size(MPI_INTEGER,taille)
    ! Retrieves the size of the MPI_INTEGER datatype.


    ! Convertir taille MPI_BSEND_OVERHEAD (octets) en nombre d’integer
    surcout = int(1+(MPI_BSEND_OVERHEAD*1.)/taille)
    allocate(buffer(nb_msg*(nb_elt+surcout)))
    taille_buf = taille * nb_msg * (nb_elt + surcout)
    call MPI_BUFFER_ATTACH(buffer,taille_buf)

    ! MPI_BSEND_OVERHEAD: This is a constant provided by MPI that represents the overhead & 
    ! associated with buffered sends. It typically represents the additional space required for buffering messages.

    ! On suppose avoir exactement 2 processus

    num_proc = mod(rang+1,2)
    call MPI_BSEND(rang+1000, nb_elt, MPI_INTEGER, num_proc, etiquette, MPI_COMM_WORLD)
    call MPI_RECV(valeur, nb_elt, MPI_INTEGER, num_proc, etiquette, MPI_COMM_WORLD, &
    MPI_STATUS_IGNORE)

    print*, 'Moi, processus', rang, ',ai recu', valeur, 'du processus', num_proc
    call MPI_BUFFER_DETACH(p,taille_buf)
    call MPI_Finalize()
 
end program bsendrecv


