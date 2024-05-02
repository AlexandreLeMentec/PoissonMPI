program open01
    use mpi_f08
    implicit none
    character(len=MPI_MAX_PROCESSOR_NAME) :: texte_erreur
    TYPE(MPI_FILe)                        :: descripteur
    integer                              :: code, texte_longueur

    call MPI_INIT()

    call MPI_FILE_OPEN(MPI_COMM_WORLD, 'fichier.txt',& 
             MPI_MODE_RDWR + MPI_MODE_CREATE, MPI_INFO_NULL, descripteur, code)

    if (code /= MPI_SUCCESS) then
        call MPI_ERROR_STRING(code, texte_erreur, texte_longueur)
        print *, texte_erreur(1:texte_longueur)
        call MPI_ABORT(MPI_COMM_WORLD,42)
    end if

    call MPI_FILE_CLOSE(descripteur, code)
    if (code /= MPI_SUCCESS) then
        print *, 'Erreur fermeture fichier'
        call MPI_ABORT(MPI_COMM_WORLD,42)
    end if

    call MPI_FINALIZE()
end program open01