! Sur 1 processus !

program creation

  use mpi_f08

  implicit none

  integer, parameter                  :: nb_valeurs=242
  integer, dimension(2*nb_valeurs)    :: valeurs
  type(MPI_Status)                    :: statut
  integer                             :: i,rang
  type(MPI_File)                      :: descripteur

  open(unit=45,file="carte.dat")
  ! Lecture X Y positions
  do i=1,nb_valeurs
    read(45,*) valeurs(i),valeurs(nb_valeurs+i)
  end do
  close(45)
  ! Valeurs=All X positions then All Y positions

  call MPI_INIT()
  call MPI_COMM_RANK(MPI_COMM_WORLD,rang)
  call MPI_FILE_SET_ERRHANDLER(MPI_FILE_NULL,MPI_ERRORS_ARE_FATAL)
  call MPI_FILE_OPEN(MPI_COMM_WORLD,"donnees.dat",MPI_MODE_RDWR + MPI_MODE_CREATE, &
                     MPI_INFO_NULL,descripteur)

  call MPI_FILE_WRITE_ALL(descripteur,valeurs,2*nb_valeurs,MPI_INTEGER, &
                          statut)

  call MPI_FILE_CLOSE(descripteur)
  call MPI_FINALIZE()

end program creation
