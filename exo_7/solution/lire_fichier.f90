!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! -*- Mode: F90 -*- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! lire_fichier.f90 --- T.P. 7 du cours MPI (à exécuter sur 4 processus)
!! 
!! Auteur          : Denis GIROU (CNRS/IDRIS - France) <Denis.Girou@idris.fr>
!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program lire_fichier

  use MPI_F08

  implicit none

  integer, parameter                  :: nb_valeurs=121
  integer(kind=MPI_OFFSET_KIND)       :: position_fichier
  integer, dimension(nb_valeurs)      :: valeurs
  TYPE(MPI_Status)                    :: statut
  integer                             :: rang,nb_octets_entier,code
  TYPE(MPI_File)                      :: descripteur

  call MPI_INIT()
  call MPI_COMM_RANK(MPI_COMM_WORLD,rang)
  call MPI_FILE_SET_ERRHANDLER(MPI_FILE_NULL,MPI_ERRORS_ARE_FATAL)

  ! Ouverture du fichier "donnees.dat" en lecture
  call MPI_FILE_OPEN(MPI_COMM_WORLD,"donnees.dat",MPI_MODE_RDONLY, &
                     MPI_INFO_NULL,descripteur)

  ! Lecture via des déplacements explicites, en mode individuel
  valeurs(:)=0
  call MPI_TYPE_SIZE(MPI_INTEGER,nb_octets_entier)
  position_fichier=rang*nb_valeurs*nb_octets_entier
  call MPI_FILE_READ_AT(descripteur,position_fichier,valeurs,nb_valeurs, &
                        MPI_INTEGER,statut)
  open(unit=45,file="fichier_dei"//achar(48+rang)//".dat")
  write(unit=45,fmt='(I3)') valeurs(:)
  close(unit=45)

  ! Lecture via les pointeurs partagés, en mode collectif
  valeurs(:)=0
  call MPI_FILE_READ_ORDERED(descripteur,valeurs,nb_valeurs, &
                             MPI_INTEGER,statut)
  open(unit=45,file="fichier_ppc"//achar(48+rang)//".dat")
  write(unit=45,fmt='(I3)') valeurs(:)
  close(unit=45)

  ! Lecture via les pointeurs individuels, en mode individuel
  valeurs(:)=0
  position_fichier = rang*nb_valeurs*nb_octets_entier
  call MPI_FILE_SEEK(descripteur,position_fichier,MPI_SEEK_SET)
  call MPI_FILE_READ(descripteur,valeurs,nb_valeurs, &
                     MPI_INTEGER,statut)
  open(unit=45,file="fichier_pii"//achar(48+rang)//".dat")
  write(unit=45,fmt='(I3)') valeurs(:)
  close(unit=45)

  ! Lecture via les pointeurs partagés, en mode individuel
  ! (on doit tout d'abord repositionner le pointeur partagé au début du fichier)
  valeurs(:)=0
  position_fichier=0
  call MPI_FILE_SEEK_SHARED(descripteur,position_fichier,MPI_SEEK_SET)
  call MPI_FILE_READ_SHARED(descripteur,valeurs,nb_valeurs, &
                            MPI_INTEGER,statut)
  open(unit=45,file="fichier_ppi"//achar(48+rang)//".dat")
  write(unit=45,fmt='(I3)') valeurs(:)
  close(unit=45)

  ! Fermeture du fichier
  call MPI_FILE_CLOSE(descripteur)

  call MPI_FINALIZE()

end program lire_fichier
