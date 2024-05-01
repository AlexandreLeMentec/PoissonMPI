!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! -*- Mode: F90 -*- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! transpose.f90  --- Utilisation d'un type derive (type_transpose)
!!                    pour transposer une matrice.
!!
!!
!! Auteur          : Isabelle DUPAYS (CNRS/IDRIS - France)
!!                   <Isabelle.Dupays@idris.fr>
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


PROGRAM transpose
  USE MPI_F08
  IMPLICIT NONE
  INTEGER, PARAMETER                     :: nb_lignes=5,nb_colonnes=4,&
                                            etiquette=1000
  INTEGER                                :: rang,taille_reel,i
  TYPE(MPI_Datatype)                     :: type_ligne, type_transpose
  REAL, DIMENSION(nb_lignes,nb_colonnes) :: A
  REAL, DIMENSION(nb_colonnes,nb_lignes) :: AT
  INTEGER(kind=MPI_ADDRESS_KIND)         :: pas
  TYPE(MPI_Status)                       :: statut


  !Initialisation de MPI
  CALL MPI_INIT()

  !-- Savoir qui je suis
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,rang)

  !-- Initialisation de la matrice AT
  AT(:,:) = 0.

  !Connaitre la taille du type de base MPI_REAL
  CALL MPI_TYPE_SIZE(MPI_REAL,taille_reel)

  !Construction du type derive type_ligne qui correspond
  !a une ligne de la matrice A
  CALL MPI_TYPE_VECTOR(nb_colonnes,1,nb_lignes,MPI_REAL,type_ligne)

  !Construction du type derive type_transpose pour transposer la
  !matrice A composee de nb_lignes et de nb_colonnes
  pas=taille_reel
  CALL MPI_TYPE_CREATE_HVECTOR(nb_lignes,1,pas,type_ligne,type_transpose)

  !Validation du type cree type_transpose
  CALL MPI_TYPE_COMMIT(type_transpose)

  IF (rang == 0) THEN
    !Initialisation de la matrice A sur le processus 0
    A(:,:) = RESHAPE( (/ (i,i=1,nb_lignes*nb_colonnes) /), &
                      (/ nb_lignes,nb_colonnes /) )

    PRINT *,'Matrice A'
    DO i=1,nb_lignes
      PRINT *,A(i,:)
    END DO
    
    !Envoi de la matrice A au processus 1 avec le type type_transpose
    CALL MPI_SEND(A,1,type_transpose,1,etiquette,MPI_COMM_WORLD)

  ELSE
    !Reception pour le processus 1 dans la matrice AT
    CALL MPI_RECV(AT,nb_colonnes*nb_lignes,MPI_REAL,0,etiquette,&
                  MPI_COMM_WORLD,statut)

    PRINT *,'Matrice transposee AT'
    DO i=1,nb_colonnes
      PRINT *,AT(i,:)
    END DO

  END IF

  ! Liberation des types MPI
  CALL MPI_TYPE_FREE(type_transpose)
  CALL MPI_TYPE_FREE(type_ligne)
  !Sortie de MPI
  CALL MPI_FINALIZE()

END PROGRAM transpose

