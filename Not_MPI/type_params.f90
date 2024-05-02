!type_params.f90
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE  type_params
  IMPLICIT NONE

!! Initialisation des parametres
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !Nombre de points en et y
  INTEGER                                   :: ntx, nty
  !Nombre iterations maximum en temps
  INTEGER, PARAMETER                        :: it_max=100000
  !Argument muet de la fonction F90 EPSILON
  REAL, PARAMETER                  :: eps=EPSILON(1._8)

END MODULE  type_params