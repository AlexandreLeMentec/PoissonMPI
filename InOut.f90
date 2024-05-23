!========================================================================
!
!                   S H A L A  Version 0.1
!
!========================================================================
!
! Copyright  Frédéric Couderc
!
! couderc@math.univ-toulouse.fr ; frederic.couderc@orange.fr
!
! This file is part of the SHALA software, a Computational Fluid Dynamics
! program whose purpose is to simulate diphasic immiscible fluids flow
! using meanwhile Level Set and Ghost Fluid methods.
!
! This software is governed by the CeCILL license under French law
! and abiding by the rules of distribution of free software. You can
! use, modify and/or redistribute the software under the terms of the
! CeCILL license as circulated by CEA, CNRS and INRIA at the following
! URL: "http://www.cecill.info".
!
! As a counterpart to the access to the source code and rights to copy,
! modify and redistribute granted by the license, users are provided
! only with a limited warranty and the software's author, the holder of
! the economic rights, and the successive licensors have only limited
! liability.
!
! In this respect, the user's attention is drawn to the risks associated
! with loading, using, modifying and/or developing or reproducing the
! software by the user in light of its specific status of free software,
! that may mean that it is complicated to manipulate, and that also
! therefore means that it is reserved for developers and experienced
! professionals having in-depth computer knowledge. Users are therefore
! encouraged to load and test the software's suitability as regards
! their requirements in conditions enabling the security of their
! systems and/or data to be ensured and, more generally, to use and
! operate it in the same conditions as regards security.
!
! The fact that you are presently reading this means that you have had
! knowledge of the CeCILL license and that you accept its terms.
!
!========================================================================
!**
!**   NAME       : InOut.f90
!**
!**   AUTHOR     : Frédéric COUDERC
!**
!**   FUNCTION   : Subprograms to compute ouput files for post-processing
!**
!**   DATES      : # Version 0.1  : from : 17 mar 2010
!**
!========================================================================

SUBROUTINE write_reprise(phi,u,v,w,pres,rho,tp)

  USE Parametres

  implicit none

  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3  ), intent(in) :: phi
  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3,2), intent(in) :: u,v,w,pres,rho,tp

  integer :: i,j,k,p
  character*30 :: nom_fichier
  character*4  :: num

  if ( proc == 0 ) then

     write(num,'(i4.4)') 0
     nom_fichier = trim('fichier_reprise')//num//'.bin'
     open(unit=30,file=nom_fichier,status='replace',form='unformatted')
     write(30) t
     close(30)

  end if

  write(num,'(i4.4)') proc+1

  nom_fichier = trim('fichier_reprise')//num//'.bin'

  open(unit=30,file=nom_fichier,status='replace',form='unformatted')

                        write(30) u
                        write(30) v
                        write(30) w
  if ( LEVEL_SET == 1 ) write(30) phi
  if ( cop       == 2 ) write(30) pres
  if ( cop       == 2 ) write(30) rho
  if ( EBUL      == 1 ) write(30) tp

  close(30)

  call MPI_barrier(comm3d,code)

  return

END SUBROUTINE write_reprise


SUBROUTINE read_reprise(phi,u,v,w,pres,rho,tp)

  USE Parametres

  implicit none

  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3  ), intent(out) :: phi
  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3,2), intent(out) :: u,v,w,pres,rho,tp

  integer :: i,j,k,p
  character*30 :: nom_fichier
  character*4  :: num

  if ( proc == 0 ) then

     write(num,'(i4.4)') 0

     nom_fichier = trim('fichier_reprise')//num//'.bin'

     open(unit=30,file=nom_fichier,status='old',form='unformatted')

     read(30) t

     close(30)

  end if

  call MPI_BCAST(t,1,realtype,0,comm3d,code)

  write(num,'(i4.4)') proc+1
  nom_fichier = trim('fichier_reprise')//num//'.bin'
  open(unit=30,file=nom_fichier,status='old',form='unformatted')

                        read(30) u
                        read(30) v
                        read(30) w
  if ( LEVEL_SET == 1 ) read(30) phi
  if ( cop       == 2 ) read(30) pres
  if ( cop       == 2 ) read(30) rho
  if ( EBUL      == 1 ) read(30) tp

  close(30)

  call MPI_barrier(comm3d,code)

  return

END SUBROUTINE read_reprise


SUBROUTINE write_reprise_mpi(phi,u,v,w,pres,rho,tp)

  USE Parametres

  implicit none

  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3  ), intent(in) :: phi
  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3,2), intent(in) :: u,v,w,pres,rho,tp

  integer :: descripteur,type_carre,type_cube,nb_octets_real

  integer(kind=MPI_address_kind) :: pas
  integer(kind=MPI_offset_kind ) :: position

  integer, dimension(MPI_status_size) :: statut

  call MPI_file_open(comm3d,'fichier_reprise.bin',MPI_mode_wronly+MPI_mode_create,MPI_info_null,descripteur,code)

  call MPI_type_size(MPI_double_precision,nb_octets_real,code)

  position = 0

  call MPI_file_write_at_all(descripteur,position,t,1,MPI_double_precision,statut,code)

  position = position + nb_octets_real

  call MPI_type_vector(ny/np_y,nx/np_x,nx,MPI_double_precision,type_carre,code)
  call MPI_type_commit(type_carre,code)

  pas = nx*ny*nb_octets_real

  call MPI_type_hvector(nz/np_z,1,pas,type_carre,type_cube,code)
  call MPI_type_commit (type_cube,code)

  position = position + ( nx*ny*(sz-1) + nx*(sy-1) + sx-1 ) * nb_octets_real

  call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_write_all(descripteur,u(sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  position = position + nx*ny*nz * nb_octets_real

  call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_write_all(descripteur,u(sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  position = position + nx*ny*nz * nb_octets_real

  call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_write_all(descripteur,v(sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  position = position + nx*ny*nz * nb_octets_real

  call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_write_all(descripteur,v(sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  position = position + nx*ny*nz * nb_octets_real

  call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_write_all(descripteur,w(sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  position = position + nx*ny*nz * nb_octets_real

  call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_write_all(descripteur,w(sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  if ( LEVEL_SET == 1 ) then

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_write_all(descripteur,phi(sx:ex,sy:ey,sz:ez),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  end if

  if ( cop == 2 ) then

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_write_all(descripteur,pres(sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_write_all(descripteur,pres(sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_write_all(descripteur,rho (sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_write_all(descripteur,rho (sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  end if

  if ( EBUL == 1 ) then

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_write_all(descripteur,tp  (sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_write_all(descripteur,tp  (sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  end if

  call MPI_type_free(type_cube,code)
  call MPI_type_free(type_carre,code)

  call MPI_file_close(descripteur,code)

  call MPI_barrier(comm3d,code)

  return

END SUBROUTINE write_reprise_mpi


SUBROUTINE read_reprise_mpi(phi,u,v,w,pres,rho,tp)

  USE Parametres

  implicit none

  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3  ), intent(out) :: phi
  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3,2), intent(out) :: u,v,w,pres,rho,tp

  integer :: descripteur,type_carre,type_cube,nb_octets_real

  integer(kind=MPI_address_kind) :: pas
  integer(kind=MPI_offset_kind ) :: position

  integer, dimension(MPI_status_size) :: statut

  call MPI_file_open(comm3d,'fichier_reprise.bin',MPI_mode_rdonly,MPI_info_null,descripteur,code)

  call MPI_type_size(MPI_double_precision,nb_octets_real,code)

  position = 0

  call MPI_file_read_at_all(descripteur,position,t,1,MPI_double_precision,statut,code)

  position = position + nb_octets_real

  call MPI_type_vector(ny/np_y,nx/np_x,nx,MPI_double_precision,type_carre,code)
  call MPI_type_commit(type_carre,code)

  pas = nx*ny*nb_octets_real

  call MPI_type_hvector(nz/np_z,1,pas,type_carre,type_cube,code)
  call MPI_type_commit (type_cube,code)

  position = position + ( nx*ny*(sz-1) + nx*(sy-1) + sx-1 ) * nb_octets_real

  call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_read_all(descripteur,u(sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  position = position + nx*ny*nz * nb_octets_real

  call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_read_all(descripteur,u(sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  position = position + nx*ny*nz * nb_octets_real

  call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_read_all(descripteur,v(sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  position = position + nx*ny*nz * nb_octets_real

  call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_read_all(descripteur,v(sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  position = position + nx*ny*nz * nb_octets_real

  call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_read_all(descripteur,w(sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  position = position + nx*ny*nz * nb_octets_real

  call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
  call MPI_file_read_all(descripteur,w(sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  if ( LEVEL_SET == 1 ) then

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view (descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_read_all(descripteur,phi(sx:ex,sy:ey,sz:ez),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  end if

  if ( cop == 2 ) then

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_read_all(descripteur,pres(sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_read_all(descripteur,pres(sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_read_all(descripteur,rho (sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_read_all(descripteur,rho (sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  end if

  if ( EBUL == 1 ) then

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_read_all(descripteur,tp  (sx:ex,sy:ey,sz:ez,1),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

     position = position + nx*ny*nz*nb_octets_real

     call MPI_file_set_view(descripteur,position,MPI_double_precision,type_cube,"native",MPI_info_null,code)
     call MPI_file_read_all(descripteur,tp  (sx:ex,sy:ey,sz:ez,2),(nx/np_x)*(ny/np_y)*(nz/np_z),MPI_double_precision,statut,code)

  end if

  call MPI_type_free(type_cube,code)
  call MPI_type_free(type_carre,code)

  call MPI_file_close(descripteur,code)

  call com_bloc(u(:,:,:,1)) ; call com_bloc(v(:,:,:,1)) ; call com_bloc(w(:,:,:,1))
  call com_bloc(u(:,:,:,2)) ; call com_bloc(v(:,:,:,2)) ; call com_bloc(w(:,:,:,2))

  if ( LEVEL_SET == 1 ) call com_bloc(phi)

  if ( cop       == 2 ) then
     call com_bloc(pres(:,:,:,1))
     call com_bloc(pres(:,:,:,2))
     call com_bloc(rho (:,:,:,1))
     call com_bloc(rho (:,:,:,2))
  end if

  if ( EBUL      == 1 ) then
     call com_bloc(tp  (:,:,:,1))
     call com_bloc(tp  (:,:,:,2))
  end if

  call MPI_barrier(comm3d,code)

  return

END SUBROUTINE read_reprise_mpi


SUBROUTINE write_result_incompressible(ini,u,v,w,phi,tp,pres,vort,vap,div,kappa,n1,n2,n3)

  USE Parametres
  USE Constantes

  implicit none

  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3,2), intent(in) :: u,v,w,tp
  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3  ), intent(in) :: phi,pres,vort,vap,div,kappa,n1,n2,n3

  logical, intent(in) :: ini

  real*8, dimension(:,:,:,:), allocatable :: uc,vc,wc
  real*8, dimension(:,:,:  ), allocatable :: tpp

  integer :: i,j,k,ph,coord_x,coord_y,coord_z
  character*30 :: nom_fichier
  character*4  :: num

  if ( ini ) then

     nom_fichier = 'initial.plt'

     if ( proc == 0 ) then

        if ( fich(1) ) then

           open(unit=10,file=nom_fichier,status='replace')
           write(10,'(A16 )') 'TITLE="RESULTAT"'
           if ( niv_ecriture == 1 ) then
              write(10,'(A54 )') 'VARIABLES= "x" "y" "z" "u" "v" "w" "phi" "pres" "vort"'
           else if ( niv_ecriture == 2 ) then
              write(10,'(A65 )') 'VARIABLES= "x" "y" "z" "u" "v" "w" "phi" "pres" "vort" "tp" "vap"'
           else if ( niv_ecriture == 3 ) then
              write(10,'(A144)') 'VARIABLES= "x" "y" "z" "u" "v" "w" "phi" "pres" "vort" "tp" "vap" "tp_v" "tp_l" "u_v" "v_v" "w_v" "u_l" "v_l" "w_l" "kappa" "div" "n1" "n2" "n3"'
           end if
           close(10)

           fich(1) = .false.

        end if

        open(unit=10,file=nom_fichier,status='old',position='append')
        write(10,'(A10,I5,A5,I5,A5,I5,A20,E11.4,A1)') ' ZONE I = ',nx,' J = ',ny,' K = ',nz,' F = POINT T="temps ',t,'"'
        close(10)

     end if

  else if ( format_result == 1 ) then

     nom_fichier = 'result_asc.plt'

     if ( proc == 0 ) then

        if ( fich(2) ) then

           open(unit=10,file=nom_fichier,status='replace')
           write(10,'(A16 )') 'TITLE="RESULTAT"'
           if ( niv_ecriture == 1 ) then
              write(10,'(A54 )') 'VARIABLES= "x" "y" "z" "u" "v" "w" "phi" "pres" "vort"'
           else if ( niv_ecriture == 2 ) then
              write(10,'(A65 )') 'VARIABLES= "x" "y" "z" "u" "v" "w" "phi" "pres" "vort" "tp" "vap"'
           else if ( niv_ecriture == 3 ) then
              write(10,'(A144)') 'VARIABLES= "x" "y" "z" "u" "v" "w" "phi" "pres" "vort" "tp" "vap" "tp_v" "tp_l" "u_v" "v_v" "w_v" "u_l" "v_l" "w_l" "kappa" "div" "n1" "n2" "n3"'
           end if
           close(10)

           fich(2) = .false.

        end if

        open(unit=10,file=nom_fichier,status='old',position='append')
        write(10,'(A10,I4,A5,I4,A5,I4,A20,E11.4,A1)') ' ZONE I = ',nx,' J = ',ny,' K = ',nz,' F = POINT T="temps ',t,'"'
        close(10)

     end if

  end if

  if ( ini .or. format_result == 1 ) then

     allocate(uc (sx:ex,sy:ey,sz:ez,3))
     allocate(vc (sx:ex,sy:ey,sz:ez,3))
     allocate(wc (sx:ex,sy:ey,sz:ez,3))
     allocate(tpp(sx:ex,sy:ey,sz:ez  ))

     do ph=1,1+EBUL
        do k=sz,ez
           do j=sy,ey
              do i=sx,ex

                 uc(i,j,k,ph) = demi * ( u(i,j,k,ph) + u(i-1,j  ,k  ,ph) )
                 vc(i,j,k,ph) = demi * ( v(i,j,k,ph) + v(i  ,j-1,k  ,ph) )
                 wc(i,j,k,ph) = demi * ( w(i,j,k,ph) + w(i  ,j  ,k-1,ph) )

              end do
           end do
        end do
     end do

     do k=sz,ez
        do j=sy,ey
           do i=sx,ex

              if ( phi(i,j,k) < zero .or. EBUL == 0 ) then
                 uc(i,j,k,3) = uc(i,j,k,1)
                 vc(i,j,k,3) = vc(i,j,k,1)
                 wc(i,j,k,3) = wc(i,j,k,1)
                 tpp(i,j,k)  = tp(i,j,k,1)
              else
                 uc(i,j,k,3) = uc(i,j,k,2)
                 vc(i,j,k,3) = vc(i,j,k,2)
                 wc(i,j,k,3) = wc(i,j,k,2)
                 tpp(i,j,k)  = tp(i,j,k,2)
              end if

           end do
        end do
     end do

     do coord_z=0,np_z-1
        do k=sz,ez
           do coord_y=0,np_y-1
              do j=sy,ey
                 do coord_x=0,np_x-1

                    if ( coords(1) == coord_x .and. coords(2) == coord_y .and. coords(3) == coord_z ) then

                       open(unit=10,file=nom_fichier,status='old',position='append')

                       do i=sx,ex

                          if ( niv_ecriture == 1 ) then
                             write(10,'( 9E14.6)')     xi(i),yj(j),zk(k),uc(i,j,k,3),vc(i,j,k,3),wc(i,j,k,3),&
                                  phi(i,j,k),pres(i,j,k),vort(i,j,k)
                          else if ( niv_ecriture == 2 ) then
                             write(10,'(11E14.6)')    xi(i),yj(j),zk(k),uc(i,j,k,3),vc(i,j,k,3),wc(i,j,k,3),&
                                  phi(i,j,k),pres(i,j,k),vort(i,j,k),tpp(i,j,k),vap(i,j,k)
                          else if ( niv_ecriture == 3 ) then
                             write(10,'(24E14.6)')    xi(i),yj(j),zk(k),uc(i,j,k,3),vc(i,j,k,3),wc(i,j,k,3),&
                                  phi(i,j,k),pres(i,j,k),vort(i,j,k),tpp(i,j,k),vap(i,j,k),&
                                  tp(i,j,k,1),tp(i,j,k,2),&
                                  uc(i,j,k,1),vc(i,j,k,1),wc(i,j,k,1),&
                                  uc(i,j,k,2),vc(i,j,k,2),wc(i,j,k,2),&
                                  kappa(i,j,k),div(i,j,k),n1(i,j,k),n2(i,j,k),n3(i,j,k)
                          end if

                       end do

                       close(10)

                    end if

                    call MPI_barrier(comm3d,code)

                 end do
              end do
           end do
        end do
     end do

     deallocate(uc) ; deallocate(vc) ; deallocate(wc) ; deallocate(tpp)

  else if ( format_result == 2 ) then

     write(num,'(i4.4)') proc+1
     nom_fichier = trim('result_proc')//num//'.bin'

     if ( fich(2) ) then

        open(unit=10,file=nom_fichier,status='replace',form='unformatted')!,convert='BIG_ENDIAN')
        fich(2) = .false.

        write(10) np_x,np_y,np_z
        write(10) coords(1),coords(2),coords(3)

     else

        open(unit=10,file=nom_fichier,status='old',form='unformatted',position='append')!,convert='BIG_ENDIAN')

     end if

     write(10) t
     write(10) u
     write(10) v
     write(10) w
     write(10) phi
     if ( EBUL == 1 ) write(10) tp

     close(10)

  end if

  call MPI_barrier(comm3d,code)

10 FORMAT (A10,I4,A5,I4,A5,I4,A20,E11.4,A1)

  return

END SUBROUTINE write_result_incompressible

SUBROUTINE write_result_poisson(ini,u)

   USE type_params
 
   implicit none
 
   real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3,2), intent(in) :: u
 
   logical, intent(in) :: ini
 
   real*8, dimension(:,:,:,:), allocatable :: uc,vc,wc
   real*8, dimension(:,:,:  ), allocatable :: tpp
 
   integer :: i,j,k,ph,coord_x,coord_y,coord_z
   character*30 :: nom_fichier
   character*4  :: num
 
   if ( ini ) then
 
      nom_fichier = 'initial.plt'
 
      if ( proc == 0 ) then
 
         if ( fich(1) ) then
 
            open(unit=10,file=nom_fichier,status='replace')
            write(10,'(A16 )') 'TITLE="RESULTAT"'
            if ( niv_ecriture == 1 ) then
               write(10,'(A54 )') 'VARIABLES= "x" "y" "z" "u"'
            else if ( niv_ecriture == 2 ) then
               write(10,'(A65 )') 'VARIABLES= "x" "y" "z" "u"'
            else if ( niv_ecriture == 3 ) then
               write(10,'(A144)') 'VARIABLES= "x" "y" "z" "u"'
            end if
            close(10)
 
            fich(1) = .false.
 
         end if
 
         open(unit=10,file=nom_fichier,status='old',position='append')
         write(10,'(A10,I5,A5,I5,A5,I5,A20,E11.4,A1)') ' ZONE I = ',nx,' J = ',ny,' K = ',nz,' F = POINT T="temps ',t,'"'
         close(10)
 
      end if
 
   else if ( format_result == 1 ) then
 
      nom_fichier = 'result_asc.plt'
 
      if ( proc == 0 ) then
 
         if ( fich(2) ) then
 
            open(unit=10,file=nom_fichier,status='replace')
            write(10,'(A16 )') 'TITLE="RESULTAT"'
            if ( niv_ecriture == 1 ) then
               write(10,'(A54 )') 'VARIABLES= "x" "y" "z" "u"'
            else if ( niv_ecriture == 2 ) then
               write(10,'(A65 )') 'VARIABLES= "x" "y" "z" "u"'
            else if ( niv_ecriture == 3 ) then
               write(10,'(A144)') 'VARIABLES= "x" "y" "z" "u"'
            end if
            close(10)
 
            fich(2) = .false.
 
         end if
 
         open(unit=10,file=nom_fichier,status='old',position='append')
         write(10,'(A10,I4,A5,I4,A5,I4,A20,E11.4,A1)') ' ZONE I = ',nx,' J = ',ny,' K = ',nz,' F = POINT T="temps ',t,'"'
         close(10)
 
      end if
 
   end if
 
   if ( ini .or. format_result == 1 ) then
 
      allocate(uc (sx:ex,sy:ey,sz:ez,3))
      allocate(vc (sx:ex,sy:ey,sz:ez,3))
      allocate(wc (sx:ex,sy:ey,sz:ez,3))
      allocate(tpp(sx:ex,sy:ey,sz:ez  ))
 
      do ph=1,1+EBUL
         do k=sz,ez
            do j=sy,ey
               do i=sx,ex
 
                  uc(i,j,k,ph) = demi * ( u(i,j,k,ph) + u(i-1,j  ,k  ,ph) )
                  vc(i,j,k,ph) = demi * ( v(i,j,k,ph) + v(i  ,j-1,k  ,ph) )
                  wc(i,j,k,ph) = demi * ( w(i,j,k,ph) + w(i  ,j  ,k-1,ph) )
 
               end do
            end do
         end do
      end do
 
      do k=sz,ez
         do j=sy,ey
            do i=sx,ex
 
               if ( phi(i,j,k) < zero .or. EBUL == 0 ) then
                  uc(i,j,k,3) = uc(i,j,k,1)
                  vc(i,j,k,3) = vc(i,j,k,1)
                  wc(i,j,k,3) = wc(i,j,k,1)
                  tpp(i,j,k)  = tp(i,j,k,1)
               else
                  uc(i,j,k,3) = uc(i,j,k,2)
                  vc(i,j,k,3) = vc(i,j,k,2)
                  wc(i,j,k,3) = wc(i,j,k,2)
                  tpp(i,j,k)  = tp(i,j,k,2)
               end if
 
            end do
         end do
      end do
 
      do coord_z=0,np_z-1
         do k=sz,ez
            do coord_y=0,np_y-1
               do j=sy,ey
                  do coord_x=0,np_x-1
 
                     if ( coords(1) == coord_x .and. coords(2) == coord_y .and. coords(3) == coord_z ) then
 
                        open(unit=10,file=nom_fichier,status='old',position='append')
 
                        do i=sx,ex
 
                           if ( niv_ecriture == 1 ) then
                              write(10,'( 9E14.6)')     xi(i),yj(j),zk(k),uc(i,j,k,3),vc(i,j,k,3),wc(i,j,k,3),&
                           else if ( niv_ecriture == 2 ) then
                              write(10,'(11E14.6)')    xi(i),yj(j),zk(k),uc(i,j,k,3),vc(i,j,k,3),wc(i,j,k,3),&
                           else if ( niv_ecriture == 3 ) then
                              write(10,'(24E14.6)')    xi(i),yj(j),zk(k),uc(i,j,k,3),vc(i,j,k,3),wc(i,j,k,3),&
                                   tp(i,j,k,1),tp(i,j,k,2),&
                                   uc(i,j,k,1),vc(i,j,k,1),wc(i,j,k,1),&
                                   uc(i,j,k,2),vc(i,j,k,2),wc(i,j,k,2),&
                           end if
 
                        end do
 
                        close(10)
 
                     end if
 
                     call MPI_barrier(comm3d,code)
 
                  end do
               end do
            end do
         end do
      end do
 
      deallocate(uc) ; deallocate(vc) ; deallocate(wc) ; deallocate(tpp)
 
   else if ( format_result == 2 ) then
 
      write(num,'(i4.4)') proc+1
      nom_fichier = trim('result_proc')//num//'.bin'
 
      if ( fich(2) ) then
 
         open(unit=10,file=nom_fichier,status='replace',form='unformatted')!,convert='BIG_ENDIAN')
         fich(2) = .false.
 
         write(10) np_x,np_y,np_z
         write(10) coords(1),coords(2),coords(3)
 
      else
 
         open(unit=10,file=nom_fichier,status='old',form='unformatted',position='append')!,convert='BIG_ENDIAN')
 
      end if
 
      write(10) t
      write(10) u
      write(10) v
      write(10) w
      if ( EBUL == 1 ) write(10) tp
 
      close(10)
 
   end if
 
   call MPI_barrier(comm3d,code)
 
 10 FORMAT (A10,I4,A5,I4,A5,I4,A20,E11.4,A1)
 
   return
 
END SUBROUTINE write_result_poisson



SUBROUTINE write_result_ensight(u,v,w,phi,pres)

  USE Parametres
  USE Constantes

  implicit none

  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3), intent(in) :: u,v,w,phi,pres

  real*8, allocatable, dimension(:,:,:,:) :: vitesse
  real*8, allocatable, dimension(:,:,:,:) :: phiEnsight,presEnsight

  integer, parameter :: iskip = 2
  integer, parameter :: nbVar = 3

  integer :: nbImages,ntini

  character(len=80)                   :: caseName,speedName,phiName,presName
  character(len=80), dimension(nbVar) :: varName

  integer, dimension(nbVar) :: varType

  real*8, allocatable, dimension(:) :: xEnsight
  real*8, allocatable, dimension(:) :: yEnsight
  real*8, allocatable, dimension(:) :: zEnsight

  integer :: i,j,k,imin,imax,jmin,jmax,kmin,kmax,istep_rep,ndv

  write(caseName,'(A)') 'sortieEnsight'
  write(varName(1),'(A)') 'vitesse'
  varType(1) = 3
  write(varName(2),'(A)') 'phi'
  varType(2) = 1
  write(varName(3),'(A)') 'press'
  varType(3) = 1
  nbImages = nstep/isto
  allocate(vitesse(3,sx-3:ex+3,sy-3:ey+3,sz-3:ez+3))
  allocate(phiEnsight(1,sx-3:ex+3,sy-3:ey+3,sz-3:ez+3))
  allocate(presEnsight(1,sx-3:ex+3,sy-3:ey+3,sz-3:ez+3))

  vitesse(1,:,:,:) = u(:,:,:)
  vitesse(2,:,:,:) = v(:,:,:)
  vitesse(3,:,:,:) = w(:,:,:)
  phiEnsight(1,:,:,:) = phi(:,:,:)
  presEnsight(1,:,:,:) = pres(:,:,:)
  allocate(xEnsight(nx))
  allocate(yEnsight(ny))
  allocate(zEnsight(nz))

  do i=1,nx
     xEnsight(i) = xmin+(i-1)*dx+demi*dx
  end do

  do j=1,ny
     yEnsight(j) =  ymin+(j-1)*dy+demi*dy
  end do

  do k=1,nz
     zEnsight(k) = zmin+(k-1)*dz+demi*dz
  end do

  kmin = 1
  kmax = nz
  jmin = 1
  jmax = ny
  imin = 1
  imax = nx

  ntini=nstep*reprise
  istep_rep=istep+ntini

  if (nstep/isto .eq. 1) then
     write(speedName,'(A,I6.6)') 'vitesse'
     write(phiName,'(A,I6.6)'  ) 'phi'
     write(presName,'(A,I6.6)' ) 'press'
  else
     write(speedName,'(A,I6.6)') 'vitesse',istep_rep
     write(phiName,'(A,I6.6)'  ) 'phi', istep_rep
     write(presName,'(A,I6.6)' ) 'press', istep_rep
  end if


  if (proc .eq. 0) then
     call EnsightCase(nbVar,varName,caseName,VarType,ntini,nbImages,isto)
     call WriteRectEnsightGeo(imin,imax,jmin,jmax,kmin,kmax,xEnsight,yEnsight,zEnsight,caseName,iskip)
  end if

  ndv=1
  call WriteEnsightVar(ndv,phiEnsight ,phiName  ,iskip)
  ndv=1
  call WriteEnsightVar(ndv,presEnsight,presName ,iskip)
  ndv=3
  call WriteEnsightVar(ndv,vitesse    ,speedName,iskip)

  call MPI_barrier(comm3d,code)
  deallocate(vitesse) ; deallocate(phiEnsight) ; deallocate(presEnsight)
  deallocate(xEnsight) ; deallocate(yEnsight) ; deallocate(zEnsight)

END SUBROUTINE write_result_ensight


SUBROUTINE WriteRectEnsightGeo(imin,imax,jmin,jmax,kmin,kmax,x1,x2,x3,FileName,iskip)

  !
  !    ******************************************************************************
  !                      subrout  WriteRectEnsightGeo
  !    ******************************************************************************
  !
  !    writes mesh data in Ensight's ascii format for rectilinear geometry
  !
  !    n1,n2,n3....: number of nodes in the x1,x2,x3 direction
  !    x1,x2,x3....: coordinates
  !

  implicit none

  INTEGER,INTENT(IN)::imin,imax,jmin,jmax,kmin,kmax
  REAL*8,DIMENSION(imax),INTENT(IN)::x1
  REAL*8,DIMENSION(jmax),INTENT(IN)::x2
  REAL*8,DIMENSION(kmax),INTENT(IN)::x3
  CHARACTER(LEN=80)     ,INTENT(IN)::FileName
  INTEGER               ,INTENT(IN)::iskip

  character(LEN=80)::binary_form
  character(LEN=80)::file_description1,file_description2
  character(LEN=80)::node_id,element_id
  character(LEN=80)::part,description_part,blocck

  integer::FileUnit,i,j,k,npart,isize,jsize,ksize
  integer::reclength

  FileUnit = 40

  binary_form      ='C Binary'
  file_description1='Ensight Model Geometry File Created by '
  file_description2='WriteRectEnsightGeo Routine'
  node_id          ='node id off'
  element_id       ='element id off'
  part             ='part'
  npart            =1
  description_part ='Sortie Ensight'
  blocck            ='block rectilinear'
  isize=(imax-imin+1)/iskip
  jsize=(jmax-jmin+1)/iskip
  ksize=(kmax-kmin+1)/iskip

  if (ksize == 0) ksize = 1 !AV

  reclength=80*8+4*(4+isize+jsize+ksize)

  open (unit=FileUnit,file=trim(FileName)//'.geo',form='UNFORMATTED',access="direct",recl=reclength,status='replace')
  write(unit=FileUnit,rec=1) binary_form &
       ,file_description1 &
       ,file_description2 &
       ,node_id &
       ,element_id &
       ,part,npart &
       ,description_part &
       ,blocck &
       ,isize,jsize,ksize &
       ,(real(sngl(x1(i)),4),i=imin,imax,iskip) &
       ,(real(sngl(x2(j)),4),j=jmin,jmax,iskip) &
       ,(real(sngl(x3(k)),4),k=kmin,kmax,iskip)

  close(FileUnit)

END SUBROUTINE WriteRectEnsightGeo


SUBROUTINE WriteEnsightVar(ndv,var,VarName,iskip)

  !
  !    ******************************************************************************
  !
  !    WriteEnsightSca writes result data in Ensight's format
  !
  !    m1,m2,m3....: size of the variable in the x1,x2,x3 direction
  !    ndv.........: number of dimension of the variable (1=>scalar   3=>vector)
  !    var.........: data to be written
  !    Varname.....: word used to build filenames
  !    imin,imax...: range of writting data in the x1 direction
  !    jmin,jmax...: range of writting data in the x2 direction
  !    kmin,kmax...: range of writting data in the x3 direction
  !
  !    ******************************************************************************

  USE Parametres
  USE Constantes

  implicit none

  INTEGER     ,INTENT(IN)::ndv
  REAL*8,DIMENSION(ndv,sx-3:ex+3,sy-3:ey+3,sz-3:ez+3),INTENT(IN)::var
  CHARACTER*80,INTENT(IN)::Varname
  INTEGER     ,INTENT(IN)::    iskip

  REAL*8, allocatable, DIMENSION(:,:) :: varTempo
  REAL*4, allocatable, DIMENSION(:)   ::   varTempo_1
  character(len=80):: VarFileName
  character(len=80):: part,blocck
  integer::FileUnit,i,j,k,ii,npart,m,descripteur,nb_octets_real,tailleCharacter,tailleInteger,tailleReel
  integer :: type_carre,type_cube,iimax
  integer(KIND=MPI_OFFSET_KIND) :: position
  integer, dimension(MPI_STATUS_SIZE) :: statut
  integer(KIND=MPI_ADDRESS_KIND) :: pas

  FileUnit = 40
  part ='part'
  npart=1
  blocck='block rectilinear'

  if (ndv.eq.1)VarFileName = trim(Varname)//'.scl'
  if (ndv.eq.3)VarFileName = trim(Varname)//'.vec'


  call MPI_FILE_OPEN(comm3d,VarFileName,MPI_MODE_WRONLY+MPI_MODE_CREATE,MPI_INFO_NULL,descripteur,code)
  call MPI_TYPE_SIZE(MPI_DOUBLE_PRECISION,nb_octets_real,code)
  call MPI_TYPE_SIZE(MPI_REAL4,tailleReel,code)
  call MPI_TYPE_SIZE(MPI_CHARACTER,tailleCharacter,code)
  call MPI_TYPE_SIZE(MPI_INTEGER,tailleInteger,code)

  position = 0
  call MPI_FILE_WRITE_AT_ALL(descripteur,position,VarFileName,80,MPI_CHARACTER,statut,code)

  position = position+80*tailleCharacter
  call MPI_FILE_WRITE_AT_ALL(descripteur,position,part,80,MPI_CHARACTER,statut,code)

  position = position+80*tailleCharacter
  call MPI_FILE_WRITE_AT_ALL(descripteur,position,npart,1,MPI_INTEGER,statut,code)

  position = position+tailleInteger
  call MPI_FILE_WRITE_AT_ALL(descripteur,position,blocck,80,MPI_CHARACTER,statut,code)

  position = position+80*tailleCharacter
  call MPI_TYPE_VECTOR((ny/iskip)/np_y,(nx/iskip)/np_x,nx/iskip,MPI_REAL4,type_carre,code)
  call MPI_TYPE_COMMIT(type_carre,code)

  pas = (nx/iskip)*(ny/iskip)*tailleReel
  call MPI_TYPE_HVECTOR((nz/iskip)/np_z,1,pas,type_carre,type_cube,code)
  call MPI_TYPE_COMMIT(type_cube,code)

  if (((nz/iskip)/np_z) == 0) then    !AV
     allocate(varTempo(ndv,((nx/iskip)/np_x)*((ny/iskip)/np_y)                  ))
  else
     allocate(varTempo(ndv,((nx/iskip)/np_x)*((ny/iskip)/np_y)*((nz/iskip)/np_z)))
  end if

  ii = 1
  do k=sz,ez,iskip
     do j=sy,ey,iskip
        do i=sx,ex,iskip
           do m=1,ndv
              varTempo(m,ii) = var(m,i,j,k)
           end do
           ii = ii+1
        end do
     end do
  end do

  iimax=ii-1
  allocate(varTempo_1(iimax))

  position = position+((nx/iskip)*(ny/iskip)*((sz-1)/iskip)+(nx/iskip)*((sy-1)/iskip)+(sx-1)/iskip)*tailleReel
  do m=1,ndv
     call MPI_FILE_SET_VIEW(descripteur,position,MPI_REAL4,type_cube,"native",MPI_INFO_NULL,code)

     do ii=1,iimax
        varTempo_1(ii)=real(sngl(varTempo(m,ii)),4)
     enddo

     call MPI_FILE_WRITE_ALL(descripteur,varTempo_1,iimax,MPI_REAL4,statut,code)
     !        call MPI_FILE_WRITE_ALL(descripteur,varTempo_1,((nx/iskip)/np_x)*((ny/iskip)/np_y)*((nz/iskip)/np_z),MPI_REAL4,statut,code)

     if (((nz/iskip)/np_z) == 0) then    !AV
        position = position+(nx/iskip)*(ny/iskip)*tailleReel
     else
        position = position+(nx/iskip)*(ny/iskip)*(nz/iskip)*tailleReel
     end if


  end do

  deallocate(varTempo)
  deallocate(varTempo_1)

  call MPI_TYPE_FREE(type_cube,code)
  call MPI_TYPE_FREE(type_carre,code)

  call MPI_FILE_CLOSE(descripteur,code)

  close(FileUnit)

END SUBROUTINE WriteEnsightVar


SUBROUTINE EnsightCase(nbVar,VarName,GeoName,VarType,ntini,nstop,nprint)

  !
  !    ******************************************************************************
  !   EnsightCase helps to write a Ensight's case file
  !
  !    VarName.....: Name of the variable
  !    GeoName.....: Name of the geometrie
  !    VarType.....: 1 => Scalar       3 => Vector
  !    ntini.......: filename start number
  !    nstop.......: filename end number
  !    nprint......: filename increment
  !
  !    nfile.......: number of result files (time steps)
  !

  USE Parametres
  USE Constantes

  implicit none

  INTEGER,INTENT(IN)::nbVar,nstop,ntini,nprint
  INTEGER, DIMENSION(nbVar),INTENT(IN)::VarType
  CHARACTER(LEN=80),DIMENSION(nbVar),INTENT(IN)::Varname
  CHARACTER(LEN=80),INTENT(IN)::GeoName
  integer::FileUnit,i,j,nfile

  nfile=nstep/isto

  FileUnit = 40
  open(FileUnit,file=trim(GeoName)//'.case',status='replace')

  write(FileUnit,10) trim(GeoName)//'.geo'
10 format('FORMAT'            ,/ ,'type: ensight gold',//,'GEOMETRY'          ,/ ,'model:    ',A         ,//,'VARIABLE')

  do i=1,nbVar
     if (nfile.eq.1) then
        if(VarType(i).eq.1) write(FileUnit,15)trim(Varname(i)),trim(Varname(i))//'.scl'
        if(VarType(i).eq.3) write(FileUnit,25)trim(Varname(i)),trim(Varname(i))//'.vec'
     else
        if(VarType(i).eq.1) write(FileUnit,15)trim(Varname(i)),trim(Varname(i))//'******.scl'
        if(VarType(i).eq.3) write(FileUnit,25)trim(Varname(i)),trim(Varname(i))//'******.vec'
     endif
  enddo

  if (nfile.gt.1) then
     write(FileUnit,45) nfile,ntini+isto,nprint
     !        write(FileUnit,'(f15.3)') (j*dt,j=1,nfile)
     !    write(FileUnit,'(E13.5)') (time0+j*dt*isto,j=1,nfile)
     write(FileUnit,'(E13.5)') (time0+j*dt*isto,j=1,nfile)

  endif

  close(FileUnit)

15 format('scalar per node: ',A,'   ', A)
25 format('vector per node: ',A,'   ', A)

45 format(/,'TIME            '      ,/,'time set: 1     '      ,/,'number of steps:'      ,i10 ,/, &
       'filename start number:',i10/,'filename increment:'   ,i10/,'time values: ')

END SUBROUTINE EnsightCase

SUBROUTINE my_write_result_ensight(u,v,w,phi,pres)

  USE Parametres
  USE Constantes

  implicit none

  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3), intent(in) :: u,v,w,phi,pres

  real*8, allocatable, dimension(:,:,:,:) :: vitesse
  real*8, allocatable, dimension(:,:,:,:) :: phiEnsight,presEnsight
  

  character(len=80) :: caseName,velocityName,phiName,presName,geoName,Varname,ns_name,tname
  character(len=80) :: path_result='../obj/results/'
  
  character(len=4)  :: num
  character(len=4)  :: stepc
  
  logical :: existe
  
  integer :: test,i,j,k,ndv,step

  allocate(vitesse(3,sx-1:ex,sy-1:ey,sz-1:ez))
  allocate(phiEnsight(1,sx-1:ex,sy-1:ey,sz-1:ez))
  allocate(presEnsight(1,sx-1:ex,sy-1:ey,sz-1:ez))
  
  write(num,'(i4.4)') proc
  
  ns_name='ns'//num
  tname='result_Ensight.time'
  
  inquire(file=trim(path_result)//ns_name, exist=existe)
  if (.NOT.existe) then
     step=0
     open(unit=50,file=trim(path_result)//ns_name,form='formatted',status='new')
     write(50,'(I10)') step
     close(50)
     
     geoName=trim(path_result)//'result_Ensight.geo'
     call my_WriteuniformEnsightGeo(geoName)
  end if
      
  open(unit=50,file=trim(path_result)//ns_name,form='formatted',position='rewind')
  read(50,'(I10)') step
  rewind 50
  step=step+1
  write(50,'(I10)') step
  close(50)
  
  write(stepc,'(i4.4)') step
  
       
  phiName=trim(path_result)//'result_Ensight.phi'//stepc
  presName=trim(path_result)//'result_Ensight.pre'//stepc
  velocityName=trim(path_result)//'result_Ensight.vel'//stepc
  do i=sx-1,ex
     do j=sy-1,ey
        do k=sz-1,ez
          vitesse(1,i,j,k)=demi*(u(i,j,k)+u(i-1,j,k))
          vitesse(2,i,j,k)=demi*(v(i,j,k)+v(i,j-1,k))
          vitesse(3,i,j,k)=demi*(w(i,j,k)+w(i,j,k-1))
        end do
     end do
  end do
  phiEnsight(1,:,:,:) = phi(sx-1:ex,sy-1:ey,sz-1:ez)
  presEnsight(1,:,:,:) = pres(sx-1:ex,sy-1:ey,sz-1:ez)

  ndv=3
  Varname='velocity'
  call my_WriteEnsightVar(ndv,vitesse,Varname,velocityName)
  ndv=1
  Varname='phi'
  call my_WriteEnsightVar(ndv,phiEnsight,Varname,phiName)
  Varname='pressure'
  call my_WriteEnsightVar(ndv,presEnsight,Varname,presName)
  
  
  if (proc==0) then
     open(unit=50,file=trim(path_result)//tname,form='formatted',position='append')
     write(50,'(E12.5)') t
     close(50)
  end if

  
  deallocate(vitesse) ; deallocate(phiEnsight) ; deallocate(presEnsight)

END SUBROUTINE my_write_result_ensight


SUBROUTINE my_WriteuniformEnsightGeo(geoName)

  USE Parametres
  USE Constantes

  implicit none
   
  CHARACTER(LEN=80),INTENT(IN)::geoName
  CHARACTER(LEN=80)::lig1,lig2,lig3,lig4,lig5,lig6,lig8,lig9
  integer ::lig7,i,j,k
  logical :: existe
  
  
  lig1='Fortran Binary'
  lig2='description1'
  lig3='description2'
  lig4='node id off'
  lig5='element id off'
  lig6='part'
  lig7=proc+1
  lig8='description'
  lig9='block rectilinear'
  
  inquire(file=geoName, exist=existe)
  open(unit=50,file=geoName,position='append',form='unformatted')
  if (.NOT.existe) then
     write(50) lig1
     write(50) lig2
     write(50) lig3
     write(50) lig4
     write(50) lig5
  end if
  write(50) lig6
  write(50) lig7
  write(50) lig8
  write(50) lig9
  if (dim==2) then
     write(50) nxp+1,nyp+1,nzp
  else
     write(50) nxp+1,nyp+1,nzp+1
  end if
  write(50) (sngl(xu(i)),i=sx-1,ex)
  write(50) (sngl(yv(j)),j=sy-1,ey)
  if (dim==2) then
     write(50) (sngl(zw(k)),k=sz,ez)
  else
     write(50) (sngl(zw(k)),k=sz-1,ez)
  end if
  
  close(50)
  
END SUBROUTINE my_WriteuniformEnsightGeo


SUBROUTINE my_WriteEnsightVar(ndv,var,VarName,filename)


  USE Parametres
  USE Constantes

  implicit none

  INTEGER,INTENT(IN)::ndv
  REAL*8,DIMENSION(ndv,sx-1:ex,sy-1:ey,sz-1:ez),INTENT(IN)::var
  CHARACTER*80,INTENT(IN)::VarName,filename
  integer::i,j,k,n,lig3
  logical :: existe
  character(len=80):: lig1,lig2,lig4
  
  lig1=VarName
  lig2='part'
  lig3=proc+1
  lig4='block'
  
  inquire(file=filename, exist=existe)
  open(unit=50,file=filename,position='append',form='unformatted')
  if (.NOT.existe) then
     write(50) lig1
  end if
  write(50) lig2
  write(50) lig3
  write(50) lig4
  if (dim==2) then
     do n=1,ndv
        write(50) (((sngl(var(n,i,j,k)),i=sx-1,ex),j=sy-1,ey),k=sz,ez)
     end do
  else
     do n=1,ndv
        write(50) (((sngl(var(n,i,j,k)),i=sx-1,ex),j=sy-1,ey),k=sz-1,ez)
     end do
  end if
  close(50)

END SUBROUTINE my_WriteEnsightVar


SUBROUTINE my_EnsightCase(caseName,geoName,phiName,presName,velocityName,tname,step)

  implicit none
  
  REAL*8 :: t
  CHARACTER(LEN=80),INTENT(IN)::caseName,geoName,phiName,presName,velocityName,tname
  CHARACTER(LEN=80) :: presdesc='pressure'
  CHARACTER(LEN=80) :: phidesc='phi'
  CHARACTER(LEN=80) :: velocitydesc='velocity'
  CHARACTER(LEN=4) :: stepc
  integer :: step,i
  real*8 :: temp
  
  
  write(stepc,'(i4.4)') step
  
  
  open(unit=50,file=trim(caseName),status='new')
  open(unit=60,file='../obj/results/'//tname,position='rewind')
  write(50,'(A)') 'FORMAT'
  write(50,'(A)') 'type: ensight gold'
  write(50,'(A)') 'GEOMETRY'
  write(50,'(A)',advance='no') 'model: '
  write(50,'(A)') trim(geoName)
  write(50,'(A)') 'VARIABLE'
  write(50,'(A)',advance='no') 'scalar per node: 1 pressure '
  write(50,'(A)') trim(presName)
  write(50,'(A)',advance='no') 'scalar per node: 1 phi '
  write(50,'(A)') trim(phiName)
  write(50,'(A)',advance='no') 'vector per node: 1 velocity '
  write(50,'(A)') trim(velocityName)
  write(50,'(A)') 'TIME'
  write(50,'(A)') 'time set: 1'
  write(50,'(A)') 'number of steps: '//stepc
  write(50,'(A)') 'filename start number: 1'
  write(50,'(A)') 'filename increment: 1'
  write(50,'(A)') 'time values: '
  do i=1,step
     read(60,'(E12.5)') temp
     write(50,'(E12.5)') temp
  end do

  close(50)
  close(60,status='delete')
END SUBROUTINE my_EnsightCase

SUBROUTINE my_ensight_final

  
  
  USE Parametres
  USE Constantes  
  
  implicit none
  
  character(len=4) :: num
  character(len=80) :: caseName,geoName,phiName,presName,velocityName,tname
  integer :: step
  
  write(num,'(i4.4)') proc
  
  if (proc==0) then
    open(unit=50,file='../obj/results/ns'//num,position='rewind')
    read(50,'(I10)') step
    close(50)
    caseName='../obj/results/result_Ensight.case'
    phiName='result_Ensight.phi****'
    presName='result_Ensight.pre****'
    velocityName='result_Ensight.vel****'
    geoName='result_Ensight.geo'
    tname='result_Ensight.time'
    call my_EnsightCase(caseName,geoName,phiName,presName,velocityName,tname,step)
  end if
  open(unit=50,file='../obj/results/ns'//num)
  close(50,STATUS='DELETE')
  

END SUBROUTINE my_ensight_final

SUBROUTINE ecriture_croix_diag(phi)

  USE Parametres
  USE Constantes  
  
  implicit none
  
  real*8, dimension(sx-3:ex+3,sy-3:ey+3,sz-3:ez+3), intent(in) :: phi
  real*8 :: haut
  integer :: i,j,k
  character*30 :: nom_fichier
  character*4  :: num
  



     write(num,'(i4.4)') proc

     nom_fichier = trim('vert')//num//'.dat'
     open(unit=50,file=nom_fichier,position='append')

     nom_fichier = trim('horz')//num//'.dat'
     open(unit=60,file=nom_fichier,position='append')
  
     
  do i=sx-1,ex
     j=0
     k=sz-1
     do while (phi(i,j,k)>0)
        k=k+1
        if (k>sz) then
           exit
        end if
      end do
      haut=zw(k)+phi(i,j,k)
      write(60,'(F16.14)',advance='no') xu(j)
      write(60,'(A)',advance='no') '  '
      write(60,'(F16.14)',advance='no') yv(j)
      write(60,'(A)',advance='no') '  '
      write(60,'(F16.14)',advance='no') haut
      write(60,'(A)',advance='no') '  '
      write(60,'(F9.7)') t
  end do
  do j=sy-1,ey
     i=0
     k=sz-1
     do while (phi(i,j,k)>0)
        k=k+1
        if (k>sz) then
           exit
        end if
      end do
      haut=zw(k)+phi(i,j,k)
      write(50,'(F16.14)',advance='no') xu(i)
      write(50,'(A)',advance='no') '  '
      write(50,'(F16.14)',advance='no') yv(j)
      write(50,'(A)',advance='no') '  '
      write(50,'(F16.14)',advance='no') haut
      write(50,'(A)',advance='no') '  '
      write(50,'(F9.7)') t
  end do


  close(50)
  close(60)

END SUBROUTINE ecriture_croix_diag
