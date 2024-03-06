!**************************
subroutine read_data(param)
!**************************

use m_type

implicit none 

type (donnees), intent(out) :: param

print*,'reading data...'

open(unit=11,file="data.txt")

close (11)

end subroutine read_data
!***********************




