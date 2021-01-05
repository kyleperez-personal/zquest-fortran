module already_picked
  implicit none
	
  character(len=1), parameter :: c_esc = achar(27)
  character(len=2), parameter :: c_start = c_esc // '['
  character(len=1), parameter :: c_end = 'm'
  character(len=*), parameter :: c_gray = '90'
  character(len=*), parameter :: c_clear = c_start // '0' // c_end
	
  contains
	
  function picked(str) result(out) ! note that out is not a protected variables
  character(len=*), intent(in) :: str
  character(len=:), allocatable :: out
  out = c_start // c_gray // c_end // str // c_clear
  end function picked

end module already_picked
