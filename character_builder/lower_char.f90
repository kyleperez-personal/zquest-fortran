module cap_functions
! This function does all the same work as the proper_cap function, but forces the first character to be lowercase.
	implicit none
	
	CONTAINS
	function lower_first_char(strIn) result(strOut)
		
	
		character(len=*),intent(in) :: strIn
		character(len=len(strIn)) :: strOut
		integer :: i,j
		
		!print*, "Enter a string:"
		!read(*,'(A)') strIn
		
		! lowercase the first character
		j = iachar(strIn(1:1))
		if (j >= iachar("a") .and. j <= iachar("z")) then
			strOut(1:1) = strIn(1:1)
		else
			strOut(1:1) = achar(iachar(strIn(1:1))+32)
		end if
	
		! From then on, keeps the string the same.
		do i=2, len(strIn)
			strOut(i:i) = strIn(i:i)
		end do
	
	!print*, strOut
	
	end function lower_first_char

end module cap_functions
