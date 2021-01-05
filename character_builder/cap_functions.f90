module cap_functions
! This function forces all characters after space to be lowercase.
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
			j = iachar(strIn(i:1)) ! j is the ASCII value for strIn(i:1)
			! if the previous character is a space, make the next one lowercase
			if (strOut(i-1:i-1) .EQ. ' ') then
				if (j >= iachar("A") .and. j <= iachar("Z")) then
					strOut(i:i) = achar(iachar(strIn(i:i))+32)
				else
					strOut(i:i) = strIn(i:i)
				end if
			else
				strOut(i:i) = strIn(i:i)
			end if
		end do
	
	!print*, strOut
	
	end function lower_first_char

end module cap_functions
