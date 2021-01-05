module proper_cap

	implicit none
	
	CONTAINS
	function fixformat(strIn) result(strOut)
		
	
		character(len=*),intent(in) :: strIn
		character(len=len(strIn)) :: strOut
		integer :: i,j
		
		!print*, "Enter a string:"
		!read(*,'(A)') strIn
		
		! Capitalize the first character
		j = iachar(strIn(1:1))
		if (j >= iachar("a") .and. j <= iachar("z")) then
			strOut(1:1) = achar(iachar(strIn(1:1))-32)
		else
			strOut(1:1) = strIn(1:1)
		end if
	
		! From then on, make every character lowercase except for those that have a space beforehand.
		do i=2, len(strIn)
			j = iachar(strIn(i:1)) ! j is the ASCII value for strIn(i:1)
			! If strIn(i:1) is between A and Z then
			if (j >= iachar("A") .and. j <= iachar("Z")) then
				! if the space before is a space, do nothing
				if (strOut(i-1:i-1) .EQ. ' ') then
					! if the character is an O, after is an f or F followed by a space, make it lowercase
					! Checks to see if the word is 'of'
					if (strIn(i:i+2) .EQ. 'Of ') then
						strOut(i:i) = achar(iachar(strIn(i:i))+32)
					else if (strIn(i:i+2) .EQ. 'OF ') then
						strOut(i:i) = achar(iachar(strIn(i:i))+32)
					! else if the character is a T followed by an h,H, then an e,E and space, then make it lowercase
					else if (strIn(i:i+3) .EQ. 'The ') then
						strOut(i:i) = achar(iachar(strIn(i:i))+32)
					else if (strIn(i:i+3) .EQ. 'THe ') then
						strOut(i:i) = achar(iachar(strIn(i:i))+32)
					else if (strIn(i:i+3) .EQ. 'ThE ') then
						strOut(i:i) = achar(iachar(strIn(i:i))+32)
					else if (strIn(i:i+3) .EQ. 'THE ') then
						strOut(i:i) = achar(iachar(strIn(i:i))+32)
					! otherwise, do nothing.
					else
						strOut(i:i) = strIn(i:i)
					end if
				! otherwise, capitalize it
				else
					strOut(i:i) = achar(iachar(strIn(i:i))+32)
				end if
			! If strIn(i:1) is between a and z then
			else if (j >= iachar("a") .and. j <= iachar("z")) then
				! if the character before is a space, make it uppercase
				if (strOut(i-1:i-1) .EQ. ' ') then
					! if the characters that follow are an f or F followed by a space, do nothing.	
					if (strIn(i:i+2) .EQ. 'of ') then
						strOut(i:i) = strIn(i:i)
					else if (strIn(i:i+2) .EQ. 'oF ') then
						strOut(i:i) = strIn(i:i)
					! if the characters that follow it are an h or H then an e or E then a space, then do nothing.
					else if (strIn(i:i+3) .EQ. 'the ') then
						strOut(i:i) = strIn(i:i)
					else if (strIn(i:i+3) .EQ. 'tHe ') then
						strOut(i:i) = strIn(i:i)
					else if (strIn(i:i+3) .EQ. 'thE ') then
						strOut(i:i) = strIn(i:i)
					else if (strIn(i:i+3) .EQ. 'tHE ') then
						strOut(i:i) = strIn(i:i)
					! otherwise, capitalize it
					else
						strOut(i:i) = achar(iachar(strIn(i:i))-32)
					end if
				! otherwise, do nothing
				else
					strOut(i:i) = strIn(i:i)
				end if
			else
				strOut(i:i) = strIn(i:i)
			end if
		end do
	
	!print*, strOut
	
	end function fixformat

end module proper_cap
