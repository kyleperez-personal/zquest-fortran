module age_selector
	use proper_cap ! allows fixformat function

	implicit none
	CONTAINS
	subroutine age_select(player_age)
	!! Age subroutine output
	integer(8), intent(out) :: player_age
	
	!! Temporary variables
	logical :: age_is_valid
	character(len=5) :: is_my_age_correct
	
	age_is_valid = .false.
	!! End temporary variables
	
	! Program crashes if a non-integer is entered.
	do while (age_is_valid .EQV. .false.)
		do while (age_is_valid .EQV. .false.)
			print*, "How old are you?"
			print*, "Enter a number between 18 and 65."
			read (*,*) player_age
			if (player_age >= 18 .and. player_age <= 65) then
				exit
			else
				print*, "Please enter a valid age."
			end if
		end do
		do while (age_is_valid .EQV. .false.)
			print '(5g0)', "You are ", player_age, " years old."
			print*, "Is this alright? Yes or No."
			read (*,*) is_my_age_correct
			is_my_age_correct = fixformat(is_my_age_correct)
			if (is_my_age_correct .EQ. 'Yes') then
				print*, "Alright."
				age_is_valid = .true.
			else if (is_my_age_correct .EQ. 'No') then
				print*, "Let's go again then."
				exit
			else
				print*, "Try again."
			end if
		end do
	end do
	end subroutine age_select

end module age_selector
