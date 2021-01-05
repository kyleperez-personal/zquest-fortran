module gender_selector
	use proper_cap ! allows fixformat function

	implicit none
	
	CONTAINS
	subroutine gender_select(player_gender,player_is_male,player_is_female)
	!! Gender subroutine output
	character(len=*), intent(out) :: player_gender
	logical, intent(out) :: player_is_male, player_is_female ! Includes logicals to be safe.
	
	!! Temporary variables.
	character(len=5) :: is_my_gender_correct
	logical :: gender_is_confirmed,gender_is_selected
	
	gender_is_confirmed = .false.
	gender_is_selected = .false.
	
	Print*, "Select a gender."
	do while (gender_is_confirmed .EQV. .false.)
		do while (gender_is_selected .EQV. .false.)
			Print*, "Is your character male or female?"
			read (*,*) player_gender
			player_gender = fixformat(player_gender)
			if (player_gender .EQ. 'Male') then
				player_gender = 'Male'
				player_is_male = .true.
				player_is_female = .false.
				gender_is_selected = .true. ! Exits the loop
			else if (player_gender .EQ. 'Female') then
				player_gender = 'Female'
				player_is_male = .false.
				player_is_female = .true.
				gender_is_selected = .true.
			else
				print*, "Try again."
			end if
		end do
		! Telling the player what gender they are and confirming it.
		
		do while (gender_is_confirmed .EQV. .false.)
			if (player_gender .EQ. 'Male') then
				print*, "You are male."
			else if (player_gender .EQ. 'Female') then
				print*, "You are female."
			else
				print*, "You are apparantly ",player_gender,"."
				print*, "How did that happen?"
			end if
		! Gender confirmation
			print*,"Is this correct? Yes or No."
			read (*,*) is_my_gender_correct
			is_my_gender_correct = fixformat(is_my_gender_correct)
			if (is_my_gender_correct .EQ. 'Yes') then
				print*, "Excellent."
				gender_is_confirmed = .true.
			else if (is_my_gender_correct .EQ. 'No') then
				print*, "Then let's pick it again."
				gender_is_selected = .false.
				exit
			else
				print*, "Try again."
			end if
		end do
	end do
	end subroutine gender_select
end module gender_selector
