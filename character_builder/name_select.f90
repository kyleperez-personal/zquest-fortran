module name_selector
	use proper_cap ! Lets you use the fixformat function
	use lower_cap ! Lets you use the lowfixformat function for nobility.

	implicit none
	
	CONTAINS
	
	subroutine name_select(player_is_nobility, player_forename, player_surname, player_nobilary,&
	player_short_name, player_short_last_name, player_nickname)
	!! Imported Variables
	logical, intent(in) :: player_is_nobility
	! Output
	character(len=*), intent(out) :: player_forename,player_surname,player_nobilary
	character(len=*), intent(out) :: player_short_name,player_short_last_name
	character(len=*), intent(out) :: player_nickname
	
	
	
	!! End imported variables
	
	! Temporary variables
	character(len=5) :: is_my_name_good ! this is used several times in the function.
	logical :: name_is_confirmed
	
	name_is_confirmed = .false.
	
	do while (name_is_confirmed .EQV. .false.)
		print*, "Your forename is all of your first names. For example, in anglophone countries, it would correspond to ",&
		"your first and middle name."
		! Forename function
		do while (name_is_confirmed .EQV. .false.)
			print*, "Enter your forename. The max length is sixty characters."
			read (*,'(A)') player_forename
			player_forename = fixformat(player_forename)
			print*, "Your forename is ",TRIM(player_forename),"."
			print*, "Is this good? Yes or No?"
			read (*,*) is_my_name_good
			is_my_name_good = fixformat(is_my_name_good)
			if (is_my_name_good .EQ.'Yes') then
				print*, "Excellent."
				exit
			else if (is_my_name_good .EQ. 'No') then
				print*, "No problem. Try again."
			else
				print*, "Invalid response."
				print*, "Please reenter your name."
			end if
		end do
		print*, "Your surname is all of your last names."
		! Surname function
		do while (name_is_confirmed .EQV. .false.)
			print*, "Enter your Surname. The max length is 60 characters."
			read(*,'(A)') player_surname
			player_surname = fixformat(player_surname)
			print*, "Your surname is ",TRIM(player_surname),"."
			print*, "Is this good? Yes or No?"
			read (*,*) is_my_name_good
			is_my_name_good = fixformat(is_my_name_good)
			if (is_my_name_good .EQ.'Yes') then
				print*, "Excellent."
				exit
			else if (is_my_name_good .EQ. 'No') then
				print*, "No problem. Try again."
			else
				print*, "Invalid response."
				print*, "Please reenter your name."
			end if
		end do
		! Nobilary function (only fires if you're a noble)
		if (player_is_nobility .EQV. .true.) then
			do while (name_is_confirmed .EQV. .false.)
				print*, "As a noble, you have a nobilary."
				print*, "A nobilary is in the form of 'of *PLACENAME*'."
				print*, "What is your nobilary? Include the 'of' in the name please."
				read (*,'(A)') player_nobilary
				player_nobilary = lowfixformat(player_nobilary)
				print*, "Your nobilary is '",TRIM(player_nobilary),"'."
				print*, "Is this good? Yes or No."
				read (*,*) is_my_name_good
				is_my_name_good = fixformat(is_my_name_good)
				if (is_my_name_good .EQ.'Yes') then
					print*, "Excellent."
					exit
				else if (is_my_name_good .EQ. 'No') then
					print*, "No problem. Try again."
				else
					print*, "Invalid response."
					print*, "Please reenter your name."
				end if
			end do
		else
			player_nobilary = "None" ! assigns a null value to prevent crash.
		end if
		! Player short name
		print*, "Your short name is a shorter version of your forename and is what you are commonly known as."
		do while (name_is_confirmed .EQV. .false.)
			print*, "Enter your short name. Max length is 20 characters."
			read(*,'(A)') player_short_name
			player_short_name = fixformat(player_short_name)
			print*, "Your short name is ",TRIM(player_short_name),"."
			print*, "Is this good? Yes or No?"
			read (*,*) is_my_name_good
			is_my_name_good = fixformat(is_my_name_good)
			if (is_my_name_good .EQ.'Yes') then
				print*, "Excellent."
				exit
			else if (is_my_name_good .EQ. 'No') then
				print*, "No problem. Try again."
			else
				print*, "Invalid response."
				print*, "Please reenter your name."
			end if
		end do
		! Player short last name
		print*, "Your shortened last name is a shorter version of your surname that you would commonly go by."
		do while (name_is_confirmed .EQV. .false.)
			print*, "Enter your shortened last name. Max length is 20 characters."
			read(*,'(A)') player_short_last_name
			player_short_last_name = fixformat(player_short_last_name)
			print*, "Your short last name is ",TRIM(player_short_last_name),"."
			print*, "Is this good? Yes or No?"
			read (*,*) is_my_name_good
			is_my_name_good = fixformat(is_my_name_good)
			if (is_my_name_good .EQ.'Yes') then
				print*, "Excellent."
				exit
			else if (is_my_name_good .EQ. 'No') then
				print*, "No problem. Try again."
			else
				print*, "Invalid response."
				print*, "Please reenter your name."
			end if
		end do
		! Player nickname
		print*, "Your nickname is the name that you would informally go by."
		do while (name_is_confirmed .EQV. .false.)
			print*, "Enter your nickname. Max length is 20 characters."
			print*, "If you don't have one, type in None."
			read(*,'(A)') player_nickname
			player_nickname = fixformat(player_nickname)
			! If player has a valid nickname, prints it, otherwise assigns one based on the short first name.
			if (player_nickname .EQ. 'None') then
				player_nickname = player_short_name
				print*, "We'll use your short first name as your nickname then."
				print*, "It'll be ",TRIM(player_nickname),"."
			else
				print*, "Your nickname is ",TRIM(player_nickname),"."
			end if

			print*, "Is this good? Yes or No?"
			read (*,*) is_my_name_good
			is_my_name_good = fixformat(is_my_name_good)
			if (is_my_name_good .EQ.'Yes') then
				print*, "Excellent."
				exit
			else if (is_my_name_good .EQ. 'No') then
				print*, "No problem. Try again."
			else
				print*, "Invalid response."
				print*, "Please reenter your name."
			end if
		end do
		! Final review
		print*, "Let's review everything."
		do while (name_is_confirmed .EQV. .false.)
			if (player_is_nobility .EQV. .true.) then
				print*, "Your full name is ",TRIM(player_forename)," ",TRIM(player_surname)," ",TRIM(player_nobilary),"."
			else
				print*, "Your full name is ",TRIM(player_forename)," ",TRIM(player_surname),"."
			end if
			print*, "Your short name is ",TRIM(player_short_name)," ",TRIM(player_short_last_name),"."
			print*, "You go by ",TRIM(player_nickname),"."
			print*, "Is this all good? Yes or No."
			read (*,*) is_my_name_good
			is_my_name_good = fixformat(is_my_name_good)
			if (is_my_name_good .EQ.'Yes') then
				print*, "That's great."
				name_is_confirmed = .true.
			else if (is_my_name_good .EQ. 'No') then
				print*, "No problem. Let's start from scratch."
				exit
			else
				print*, "Try again."
			end if
		end do	
	end do
	end subroutine name_select
end module name_selector
