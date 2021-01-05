program character_builder
	use proper_cap ! Lets you use the fixformat function
	use cap_functions ! lets you use the lower_first_char function
	use ansi_colors ! lets you color text using the color function
	
	use gender_selector ! Lets you use the gender_select subroutine.
	use age_selector ! Lets you use the age_select subroutine
	use race_selector ! Lets you use the race_select subroutine
	use location_selector ! Lets you use the location_select subroutine
	use holy_order_selector ! Lets you use the holy_order_select subroutine
	use career_selector ! Lets you use the career_select subroutine
	use background_selector ! Lets you use the background_select subroutine
	use name_selector ! Lets you use the name_select subroutine
	implicit none
	
	!! Character parameters
	! Gender variables
	character(len=6) :: player_gender
	logical :: player_is_male, player_is_female
	
	! Age variables
	integer(8) :: player_age
	
	! Race variables
	character(len=20) :: player_race
	
	! Location variables (citizenship and sublocation)
	character(len=30) :: player_citizenship, player_area
	
	! Holy Order Variables
	character(len=50) :: player_holy_order
	logical :: player_is_in_holy_order
	
	! Career Select Variables
	character(len=20) :: player_career
	logical :: player_is_doctor, player_is_military
	
	! Background Select Variables
	character(len=20) :: player_background, player_imperial_class
	logical :: player_is_convent_auroran, player_is_convent_exile
	logical :: player_is_diasporic, player_is_settler, player_is_nobility
	logical :: player_is_cstphene_guardsman, player_served_in_banact_war
	
	! Name Select variables
	character(len=60) :: player_forename,player_surname,player_nobilary
	character(len=20) :: player_short_name,player_short_last_name
	character(len=20) :: player_nickname
	
	! Driver license parameter
	logical :: player_has_driver_license
	
	!! End character parameters
	
	!! Temporary variables
	logical :: character_is_final
	character(len=5) :: is_my_character_final
	character(len=6) :: do_i_want_license
	
	character_is_final = .false.
	
	player_has_driver_license = .true. ! should be true for singleplayer
	

	
	
	do while (character_is_final .EQV. .false.)
		! First, you select your gender
		call gender_select(player_gender, player_is_male, player_is_female)
	
		! Then, select your age
		call age_select(player_age)
	
		! Then, select your race
		call race_select(player_race)
	
		! Then select your locations
		call location_select(player_citizenship, player_area)
	
		! Then pick whether or not you are in a Holy Order
		call holy_order_select(player_race, player_holy_order, player_is_in_holy_order)
	
		! Then pick your career
		call career_select(player_is_male, player_citizenship, player_is_in_holy_order, &
		player_career, player_is_doctor, player_is_military)
	
		! Then pick your background
		call background_select(player_race, player_career, player_citizenship, player_area, &
		player_holy_order, player_is_in_holy_order, player_is_military, player_age, &
		player_background, player_imperial_class, player_is_convent_auroran, &
		player_is_convent_exile, player_is_diasporic, player_is_settler, player_is_nobility, &
		player_is_cstphene_guardsman, player_served_in_banact_war)
	
		! Then pick your name
		call name_select(player_is_nobility, player_forename, player_surname, player_nobilary,&
		player_short_name, player_short_last_name, player_nickname)
	
		! Driver's License Selector (disabled for singleplayer)
		!do while (player_has_driver_license .EQV. .false.)
		!	print*, "Do you want a driver's license?"
		!	print*, "Yes or No."
		!	read (*,*) do_i_want_license
		!	do_i_want_license = fixformat(do_i_want_license)
		!	if (do_i_want_license .EQ. 'Yes') then
		!		player_has_driver_license = .true.
		!	else if (do_i_want_license .EQ. 'No')
		!		player_has_driver_license = .false.
		!		exit
		!	else
		!		print*, "Try again."
		!	end if
		!end do
	
		!! Final check system.
		! Prints name
		if (player_is_nobility .EQV. .true.) then
			print*, "Your name is ", TRIM(player_forename)," ", TRIM(player_surname)," ", TRIM(player_nobilary),"."
		else
			print*, "Your name is ", TRIM(player_forename)," ",TRIM(player_surname),"."
		end if
		print*, "In short, ",TRIM(player_short_name), " ", TRIM(player_short_last_name), "."
		print*, "Or just ",TRIM(player_nickname), "."
		! prints gender, race, area, and background
		print*, "You are a ",TRIM(lower_first_char(player_gender))," ",TRIM(player_race), " from ", TRIM(player_area),&
		". You come from a ",TRIM(lower_first_char(player_background))," background."
		! prints career
		if (player_career .EQ. 'Archaeologist') then
			print*, "You are an ",TRIM(lower_first_char(player_career)),"."
		else if (player_career .EQ. 'Anthropologist') then
			print*, "You are an ",TRIM(lower_first_char(player_career)),"."
		else if (player_career .EQ. 'Engineer') then
			print*, "You are an ",TRIM(lower_first_char(player_career)),"."
		else if (player_career .EQ. 'Officer') then
			print*, "You are an ",TRIM(lower_first_char(player_career)),"."
		else if (player_career .EQ. 'Airman') then
			print*, "You are an ",TRIM(lower_first_char(player_career)),"."
		else if (player_career .EQ. 'Ordinance') then
			print*, "You are an ",TRIM(lower_first_char(player_career))," specialist."
		else if (player_career .EQ. 'Police') then
			print*, "You serve in the police."
		else if (player_career .EQ. 'Infantry') then
			print*, "You serve in the infantry."
		else if (player_career .EQ. 'Calvary') then
			print*, "You serve in the calvary."
		else if (player_career .EQ. 'Covert Operations') then
			print*, "You work in ",TRIM(lower_first_char(player_career)),"."
		else
			print*, "You are a ",TRIM(lower_first_char(player_career)),"."
		end if
		print '(5g0)', "You are ",player_age," years old."
		if (player_is_in_holy_order .EQV. .true.) then
			print*, "You are a member of ", TRIM(player_holy_order),"."
		else
			print*, "You are not a member of a Holy Order."
		end if
		
		do while (character_is_final .EQV. .false.)
			print*, "Is this who you want to be?"
			print*, "Yes or No?"
			read (*,*) is_my_character_final
			is_my_character_final = fixformat(is_my_character_final)
			if (is_my_character_final .EQ. 'Yes') then
				print*, "Excellent."
				character_is_final = .true.
			else if (is_my_character_final .EQ. 'No') then
				print*, "Let's start again then."
				exit
			else
				print*, "Try again."
			end if
		end do
		
	end do

end program character_builder
