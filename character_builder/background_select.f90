module background_selector
	use proper_cap ! allows use of the fixformat function.
	
	implicit none
	
	CONTAINS
	subroutine background_select(player_race, player_career, player_citizenship, player_area, &
	player_holy_order, player_is_in_holy_order, player_is_military, player_age, &
	player_background, player_imperial_class, player_is_convent_auroran, &
	player_is_convent_exile, player_is_diasporic, player_is_settler, player_is_nobility, &
	player_is_cstphene_guardsman, player_served_in_banact_war)
	!! Imported values
	character(len=*), intent(in) :: player_race, player_career
	character(len=*), intent(in) :: player_citizenship, player_area
	character(len=*), intent(in) :: player_holy_order
	logical, intent(in) :: player_is_in_holy_order,player_is_military
	integer(8), intent(in) :: player_age
	!! End Imported values
	
	!! Subroutine output variables
	character(len=*), intent(out) :: player_background, player_imperial_class
	logical, intent(out) :: player_is_convent_auroran, player_is_convent_exile
	logical, intent(out) :: player_is_diasporic, player_is_settler, player_is_nobility
	logical, intent(out) :: player_is_cstphene_guardsman, player_served_in_banact_war
	!! End player bonus variables
	
	!!
	integer(8) :: age_cap ! age cap for being in the Banact War.
	
	!! Temporary Variables for functions
	character(len=5) :: do_i_want_background_info,my_background_is_okay,am_i_convent_exile
	character(len=5) :: am_i_cstphene_guardsman,did_i_serve_in_banact_war
	character(len=20) :: bckgrnd_i_want_to_know_about
	logical :: background_is_picked,i_want_background_info,i_have_decided_my_exile_status
	
	age_cap = 37
	
	background_is_picked = .false.
	i_want_background_info = .false.
	i_have_decided_my_exile_status = .false.
	player_is_cstphene_guardsman = .false.
	player_served_in_banact_war = .false.
	
	do while (background_is_picked .EQV. .false.)
		print*, "Pick a background:"
		print*, "Rural"
		print*, "Urban"
		print*, "Lowborn"
		print*, "Spiritual"
		print*, "Militarist"
		! Players from the Empire and not from Kathay can be nobility!
		if (player_citizenship .EQ. 'The Empire' .and. player_area .NE. 'Kathay') then
			print*, "Minor Nobility"
			print*, "Nobility"
			print*, "High Nobility"
		end if
		! Info section.
		do while (i_want_background_info .EQV. .false.)
			print*, "Do you want to know about any of these backgrounds?"
			print*, "Yes or No?"
			read (*,*) do_i_want_background_info
			do_i_want_background_info = fixformat(do_i_want_background_info)
			if (do_i_want_background_info .EQ. 'Yes') then
				print*, "Excellent."
				i_want_background_info = .true.
			else if (do_i_want_background_info .EQ. 'No') then
				print*, "Alright."
				exit
			else
				print*, "Try again."
			end if
		end do
		! If you want background info, this loop triggers.
		do while (i_want_background_info .EQV. .true.)
			print*, "Which background do you want to know about?"
			print*, "Type Backgrounds to see the list of backgrounds."
			print*, "Type Exit to stop learning."
			read (*,'(A)') bckgrnd_i_want_to_know_about
			bckgrnd_i_want_to_know_about = fixformat(bckgrnd_i_want_to_know_about)
			! print*, bckgrnd_i_want_to_know_about ! useful for debugging.
			if (bckgrnd_i_want_to_know_about .EQ. 'Exit') then
				print*, "Let's move on then."
				i_want_background_info = .false.
			else if (bckgrnd_i_want_to_know_about .EQ. 'Backgrounds') then
				print*, "Backgrounds:"
				print*, "Rural"
				print*, "Urban"
				print*, "Lowborn"
				print*, "Spiritual"
				print*, "Militarist"
				if (player_citizenship .EQ. 'The Empire' .and. player_area .NE. 'Kathay') then
					print*, "Minor Nobility"
					print*, "Nobility"
					print*, "High Nobility"
				end if
			else if (bckgrnd_i_want_to_know_about .EQ. 'Rural') then
				print*, "You were born and raised in a rural area, far from the urban centers of civilization. ",&
				"You have likely lived in a small village, isolated for most of your life, and may have a limited formal ",&
				"education. Many leave rural life to pursue careers in the cities, and many who remain behind have little ",&
				"opportunity in life."
			else if (bckgrnd_i_want_to_know_about .EQ. 'Urban') then
				print*, "You were born and raised in one of the large urban areas of the Continent, in a city of a size anywhere ",&
				"between a few hundred thousand to in the tens of millions. Urbanites can be poor, rich, or anything in between, ",&
				"and of a variety of education levels. Urban citizens tend to be the primary pool of manpower for most of the ",&
				"governorate militaries in the Empire."
			else if (bckgrnd_i_want_to_know_about .EQ. 'Lowborn') then
				print*, "You are a distinctly successful individual that was not born into wealth or status. These lowborn ",&
				"members of society are noted for their ambition and their distinct differences from the standard elite. ",&
				"Some hail the presence of the Lowborn as a success of the Empire, but many counter that. Most Lowborn are ",&
				"Imperial, but there are some of all races."
			else if (bckgrnd_i_want_to_know_about .EQ. 'Spiritual') then
				print*, "You were born and raised in a heavily spiritual environment. Children of this background were often ",&
				"raised by parents who were active members of a Holy Order, or just pious individuals. While Highlanders have the ",&
				"highest rate of spirituality, there are broad pockets of the spiritual found everywhere. These Spiritual people ",&
				"tend to be well educated and often serve in Holy Orders themselves, and a select few from the Empire and beyond even ",&
				"immigrate to the Highlands to serve the spirits more directly."
			else if (bckgrnd_i_want_to_know_about .EQ. 'Militarist') then
				print*, "You were raised by a family who served in secular military forces for most of their life. These ",&
				"Milistarist people have often been groomed for military service and often pursue careers in the officer ",&
				"corps of their local militaries. People with a martial background are highly valued by the societies in ",&
				"the Highlands and Cstphon, with an increasing following in Kathay."
			else if (bckgrnd_i_want_to_know_about .EQ. 'Minor Nobility') then
				if (player_citizenship .EQ. 'The Empire' .and. player_area .NE. 'Kathay') then
					print*, "You are from a minor noble family, right on the lowest rung of the feudal system in the Empire. ",&
					"Families of minor nobility are often either poor or middle class, but are noted for their staunch loyalty ",&
					"to the Empire, even those who are not Imperial themselves. Many leave their minor noble roots behind to ",&
					"pursue something more. Many in the minor nobility provide the backbone of the enlisted personell for the ",&
					"governorate militaries in the Empire."
				else
					print*, "You aren't allowed to be minor nobility."
				end if
			else if (bckgrnd_i_want_to_know_about .EQ. 'Nobility') then
				if (player_citizenship .EQ. 'The Empire' .and. player_area .NE. 'Kathay') then
					print*, "You were born to an upper class noble family, mediating between the lowest nobles and the ",&
					"high nobility. People from such a status tend to be well educated and many serve in the military as ",&
					"officers before returning to rule their holdings. Most tend to be Imperials, but there are exceptions, ",&
					"with most exceptions being foreign to the area they rule over."
				else
					print*, "You aren't allowed to be nobility."
				end if
			else if (bckgrnd_i_want_to_know_about .EQ. 'High Nobility') then
				if (player_citizenship .EQ. 'The Empire' .and. player_area .NE. 'Kathay') then
					print*, "You were born to a family in the High Nobility, at the very top of the Imperial feudal system. ",&
					"Such individuals tend to be incredibly wealthy, very well educated, and most serve at the top of the ",&
					"Imperial government and military. They are often criticized as decadent, but they have remained competent ",&
					"at the top of the Empire for hundreds of years. They are often disliked by their many non-Imperial subjects. ",&
					"While the majority of the high nobililty is Imperial in nature, there are several notable exceptions to this rule."
				else
					print*, "You aren't allowed to be high nobility."
				end if
			else
				print*, "Try again."
			end if
		end do
		do while (background_is_picked .EQV. .false.)
			print*, "What is your background?"
			print*, "Type in Backgrounds to see the list again."
			read (*,'(A)') player_background
			player_background = fixformat(player_background)
			if (player_background .EQ. 'Backgrounds') then
				print*, "Backgrounds:"
				print*, "Rural"
				print*, "Urban"
				print*, "Lowborn"
				print*, "Spiritual"
				print*, "Militarist"
				if (player_citizenship .EQ. 'The Empire' .and. player_area .NE. 'Kathay') then
					print*, "Minor Nobility"
					print*, "Nobility"
					print*, "High Nobility"
				end if
			else if (player_background .EQ. 'Rural') then
				player_is_nobility = .false.
				exit
			else if (player_background .EQ. 'Urban') then
				player_is_nobility = .false.
				exit
			else if (player_background .EQ. 'Lowborn') then
				player_is_nobility = .false.
				exit
			else if (player_background .EQ. 'Spiritual') then
				player_is_nobility = .false.
				exit
			else if (player_background .EQ. 'Militarist') then
				player_is_nobility = .false.
				exit
			else if (player_background .EQ. 'Minor Nobility') then
				if (player_citizenship .EQ. 'The Empire' .and. player_area .ne. 'Kathay') then
					player_is_nobility = .true.
					exit
				else
					print*, "You can't be minor nobility. Please select another background."
				end if
			else if (player_background .EQ. 'Nobility') then
				if (player_citizenship .EQ. 'The Empire' .and. player_area .ne. 'Kathay') then
					player_is_nobility = .true.
					exit
				else
					print*, "You can't be nobility. Please select another background."
				end if
			else if (player_background .EQ. 'High Nobility') then
				if (player_citizenship .EQ. 'The Empire' .and. player_area .ne. 'Kathay') then
					player_is_nobility = .true.
					exit
				else
					print*, "You can't be high nobility. Please select another background."
				end if
			else
				print*, "Try again."
			end if
		end do
		! Confirming background
		do while (background_is_picked .EQV. .false.)
			if (player_is_nobility .EQV. .true.) then
				if (player_background .EQ. 'Minor Nobility') then
					print*, "You are a Minor Noble."
				else if (player_background .EQ. 'Nobility') then
					print*, "You are of a Noble background."
				else
					print*, "You are a High Noble."
				end if
			else
				print*, "You are of a ",TRIM(player_background)," background."
			end if
			print*, "Is this okay? Yes or No."
			read (*,*) my_background_is_okay
			my_background_is_okay = fixformat(my_background_is_okay)
			if (my_background_is_okay .EQ. 'Yes') then
				print*, "Excellent."
				background_is_picked = .true.
			else if (my_background_is_okay .EQ. 'No') then
				print*, "Let's pick again then."
				exit
			else
				print*, "Try again."
			end if
		end do
	end do
	
	! Determining Backgrounds:
		! Rural
		! Urban
		! Lowborn
		! Spiritual
		! Militarist
		! Minor, normal or High nobility
			! Only triggers if party member is from the Empire.
	
	
		
	! Diasporic and settler background
		! Diasporic if player is from a race not native to their location.
		! Settler if player is from an Imperial subject race and a citizen of the Empire and not in their native land.
			! All others false.
		
	! Foyerian, Vitalian, and Orieni Highlander players are diasporic if not from the Highlands.
	if (player_race .EQ. 'Foyerian' .and. player_citizenship .NE. 'The Highlands') then
		player_is_diasporic = .true.
		player_is_settler = .false.
	else if (player_race .EQ. 'Vitalian' .and. player_citizenship .NE. 'The Highlands') then
		player_is_diasporic = .true.
		player_is_settler = .false.
	else if (player_race .EQ. 'Orieni Highlander' .and. player_citizenship .NE. 'The Highlands') then
		player_is_diasporic = .true.
		player_is_settler = .false.
	! Imperial characters are diasporic if not from the Empire (but can be from anywhere in there)
	else if (player_race .EQ. 'Imperial' .and. player_citizenship .NE. 'The Empire') then
		player_is_diasporic = .true.
		player_is_settler = .false.
	! The various Imperial subject races are diasporic if they don't live in their native land.
	else if (player_race .EQ. 'Kathaic' .and. player_area .NE. 'Kathay') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Thurop' .and. player_area .NE. 'Thurop') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Cstphene' .and. player_area .NE. 'Cstphon') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Entranan' .and. player_area .NE. 'Entrana') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Fetan' .and. player_area .NE. 'Fetedal') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Ralois' .and. player_area .NE. 'Ralaer') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Hanoir' .and. player_area .NE. 'Hanor') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Nerhest' .and. player_area .NE. 'Nerhast') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Aryan' .and. player_area .NE. 'Arya') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Tiblan' .and. player_area .NE. 'Tiblus') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Sophene' .and. player_area .NE. 'Sophos') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Algus' .and. player_area .NE. 'Algus') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Anoch' .and. player_area .NE. 'Anoch') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Dessan' .and. player_area .NE. 'Edessa') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Rihden' .and. player_area .NE. 'Rihde') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ.'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Inden' .and. player_area .NE. 'Inden') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Sinias' .and. player_area .NE. 'Siniasus') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Mokven' .and. player_area .NE. 'Mokvon') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	else if (player_race .EQ. 'Vanois' .and. player_area .NE. 'Vanas') then
		player_is_diasporic = .true.
		if (player_citizenship .EQ. 'The Empire') then
			player_is_settler = .true.
		else
			player_is_settler = .false.
		end if
	! Only triggers if you're from the same general location as your ethnic group is from.
	else
		player_is_diasporic = .false.
		player_is_settler = .false.
	end if
	
	! Determining Imperial type: Provincial or Governorate
	if (player_race .EQ. 'Imperial') then
		! Imperials from a Highlander or Imperial province are provincial.
		if (player_area .EQ. 'Foyer') then
			player_imperial_class = 'Provincial'
		else if (player_area .EQ. 'Vility') then
			player_imperial_class = 'Provincial'
		else if (player_area .EQ. 'Zarata') then
			player_imperial_class = 'Provincial'
		else if (player_area .EQ. 'Imperia') then
			player_imperial_class = 'Provincial'
		else if (player_area .EQ. 'Valle') then
			player_imperial_class = 'Provincial'
		else if (player_area .EQ. 'Petral') then
			player_imperial_class = 'Provincial'
		else if (player_area .EQ. 'Aurora Nova') then
			player_imperial_class = 'Provincial'
		else if (player_area .EQ. 'Coulon') then
			player_imperial_class = 'Provincial'
		else if (player_area .EQ. 'Enrenan') then
			player_imperial_class = 'Provincial'
		else if (player_area .EQ. 'Hiten') then
			player_imperial_class = 'Provincial'
		! Imperials from anywhere that isn't a province are from a governorate by default.
		else
			player_imperial_class = 'Governorate'
		end if
	else
		player_imperial_class = 'False' ! Non-Imperials gets a 'False' string assigned instead of a proper tag to prevent crashes.
	end if
	
	
	! Convent Auroran
	! Is true only if party member is a Convent Auroran
	if (player_race .EQ. 'Convent Auroran') then
		player_is_convent_auroran = .true.
	else
		player_is_convent_auroran = .false.
	end if
	
	! Determining Convent Auroran exile status
	! Is exile automatically if their Holy Order is incorrect
	if (player_race .EQ. 'Convent Auroran') then
		if (player_is_in_holy_order .EQV. .true.) then
			! A member of the Holy Order of the Queen of the Ice is not necessarily an Exile.
			if (player_holy_order .EQ. 'The Holy Order of the Queen of the Ice') then
				do while (i_have_decided_my_exile_status .EQV. .false.)
					print*, "Do you want to be a Convent Auroran Exile?"
					print*, "Yes or No? Type Info for more information."
					read (*,*) am_i_convent_exile
					am_i_convent_exile = fixformat(am_i_convent_exile)
					if (am_i_convent_exile .EQ. 'Yes') then
						print*, "You are a Convent Auroran Exile then."
						player_is_convent_exile = .true.
						i_have_decided_my_exile_status = .true.
					else if (am_i_convent_exile .EQ. 'No') then
						print*, "You are not a Convent Auroran Exile then."
						player_is_convent_exile = .false.
						i_have_decided_my_exile_status = .true.
					else if (am_i_convent_exile .EQ. 'Info') then
						print*, "A Convent Auroran Exile is a Convent Auroran who has left behind their origins. They are still ",&
						"permanently branded as Convent Aurorans by society at large, but are isolated and shunned from the common ",&
						"Convent Auroran community. People become exiles for many reasons, from pursuing servitude to spirits outside ",&
						"a select few spirits, love to a person from a different race or another Exile, committing crimes deemed ",&
						"unforgivable, or violating the extensive cultural rules commonly accepted by Convent Auroran society."
						print*, "An Exile can expect to be discriminated against by communed Convent Aurorans, but find ",&
						"acceptance with their fellow Exiles, and with a good portion of the population who opposes the otherness ",&
						"of those Convent Aurorans who do not expose themselves to the common society at large."
					else
						print*, "Try again."
					end if
				end do
			! A player from the Holy Order of the Fair Essence is not necessarily an Exile.
			else if (player_holy_order .EQ. 'The Holy Order of the Fair Essence') then
				do while (i_have_decided_my_exile_status .EQV. .false.)
					print*, "Do you want to be a Convent Auroran Exile?"
					print*, "Yes or No? Type Info for more information."
					read (*,*) am_i_convent_exile
					am_i_convent_exile = fixformat(am_i_convent_exile)
					if (am_i_convent_exile .EQ. 'Yes') then
						print*, "You are a Convent Auroran Exile then."
						player_is_convent_exile = .true.
						i_have_decided_my_exile_status = .true.
					else if (am_i_convent_exile .EQ. 'No') then
						print*, "You are not a Convent Auroran Exile then."
						player_is_convent_exile = .false.
						i_have_decided_my_exile_status = .true.
					else if (am_i_convent_exile .EQ. 'Info') then
						print*, "A Convent Auroran Exile is a Convent Auroran who has left behind their origins. They are still ",&
						"permanently branded as Convent Aurorans by society at large, but are isolated and shunned from the common ",&
						"Convent Auroran community. People become exiles for many reasons, from pursuing servitude to spirits outside ",&
						"a select few spirits, love to a person from a different race or another Exile, committing crimes deemed ",&
						"unforgivable, or violating the extensive cultural rules commonly accepted by Convent Auroran society."
						print*, "An Exile can expect to be discriminated against by communed Convent Aurorans, but find ",&
						"acceptance with their fellow Exiles, and with a good portion of the population who opposes the otherness ",&
						"of those Convent Aurorans who do not expose themselves to the common society at large."
					else
						print*, "Try again."
					end if
				end do
			! You are automatically an exile due to your Holy Order
			else
				print*, "You are a member of ",TRIM(player_holy_order),"."
				print*, "By joining this Holy Order, you have been exiled by your fellow Convent Aurorans."
				player_is_convent_exile = .true.
			end if
		! If you are not from a Holy Order, you can openly decide your Exile status.
		else
			do while (i_have_decided_my_exile_status .EQV. .false.)
				print*, "Do you want to be a Convent Auroran Exile?"
				print*, "Yes or No? Type Info for more information."
				read (*,*) am_i_convent_exile
				am_i_convent_exile = fixformat(am_i_convent_exile)
				if (am_i_convent_exile .EQ. 'Yes') then
					print*, "You are a Convent Auroran Exile then."
					player_is_convent_exile = .true.
					i_have_decided_my_exile_status = .true.
				else if (am_i_convent_exile .EQ. 'No') then
					print*, "You are not a Convent Auroran Exile then."
					player_is_convent_exile = .false.
					i_have_decided_my_exile_status = .true.
				else if (am_i_convent_exile .EQ. 'Info') then
					print*, "A Convent Auroran Exile is a Convent Auroran who has left behind their origins. They are still ",&
					"permanently branded as Convent Aurorans by society at large, but are isolated and shunned from the common ",&
					"Convent Auroran community. People become exiles for many reasons, from pursuing servitude to spirits outside ",&
					"a select few spirits, love to a person from a different race or another Exile, committing crimes deemed ",&
					"unforgivable, or violating the extensive cultural rules commonly accepted by Convent Auroran society."
					print*, "An Exile can expect to be discriminated against by communed Convent Aurorans, but find ",&
					"acceptance with their fellow Exiles, and with a good portion of the population who opposes the otherness ",&
					"of those Convent Aurorans who do not expose themselves to the common society at large."
				else
					print*, "Try again."
				end if
			end do
		end if
	! If you're not a Convent Auroran.
	else
		player_is_convent_exile = .false. ! for all other races, they are not considered Convent Exiles to prevent exception..
	end if
	
	! Cstphene Guardsman select
	! Triggers if player is not in a holy order and if they are from Cstphon and they are either in the infantry or an officer.
	if (player_area .EQ. 'Cstphon') then
		if (player_is_military .EQV. .true. .and. player_is_in_holy_order .EQV. .false.) then
			if (player_career .EQ. 'Infantry' .OR. player_career .EQ. 'Officer') then
				do while (player_is_cstphene_guardsman .EQV. .false.)
					print*, "Are you a Cstphene Guardsman?"
					print*, "Yes or No. Type Info for information on what a Cstphene Guardsman is."
					read (*,*) am_i_cstphene_guardsman
					am_i_cstphene_guardsman = fixformat(am_i_cstphene_guardsman)
					if (am_i_cstphene_guardsman .EQ. 'Yes') then
						print*, "You are a Cstphene Guardsman then."
						player_is_cstphene_guardsman = .true.
					else if (am_i_cstphene_guardsman .EQ. 'No') then
						print*, "Alright."
						player_is_cstphene_guardsman = .false.
						exit
					else if (am_i_cstphene_guardsman .EQ. 'Info') then
						print*, "The Cstphene Guard is an elite military unit based in the City of Cstphon, in the Governorate of ",&
						"Cstphon. Its ranks are populated by the various people who inhabit the governorate, and the members ",&
						"often see prestigious deployments around the Empire. They may form a portion of the Imperial military ",&
						"deployment in a governorate's capital, or in other choice locations. It is a well-respected and prestigious " ,&
						"career."
					end if
				end do
			else
				player_is_cstphene_guardsman = .false.
			end if
		else
			player_is_cstphene_guardsman = .false.
		end if
	else
		player_is_cstphene_guardsman = .false.
	end if
	
	! Seeing if the player served in the Banact War.
	! If  the player is in the military and is old enough, they can choose if they served in the war.
	if (player_age >= age_cap) then
		if (player_is_military .EQV. .true.) then
			do while (player_served_in_banact_war .EQV. .false.)
				print*, "Did you serve in the Banact War?"
				print*, "Yes or No. Type Info for information."
				read (*,*) did_i_serve_in_banact_war
				did_i_serve_in_banact_war = fixformat(did_i_serve_in_banact_war)
				if (did_i_serve_in_banact_war .EQ. 'Yes') then
					print*, "Excellent. You no doubt served well."
					player_served_in_banact_war = .true.
				else if (did_i_serve_in_banact_war .EQ. 'No') then
					print*, "No doubt that you had more pressing issues."
					player_served_in_banact_war = .false.
					exit
				else if (did_i_serve_in_banact_war .EQ. 'Info') then
					print*, "The Banact War was a conflict with the Empire of the Eternal Warlord, the Highlands, and Aurora against ",&
					"the Banact, a reptilian people of a supposed celestial origin; it ended twenty years ago. The brutal war would ",&
					"see a significant portion of the Empire and even the Highlands occupied. It was a wildly destructive war that ",&
					"left the Empire significantly weakened and on the brink of collapse. Millions of men from all walks of life joined ",&
					"the armies of the world to fight against the Banact menace."
				else
					print*, "Try again."
				end if
			end do
		else
			player_served_in_banact_war = .false.
		end if
	else
		player_served_in_banact_war = .false.
	end if
	end subroutine background_select
end module background_selector
