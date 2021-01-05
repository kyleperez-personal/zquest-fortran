module location_selector
	use proper_cap ! lets one fix capitalization errors with the fixformat function.

	implicit none
	
	CONTAINS
	subroutine location_select(player_citizenship,player_area)
	!! Player Origin variables
	character(len=*), intent(out) :: player_citizenship, player_area ! Realm that the player is from and where from in particular
	!! TO ADD: variables to pick city you are from and other sublocations!
	!! End Origin variables
		
	!! Temporary variables for selection
	character(len=5) :: more_realm_knowledge,my_region_is_correct ! Responses to questions
	character(len=7) :: i_want_area_info ! Response to question + can change location
	logical :: nation_is_confirmed,i_want_more_realm_info,citizenship_is_valid,&
	citizenship_cannot_change,area_info_wanted,region_chosen,region_is_correct ! Used to keep loops going
	character(len=25) :: realm_info_request,area_info_request ! The name of the realm/area information is requested for.
	
	nation_is_confirmed = .false.
	i_want_more_realm_info = .false.
	citizenship_is_valid = .false.
	citizenship_cannot_change = .false.
	area_info_wanted = .false.
	region_chosen = .false.
	region_is_correct = .false.
	
	! Location of Player Origin Selector
	print*, "Where are you from?"
	do while(nation_is_confirmed .EQV. .false.) ! While nation_is_confirmed is false, keep loop going.
		print*, "Pick from the following realms:"
		print*, "The Highlands"
		print*, "The Empire"
		do while (i_want_more_realm_info .EQV. .false.)
			print*, "Do you want to know any information about the realms? Yes or No?"
			read (*,*) more_realm_knowledge ! Whether or not you want to know more about realms
			more_realm_knowledge = fixformat(more_realm_knowledge)
			if (more_realm_knowledge .EQ. 'Yes') then
				i_want_more_realm_info = .true. ! I want to know more
			else if (more_realm_knowledge .EQ. 'No') then
				i_want_more_realm_info = .false. ! I don't want to know anything
				exit
			else
				print*, "Yes or No please."
			end if
		end do
	!! Realm Information
		do while (i_want_more_realm_info .EQV. .true.) ! while i_want_more_realm_info is true, you can learn about the realms
			print*, "Which realm do you want to know about?"
			print*, "Type 'Realms' to see the list of realms again."
			print*, "Type 'Exit' to end the information session."
			read (*,'(A)') realm_info_request	
			realm_info_request = fixformat(realm_info_request)
			if (realm_info_request .EQ. 'Exit') then
				print*, "Moving on then." ! I don't want to know anything else
				i_want_more_realm_info = .false.
			else if (realm_info_request .EQ. 'Realms') then
				print*, "Realm List:"
				print*, "The Highlands"
				print*, "The Empire"
			else if (realm_info_request .EQ. 'The Highlands') then
				print*, "A realm headed by Foyer, the Commander of ",&
				"Weapons, an ancient spirit who ascended long ago. The core of ",&
				"the realm is in the west, where it is mostly mountainous, with dense urban pockets in the various isolated valleys. ",&
				"Home to more than just mountains, the Highlands is home to ",&
				"Plains, forests, and deserts across its territory."
				Print*, "The Highlands is the spiritual center of the Continent, where spirits ",&
				"from all across the known world commune and live under the theocracy headed by Foyer."
				Print*, "The Highlands is home to some 750 million people, and is well regarded as ",&
				"a highly militarized state of considerable power alongside its heavily industrialized ",&
				"economy and significant mineral wealth."
				print*, "Capital: City of Foyer (pop: 25,992,000)"
				print*, "Population (est): 751,848,000"
				print*, "By ethnicity:"
				print*, "   Foyerian - 402,033,000"
				print*, "   Vitalian - 288,767,000"
				print*, "   Orieni Highlander - 34,764,000"
				print*, "   Convent Auroran - 12,150,000"
				print*, "   Imperial - 7,969,000"
				print*, "   Imperial Settler - 6,165,000"			
			else if (realm_info_request .EQ. 'The Empire') then
				print*, "The Empire is far from monolithic. Headed by the Eternal Warlord, an undead ",&
				"being of ancient origins, the Empire stretches across most of the Continent, from ",&
				"the border with Aurora to the edge of the Great Desert, to the East Sea, to the ",&
				"border with Oriena."
				print*, "Home to a vast array of peoples scattered throughout the lands, the Empire is ",&
				"home to around 2.5 billion people. It is, however, subject to a great amount of ",&
				"inequalilty in wealth and standard of living; the Empire is home to both the richest ",&
				"and the poorest areas on the Continent. Much of the Empire remains vastly underdeveloped ",&
				"except for a great handful of populous urban centers."
				print*, "Capital: The City of the Eternal Warlord (pop: 84,221,000)"
				print*, "Population (est): 2,502,874,000"
				print*, "By ethnicity (excluding minor groups):"
				print*, "   Imperial - 730,785,000"
				print*, "   Convent Auroran - 60,471,000"
				print*, "   Indigenous - 1,587,091,000"
				print*, "   Settlers - 124,580,000"
			else
				print*, "Try again."
			end if
		end do
		do while (citizenship_is_valid .EQV. .false.) ! Ensures that a valid citizenship is entered.
			print*, "What realm are you from? The Highlands or The Empire?"
			read (*,'(A)') player_citizenship ! The Player's citizenship
			player_citizenship = fixformat(player_citizenship)
			if (player_citizenship .EQ. 'The Highlands') then
				citizenship_is_valid = .true.
			else if (player_citizenship .EQ. 'Highlands') then
				player_citizenship = 'The Highlands'
				citizenship_is_valid = .true.
			else if (player_citizenship .EQ. 'The Empire') then
				citizenship_is_valid = .true.
			else if (player_citizenship .EQ. 'Empire') then
				player_citizenship = 'The Empire'
				citizenship_is_valid = .true.
			else
				print*, "You can only be from The Empire or The Highlands."
			end if
		end do
		do while (citizenship_cannot_change .EQV. .false.) ! While loop is still active, you can change citizenship
			print*, "Select a particular location to be from:"
			!! Block for Highlander Code
			if (player_citizenship .EQ. 'The Highlands') then
				print*, "Province of Foyer"
				print*, "Province of Vility"
				print*, "Province of Zarata"
				print*, "Governorate of Narena"
				print*, "Governorate of Centa"
				print*, "Governorate of Betiera"
				print*, "Governorate of Pallon"
				print*, "Ostorn Governorate"
				print*, "Lifus Governorate"
				print*, "Governorate of Sonal"
				print*, "Governorate of Deseret"
				print*, "Governorate of Anglia"
				print*, "Governorate of Halfon"
				print*, "Era Free Trade State"
				print*, "Do you want to know about any of the areas? Yes or No?"
				do while (area_info_wanted .EQV. .false.)
					read (*,*) i_want_area_info ! Response to the above question
					i_want_area_info = fixformat(i_want_area_info)
					if (i_want_area_info .EQ. 'Yes') then
						print*, "Okay."
						area_info_wanted = .true.
					else if (i_want_area_info .EQ. 'No') then
						print*, "Okay."
						area_info_wanted = .false.
						exit
					else
						print*, "Yes or No please."
					end if		
				end do
				do while (area_info_wanted .EQV. .true.) ! asking about the areas in the Highlands
					print*, "Which area would you like to learn about? Type it in its long name or short name to see information about it."
					print*, "Type 'Exit' to stop learning."
					print*, "Type 'Regions' to see the list of regions again."
					read (*,'(A)') area_info_request ! Answer to question above
					area_info_request = fixformat(area_info_request)
					if (area_info_request .EQ. 'Exit') then
						print*, "Moving on then." ! No more info
						area_info_wanted = .false.
					else if (area_info_request .EQ. 'Regions') then
						! List of regions
						print*, "Province of Foyer"
						print*, "Province of Vility"
						print*, "Province of Zarata"
						print*, "Governorate of Narena"
						print*, "Governorate of Centa"
						print*, "Governorate of Betiera"
						print*, "Governorate of Pallon"
						print*, "Ostorn Governorate"
						print*, "Lifus Governorate"
						print*, "Governorate of Sonal"
						print*, "Governorate of Deseret"
						print*, "Governorate of Anglia"
						print*, "Governorate of Halfon"
						print*, "Era Free Trade State"
					!! Begin Region descriptions
					! Foyer
					else if (area_info_request .EQ. 'Province of Foyer') then
						print*, "The Province of Foyer is the core of the Highlander ",&
						"state. Known simply as Foyer colloquially, it is home to the ",&
						"traditional home of the Highlanders. A significant portion of the ",&
						"Highlander industrial base is located here, and it is among the most ",&
						"urbanized of the Highlander governorates. Home to many urban areas packed ",&
						"in the valleys between the mountainous region, and much of the territory set on a ",&
						"plateau."
						print*, "Capital: City of Foyer (pop: 25,992,000)"
						print*, "Population (est): 208,154,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 151,521,000"
						print*, "   Vitalian - 50,582,000"
						print*, "   Orieni Highlander - 3,398,000"
						print*, "   Convent Auroran - 543,000"
						print*, "   Imperial - 1,361,000"
						print*, "   Imperial Settler - 719,000"
					else if (area_info_request .EQ. 'Foyer') then
						print*, "The Province of Foyer is the core of the Highlander ",&
						"state. Known simply as Foyer colloquially, it is home to the ",&
						"traditional home of the Highlanders. A significant portion of the ",&
						"Highlander industrial base is located here, and it is among the most ",&
						"urbanized of the Highlander governorates. Home to many urban areas packed ",&
						"in the valleys between the mountainous region, and much of the territory set on a ",&
						"plateau."
						print*, "Capital: City of Foyer (pop: 25,992,000)"
						print*, "Population (est): 208,154,000"
						print*, "By ethnicity:"
						print*, "Population (est): 208,154,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 151,521,000"
						print*, "   Vitalian - 50,582,000"
						print*, "   Orieni Highlander - 3,398,000"
						print*, "   Convent Auroran - 543,000"
						print*, "   Imperial - 1,361,000"
						print*, "   Imperial Settler - 719,000"
					! Vility
					else if (area_info_request .EQ. 'Province of Vility') then
						print*, "The Province of Vililty is often referred to as the second ",&
						"homeland of the Highlanders. It maintains a similar climate and terrain as ",&
						"Foyer, and is indistiguishable from it in many ways. This province ",&
						"is most noted for the Vitalians, a hybridized people sourced mostly from ",&
						"Aurorans, but with significant Highlander input. The Vitalians, while the ",&
						"largest group in Vility, make up large minorities through the Highlands."
						print*, "Capital: City of Vility (pop: 24,061,000)"
						print*, "Population (est): 162,003,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 36,613,000"
						print*, "   Vitalian - 120,551,000"
						print*, "   Orieni Highlander - 2,547,000"
						print*, "   Convent Auroran - 969,000"
						print*, "   Imperial - 1,031,000"
						print*, "   Imperial Settler - 292,000"
					else if (area_info_request .EQ. 'Vility') then
						print*, "The Province of Vililty is often referred to as the second ",&
						"homeland of the Highlanders. It maintains a similar climate and terrain as ",&
						"Foyer, and is indistiguishable from it in many ways. This province ",&
						"is most noted for the Vitalians, a hybridized people sourced mostly from ",&
						"Aurorans, but with significant Highlander input. The Vitalians, while the ",&
						"largest group in Vility, make up large minorities through the Highlands."
						print*, "Capital: City of Vility (pop: 24,061,000)"
						print*, "Population (est): 162,003,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 36,613,000"
						print*, "   Vitalian - 120,551,000"
						print*, "   Orieni Highlander - 2,547,000"
						print*, "   Convent Auroran - 969,000"
						print*, "   Imperial - 1,031,000"
						print*, "   Imperial Settler - 292,000"
					! Zarata /Zerixa
					else if (area_info_request .EQ. 'Province of Zarata') then
						print*, "The Province of Zarata is quite similar to Foyer and Vility. ",&
						"However, like Vility, it is distinguished by its unique ethnic group: ",&
						"Orieni Highlanders. This group of people are descended from both Orieni ",&
						"migrants from ancient times and Highlanders. This group makes up around ",&
						"a quarter of the population and form smaller minorities through the realm."
						print*, "Capital: City of Zerixa (pop: 13,617,000)"
						print*, "Population (est): 103,806,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 37,834,000"
						print*, "   Vitalian - 39,673,000"
						print*, "   Orieni Highlander - 25,139,000"
						print*, "   Convent Auroran - 179,000"
						print*, "   Imperial - 708,000"
						print*, "   Imperial Settler - 274,000"
					else if (area_info_request .EQ. 'Zarata') then
						print*, "The Province of Zarata is quite similar to Foyer and Vility. ",&
						"However, like Vility, it is distinguished by its unique ethnic group: ",&
						"Orieni Highlanders. This group of people are descended from both Orieni ",&
						"migrants from ancient times and Highlanders. This group makes up around ",&
						"a quarter of the population and form smaller minorities through the realm."
						print*, "Capital: City of Zerixa (pop: 13,617,000)"
						print*, "Population (est): 103,806,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 37,834,000"
						print*, "   Vitalian - 39,673,000"
						print*, "   Orieni Highlander - 25,139,000"
						print*, "   Convent Auroran - 179,000"
						print*, "   Imperial - 708,000"
						print*, "   Imperial Settler - 274,000"
					else if (area_info_request .EQ. 'Zerixa') then
						print*, "The Province of Zarata is quite similar to Foyer and Vility. ",&
						"However, like Vility, it is distinguished by its unique ethnic group: ",&
						"Orieni Highlanders. This group of people are descended from both Orieni ",&
						"migrants from ancient times and Highlanders. This group makes up around ",&
						"a quarter of the population and form smaller minorities through the realm."
						print*, "Capital: City of Zerixa (pop: 13,617,000)"
						print*, "Population (est): 103,806,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 37,834,000"
						print*, "   Vitalian - 39,673,000"
						print*, "   Orieni Highlander - 25,139,000"
						print*, "   Convent Auroran - 179,000"
						print*, "   Imperial - 708,000"
						print*, "   Imperial Settler - 274,000"
					! Narena
					else if (area_info_request .EQ. 'Governorate of Narena') then
						print*, "The Governorate of Narena is the most far-flung of the governorates. ",&
						"Set on a coastal plain, it is currently experiencing a period of extreme growth, ",&
						"unlike the other eastern governorates. Narena is home to notable populations of ",&
						"minorities, notably Convent Aurorans and Thurops."
						print*, "Capital: Veneta (pop: 7,218,000)"
						print*, "Population (est): 60,401,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 42,312,000"
						print*, "   Vitalian - 9,095,000"
						print*, "   Orieni Highlander - 410,000"
						print*, "   Convent Auroran - 4,395,000"
						print*, "   Imperial - 177,000"
						print*, "   Imperial Settler - 4,012,000"
					else if (area_info_request .EQ. 'Narena') then
						print*, "The Governorate of Narena is the most far-flung of the governorates. ",&
						"Set on a coastal plain, it is currently experiencing a period of extreme growth, ",&
						"unlike the other eastern governorates. Narena is home to notable populations of ",&
						"minorities, notably Convent Aurorans and Thurops."
						print*, "Capital: Veneta (pop: 7,218,000)"
						print*, "Population (est): 60,401,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 42,312,000"
						print*, "   Vitalian - 9,095,000"
						print*, "   Orieni Highlander - 410,000"
						print*, "   Convent Auroran - 4,395,000"
						print*, "   Imperial - 177,000"
						print*, "   Imperial Settler - 4,012,000"
					! Centa
					else if (area_info_request .EQ. 'Governorate of Centa') then
						print*, "The Governorate of Centa borders Nareana. It is mostly flat and ",&
						"agricultural, being the breadbasket of the Eastern Highlands. Despite this, ",&
						"it experienced great damage during the Lizard War, including occupation. It has begun to ",&
						"hemorrhage residents who are moving to better lives in Narena."
						print*, "Capital: Centiphon (pop: 5,971,000)"
						print*, "Population (est): 61,645,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 48,819,000"
						print*, "   Vitalian - 9,143,000"
						print*, "   Orieni Highlander - 372,000"
						print*, "   Convent Auroran - 3,141,000"
						print*, "   Imperial - 98,000"
						print*, "   Imperial Settler - 72,000"
					else if (area_info_request .EQ. 'Centa') then
						print*, "The Governorate of Centa borders Nareana. It is mostly flat and ",&
						"agricultural, being the breadbasket of the Eastern Highlands. Despite this, ",&
						"it experienced great damage during the Lizard War, including occupation. It has begun to ",&
						"hemorrhage residents who are moving to better lives in Narena."
						print*, "Capital: Centiphon (pop: 5,971,000)"
						print*, "Population (est): 61,645,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 48,819,000"
						print*, "   Vitalian - 9,143,000"
						print*, "   Orieni Highlander - 372,000"
						print*, "   Convent Auroran - 3,141,000"
						print*, "   Imperial - 98,000"
						print*, "   Imperial Settler - 72,000"
					! Betiera
					else if (area_info_request .EQ. 'Governorate of Betiera') then
						print*, "The Governorate of Betiera is thought of as the gateway to the ",&
						"Empire, as it holds the busiest border between the two realms. Betiera is ",&
						"home to a mountainous south, but the north is largely flat farmland. Quite ",&
						"homogeneous, populated by mostly Highlanders, it is quite a bit more rural than ",&
						"its neighoring provinces."
						print*, "Capital: Iana (pop: 8,236,000)"
						print*, "Population (est): 24,109,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 18,529,000"
						print*, "   Vitalian - 4,671,000"
						print*, "   Orieni Highlander - 203,000"
						print*, "   Convent Auroran - 72,000"
						print*, "   Imperial - 524,000"
						print*, "   Imperial Settler - 110,000"
					else if (area_info_request .EQ. 'Betiera') then
						print*, "The Governorate of Betiera is thought of as the gateway to the ",&
						"Empire, as it holds the busiest border between the two realms. Betiera is ",&
						"home to a mountainous south, but the north is largely flat farmland. Quite ",&
						"homogeneous, populated by mostly Highlanders, it is quite a bit more rural than ",&
						"its neighoring provinces."
						print*, "Capital: Iana (pop: 8,236,000)"
						print*, "Population (est): 24,109,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 18,529,000"
						print*, "   Vitalian - 4,671,000"
						print*, "   Orieni Highlander - 203,000"
						print*, "   Convent Auroran - 72,000"
						print*, "   Imperial - 524,000"
						print*, "   Imperial Settler - 110,000"
					! Pallon
					else if (area_info_request .EQ. 'Governorate of Pallon') then
						print*, "The Governorate of Pallon is known for its isolation, despite its ",&
						"rather central location in the Highlands. It is centered around the ",&
						"Valley of Pallon, the single largest valley in the Highlands. Despite this, ",&
						"the governorate remains quiet and rather unpopulated, although it is home ",&
						"to large scale mining operations in its tall moutains. It remains the largest ",&
						"producer of iron ore and salt in the Highlands."
						print*, "Capital: Pallum (pop: 1,006,000)"
						print*, "Population (est): 7,457,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 5,821,000"
						print*, "   Vitalian - 1,356,000"
						print*, "   Orieni Highlander - 177,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Imperial - 103,000"
						print*, "   Imperial Settler - <1000"
					else if (area_info_request .EQ. 'Pallon') then
						print*, "The Governorate of Pallon is known for its isolation, despite its ",&
						"rather central location in the Highlands. It is centered around the ",&
						"Valley of Pallon, the single largest valley in the Highlands. Despite this, ",&
						"the governorate remains quiet and rather unpopulated, although it is home ",&
						"to large scale mining operations in its tall moutains. It remains the largest ",&
						"producer of iron ore and salt in the Highlands."
						print*, "Capital: Pallum (pop: 1,006,000)"
						print*, "Population (est): 7,457,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 5,821,000"
						print*, "   Vitalian - 1,356,000"
						print*, "   Orieni Highlander - 177,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Imperial - 103,000"
						print*, "   Imperial Settler - <1000"
					! Ostorn
					else if (area_info_request .EQ. 'Ostorn Governorate') then
						print*, "The Ostorn Governorate is the most heavily forested area in the ",&
						"Highlands, home to its logging industry. It also serves as a buffer between ",&
						"Vility and the Empire, with its nigh-impassible forests making an ",&
						"excellent deterrent for any potential invader."
						print*, "Capital: Bastion (pop: 1,753,000)"
						print*, "Population (est): 8,210,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 1,991,000"
						print*, "   Vitalian - 5,224,000"
						print*, "   Orieni Highlander - 263,000"
						print*, "   Convent Auroran - 319,000"
						print*, "   Imperial - 316,000"
						print*, "   Imperial Settler - 97,000"
					else if (area_info_request .EQ. 'Governorate of Ostorn') then
						print*, "The Ostorn Governorate is the most heavily forested area in the ",&
						"Highlands, home to its logging industry. It also serves as a buffer between ",&
						"Vility and the Empire, with its nigh-impassible forests making an ",&
						"excellent deterrent for any potential invader."
						print*, "Capital: Bastion (pop: 1,753,000)"
						print*, "Population (est): 8,210,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 1,991,000"
						print*, "   Vitalian - 5,224,000"
						print*, "   Orieni Highlander - 263,000"
						print*, "   Convent Auroran - 319,000"
						print*, "   Imperial - 316,000"
						print*, "   Imperial Settler - 97,000"
					else if (area_info_request .EQ. 'Ostorn') then
						print*, "The Ostorn Governorate is the most heavily forested area in the ",&
						"Highlands, home to its logging industry. It also serves as a buffer between ",&
						"Vility and the Empire, with its nigh-impassible forests making an ",&
						"excellent deterrent for any potential invader."
						print*, "Capital: Bastion (pop: 1,753,000)"
						print*, "Population (est): 8,210,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 1,991,000"
						print*, "   Vitalian - 5,224,000"
						print*, "   Orieni Highlander - 263,000"
						print*, "   Convent Auroran - 319,000"
						print*, "   Imperial - 316,000"
						print*, "   Imperial Settler - 97,000"
					! Lifus
					else if (area_info_request .EQ. 'Lifus Governorate') then
						print*, "The Lifus Governorate is the resource capital of the ",&
						"Highlands. It is the largest producer of raw materials in the ",&
						"whole realm, producing coal, oil, and natural gas alongside a variety of ",&
						"heavy and light metals. Its permanent population is rather small, though ",&
						"it is home to a large population of temporary workers."
						print*, "Capital: Fort Ternan (pop: 634,000)"
						print*, "Population (est): 3,851,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 2,243,000"
						print*, "   Vitalian - 1,099,000"
						print*, "   Orieni Highlander - 113,000"
						print*, "   Convent Auroran - 396,000"
						print*, "   Imperial - <1000"
						print*, "   Imperial Settler - <1000"
					else if (area_info_request .EQ. 'Governorate of Lifus') then
						print*, "The Lifus Governorate is the resource capital of the ",&
						"Highlands. It is the largest producer of raw materials in the ",&
						"whole realm, producing coal, oil, and natural gas alongside a variety of ",&
						"heavy and light metals. Its permanent population is rather small, though ",&
						"it is home to a large population of temporary workers."
						print*, "Capital: Fort Ternan (pop: 634,000)"
						print*, "Population (est): 3,851,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 2,243,000"
						print*, "   Vitalian - 1,099,000"
						print*, "   Orieni Highlander - 113,000"
						print*, "   Convent Auroran - 396,000"
						print*, "   Imperial - <1000"
						print*, "   Imperial Settler - <1000"
					else if (area_info_request .EQ. 'Lifus') then
						print*, "The Lifus Governorate is the resource capital of the ",&
						"Highlands. It is the largest producer of raw materials in the ",&
						"whole realm, producing coal, oil, and natural gas alongside a variety of ",&
						"heavy and light metals. Its permanent population is rather small, though ",&
						"it is home to a large population of temporary workers."
						print*, "Capital: Fort Ternan (pop: 634,000)"
						print*, "Population (est): 3,851,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 2,243,000"
						print*, "   Vitalian - 1,099,000"
						print*, "   Orieni Highlander - 113,000"
						print*, "   Convent Auroran - 396,000"
						print*, "   Imperial - <1000"
						print*, "   Imperial Settler - <1000"
					! Sonal
					else if (area_info_request .EQ. 'Governorate of Sonal') then
						print*, "The Governorate of Sonal is home to the production of many cash ",&
						"crops for the Highlander economy, producing plant oils, cotton, tobacco, and ",&
						"a variety of other plants used to manufacture goods. Largely rural and ",&
						"agricultural, it remains as a buffer to the Empire, although exposed."
						print*, "Capital: Aurorus Sonus (pop: 4,465,000)"
						print*, "Population (est): 22,996,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 14,275,000"
						print*, "   Vitalian - 6,289,000"
						print*, "   Orieni Highlander - 371,000"
						print*, "   Convent Auroran - 540,000"
						print*, "   Imperial - 1,278,000"
						print*, "   Imperial Settler - 243,000"
					else if (area_info_request .EQ. 'Sonal') then
						print*, "The Governorate of Sonal is home to the production of many cash ",&
						"crops for the Highlander economy, producing plant oils, cotton, tobacco, and ",&
						"a variety of other plants used to manufacture goods. Largely rural and ",&
						"agricultural, it remains as a buffer to the Empire, although exposed."
						print*, "Capital: Aurorus Sonus (pop: 4,465,000)"
						print*, "Population (est): 22,996,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 14,275,000"
						print*, "   Vitalian - 6,289,000"
						print*, "   Orieni Highlander - 371,000"
						print*, "   Convent Auroran - 540,000"
						print*, "   Imperial - 1,278,000"
						print*, "   Imperial Settler - 243,000"
					! Deseret
					else if (area_info_request .EQ. 'Governorate of Deseret') then
						print*, "The Governorate of Deseret is split between flat plains and desert. ",&
						"The upper half is composed of flat plains, not dissimilar from Centa. The ",&
						"notable portion of it is composed of deserts with the occasional oasis in between. ",&
						"The whole desert is usually seen as an impenetrable fortification, although it is home ",&
						"to a developed road system connecting the oasis cities."
						print*, "Capital: The Oasis of Aurora (pop: 9,122,000)"
						print*, "Population (est): 43,025,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 22,091,000"
						print*, "   Vitalian - 20,368,000"
						print*, "   Orieni Highlander - 56,000"
						print*, "   Convent Auroran - 87,000"
						print*, "   Imperial - 423,000"
						print*, "   Imperial Settler - <1000"
					else if (area_info_request .EQ. 'Deseret') then
						print*, "The Governorate of Deseret is split between flat plains and desert. ",&
						"The upper half is composed of flat plains, not dissimilar from Centa. The ",&
						"notable portion of it is composed of deserts with the occasional oasis in between. ",&
						"The whole desert is usually seen as an impenetrable fortification, although it is home ",&
						"to a developed road system connecting the oasis cities."
						print*, "Capital: The Oasis of Aurora (pop: 9,122,000)"
						print*, "Population (est): 43,025,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 22,091,000"
						print*, "   Vitalian - 20,368,000"
						print*, "   Orieni Highlander - 56,000"
						print*, "   Convent Auroran - 87,000"
						print*, "   Imperial - 423,000"
						print*, "   Imperial Settler - <1000"
					! Anglia
					else if (area_info_request .EQ. 'Governorate of Anglia') then
						print*, "The Governorate of Anglia is largely considered an ",&
						"agricultural province, with agriculture being the largest source of ",&
						"employment, though it is not the largest food producer in the ",&
						"Highlands. Despite this, it continues to produce food for the ",&
						"packed urban centers, taking pressure off farms in Vility and Foyer."
						print*, "Capital: Caire (pop: 6,385,000)"
						print*, "Population (est): 37,250,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 16,789,000"
						print*, "   Vitalian - 18,121,000"
						print*, "   Orieni Highlander - 358,000"
						print*, "   Convent Auroran - 1,297,000"
						print*, "   Imperial - 651,000"
						print*, "   Imperial Settler - 34,000"
					else if (area_info_request .EQ. 'Anglia') then
						print*, "The Governorate of Anglia is largely considered an ",&
						"agricultural province, with agriculture being the largest source of ",&
						"employment, though it is not the largest food producer in the ",&
						"Highlands. Despite this, it continues to produce food for the ",&
						"packed urban centers, taking pressure off farms in Vility and Foyer."
						print*, "Capital: Caire (pop: 6,385,000)"
						print*, "Population (est): 37,250,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 16,789,000"
						print*, "   Vitalian - 18,121,000"
						print*, "   Orieni Highlander - 358,000"
						print*, "   Convent Auroran - 1,297,000"
						print*, "   Imperial - 651,000"
						print*, "   Imperial Settler - 34,000"
					! Halfon
					else if (area_info_request .EQ. 'Governorate of Halfon') then
						print*, "The Governorate of Halfon is mostly dry, mountainous terrain. ",&
						"It remains as a buffer against the Empire, and is largely unpopulated. ",&
						"It houses testing grounds for the military despite its spare population."
						print*, "Capital: Fort Vollon (pop: 372,000)"
						print*, "Population (est): 2,683,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 1,773,000"
						print*, "   Vitalian - 698,000"
						print*, "   Orieni Highlander - <1000"
						print*, "   Convent Auroran - 212,000"
						print*, "   Imperial - <1000"
						print*, "   Imperial Settler - <1000"
					else if (area_info_request .EQ. 'Halfon') then
						print*, "The Governorate of Halfon is mostly dry, mountainous terrain. ",&
						"It remains as a buffer against the Empire, and is largely unpopulated. ",&
						"It houses testing grounds for the military despite its spare population."
						print*, "Capital: Fort Vollon (pop: 372,000)"
						print*, "Population (est): 2,683,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 1,773,000"
						print*, "   Vitalian - 698,000"
						print*, "   Orieni Highlander - <1000"
						print*, "   Convent Auroran - 212,000"
						print*, "   Imperial - <1000"
						print*, "   Imperial Settler - <1000"
					! Era Free Trade
					else if (area_info_request .EQ. 'Era Free Trade State') then
						print*, "The Era Free Trade State began as the City of Era in ",&
						"the Province of Zarata. As an effort to attract business from ",&
						"the Empire, Foyer declared Era its own political entity and gave it ",&
						"significant legal autonomy, often exempt from certain laws and taxes. ",&
						"The division has laws that make it quite cheap for Imperial businesses to ",&
						"use Era as their base of operations for their local Imperial branches."
						print*, "Population (est): 5,765,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 1,129,000"
						print*, "   Vitalian - 1,683,000"
						print*, "   Orieni Highlander - 1,346,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Imperial - 1,296,000"
						print*, "   Imperial Settler - 311,000"
					else if (area_info_request .EQ. 'Era') then
						print*, "The Era Free Trade State began as the City of Era in ",&
						"the Province of Zarata. As an effort to attract business from ",&
						"the Empire, Foyer declared Era its own political entity and gave it ",&
						"significant legal autonomy, often exempt from certain laws and taxes. ",&
						"The division has laws that make it quite cheap for Imperial businesses to ",&
						"use Era as their base of operations for their local Imperial branches."
						print*, "Population (est): 5,765,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 1,129,000"
						print*, "   Vitalian - 1,683,000"
						print*, "   Orieni Highlander - 1,346,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Imperial - 1,296,000"
						print*, "   Imperial Settler - 311,000"
					else if (area_info_request .EQ. 'Era Free Trade') then
						print*, "The Era Free Trade State began as the City of Era in ",&
						"the Province of Zarata. As an effort to attract business from ",&
						"the Empire, Foyer declared Era its own political entity and gave it ",&
						"significant legal autonomy, often exempt from certain laws and taxes. ",&
						"The division has laws that make it quite cheap for Imperial businesses to ",&
						"use Era as their base of operations for their local Imperial branches."
						print*, "Population (est): 5,765,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 1,129,000"
						print*, "   Vitalian - 1,683,000"
						print*, "   Orieni Highlander - 1,346,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Imperial - 1,296,000"
						print*, "   Imperial Settler - 311,000"
					else if (area_info_request .EQ. 'EFT') then
						print*, "The Era Free Trade State began as the City of Era in ",&
						"the Province of Zarata. As an effort to attract business from ",&
						"the Empire, Foyer declared Era its own political entity and gave it ",&
						"significant legal autonomy, often exempt from certain laws and taxes. ",&
						"The division has laws that make it quite cheap for Imperial businesses to ",&
						"use Era as their base of operations for their local Imperial branches."
						print*, "Population (est): 5,765,000"
						print*, "By ethnicity:"
						print*, "   Foyerian - 1,129,000"
						print*, "   Vitalian - 1,683,000"
						print*, "   Orieni Highlander - 1,346,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Imperial - 1,296,000"
						print*, "   Imperial Settler - 311,000"
					! Spirit Archipelago. Secret region to pick to be from!
					else if (area_info_request .EQ. 'Spirit Archipelago') then
						print*, "The Territory of the Spirit Archipelago is a recent conquest ",&
						"out on the East Ocean. This land is home to an indigenous population of men not ",&
						"deemed fit for Highlander society owing to their phenotypical differences and ",&
						"general lack of civilization. At present the military is attempting to rid ",&
						"the islands of these people, resulting in a violent life on the islands for many ",&
						"residents, including Highlanders. Despite this violence, settlers continue ",&
						"to arrive in large numbers, often drawn from the damaged East. The Spirit ",&
						"Archipelago is expected to be ceded to Narena for administration until more ",&
						"territory can be conquered. Migration to the Spirit Archipelago is strictly ",&
						"monitored, with exact population numbers constantly being updated to ",&
						"reflect births, deaths, and arrivals."
						print*, "Capital: Port Foyer (pop: 406,337)"
						print*, "Population: 523,850 legal residents and ~200,000 indigenous"
						print*, "By ethnicity:"
						print*, "   Foyerian - 293,150"
						print*, "   Vitalian - 215,430"
						print*, "   Orieni Highlander - 10,980"
						print*, "   Convent Auroran - 37 (exact)"
						print*, "   Imperial - 3290"
						print*, "   Imperial Settler - 960"
					!! End region descriptions
					else
						print*, "Try again."
					end if		
				end do
				
				do while (region_chosen .EQV. .false.) ! Makes sure that the region input is correct
					print*, "What area do you want to be from?"
					print*, "Type in 'Change' if you want to change your realm."
					read (*,'(A)') player_area ! Actually pick your region.
					player_area = fixformat(player_area)
					if (player_area .EQ. 'Change') then
						print*, "Changing realm."
						player_citizenship = 'The Empire'
						exit
					! Foyer
					else if (player_area .EQ. 'Province of Foyer') then
						player_citizenship = 'The Highlands'
						player_area = 'Foyer'
						region_chosen = .true. ! Exits region_chosen do loop
						citizenship_cannot_change = .true. ! Exits overall encompasing do loop
					else if (player_area .EQ. 'Foyer') then
						player_citizenship = 'The Highlands'
						player_area = 'Foyer'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Vility
					else if (player_area .EQ. 'Province of Vility') then
						player_citizenship = 'The Highlands'
						player_area = 'Vility'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Vility') then
						player_citizenship = 'The Highlands'
						player_area = 'Vility'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Zarata /Zerixa
					else if (player_area .EQ. 'Province of Zarata') then
						player_citizenship = 'The Highlands'
						player_area = 'Zarata'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Zarata') then
						player_citizenship = 'The Highlands'
						player_area = 'Zarata'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Zerixa') then
						player_citizenship = 'The Highlands'
						player_area = 'Zarata'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Narena
					else if (player_area .EQ. 'Governorate of Narena') then
						player_citizenship = 'The Highlands'
						player_area = 'Narena'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Narena') then
						player_citizenship = 'The Highlands'
						player_area = 'Narena'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Centa
					else if (player_area .EQ. 'Governorate of Centa') then
						player_citizenship = 'The Highlands'
						player_area = 'Centa'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Centa') then
						player_citizenship = 'The Highlands'
						player_area = 'Centa'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Betiera
					else if (player_area .EQ. 'Governorate of Betiera') then
						player_citizenship = 'The Highlands'
						player_area = 'Betiera'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Betiera') then
						player_citizenship = 'The Highlands'
						player_area = 'Betiera'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Pallon
					else if (player_area .EQ. 'Governorate of Pallon') then
						player_citizenship = 'The Highlands'
						player_area = 'Pallon'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Pallon') then
						player_citizenship = 'The Highlands'
						player_area = 'Pallon'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Ostorn
					else if (player_area .EQ. 'Ostorn Governorate') then
						player_citizenship = 'The Highlands'
						player_area = 'Ostorn'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Governorate of Ostorn') then
						player_citizenship = 'The Highlands'
						player_area = 'Ostorn'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Ostorn') then
						player_citizenship = 'The Highlands'
						player_area = 'Ostorn'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Lifus
					else if (player_area .EQ. 'Lifus Governorate') then
						player_citizenship = 'The Highlands'
						player_area = 'Lifus'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Governorate of Lifus') then
						player_citizenship = 'The Highlands'
						player_area = 'Lifus'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Lifus') then
						player_citizenship = 'The Highlands'
						player_area = 'Lifus'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Sonal
					else if (player_area .EQ. 'Governorate of Sonal') then
						player_citizenship = 'The Highlands'
						player_area = 'Sonal'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Sonal') then
						player_citizenship = 'The Highlands'
						player_area = 'Sonal'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Deseret
					else if (player_area .EQ. 'Governorate of Deseret') then
						player_citizenship = 'The Highlands'
						player_area = 'Deseret'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Deseret') then
						player_citizenship = 'The Highlands'
						player_area = 'Deseret'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Anglia
					else if (player_area .EQ. 'Governorate of Anglia') then
						player_citizenship = 'The Highlands'
						player_area = 'Anglia'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Anglia') then
						player_citizenship = 'The Highlands'
						player_area = 'Anglia'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Halfon
					else if (player_area .EQ. 'Governorate of Halfon') then
						player_citizenship = 'The Highlands'
						player_area = 'Halfon'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Halfon') then
						player_citizenship = 'The Highlands'
						player_area = 'Halfon'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Era Free Trade
					else if (player_area .EQ. 'Era Free Trade State') then
						player_citizenship = 'The Highlands'
						player_area = 'Era'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Era') then
						player_citizenship = 'The Highlands'
						player_area = 'Era'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Era Free Trade') then
						player_citizenship = 'The Highlands'
						player_area = 'Era'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'EFT') then
						player_citizenship = 'The Highlands'
						player_area = 'Era'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Spirit Archipelago. Secret region to pick to be from!
					else if (player_area .EQ. 'Spirit Archipelago') then
						player_citizenship = 'The Highlands'
						player_area = 'Spirit Islands' ! note the name change
						region_chosen = .true.
						citizenship_cannot_change = .true.
					!! End region descriptions
					else
						print*, "Try again."
					end if	
				end do
				region_chosen = .false.
			!! End the Section about the Highlands
			!! Beginning about the Empire
			else if (player_citizenship .EQ. 'The Empire') then
				print*, "Province of Imperia"
				print*, "Province of Valle"
				print*, "Province of Petral"
				print*, "Province of Aurora Nova"
				print*, "Province of Coulon"
				print*, "Province of Enrenan"
				print*, "Province of Hiten"
				print*, "Protectorate of Kathay"
				print*, "Thurop Authoritarian State"
				print*, "Governorate of Entrana"
				print*, "Governorate of Ralaer"
				print*, "Governorate of Hanor"
				print*, "Governorate of Fetedal"
				print*, "Governorate of Nerhast"
				print*, "Governorate of Cstphon"
				print*, "Governorate of Arya"
				print*, "Governorate of Tiblus"
				print*, "Governorate of Sophos"
				print*, "Governorate of Alges"
				print*, "Governorate of Anoch"
				print*, "Governorate of Edessa"
				print*, "Governorate of Rihde"
				print*, "Governorate of Inden"
				print*, "Governorate of Siniasus"
				print*, "Governorate of Mokvon"
				print*, "Governorate of Vanas"
				print*, "Do you want to know about any of the areas? Yes or No?"
				do while (area_info_wanted .EQV. .false.)
					read (*,*) i_want_area_info ! Response to the above question
					i_want_area_info = fixformat(i_want_area_info)
					if (i_want_area_info .EQ. 'Yes') then
						print*, "Okay."
						area_info_wanted = .true.
					else if (i_want_area_info .EQ. 'No') then
						print*, "Okay."
						area_info_wanted = .false.
						exit
					else
						print*, "Yes or No please."
					end if
				end do		
				do while (area_info_wanted .EQV. .true.) ! asking about the areas in the Empire
					print*, "Which area would you like to learn about? Type it in its long name or short name to see information about it."
					print*, "Type 'Exit' to stop learning."
					print*, "Type 'Regions' to see the list of regions again."
					read (*,'(A)') area_info_request ! Answer to question above
					area_info_request = fixformat(area_info_request)
					if (area_info_request .EQ. 'Exit') then
						print*, "Moving on then." ! No more info
						area_info_wanted = .false.
					else if (area_info_request .EQ. 'Regions') then
						! List of regions
						print*, "Province of Imperia"
						print*, "Province of Valle"
						print*, "Province of Petral"
						print*, "Province of Aurora Nova"
						print*, "Province of Coulon"
						print*, "Province of Enrenan"
						print*, "Province of Hiten"
						print*, "Protectorate of Kathay"
						print*, "Thurop Authoritarian State"
						print*, "Governorate of Entrana"
						print*, "Governorate of Ralaer"
						print*, "Governorate of Hanor"
						print*, "Governorate of Fetedal"
						print*, "Governorate of Nerhast"
						print*, "Governorate of Cstphon"
						print*, "Governorate of Arya"
						print*, "Governorate of Tiblus"
						print*, "Governorate of Sophos"
						print*, "Governorate of Alges"
						print*, "Governorate of Anoch"
						print*, "Governorate of Edessa"
						print*, "Governorate of Rihde"
						print*, "Governorate of Inden"
						print*, "Governorate of Siniasus"
						print*, "Governorate of Mokvon"
						print*, "Governorate of Vanas"
					!! Begin Region descriptions (includes nicknames)
					! Imperia
					else if (area_info_request .EQ. 'Province of Imperia') then
						print*, "The Province of Imperia is the most important area in ",&
						"the whole Empire; the home of the Imperial capital, and where the most heavily ",&
						"industrialized and developed portions of the Empire lay, it is an absolute ",&
						"economic and military powerhouse. There is no doubt that even as the Empire ",&
						"declines that this region will carry the legacy of it."
						print*, "Capital: The City of the Eternal Warlord (aka: The City) ",&
						"(pop: 84,221,000)"
						print*, "Population (est): 409,851,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 373,841,000"
						print*, "   Convent Auroran - 874,000"
						print*, "   Settler - 33,611,000"
						print*, "   Highlander - 1,171,850"
						print*, "   Auroran - 158,723"
						print*, "   Orieni - 194,281"
					else if (area_info_request .EQ. 'Imperia') then
						print*, "The Province of Imperia is the most important area in ",&
						"the whole Empire; the home of the Imperial capital, and where the most heavily ",&
						"industrialized and developed portions of the Empire lay, it is an absolute ",&
						"economic and military powerhouse. There is no doubt that even as the Empire ",&
						"declines that this region will carry the legacy of it."
						print*, "Capital: The City of the Eternal Warlord (aka: The City) ",&
						"(pop: 84,221,000)"
						print*, "Population (est): 409,851,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 373,841,000"
						print*, "   Convent Auroran - 874,000"
						print*, "   Settler - 33,611,000"
						print*, "   Highlander - 1,171,850"
						print*, "   Auroran - 158,723"
						print*, "   Orieni - 194,281"
					else if (area_info_request .EQ. 'Center State') then
						print*, "The Province of Imperia is the most important area in ",&
						"the whole Empire; the home of the Imperial capital, and where the most heavily ",&
						"industrialized and developed portions of the Empire lay, it is an absolute ",&
						"economic and military powerhouse. There is no doubt that even as the Empire ",&
						"declines that this region will carry the legacy of it."
						print*, "Capital: The City of the Eternal Warlord (aka: The City) ",&
						"(pop: 84,221,000)"
						print*, "Population (est): 409,851,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 373,841,000"
						print*, "   Convent Auroran - 874,000"
						print*, "   Settler - 33,611,000"
						print*, "   Highlander - 1,171,850"
						print*, "   Auroran - 158,723"
						print*, "   Orieni - 194,281"
					else if (area_info_request .EQ. 'Imperial Center State') then
						print*, "The Province of Imperia is the most important area in ",&
						"the whole Empire; the home of the Imperial capital, and where the most heavily ",&
						"industrialized and developed portions of the Empire lay, it is an absolute ",&
						"economic and military powerhouse. There is no doubt that even as the Empire ",&
						"declines that this region will carry the legacy of it."
						print*, "Capital: The City of the Eternal Warlord (aka: The City) ",&
						"(pop: 84,221,000)"
						print*, "Population (set): 409,851,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 373,841,000"
						print*, "   Convent Auroran - 874,000"
						print*, "   Settler - 33,611,000"
						print*, "   Highlander - 1,171,850"
						print*, "   Auroran - 158,723"
						print*, "   Orieni - 194,281"
					! Valle
					else if (area_info_request .EQ. 'Province of Valle') then
						print*, "The Province of Valle is a small province located just to the ",&
						"south of Imperia. Home to a significant agriculture industry, most inhabitants ",&
						"are ethnically Imperial. In particular, this province has seen great ",&
						"benefit from technological advanced in the Center State, and has allowed ",&
						"it to generate significant economy activity through increased crop yields."
						print*, "Capital: Foyer of Valle (pop: 4,814,000)"
						print*, "Population (est): 31,098,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 30,129,000"
						print*, "   Convent Auroran - 213,000"
						print*, "   Settler - 756,000"
					else if (area_info_request .EQ. 'Valle') then
						print*, "The Province of Valle is a small province located just to the ",&
						"south of Imperia. Home to a significant agriculture industry, most inhabitants ",&
						"are ethnically Imperial. In particular, this province has seen great ",&
						"benefit from technological advanced in the Center State, and has allowed ",&
						"it to generate significant economy activity through increased crop yields."
						print*, "Capital: Foyer of Valle (pop: 4,814,000)"
						print*, "Population (est): 31,098,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 30,129,000"
						print*, "   Convent Auroran - 213,000"
						print*, "   Settler - 756,000"
					! Petral
					else if (area_info_request .EQ. 'Province of Petral') then
						print*, "The Province of Petral is located to the northeast of Imperia. ",&
						"Noted for its wealth of resources, it was the primary source of raw materials ",&
						"used to industrialize the Center State."
						print*, "Capital: Yurum (pop: 6,880,000)"
						print*, "Population (est): 39,725,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 38,245,000"
						print*, "   Convent Auroran - 213,000"
						print*, "   Settler - 1,267,000"
					else if (area_info_request .EQ. 'Petral') then
						print*, "The Province of Petral is located to the northeast of Imperia. ",&
						"Noted for its wealth of resources, it was the primary source of raw materials ",&
						"used to industrialize the Center State."
						print*, "Capital: Yurum (pop: 6,880,000)"
						print*, "Population (est): 39,725,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 38,245,000"
						print*, "   Convent Auroran - 213,000"
						print*, "   Settler - 1,267,000"
					! Aurora Nova
					else if (area_info_request .EQ. 'Province of Aurora Nova') then
						print*, "The Province of Aurora Nova is located to the west of the Center State. ",&
						"Despite its name, the area is nearly devoid of Convent Aurorans, as is typical ",&
						"for the Northern Empire. It relies heavily on its heavily developed industrial ",&
						"base. As is standard for a province, it is small, but punches well above the ",&
						"governorates in terms of economic activity; Aurora Nova is among the richest regions ",&
						"in the Empire."
						print*, "Capital: New Aurora (pop: 8,539,000)"
						print*, "Population (est): 35,382,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 31,921,000"
						print*, "   Convent Auroran - 54,000"
						print*, "   Settler - 3,407,000"
					else if (area_info_request .EQ. 'Aurora Nova') then
						print*, "The Province of Aurora Nova is located to the west of the Center State. ",&
						"Despite its name, the area is nearly devoid of Convent Aurorans, as is typical ",&
						"for the Northern Empire. It relies heavily on its heavily developed industrial ",&
						"base. As is standard for a province, it is small, but punches well above the ",&
						"governorates in terms of economic activity; Aurora Nova is among the richest regions ",&
						"in the Empire."
						print*, "Capital: New Aurora (pop: 8,539,000)"
						print*, "Population (est): 35,382,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 31,921,000"
						print*, "   Convent Auroran - 54,000"
						print*, "   Settler - 3,407,000"
					else if (area_info_request .EQ. 'New Aurora') then
						print*, "The Province of Aurora Nova is located to the west of the Center State. ",&
						"Despite its name, the area is nearly devoid of Convent Aurorans, as is typical ",&
						"for the Northern Empire. It relies heavily on its heavily developed industrial ",&
						"base. As is standard for a province, it is small, but punches well above the ",&
						"governorates in terms of economic activity; Aurora Nova is among the richest regions ",&
						"in the Empire."
						print*, "Capital: New Aurora (pop: 8,539,000)"
						print*, "Population (est): 35,382,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 31,921,000"
						print*, "   Convent Auroran - 54,000"
						print*, "   Settler - 3,407,000"
					else if (area_info_request .EQ. 'Imperial Aurora') then
						print*, "The Province of Aurora Nova is located to the west of the Center State. ",&
						"Despite its name, the area is nearly devoid of Convent Aurorans, as is typical ",&
						"for the Northern Empire. It relies heavily on its heavily developed industrial ",&
						"base. As is standard for a province, it is small, but punches well above the ",&
						"governorates in terms of economic activity; Aurora Nova is among the richest regions ",&
						"in the Empire."
						print*, "Capital: New Aurora (pop: 8,539,000)"
						print*, "Population (est): 35,382,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 31,921,000"
						print*, "   Convent Auroran - 54,000"
						print*, "   Settler - 3,407,000"
					! Coulon
					else if (area_info_request .EQ. 'Province of Coulon') then
						print*, "The Province of Coulon is coastal, and well known for its ",&
						"deep water port that processes the heavy trade between the north ",&
						"and south portions of the Empire. Other than ports, Coulon offers well-endowed ",&
						"fishing waters alongside the industry to process fish."
						print*, "Capital: Farsan (pop: 7,311,000)"
						print*, "Population (est): 44,207,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 42,419,000"
						print*, "   Convent Auroran - 357,000"
						print*, "   Settler - 1,431,000"
					else if (area_info_request .EQ. 'Coulon') then
						print*, "The Province of Coulon is coastal, and well known for its ",&
						"deep water port that processes the heavy trade between the north ",&
						"and south portions of the Empire. Other than ports, Coulon offers well-endowed ",&
						"fishing waters alongside the industry to process fish."
						print*, "Capital: Farsan (pop: 7,311,000)"
						print*, "Population (est): 44,207,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 42,419,000"
						print*, "   Convent Auroran - 357,000"
						print*, "   Settler - 1,431,000"
					! Enrenan
					else if (area_info_request .EQ. 'Province of Enrenan') then
						print*, "The Province of Enrenan is an agricultural province of ",&
						"predominantly Imperial origins. In particular, its economy is dominated ",&
						"by agricultural exports to the southern Empire. Noble families in this province ",&
						"dominate the market for meat in the southern Empire."
						print*, "Capital: Vaigorn (pop: 4,133,000)"
						print*, "Population (est): 29,261,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 28,123,000"
						print*, "   Convent Auroran - 188,000"
						print*, "   Settler - 950,000"
					else if (area_info_request .EQ. 'Enrenan') then
						print*, "The Province of Enrenan is an agricultural province of ",&
						"predominantly Imperial origins. In particular, its economy is dominated ",&
						"by agricultural exports to the southern Empire. Noble families in this province ",&
						"dominate the market for meat in the southern Empire."
						print*, "Capital: Vaigorn (pop: 4,133,000)"
						print*, "Population (est): 29,261,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 28,123,000"
						print*, "   Convent Auroran - 188,000"
						print*, "   Settler - 950,000"
					! Hiten
					else if (area_info_request .EQ. 'Province of Hiten') then
						print*, "The Province of Hiten is largely industrial; it ",&
						"specializes in processing chemicals and other dangerous materials. ",&
						"Its value is unspeakable to the military capabilities of the Empire, ",&
						"as it produces a notable amount of the explosives that keep the Empire armed."
						print*, "Capital: Ceris (pop: 1,909,000)"
						print*, "Population (est): 27,491,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 22,631,000"
						print*, "   Convent Auroran - 211,000"
						print*, "   Settler - 4,649,000"
					else if (area_info_request .EQ. 'Hiten') then
						print*, "The Province of Hiten is largely industrial; it ",&
						"specializes in processing chemicals and other dangerous materials. ",&
						"Its value is unspeakable to the military capabilities of the Empire, ",&
						"as it produces a notable amount of the explosives that keep the Empire armed."
						print*, "Capital: Ceris (pop: 1,909,000)"
						print*, "Population (est): 27,491,000"
						print*, "By ethnicity:"
						print*, "   Imperial - 22,631,000"
						print*, "   Convent Auroran - 211,000"
						print*, "   Settler - 4,649,000"
					! Kathay
					else if (area_info_request .EQ. 'Protectorate of Kathay') then
						print*, "The Protectorate of Kathay is an Autonomy in the Empire. ",&
						"Founded hardly twenty years ago, this inland holding is in the process ",&
						"of transforming from a rural, poor area to one of the most heavily industrialized ",&
						"regions of the Continent. Previously home to notable populations of Imperials ",&
						"and Settlers, which have since been expelled in the aftermath of a rebellion, Kathay has ",&
						"developed a powerful, distinct ethnic identity. It remains a constant source of anti-Imperial ",&
						"rhetoric and a constant troublemaker in internal Imperial affairs. The capital of Aurea is ",&
						"no doubt a modern city that is among the largest in the Empire, but the rest of the Protectorate ",&
						"sorely lags behind, remaining mostly untamed wilderness populated by villagers."
						print*, "Note: the following census data is prewar. The demographics of Kathay have ",&
						"changed significantly; there are almost no Imperials or Settlers left."
						print*, "Capital: Aurea (pop: 33,328,000)"
						print*, "Population (est): 305,432,000"
						print*, "By ethnicity:"
						print*, "   Kathaic - 258,376,000"
						print*, "   Imperial - 20,452,000"
						print*, "   Convent Auroran - 8,193,000"
						print*, "   Settler - 18,411,000"
					else if (area_info_request .EQ. 'Governorate of Kathay') then
						print*, "The Protectorate of Kathay is an Autonomy in the Empire. ",&
						"Founded hardly twenty years ago, this inland holding is in the process ",&
						"of transforming from a rural, poor area to one of the most heavily industrialized ",&
						"regions of the Continent. Previously home to notable populations of Imperials ",&
						"and Settlers, which have since been expelled in the aftermath of a rebellion, Kathay has ",&
						"developed a powerful, distinct ethnic identity. It remains a constant source of anti-Imperial ",&
						"rhetoric and a constant troublemaker in internal Imperial affairs. The capital of Aurea is ",&
						"no doubt a modern city that is among the largest in the Empire, but the rest of the Protectorate ",&
						"sorely lags behind, remaining mostly untamed wilderness populated by villagers."
						print*, "Note: the following census data is prewar. The demographics of Kathay have ",&
						"changed significantly; there are almost no Imperials or Settlers left."
						print*, "Capital: Aurea (pop: 33,328,000)"
						print*, "Population (est): 305,432,000"
						print*, "By ethnicity:"
						print*, "   Kathaic - 258,376,000"
						print*, "   Imperial - 20,452,000"
						print*, "   Convent Auroran - 8,193,000"
						print*, "   Settler - 18,411,000"
					else if (area_info_request .EQ. 'Imperial Kathay') then
						print*, "The Protectorate of Kathay is an Autonomy in the Empire. ",&
						"Founded hardly twenty years ago, this inland holding is in the process ",&
						"of transforming from a rural, poor area to one of the most heavily industrialized ",&
						"regions of the Continent. Previously home to notable populations of Imperials ",&
						"and Settlers, which have since been expelled in the aftermath of a rebellion, Kathay has ",&
						"developed a powerful, distinct ethnic identity. It remains a constant source of anti-Imperial ",&
						"rhetoric and a constant troublemaker in internal Imperial affairs. The capital of Aurea is ",&
						"no doubt a modern city that is among the largest in the Empire, but the rest of the Protectorate ",&
						"sorely lags behind, remaining mostly untamed wilderness populated by villagers."
						print*, "Note: the following census data is prewar. The demographics of Kathay have ",&
						"changed significantly; there are almost no Imperials or Settlers left."
						print*, "Capital: Aurea (pop: 33,328,000)"
						print*, "Population (est): 305,432,000"
						print*, "By ethnicity:"
						print*, "   Kathaic - 258,376,000"
						print*, "   Imperial - 20,452,000"
						print*, "   Convent Auroran - 8,193,000"
						print*, "   Settler - 18,411,000"
					else if (area_info_request .EQ. 'Kathay') then
						print*, "The Protectorate of Kathay is an Autonomy in the Empire. ",&
						"Founded hardly twenty years ago, this inland holding is in the process ",&
						"of transforming from a rural, poor area to one of the most heavily industrialized ",&
						"regions of the Continent. Previously home to notable populations of Imperials ",&
						"and Settlers, which have since been expelled in the aftermath of a rebellion, Kathay has ",&
						"developed a powerful, distinct ethnic identity. It remains a constant source of anti-Imperial ",&
						"rhetoric and a constant troublemaker in internal Imperial affairs. The capital of Aurea is ",&
						"no doubt a modern city that is among the largest in the Empire, but the rest of the Protectorate ",&
						"sorely lags behind, remaining mostly untamed wilderness populated by villagers."
						print*, "Note: the following census data is prewar. The demographics of Kathay have ",&
						"changed significantly; there are almost no Imperials or Settlers left."
						print*, "Capital: Aurea (pop: 33,328,000)"
						print*, "Population (est): 305,432,000"
						print*, "By ethnicity:"
						print*, "   Kathaic - 258,376,000"
						print*, "   Imperial - 20,452,000"
						print*, "   Convent Auroran - 8,193,000"
						print*, "   Settler - 18,411,000"
					! Thurop
					else if (area_info_request .EQ. 'Thurop Authoritarian State') then
						print*, "The Thurop Authoritarian State is an Autonomy of the Empire. ",&
						"Founded around twenty years ago, in the aftermath of a rebellion, the state is headed ",&
						"up by a naval dictatorship that is a nominal vassal of the Empire, despite ",&
						"constant violations of the terms of autonomy. Thurop is well-developed and ",&
						"urbanized, unlike many governorates, being home to the second largest city in the ",&
						"Empire. Prosperous, with heavily developed industry and services, Thurop is already ",&
						"one of the most powerful areas of the Empire, home to both a powerful indigenous group ",&
						"and a large Imperial population."
						print*, "Capital: Caleoda (pop: 42,178,000)"
						print*, "Population (est): 278,309,000"
						print*, "By ethnicity:"
						print*, "   Thurop - 215,291,000"
						print*, "   Imperial - 41,937,000"
						print*, "   Convent Auroran - 2,504,000"
						print*, "   Settler - 18,577,000"
					else if (area_info_request .EQ. 'Governorate of Thurop') then
						print*, "The Thurop Authoritarian State is an Autonomy of the Empire. ",&
						"Founded around twenty years ago, in the aftermath of a rebellion, the state is headed ",&
						"up by a naval dictatorship that is a nominal vassal of the Empire, despite ",&
						"constant violations of the terms of autonomy. Thurop is well-developed and ",&
						"urbanized, unlike many governorates, being home to the second largest city in the ",&
						"Empire. Prosperous, with heavily developed industry and services, Thurop is already ",&
						"one of the most powerful areas of the Empire, home to both a powerful indigenous group ",&
						"and a large Imperial population."
						print*, "Capital: Caleoda (pop: 42,178,000)"
						print*, "Population (est): 278,309,000"
						print*, "By ethnicity:"
						print*, "   Thurop - 215,291,000"
						print*, "   Imperial - 41,937,000"
						print*, "   Convent Auroran - 2,504,000"
						print*, "   Settler - 18,577,000"
					else if (area_info_request .EQ. 'Thurop') then
						print*, "The Thurop Authoritarian State is an Autonomy of the Empire. ",&
						"Founded around twenty years ago, in the aftermath of a rebellion, the state is headed ",&
						"up by a naval dictatorship that is a nominal vassal of the Empire, despite ",&
						"constant violations of the terms of autonomy. Thurop is well-developed and ",&
						"urbanized, unlike many governorates, being home to the second largest city in the ",&
						"Empire. Prosperous, with heavily developed industry and services, Thurop is already ",&
						"one of the most powerful areas of the Empire, home to both a powerful indigenous group ",&
						"and a large Imperial population."
						print*, "Capital: Caleoda (pop: 42,178,000)"
						print*, "Population (est): 278,309,000"
						print*, "By ethnicity:"
						print*, "   Thurop - 215,291,000"
						print*, "   Imperial - 41,937,000"
						print*, "   Convent Auroran - 2,504,000"
						print*, "   Settler - 18,577,000"
					! Entrana
					else if (area_info_request .EQ. 'Governorate of Entrana') then
						print*, "The Governorate of Entrana is a costal state. While not ",&
						"particularly well-developed, it is home to a sizable fishing industry ",&
						"that feeds many landlocked governorates. It also holds a sizable navy, the second ",&
						"largest in the Empire, although this takes up a significant portion of the ",&
						"local budget. Entrana maintains a one-sided rivalry with its neighbor Thurop."
						print*, "Capital: Livonus (pop: 13,017,000)"
						print*, "Population (est): 107,356,000"
						print*, "By ethnicity:"
						print*, "   Entranan - 81,059,000"
						print*, "   Imperial - 16,311,000"
						print*, "   Convent Auroran - 1,983,000"
						print*, "   Settler - 8,003,000"
					else if (area_info_request .EQ. 'Entrana') then
						print*, "The Governorate of Entrana is a costal state. While not ",&
						"particularly well-developed, it is home to a sizable fishing industry ",&
						"that feeds many landlocked governorates. It also holds a sizable navy, the second ",&
						"largest in the Empire, although this takes up a significant portion of the ",&
						"local budget. Entrana maintains a one-sided rivalry with its neighbor Thurop."
						print*, "Capital: Livonus (pop: 13,017,000)"
						print*, "Population (est): 107,356,000"
						print*, "By ethnicity:"
						print*, "   Entranan - 81,059,000"
						print*, "   Imperial - 16,311,000"
						print*, "   Convent Auroran - 1,983,000"
						print*, "   Settler - 8,003,000"
					! Ralaer
					else if (area_info_request .EQ. 'Governorate of Ralaer') then
						print*, "The Governorate of Ralaer is a notable dichotomy. While it is mostly ",&
						"agricultural, it is home to a prime Imperial city known for its strong legal tradition ",&
						"and history of philosophy. The differences can cause problems between the Ralois, ",&
						"though the Imperial nobility can largely enforce peace in the governorate."
						print*, "Capital: Novglaitier (pop: 9,492,000)"
						print*, "Population (est): 89,236,000"
						print*, "By ethnicity:"
						print*, "   Ralois - 72,431,000"
						print*, "   Imperial - 8,621,000"
						print*, "   Convent Auroran - 1,731,000"
						print*, "   Settler - 6,453,000"
					else if (area_info_request .EQ. 'Ralaer') then
						print*, "The Governorate of Ralaer is a notable dichotomy. While it is mostly ",&
						"agricultural, it is home to a prime Imperial city known for its strong legal tradition ",&
						"and history of philosophy. The differences can cause problems between the Ralois, ",&
						"though the Imperial nobility can largely enforce peace in the governorate."
						print*, "Capital: Novglaitier (pop: 9,492,000)"
						print*, "Population (est): 89,236,000"
						print*, "By ethnicity:"
						print*, "   Ralois - 72,431,000"
						print*, "   Imperial - 8,621,000"
						print*, "   Convent Auroran - 1,731,000"
						print*, "   Settler - 6,453,000"
					! Hanor
					else if (area_info_request .EQ. 'Governorate of Hanor') then
						print*, "The Governorate of Hanor is mostly used for its military ",&
						"capabilities. While the land is mostly harsh and not usable for ",&
						"large-scale agriculture or resource extraction, its access to the Orieni ",&
						"Lake allows it to facilitate trade between the Empire and Oriena."
						print*, "Capital: Equis (pop: 3,973,000)"
						print*, "Population (est): 52,249,000"
						print*, "By ethnicity:"
						print*, "   Hanoir - 47,083,000"
						print*, "   Imperial - 3,558,000"
						print*, "   Convent Auroran - 512,000"
						print*, "   Settler - 1,096,000"
					else if (area_info_request .EQ. 'Hanor') then
						print*, "The Governorate of Hanor is mostly used for its military ",&
						"capabilities. While the land is mostly harsh and not usable for ",&
						"large-scale agriculture or resource extraction, its access to the Orieni ",&
						"Lake allows it to facilitate trade between the Empire and Oriena."
						print*, "Capital: Equis (pop: 3,973,000)"
						print*, "Population (est): 52,249,000"
						print*, "By ethnicity:"
						print*, "   Hanoir - 47,083,000"
						print*, "   Imperial - 3,558,000"
						print*, "   Convent Auroran - 512,000"
						print*, "   Settler - 1,096,000"
					! Fetedal
					else if (area_info_request .EQ. 'Governorate of Fetedal') then
						print*, "The Governorate of Fetedal is quite isolated and is located ",&
						"in a harsh, volcanic environment. Despite this, it is notably well populated, and ",&
						"is even under the authority of a constitutional theocracy, although the local ",&
						"Imperial power has always predominated. Fetedal continually exports the ",&
						"volcanic ash used to create the concrete that is used to build the infrastructure ",&
						"of the Empire. There has been a recent resurgence in the movement to restore ",&
						"Farar, the spirit of the land, to absolutism."
						print*, "Capital: Psonus (pre-conquest: Farar) (pop: 25,093,000)"
						print*, "Population (est): 145,131,000"
						print*, "By ethnicity:"
						print*, "   Fetan - 125,911,000"
						print*, "   Imperial - 14,636,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Settler - 4,584,000"
					else if (area_info_request .EQ. 'Fetedal') then
						print*, "The Governorate of Fetedal is quite isolated and is located ",&
						"in a harsh, volcanic environment. Despite this, it is notably well populated, and ",&
						"is even under the authority of a constitutional theocracy, although the local ",&
						"Imperial power has always predominated. Fetedal continually exports the ",&
						"volcanic ash used to create the concrete that is used to build the infrastructure ",&
						"of the Empire. There has been a recent resurgence in the movement to restore ",&
						"Farar, the spirit of the land, to absolutism."
						print*, "Capital: Psonus (pre-conquest: Farar) (pop: 25,093,000)"
						print*, "Population (est): 145,131,000"
						print*, "By ethnicity:"
						print*, "   Fetan - 125,911,000"
						print*, "   Imperial - 14,636,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Settler - 4,584,000"
					else if (area_info_request .EQ. 'Imperial Fetedal') then
						print*, "The Governorate of Fetedal is quite isolated and is located ",&
						"in a harsh, volcanic environment. Despite this, it is notably well populated, and ",&
						"is even under the authority of a constitutional theocracy, although the local ",&
						"Imperial power has always predominated. Fetedal continually exports the ",&
						"volcanic ash used to create the concrete that is used to build the infrastructure ",&
						"of the Empire. There has been a recent resurgence in the movement to restore ",&
						"Farar, the spirit of the land, to absolutism."
						print*, "Capital: Psonus (pre-conquest: Farar) (pop: 25,093,000)"
						print*, "Population (est): 145,131,000"
						print*, "By ethnicity:"
						print*, "   Fetan - 125,911,000"
						print*, "   Imperial - 14,636,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Settler - 4,584,000"
					else if (area_info_request .EQ. 'Faran Theocracy') then
						print*, "The Faran Theocracy is quite isolated and is located ",&
						"in a harsh, volcanic environment. Notably well populated, and ",&
						"headed up by Farar, the Essence of Flame, an ancient spirit. ",&
						"Imperial power has always predominated his rule since the conquest. ",&
						"Fetedal continually exports the volcanic ash used to create the concrete ",&
						"that is used to build the infrastructure of the Empire. There has been a ",&
						"recent resurgence in the movement to restore Farar to absolutism."
						print*, "Capital: City of Farar (pop: 25,093,000)"
						print*, "Population (est): 145,131,000"
						print*, "By ethnicity:"
						print*, "   Fetan - 125,911,000"
						print*, "   Imperial - 14,636,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Settler - 4,584,000"
					! Nerhast
					else if (area_info_request .EQ. 'Governorate of Nerhast') then
						print*, "The Governorate of Nerhast is largely agricultural, and serves ",&
						"as a major food exporter to the many hungry subjects of the Empire. ",&
						"The money produced by this is largely funneled in to the Imperial nobility, leaving ",&
						"most of the land's population poor and destitute. The many Nerhest are often seen ",&
						"as unintelligent and backwards due to their willingness to continue being ",&
						"exploited by the nobility. Regardless, the successful Convent Auroran ",&
						"population is heavily urbanized and concentrated in the capital of Hall."
						print*, "Capital: Hall (pop: 6,909,000)"
						print*, "Population (est): 105,897,000"
						print*, "By ethnicity:"
						print*, "   Nerhest - 98,046,000"
						print*, "   Imperial - 2,732,000"
						print*, "   Convent Auroran - 4,725,000"
						print*, "   Settler - 394,000"
					else if (area_info_request .EQ. 'Nerhast') then
						print*, "The Governorate of Nerhast is largely agricultural, and serves ",&
						"as a major food exporter to the many hungry subjects of the Empire. ",&
						"The money produced by this is largely funneled in to the Imperial nobility, leaving ",&
						"most of the land's population poor and destitute. The many Nerhest are often seen ",&
						"as unintelligent and backwards due to their willingness to continue being ",&
						"exploited by the nobility. Regardless, the successful Convent Auroran ",&
						"population is heavily urbanized and concentrated in the capital of Hall."
						print*, "Capital: Hall (pop: 6,909,000)"
						print*, "Population (est): 105,897,000"
						print*, "By ethnicity:"
						print*, "   Nerhest - 98,046,000"
						print*, "   Imperial - 2,732,000"
						print*, "   Convent Auroran - 4,725,000"
						print*, "   Settler - 394,000"
					! Cstphon
					else if (area_info_request .EQ. 'Governorate of Cstphon') then
						print*, "The Governorate of Cstphon is well known for its militarism, where ",&
						"the majority ethnic group: the Cstphene, are a culturally militant people ",&
						"who serve both locally and all across the Empire. Cstphon is heavily urbanized, ",&
						"and its capital is a great Imperial city that has housed a great deal of conquerers ",&
						"before the conquest by the Empire. Today, it stands as a notably diverse city."
						print*, "Capital: City of Cstphon (pop: 30,768,000)"
						print*, "Population (est): 109,473,000"
						print*, "By ethnicity:"
						print*, "   Cstphene - 76,081,000"
						print*, "   Imperial - 16,934,000"
						print*, "   Convent Auroran - 7,847,000"
						print*, "   Settler - 8,611,000"
					else if (area_info_request .EQ. 'Cstphon') then
						print*, "The Governorate of Cstphon is well known for its militarism, where ",&
						"the majority ethnic group: the Cstphene, are a culturally militant people ",&
						"who serve both locally and all across the Empire. Cstphon is heavily urbanized, ",&
						"and its capital is a great Imperial city that has housed a great deal of conquerers ",&
						"before the conquest by the Empire. Today, it stands as a notably diverse city."
						print*, "Capital: City of Cstphon (pop: 30,768,000)"
						print*, "Population (est): 109,473,000"
						print*, "By ethnicity:"
						print*, "   Cstphene - 76,081,000"
						print*, "   Imperial - 16,934,000"
						print*, "   Convent Auroran - 7,847,000"
						print*, "   Settler - 8,611,000"
					! Arya
					else if (area_info_request .EQ. 'Governorate of Arya') then
						print*, "The Governorate of Arya is noted for its heavy presence in the desert, ",&
						"indeed it is almost completely encapsulated by desert. It is the largest ",&
						"salt supplier in the Empire, but remains heavily populated by only recently ",&
						"settled tribesmen. The local Aryans still manage to maintain vague tribal structures ",&
						"and traditions despite their modern language and religious beliefs."
						print*, "Capital: Arya Nova (pop: 2,945,000)"
						print*, "Population (est): 54,861,000"
						print*, "By ethnicity:"
						print*, "   Aryan - 51,349,000"
						print*, "   Imperial - 2,346,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Settler - 1,116,000"
					else if (area_info_request .EQ. 'Arya') then
						print*, "The Governorate of Arya is noted for its heavy presence in the desert, ",&
						"indeed it is almost completely encapsulated by desert. It is the largest ",&
						"salt supplier in the Empire, but remains heavily populated by only recently ",&
						"settled tribesmen. The local Aryans still manage to maintain vague tribal structures ",&
						"and traditions despite their modern language and religious beliefs."
						print*, "Capital: Arya Nova (pop: 2,945,000)"
						print*, "Population (est): 54,861,000"
						print*, "By ethnicity:"
						print*, "   Aryan - 51,349,000"
						print*, "   Imperial - 2,346,000"
						print*, "   Convent Auroran - <1000"
						print*, "   Settler - 1,116,000"
					! Tiblus
					else if (area_info_request .EQ. 'Governorate of Tiblus') then
						print*, "The Governorate of Tiblus is a hilly region that borders the Highlands on the ",&
						"south. Its primary economic activity is mining, where the land is home to a variety ",&
						"of rare materials that are sourced from mines set deep in the mountains. ",&
						"Home to a culture of mountainous people, direct Imperial authority largely remains ",&
						"absent outside of the valleys. Every few years, the local nobility goes out to reinforce ",&
						"their control of the hills. The Convent Auroran population has concentrated itself in ",&
						"the capital, Cobal, in response to persecution by the local Tiblan in the aftermath of the ",&
						"Imperial conquest."
						print*, "Capital: Cobal (pop: 3,745,000)"
						print*, "Population (est): 63,120,000"
						print*, "By ethnicity:"
						print*, "   Tiblan - 57,302,000"
						print*, "   Imperial - 3,644,000"
						print*, "   Convent Auroran - 1,591,000"
						print*, "   Settler - 583,000"
					else if (area_info_request .EQ. 'Tiblus') then
						print*, "The Governorate of Tiblus is a hilly region that borders the Highlands on the ",&
						"south. Its primary economic activity is mining, where the land is home to a variety ",&
						"of rare materials that are sourced from mines set deep in the mountains. ",&
						"Home to a culture of mountainous people, direct Imperial authority largely remains ",&
						"abset outside of the valleys. Every few years, the local nobility goes out to reinforce ",&
						"their control of the hills. The Convent Auroran population has concentrated itself in ",&
						"the capital, Cobal, in response to persecution by the local Tiblan in the aftermath of the ",&
						"Imperial conquest."
						print*, "Capital: Cobal (pop: 3,745,000)"
						print*, "Population (est): 63,120,000"
						print*, "By ethnicity:"
						print*, "   Tiblan - 57,302,000"
						print*, "   Imperial - 3,644,000"
						print*, "   Convent Auroran - 1,591,000"
						print*, "   Settler - 583,000"
					! Sophos
					else if (area_info_request .EQ. 'Governorate of Sophos') then
						print*, "The Governorate of Sophos is a rocky, coastal state. It constrasts ",&
						"starkly with the flat coastal plains of Thurop and Entrana. Sophos is mostly ",&
						"unsuitable for naval activities beyond small-scale fishing. It remains an economic ",&
						"burden, and is an endless sink for Imperial tax money, and has the only benefit ",&
						"of being peaceful."
						print*, "Capital: Atenos (pop: 4,850,000)"
						print*, "Population (est): 38,642,000"
						print*, "By ethnicity:"
						print*, "   Sophene - 35,368,000"
						print*, "   Imperial - 1,247,000"
						print*, "   Convent Auroran - 1,599,000"
						print*, "   Settler - 428,000"
					else if (area_info_request .EQ. 'Sophos') then
						print*, "The Governorate of Sophos is a rocky, coastal state. It constrasts ",&
						"starkly with the flat coastal plains of Thurop and Entrana. Sophos is mostly ",&
						"unsuitable for naval activities beyond small-scale fishing. It remains an economic ",&
						"burden, and is an endless sink for Imperial tax money, and has the only benefit ",&
						"of being peaceful."
						print*, "Capital: Atenos (pop: 4,850,000)"
						print*, "Population (est): 38,642,000"
						print*, "By ethnicity:"
						print*, "   Sophene - 35,368,000"
						print*, "   Imperial - 1,247,000"
						print*, "   Convent Auroran - 1,599,000"
						print*, "   Settler - 428,000"
					! Alges
					else if (area_info_request .EQ. 'Governorate of Alges') then
						print*, "The Governorate of Alges is the most heavily forested area ",&
						"in the Empire. Located in the far north of the Empire, bordering Aurora, it ",&
						"is quite large, though also poor and sparsely populated. Its largest economic ",&
						"contribution is in logging, which brings in poor people from all around the Empire."
						print*, "Capital: Redwood (pop: 2,491,000)"
						print*, "Population (est): 38,642,000"
						print*, "By ethnicity:"
						print*, "   Algus - 19,256,000"
						print*, "   Imperial - 6,102,000"
						print*, "   Convent Auroran - 3,113,000"
						print*, "   Settler - 4,472,000"
					else if (area_info_request .EQ. 'Alges') then
						print*, "The Governorate of Alges is the most heavily forested area ",&
						"in the Empire. Located in the far north of the Empire, bordering Aurora, it ",&
						"is quite large, though also poor and sparsely populated. Its largest economic ",&
						"contribution is in logging, which brings in poor people from all around the Empire."
						print*, "Capital: Redwood (pop: 2,491,000)"
						print*, "Population (est): 38,642,000"
						print*, "By ethnicity:"
						print*, "   Algus - 19,256,000"
						print*, "   Imperial - 6,102,000"
						print*, "   Convent Auroran - 3,113,000"
						print*, "   Settler - 4,472,000"
					! Anoch
					else if (area_info_request .EQ. 'Governorate of Anoch') then
						print*, "The Governorate of Anoch remains hopelessly underdeveloped. ",&
						"With extremely low urbanization, and even low literacy and high rates ",&
						"of disease, it serves as an unfortunate and unintentional glimpse into ",&
						"the past. Its native population is largely unmixed, and many of their red-haired ",&
						"women marry men from elsewhere in the Empire and send remittances back home, which ",&
						"serves as the largest source of taxes collected in payment."
						print*, "Capital: Mercy (pop: 6,087,000)"
						print*, "Population (est): 91,861,000"
						print*, "By ethnicity:"
						print*, "   Anoch - 85,032,000"
						print*, "   Imperial - 2,585,000"
						print*, "   Convent Auroran - 3,674,000"
						print*, "   Settler - 4,472,000"
					else if (area_info_request .EQ. 'Anoch') then
						print*, "The Governorate of Anoch remains hopelessly underdeveloped. ",&
						"With extremely low urbanization, and even low literacy and high rates ",&
						"of disease, it serves as an unfortunate and unintentional glimpse into ",&
						"the past. Its native population is largely unmixed, and many of their red-haired ",&
						"women marry men from elsewhere in the Empire and send remittances back home, which ",&
						"serves as the largest source of taxes collected in payment."
						print*, "Capital: Mercy (pop: 6,087,000)"
						print*, "Population (est): 91,861,000"
						print*, "By ethnicity:"
						print*, "   Anoch - 85,032,000"
						print*, "   Imperial - 2,585,000"
						print*, "   Convent Auroran - 3,674,000"
						print*, "   Settler - 4,472,000"
					! Edessa
					else if (area_info_request .EQ. 'Governorate of Edessa') then
						print*, "The Governorate of Edessa is small, quiet, stable, and of average ",&
						"means. While mostly rural, it produces a small surplus in agriculture which ",&
						"allows it to tax the outgoing food and keep the government afloat."
						print*, "Capital: City of Edessa (pop: 4,952,000)"
						print*, "Population (est): 63,752,000"
						print*, "By ethnicity:"
						print*, "   Dessan - 56,476,000"
						print*, "   Imperial - 2,448,000"
						print*, "   Convent Auroran - 4,192,000"
						print*, "   Settler - 636,000"
					else if (area_info_request .EQ. 'Edessa') then
						print*, "The Governorate of Edessa is small, quiet, stable, and of average ",&
						"means. While mostly rural, it produces a small surplus in agriculture which ",&
						"allows it to tax the outgoing food and keep the government afloat."
						print*, "Capital: City of Edessa (pop: 4,952,000)"
						print*, "Population (est): 63,752,000"
						print*, "By ethnicity:"
						print*, "   Dessan - 56,476,000"
						print*, "   Imperial - 2,448,000"
						print*, "   Convent Auroran - 4,192,000"
						print*, "   Settler - 636,000"
					! Rihde
					else if (area_info_request .EQ. 'Governorate of Rihde') then
						print*, "The Governorate of Rihde is a flat region in the south of the ",&
						"Continent. Home to vast steppelands, many of its past inhabitants were nomadic. ",&
						"Nowadays, Rihde maintains a small grazing industry while most of the population is ",&
						"involved is subsistence farming. Periodic famines sweep the area and cause tensions to ",&
						"flare between the locals and the Imperial and Convent Auroran populations. ",&
						"There was controversy over its choice of capital at Asuw, which was the center of Rihden ",&
						"power at the time of conquest, and was an important religious city for them. In particular, ",&
						"the Convent Auroran population, based out of Aurora on the Steppe, has maintained that the ",&
						"capital should be relocated to their center of power."
						print*, "Capital: Asuw (pop: 4,776,000)"
						print*, "Population (est): 112,288,000"
						print*, "By ethnicity:"
						print*, "   Rihden - 101,744,000"
						print*, "   Imperial - 5,305,000"
						print*, "   Convent Auroran - 3,892,000"
						print*, "   Settler - 1,346,000"
					else if (area_info_request .EQ. 'Rihde') then
						print*, "The Governorate of Rihde is a flat region in the south of the ",&
						"Continent. Home to vast steppelands, many of its past inhabitants were nomadic. ",&
						"Nowadays, Rihde maintains a small grazing industry while most of the population is ",&
						"involved is subsistence farming. Periodic famines sweep the area and cause tensions to ",&
						"flare between the locals and the Imperial and Convent Auroran populations. ",&
						"There was controversy over its choice of capital at Asuw, which was the center of Rihden ",&
						"power at the time of conquest, and was an important religious city for them. In particular, ",&
						"the Convent Auroran population, based out of Aurora on the Steppe, has maintained that the ",&
						"the capital should be relocated to their center of power."
						print*, "Capital: Asuw (pop: 4,776,000)"
						print*, "Population (est): 112,288,000"
						print*, "By ethnicity:"
						print*, "   Rihden - 101,744,000"
						print*, "   Imperial - 5,305,000"
						print*, "   Convent Auroran - 3,892,000"
						print*, "   Settler - 1,346,000"
					! Inden
					else if (area_info_request .EQ. 'Governorate of Inden') then
						print*, "The Governorate of Inden is noted for its wealth of lakes. The lakes ",&
						"themselves are hardly of any economic value given the rural nature of the governorate, ",&
						"but it is thought that with sufficient techology that the lakes can be used to sate the ",&
						"thirst of more developed governorates in the southern Empire. Regardless, most of the ",&
						"population are subsistence farmers."
						print*, "Capital: The City of the Great Lake (known to Convent Aurorans as ",&
						"Aurora on the Great Lake) (pop: 10,873,000)"
						print*, "Population (est): 72,868,000"
						print*, "By ethnicity:"
						print*, "   Inden - 63,559,000"
						print*, "   Imperial - 2,237,000"
						print*, "   Convent Auroran - 6,213,000"
						print*, "   Settler - 859,000"
					else if (area_info_request .EQ. 'Inden') then
						print*, "The Governorate of Inden is noted for its wealth of lakes. The lakes ",&
						"themselves are hardly of any economic value given the rural nature of the governorate, ",&
						"but it is thought that with sufficient techology that the lakes can be used to sate the ",&
						"thirst of more developed governorates in the southern Empire. Regardless, most of the ",&
						"population are subsistence farmers."
						print*, "Capital: The City of the Great Lake (known to Convent Aurorans as ",&
						"Aurora on the Great Lake) (pop: 10,873,000)"
						print*, "Population (est): 72,868,000"
						print*, "By ethnicity:"
						print*, "   Inden - 63,559,000"
						print*, "   Imperial - 2,237,000"
						print*, "   Convent Auroran - 6,213,000"
						print*, "   Settler - 859,000"
					! Siniasus
					else if (area_info_request .EQ. 'Governorate of Siniasus') then
						print*, "The Governorate of Siniasus is a coastal province. Located in the ",&
						"southern Empire, while the coast is calm and clear, it remains underdeveloped. ",&
						"It is believed that the previous religion of the Sinias people forbid the ",&
						"consumption of fish, as this quirk remains in the local people today. ",&
						"Fishing and exploiting the sea for resources remains effectively banned ",&
						"in Siniasus to prevent outrage and social unrest from the local population."
						print*, "Capital: Peken (pop: 4,866,000)"
						print*, "Population (est): 63,879,000"
						print*, "By ethnicity:"
						print*, "   Sinias - 60,000,000"
						print*, "   Imperial - 2,731,000"
						print*, "   Convent Auroran - 610,000"
						print*, "   Settler - 538,000"
					else if (area_info_request .EQ. 'Siniasus') then
						print*, "The Governorate of Siniasus is a coastal province. Located in the ",&
						"southern Empire, while the coast is calm and clear, it remains underdeveloped. ",&
						"It is believed that the previous religion of the Sinias people forbid the ",&
						"consumption of fish, as this quirk remains in the local people today. ",&
						"Fishing and exploiting the sea for resources remains effectively banned ",&
						"in Siniasus to prevent outrage and social unrest from the local population."
						print*, "Capital: Peken (pop: 4,866,000)"
						print*, "Population (est): 63,879,000"
						print*, "By ethnicity:"
						print*, "   Sinias - 60,000,000"
						print*, "   Imperial - 2,731,000"
						print*, "   Convent Auroran - 610,000"
						print*, "   Settler - 538,000"
					! Mokvon
					else if (area_info_request .EQ. 'Governorate of Mokvon') then
						print*, "The Governorate of Mokvon is flat, but far too cold to be useful ",&
						"for agriculture. It ussed to be covered in great forests, but they were cut ",&
						"down in ancient times, and what remains is a barren landscape. Sparsely populated ",&
						"and unable to support many people, many Mokven leave for elsewhere in the Empire."
						print*, "Capital: Fort Amman (pop: 281,000)"
						print*, "Population (est): 14,868,000"
						print*, "By ethnicity:"
						print*, "   Mokven - 8,112,000"
						print*, "   Imperial - 4,753,000"
						print*, "   Convent Auroran - 1,640,000"
						print*, "   Settler - 363,000"
					else if (area_info_request .EQ. 'Mokvon') then
						print*, "The Governorate of Mokvon is flat, but far too cold to be useful ",&
						"for agriculture. It ussed to be covered in great forests, but they were cut ",&
						"down in ancient times, and what remains is a barren landscape. Sparsely populated ",&
						"and unable to support many people, many Mokven leave for elsewhere in the Empire."
						print*, "Capital: Fort Amman (pop: 281,000)"
						print*, "Population (est): 14,868,000"
						print*, "By ethnicity:"
						print*, "   Mokven - 8,112,000"
						print*, "   Imperial - 4,753,000"
						print*, "   Convent Auroran - 1,640,000"
						print*, "   Settler - 363,000"
					! Vanas
					else if (area_info_request .EQ. 'Governorate of Vanas') then
						print*, "The Governorate of Vanas is poorly developed and home to ",&
						"a small urban population. A large portion of its rural population lives under ",&
						"the feudal system, working lands owned by Imperial nobility. Its border on Oriena ",&
						"means that there is a notbale Imperial military presence along with a small Orieni ",&
						"population. Indeed, the capital of Orosc used to be a center of Orieni power, while the ",&
						"Convent Auroran population retain their power base in Aurora of the West."
						print*, "Capital: Orosc (pop: 2,756,000)"
						print*, "Population (est): 83,424,000"
						print*, "By ethnicity:"
						print*, "   Vanois - 74,615,000"
						print*, "   Imperial - 3,108,000"
						print*, "   Convent Auroran - 4,342,000"
						print*, "   Settler - 1,149,000"
						print*, "   Orieni - 210,000"
					else if (area_info_request .EQ. 'Vanas') then
						print*, "The Governorate of Vanas is poorly developed and home to ",&
						"a small urban population. A large portion of its rural population lives under ",&
						"the feudal system, working lands owned by Imperial nobility. Its border on Oriena ",&
						"means that there is a notbale Imperial military presence along with a small Orieni ",&
						"population. Indeed, the capital of Orosc used to be a center of Orieni power, while the ",&
						"Convent Auroran population retain their power base in Aurora of the West."
						print*, "Capital: Orosc (pop: 2,756,000)"
						print*, "Population (est): 83,424,000"
						print*, "By ethnicity:"
						print*, "   Vanois - 74,615,000"
						print*, "   Imperial - 3,108,000"
						print*, "   Convent Auroran - 4,342,000"
						print*, "   Settler - 1,149,000"
						print*, "   Orieni - 210,000"
					!! End Region Descriptions
					else
						print*, "Try again."
					end if		
				end do
				print*, "What area do you want to be from?"
				print*, "Type in 'Change' if you want to change your realm."
				do while (region_chosen .EQV. .false.) ! Makes sure that the region input is correct
					read (*,'(A)') player_area ! Actually pick your region.
					player_area = fixformat(player_area)
					if (player_area .EQ. 'Change') then
						print*, "Changing realm."
						player_citizenship = 'The Highlands'
						exit
					! Center State
					else if (player_area .EQ. 'Province of Imperia') then
						player_citizenship = 'The Empire'
						player_area = 'Imperia'
						region_chosen = .true. ! Exits region_chosen do loop
						citizenship_cannot_change = .true. ! Exits overall encompasing do loop
					else if (player_area .EQ. 'Imperia') then
						player_citizenship = 'The Empire'
						player_area = 'Imperia'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Center State') then
						player_citizenship = 'The Empire'
						player_area = 'Imperia'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Imperial Center State') then
						player_citizenship = 'The Empire'
						player_area = 'Imperia'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Valle
					else if (player_area .EQ. 'Province of Valle') then
						player_citizenship = 'The Empire'
						player_area = 'Valle'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Valle') then
						player_citizenship = 'The Empire'
						player_area = 'Valle'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Petral
					else if (player_area .EQ. 'Province of Petral') then
						player_citizenship = 'The Empire'
						player_area = 'Petral'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Petral') then
						player_citizenship = 'The Empire'
						player_area = 'Petral'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Aurora Nova
					else if (player_area .EQ. 'Province of Aurora Nova') then
						player_citizenship = 'The Empire'
						player_area = 'Aurora Nova'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Aurora Nova') then
						player_citizenship = 'The Empire'
						player_area = 'Aurora Nova'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'New Aurora') then
						player_citizenship = 'The Empire'
						player_area = 'Aurora Nova'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Imperial Aurora') then
						player_citizenship = 'The Empire'
						player_area = 'Aurora Nova'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Coulon
					else if (player_area .EQ. 'Province of Coulon') then
						player_citizenship = 'The Empire'
						player_area = 'Coulon'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Coulon') then
						player_citizenship = 'The Empire'
						player_area = 'Coulon'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Enrenan
					else if (player_area .EQ. 'Province of Enrenan') then
						player_citizenship = 'The Empire'
						player_area = 'Enrenan'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Enrenan') then
						player_citizenship = 'The Empire'
						player_area = 'Enrenan'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Hiten
					else if (player_area .EQ. 'Province of Hiten') then
						player_citizenship = 'The Empire'
						player_area = 'Hiten'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Hiten') then
						player_citizenship = 'The Empire'
						player_area = 'Hiten'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Kathay
					else if (player_area .EQ. 'Protectorate of Kathay') then
						player_citizenship = 'The Empire'
						player_area = 'Kathay'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Governorate of Kathay') then
						player_citizenship = 'The Empire'
						player_area = 'Kathay'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Imperial Kathay') then
						player_citizenship = 'The Empire'
						player_area = 'Kathay'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Kathay') then
						player_citizenship = 'The Empire'
						player_area = 'Kathay'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Thurop
					else if (player_area .EQ. 'Thurop Authoritarian State') then
						player_citizenship = 'The Empire'
						player_area = 'Thurop'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Governorate of Thurop') then
						player_citizenship = 'The Empire'
						player_area = 'Thurop'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Thurop') then
						player_citizenship = 'The Empire'
						player_area = 'Thurop'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Entrana
					else if (player_area .EQ. 'Governorate of Entrana') then
						player_citizenship = 'The Empire'
						player_area = 'Entrana'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Entrana') then
						player_citizenship = 'The Empire'
						player_area = 'Entrana'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Ralaer
					else if (player_area .EQ. 'Governorate of Ralaer') then
						player_citizenship = 'The Empire'
						player_area = 'Ralaer'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Ralaer') then
						player_citizenship = 'The Empire'
						player_area = 'Ralaer'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Hanor
					else if (player_area .EQ. 'Governorate of Hanor') then
						player_citizenship = 'The Empire'
						player_area = 'Hanor'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Hanor') then
						player_citizenship = 'The Empire'
						player_area = 'Hanor'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Fetedal
					else if (player_area .EQ. 'Governorate of Fetedal') then
						player_citizenship = 'The Empire'
						player_area = 'Fetedal'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Fetedal') then
						player_citizenship = 'The Empire'
						player_area = 'Fetedal'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Imperial Fetedal') then
						player_citizenship = 'The Empire'
						player_area = 'Fetedal'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Faran Theocracy') then
						player_citizenship = 'The Empire'
						player_area = 'Fetedal'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Nerhast
					else if (player_area .EQ. 'Governorate of Nerhast') then
						player_citizenship = 'The Empire'
						player_area = 'Nerhast'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Nerhast') then
						player_citizenship = 'The Empire'
						player_area = 'Nerhast'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Cstphon
					else if (player_area .EQ. 'Governorate of Cstphon') then
						player_citizenship = 'The Empire'
						player_area = 'Cstphon'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Cstphon') then
						player_citizenship = 'The Empire'
						player_area = 'Cstphon'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Arya
					else if (player_area .EQ. 'Governorate of Arya') then
						player_citizenship = 'The Empire'
						player_area = 'Arya'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Arya') then
						player_citizenship = 'The Empire'
						player_area = 'Arya'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Tiblus
					else if (player_area .EQ. 'Governorate of Tiblus') then
						player_citizenship = 'The Empire'
						player_area = 'Tiblus'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Tiblus') then
						player_citizenship = 'The Empire'
						player_area = 'Tiblus'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Sophos
					else if (player_area .EQ. 'Governorate of Sophos') then
						player_citizenship = 'The Empire'
						player_area = 'Sophos'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Sophos') then
						player_citizenship = 'The Empire'
						player_area = 'Sophos'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Alges
					else if (player_area .EQ. 'Governorate of Alges') then
						player_citizenship = 'The Empire'
						player_area = 'Alges'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Alges') then
						player_citizenship = 'The Empire'
						player_area = 'Alges'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Anoch
					else if (player_area .EQ. 'Governorate of Anoch') then
						player_citizenship = 'The Empire'
						player_area = 'Anoch'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Anoch') then
						player_citizenship = 'The Empire'
						player_area = 'Anoch'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Edessa
					else if (player_area .EQ. 'Governorate of Edessa') then
						player_citizenship = 'The Empire'
						player_area = 'Edessa'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Edessa') then
						player_citizenship = 'The Empire'
						player_area = 'Edessa'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Rihde
					else if (player_area .EQ. 'Governorate of Rihde') then
						player_citizenship = 'The Empire'
						player_area = 'Rihde'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Rihde') then
						player_citizenship = 'The Empire'
						player_area = 'Rihde'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Inden
					else if (player_area .EQ. 'Governorate of Inden') then
						player_citizenship = 'The Empire'
						player_area = 'Inden'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Inden') then
						player_citizenship = 'The Empire'
						player_area = 'Inden'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Siniasus
					else if (player_area .EQ. 'Governorate of Siniasus') then
						player_citizenship = 'The Empire'
						player_area = 'Siniasus'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Siniasus') then
						player_citizenship = 'The Empire'
						player_area = 'Siniasus'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Mokvon
					else if (player_area .EQ. 'Governorate of Mokvon') then
						player_citizenship = 'The Empire'
						player_area = 'Mokvon'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Mokvon') then
						player_citizenship = 'The Empire'
						player_area = 'Mokvon'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					! Vanas
					else if (player_area .EQ. 'Governorate of Vanas') then
						player_citizenship = 'The Empire'
						player_area = 'Vanas'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else if (player_area .EQ. 'Vanas') then
						player_citizenship = 'The Empire'
						player_area = 'Vanas'
						region_chosen = .true.
						citizenship_cannot_change = .true.
					else
						print*, "Try again."
					end if
				end do
				!! End the Section about the Empire
			else
				player_citizenship = 'The Highlands'
				player_area = 'Foyer'
				! Assigns citizenship automatically in case of exception being generated!
			end if
		end do
		!! Make sure that choice is correct. If not, you can rechoose, including all of the options.
		print*, "You are from ",trim(player_area)," in ",trim(player_citizenship),"."
		print*, "Is this correct? Yes or No?"
		print*, "If you want to change this, type No."
		do while (region_is_correct .EQV. .false.)
			read (*,*) my_region_is_correct 
			my_region_is_correct = fixformat(my_region_is_correct)
			if (my_region_is_correct .EQ. 'Yes') then
				print*, "Excellent."
				print*, "You are from ",trim(player_area),"."
				region_is_correct = .true.
				nation_is_confirmed = .true.
			else if (my_region_is_correct .EQ. 'No') then
				print*, "Alright. Let's try again."
				player_area = 'reset'
				citizenship_is_valid = .false.
				citizenship_cannot_change = .false.
				region_chosen = .false.
				exit
			else
				print*, "Try again."
			end if
		end do
	end do
	end subroutine location_select
end module location_selector
