program testquest
	implicit none
	!! Initial Quest Variables
	!Player race, background, and career variables
	character(len=20) :: player_race,player_citizenship,player_background,player_career,
	! If the player is an Imperial or Convent Auroran, it determines the type they are.
	character(len=20) :: imperial_type ! Either Provincial or Governorate
	logical :: is_convent_exile ! If true, the player is a Convent Auroran Exile
	! End Initial Quest Variables
	
	! values that determine the lastingness of the loops to determine statistics.
	character(len=5) :: race_info_response,realm_info_response,convent_exile_response
	logical :: race_value,race_info_value,nation_value,realm_info_value,background_value,career_value
	logical :: imperial_type_value,convent_type_value
	character(len=20) :: race_info_request ! The name of the race information is requested for.
	convent_type_value = .false.
	
	race_value = .false.
	nation_value = .false.
	background_value = .false.
	career_value = .false.
	
	
	print*, "Select a Race"
	
	! Race Selector
	do while(race_value .EQV. .false.)
		print*, "Pick from the following races:"
		print*, "Foyerian"
		print*, "Vitalian"
		print*, "Orieni Highlander"
		print*, "Convent Auroran"
		print*, "Imperial"
		print*, "Kathaic"
		print*, "Thurop"
		print*, "Cstphene"
		print*, "Entranan"
		print*, "Fetan"
		print*, "Ralois"
		print*, "Hanoir"
		print*, "Nerhest"
		print*, "Aryan"
		print*, "Tiblan"
		print*, "Sophene"
		print*, "Algus"
		print*, "Anoch"
		print*, "Dessan"
		print*, "Rihden"
		print*, "Inden"
		print*, "Sinias"
		print*, "Mokven"
		print*, "Vanois"
		print*, "Do you want to know any information about the races? Yes or No?"
		read (*,*) race_info_response
		if (race_info_response .EQ. 'Yes') then
			race_info_value = .true.
		else if (race_info_response .EQ. 'No') then
			race_info_value = .false.
		end if
		!! Race Information
		do while (race_info_value .EQV. .true.)
			print*, "Type a race name to see information about them."
			print*, "Type 'Races' to see the list of races again."
			print*, "Type 'Exit' to end the information session."
			read (*,'(A)') race_info_request
			
			if (race_info_request .EQ. 'Exit') then
				print*, "Moving on then."
				race_info_value = .false.
			else if (race_info_request .EQ. 'Races') then
				print*, "Race List:"
				print*, "Foyerian"
				print*, "Vitalian"
				print*, "Orieni Highlander"
				print*, "Convent Auroran"
				print*, "Imperial"
				print*, "Kathaic"
				print*, "Thurop"
				print*, "Cstphene"
				print*, "Entranan"
				print*, "Fetan"
				print*, "Ralois"
				print*, "Hanoir"
				print*, "Nerhest"
				print*, "Aryan"
				print*, "Tiblan"
				print*, "Sophene"
				print*, "Algus"
				print*, "Anoch"
				print*, "Dessan"
				print*, "Rihden"
				print*, "Inden"
				print*, "Sinias"
				print*, "Mokven"
				print*, "Vanois"
			else if (race_info_request .EQ. 'Foyerian') then
				print*, "A large grouping of the standard fair-skinned Highlanders. ",&
				"Found all across the Highlands in large amounts, with a notable diaspora scattered about the Empire. ",&
				"This is a proud, martial people that is well recognized for their stature, spiritualism and work ethic."			
			else if (race_info_request .EQ. 'Vitalian') then
				print*, "A major ethnic group in the Highlands ",&
				"notably with pale skin and hair that comes in blonde, black, and red ",&
				"aside from the standard brown. Vitalians can be found all across ",&
				"the Highlands, even in areas in which they are not the dominant ethnic group. ",&
				"They are very much a reflection of their Foyerian counterparts, sharing much in common."
			else if (race_info_request .EQ. 'Orieni Highlander') then
				print*, "The third traditional Highlander ethnic group, this race ",&
				"is particularly noted for their Orieni admixture. Despite this unique trait, ",&
				"they are grouped in with the general Highlander ethnicities and are not subject to ",&
				"discrimination or special treatment. Importantly, they have a wide rage of phenotypes."
			else if (race_info_request .EQ. 'Convent Auroran') then
				print*, "The Convent Auroran ethnic group is a closed-off, largely urban ",&
				"population that is ethnically Auroran. These pale-skinned, fair-haired people used ",&
				"to be found practically everywhere on the Continent. Nowadays, most live in the ",&
				"urban cities of the Southern Empire, where they dominate the capitals. ",&
				"Many assist in propping up the Imperial regime, as the Empire has allowed them to be ",&
				"disproportionally powerful. Many also serve in the military or become highly educated. ",&
				"Indeed, Convent Aurorans are often seen as particularly ambitious and are among the ",&
				"Richest and most educated races on the Continent."
			else if (race_info_request .EQ. 'Imperial') then
				print*, "The premier race of the Empire, this race heavily resembles ",&
				"that of other true Foyerian Auroran races, such as Foyerians and Vitalians. ",&
				"Imperials are often split between coming form the rich Provinces and the more humble Governorates.",&
				"Provincial Imperials are among the best educated and prestigous groups of the world ",&
				"while ones from the governorates tend to be in great positions of power and form the core ",&
				"of their local militaries."
			else if (race_info_request .EQ. 'Kathaic') then
				print*, "A broad classification for people from the Governorate of ",&
				"Kathay. Often looked down upon by their Convent Auroran and Imperial overlords, ",&
				"the Kathaic appear to be quite rugged, and many have claimed that they have a great ",&
				"martial potential. Regardless, the Kathaic now have major expectations, and are expected ",&
				"to become one of the major ethnic groups in the world. ",&
				"Most Kathaic are of average stature and have both brown hair and brown eyes."
			else if (race_info_request .EQ. 'Thurop') then
				print*, "Thurops are one of the few examples of an ethnic group in the Empire ",&
				"that manages to dominate affairs in its own governorate. Quite numerous, ",&
				"this coastal people maintains a powerful naval tradition in addition to being ",&
				"one of the most influential diaspora groups in the Empire."
			else if (race_info_request .EQ. 'Cstphene') then
				print*, "The people of Cstphon are proud of their martial backgrounds, and indeed, ",&
				"this race is often used for its military potential. They are another example of ",&
				"an ethnic group that rises above many others in the Empire. The Cstphene can ",&
				"often be ambitious and powerful, and those who do not take up military careers ",&
				"abroad often obtain formal education and serve governments otherwise."
			else if (race_info_request .EQ. 'Entranan') then
				print*, "A supposed close relative of Thurops, Entranans come from quite a similar ",&
				"environment to the Thurops. Entranans do have the common conception of being less ",&
				"civilized and more servile. Indeed, Entrana has been looked down upon for ",&
				"centuries while under the Imperial yoke, and its people are often seen as ",&
				"inferior Thurops."
			else if (race_info_request .EQ. 'Fetan') then
				print*, "The Fetan come from a harsh, volcanic environment with a long history of ",&
				"spiritualism and civilization. Indeed, the Imperial rule is seen as morally wrong to ",&
				"many Fetan, and they wish to see their constitutional theocracy restored to ",&
				"absolutism."
			else if (race_info_request .EQ. 'Ralois') then
				print*, "The Ralois are often split by their origins. Most live in rural, ",&
				"agricultural areas, but there is a notable amount who live in the capital of Ralaer: ",&
				"Novglatier, where the local Raloir maintain a strong tradition of legalism and ",&
				"philosophy."
			else if (race_info_request .EQ. 'Hanoir') then
				print*, "The Hanoir hail from a land that is all too used to military occupation. ",&
				"Their land is otherwise h arsh and not usable for large-scale farming or resource ",&
				"extraction. The Hanoir, however, remain close to the border with Oriena ",&
				"and thus there are a notable few who are well educated in Orieni topics. ",&
				"Otherise, many take up military careers elsewhere."
			else if (race_info_request .EQ. 'Nerhest') then
				print*, "The Nerhest are a largely poor, agrarian people. Largely seen as ",&
				"backwards, most that leave their governorate find work in the urban areas of ",&
				"the Empire, where they toil as unskilled laborers."
			else if (race_info_request .EQ. 'Aryan') then
				print*, "A desert people, composed of recently settled tribesmen, they still retain ",&
				"vague tribal structures, a stark contrast to the barriers that have been otherwise ",&
				"broken by Imperial colonization."
			else if (race_info_request .EQ. 'Tiblan') then
				print*, "A mountainous people that do their best to not be subjected to Imperial ",&
				"authority, this group does their best to keep isolated and alone from others. ",&
				"Very few leave their governorate, and those that do often have motives of ",&
				"furthering the cause of independence."
			else if (race_info_request .EQ. 'Sophene') then
				print*, "The Sophene are a coastal people, but unlike the Thurop or Entranans, ",&
				"the Sophene are used to jagged, rocky terrain on their coast. Indeed, ",&
				"this makes the Sophene notably backwards compared to others. Many move to the parts ",&
				"of the Empire to serve in the military."
			else if (race_info_request .EQ. 'Algus') then
				print*, "The Algus are seen as a forested people, where many live in isolated ",&
				"villages deep in the endless forests. This often brings them into conflict with ",&
				"their Imperial overlords, who look to exploit the material wealth of the area."
			else if (race_info_request .EQ. 'Anoch') then
				print*, "The vast majority of Anoch live in rural areas, and indeed, they are one ",&
				"of the least mixed populations on the continent. Unlike most other races, the Anoch ",&
				"have red hair as a common trait, and this curiosity causes many Anoch women ",&
				"to leave and find nobles to marry. The rural populations do their best to survive off ",&
				"of farming, surviving disease, and overcoming poor education."
			else if (race_info_request .EQ. 'Dessan') then
				print*, "Hardly notable, the Dessans are seen as quite average. Not troublesome, ",&
				"but hardly collaborators to the Empire. They remain mostly rural."
			else if (race_info_request .EQ. 'Rihden') then
				print*, "In the recent past, the Rihden were nomadic, and indeed, they have only ",&
				"recently settled. Now most are subsistence farmers, and there are often tensions ",&
				"between them and the local Imperials and Convent Aurorans."
			else if (race_info_request .EQ. 'Inden') then
				print*, "The Inden are adept at navigating the lake-filled governorate they ",&
				"live in. But otherwise, they are hardly notable, as they live in a rural area ",&
				"and very few leave to go elsewhere."
			else if (race_info_request .EQ. 'Sinias') then
				print*, "The last coastal people, the Sinias are only notable in that most refuse ",&
				"to eat fish, a holdover form the previous religion that occupied the area."
			else if (race_info_request .EQ. 'Mokven') then
				print*, "There are very few Mokven. Many leave their governorate to perform a ",&
				"wide variety of unskilled services in the Empire, including military service and labor."
			else if (race_info_request .EQ. 'Vanois') then
				print*, "The Vanois share a border with Oriena, and thus they are quite used to ",&
				"this strange ethnic group. Indeed, there is a small population of Vanois scholars who ",&
				"specialize in Orieni Studies."
			end if
		end do
		
		print*, "What is your race?"
		read (*,'(A)') player_race
		if (player_race .EQ. 'Foyerian') then
			race_value = .true.
		else if (player_race .EQ. 'Vitalian') then
			race_value = .true.
		else if (player_race .EQ. 'Orieni Highlander') then
			race_value = .true.
		else if (player_race .EQ. 'Convent Auroran') then
			do while (convent_type_value .EQV. .false.)
				print*, "Are you an Exile? Yes or No?"
				read (*,*) convent_exile_response
				if (convent_exile_response .EQ. 'Yes') then
					is_convent_exile = .true.
					convent_type_value = .true.
				else if (convent_exile_response .EQ. 'yes') then
					is_convent_exile = .true.
					convent_type_value = .true.
				else if (convent_exile_response .EQ. 'No') then
					is_convent_exile = .false.
					convent_type_value = .true.
				else if (convent_exile_response .EQ. 'no') then
					is_convent_exile = .false.
					convent_type_value = .true.
				end if
			end do
			race_value = .true.
		else if (player_race .EQ. 'Imperial') then
			race_value = .true.
		else if (player_race .EQ. 'Kathaic') then
			race_value = .true.
		else if (player_race .EQ. 'Thurop') then
			race_value = .true.
		else if (player_race .EQ. 'Cstphene') then
			race_value = .true.
		else if (player_race .EQ. 'Entranan') then
			race_value = .true.
		else if (player_race .EQ. 'Fetan') then
			race_value = .true.
		else if (player_race .EQ. 'Ralois') then
			race_value = .true.
		else if (player_race .EQ. 'Hanoir') then
			race_value = .true.
		else if (player_race .EQ. 'Nerhest') then
			race_value = .true.
		else if (player_race .EQ. 'Aryan') then
			race_value = .true.
		else if (player_race .EQ. 'Tiblan') then
			race_value = .true.
		else if (player_race .EQ. 'Sophene') then
			race_value = .true.
		else if (player_race .EQ. 'Algus') then
			race_value = .true.
		else if (player_race .EQ. 'Anoch') then
			race_value = .true.
		else if (player_race .EQ. 'Dessan') then
			race_value = .true.
		else if (player_race .EQ. 'Rihden') then
			race_value = .true.
		else if (player_race .EQ. 'Inden') then
			race_value = .true.
		else if (player_race .EQ. 'Sinias') then
			race_value = .true.
		else if (player_race .EQ. 'Mokven') then
			race_value = .true.
		else if (player_race .EQ. 'Vanois') then
			race_value = .true.
		else
			print*, "Invalid race. Pick from the given list."
		end if
	enddo
	
	! Location of Player Origin Selector
	print*, "Where are you from?"
	do while(nation_value .EQV. .false.)
		print*, "Pick from the following realms:"
		print*, "The Highlands"
		print*, "The Empire"
		print*, "Do you want to know any information about the realms? Yes or No?"
		read (*,*) realm_info_response
		if (realm_info_response .EQ. 'Yes') then
			realm_info_value = .true.
		else if (race_info_response .EQ. 'No') then
			realm_info_value = .false.
		end if
		!! Race Information
		do while (realm_info_value .EQV. .true.)
			print*, "Which realm do you want to know about?"
			print*, "Type 'Realms' to see the list of realms again."
			print*, "Type 'Exit' to end the information session."
			read (*,'(A)') realm_info_request
			
			if (realm_info_request .EQ. 'Exit') then
				print*, "Moving on then."
				realm_info_value = .false.
			else if (realm_info_request .EQ. 'Races') then
				print*, "Realm List:"
				print*, "The Highlands"
				print*, "The Empire"
			else if (realm_info_request .EQ. 'The  Highlands') then
				print*, "A realm headed by Foyer, the Commander of "&,
				"Weapons, an ancient spirit who ascended long ago. The core of ",&
				"the realm is in the west, where it is mostly mountainous, with dense urban pockets in the various isolated valleys. ",&
				"Home to more than just mountains, the Highlands is home to ",&
				"Plains, forests, and deserts across its territory."
				Print*, "The Highlands is the spiritual center of the Continent, where spirits ",*
				"from all across the known world commune and live under the theocracy headed by Foyer."
				Print*, "The Highlands is home to some 750 million people, and is well regarded as ",&
				"a highly militarized state of considerable power alongside its heavily industrialized ",&
				"economy and significant mineral wealth."			
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
			
		end do
		
		print*, "What is your race?"
		read (*,'(A)') player_race
		if (player_race .EQ. 'Foyerian') then
			race_value = .true.
		else if (player_race .EQ. 'Vitalian') then
			race_value = .true.
		else if (player_race .EQ. 'Orieni Highlander') then
			race_value = .true.
		else if (player_race .EQ. 'Convent Auroran') then
			do while (convent_type_value .EQV. .false.)
				print*, "Are you an Exile? Yes or No?"
				read (*,*) convent_exile_response
				if (convent_exile_response .EQ. 'Yes') then
					is_convent_exile = .true.
					convent_type_value = .true.
				else if (convent_exile_response .EQ. 'yes') then
					is_convent_exile = .true.
					convent_type_value = .true.
				else if (convent_exile_response .EQ. 'No') then
					is_convent_exile = .false.
					convent_type_value = .true.
				else if (convent_exile_response .EQ. 'no') then
					is_convent_exile = .false.
					convent_type_value = .true.
				end if
			end do
			race_value = .true.
		else if (player_race .EQ. 'Imperial') then
			race_value = .true.
		else if (player_race .EQ. 'Kathaic') then
			race_value = .true.
		else if (player_race .EQ. 'Thurop') then
			race_value = .true.
		else if (player_race .EQ. 'Cstphene') then
			race_value = .true.
		else if (player_race .EQ. 'Entranan') then
			race_value = .true.
		else if (player_race .EQ. 'Fetan') then
			race_value = .true.
		else if (player_race .EQ. 'Ralois') then
			race_value = .true.
		else if (player_race .EQ. 'Hanoir') then
			race_value = .true.
		else if (player_race .EQ. 'Nerhest') then
			race_value = .true.
		else if (player_race .EQ. 'Aryan') then
			race_value = .true.
		else if (player_race .EQ. 'Tiblan') then
			race_value = .true.
		else if (player_race .EQ. 'Sophene') then
			race_value = .true.
		else if (player_race .EQ. 'Algus') then
			race_value = .true.
		else if (player_race .EQ. 'Anoch') then
			race_value = .true.
		else if (player_race .EQ. 'Dessan') then
			race_value = .true.
		else if (player_race .EQ. 'Rihden') then
			race_value = .true.
		else if (player_race .EQ. 'Inden') then
			race_value = .true.
		else if (player_race .EQ. 'Sinias') then
			race_value = .true.
		else if (player_race .EQ. 'Mokven') then
			race_value = .true.
		else if (player_race .EQ. 'Vanois') then
			race_value = .true.
		else
			print*, "Invalid race. Pick from the given list."
		end if
	enddo
	
	! Career Selector
	do while(career_value .EQV. .false.)
		print*, "Pick from the following careers:"
		print*, "Uneducated"
		print*, "Farmer"
		print*, "Laborer"
		print*, "Factory Worker"
		print*, "Driver"
		print*, "Student"
		print*, "Graduate"
		print*, "Professor"
		print*, "Doctor"
		print*, "Engineer"
		print*, "Archaeologist"
		print*, "Historian"
		print*, "Merchant"
		print*, "Linguist"
		print*, "Police"
		print*, "Sailor"
		print*, "Airman"
		print*, "Infantry"
		print*, "Calvary"
		print*, "Ordinance"
		print*, "Medic"
		print*, "Sniper"
		print*, "Military Doctor"
		print*, "Officer"
		print*, "Covert Operations"
		if (player_nation)
		print*, "Cstphene Guardsman"
		print*, "Holy Servant"
		print*, "Holy Warrior"
		print*, "Holy Order Doctor"
		print*, "Do you want to know any information about the races? Yes or No?"
		read (*,*) race_info_response
		if (race_info_response .EQ. 'Yes') then
			race_info_value = .true.
		else if (race_info_response .EQ. 'No') then
			race_info_value = .false.
		end if
		!! Race Information
		do while (race_info_value .EQV. .true.)
			print*, "Type a race name to see information about them."
			print*, "Type 'Races' to see the list of races again."
			print*, "Type 'Exit' to end the information session."
			read (*,'(A)') race_info_request
			
			if (race_info_request .EQ. 'Exit') then
				print*, "Moving on then."
				race_info_value = .false.
			else if (race_info_request .EQ. 'Races') then
				print*, "Race List:"
				print*, "Foyerian"
				print*, "Vitalian"
				print*, "Orieni Highlander"
				print*, "Convent Auroran"
				print*, "Imperial"
				print*, "Kathaic"
				print*, "Thurop"
				print*, "Cstphene"
				print*, "Entranan"
				print*, "Fetan"
				print*, "Ralois"
				print*, "Hanoir"
				print*, "Nerhest"
				print*, "Aryan"
				print*, "Tiblan"
				print*, "Sophene"
				print*, "Algus"
				print*, "Anoch"
				print*, "Dessan"
				print*, "Rihden"
				print*, "Inden"
				print*, "Sinias"
				print*, "Mokven"
				print*, "Vanois"
			else if (race_info_request .EQ. 'Foyerian') then
				print*, "A large grouping of the standard fair-skinned Highlanders. ",&
				"Found all across the Highlands in large amounts, with a notable diaspora scattered about the Empire. ",&
				"This is a proud, martial people that is well recognized for their stature, spiritualism and work ethic."			
			else if (race_info_request .EQ. 'Vitalian') then
				print*, "A major ethnic group in the Highlands ",&
				"notably with pale skin and hair that comes in blonde, black, and red ",&
				"aside from the standard brown. Vitalians can be found all across ",&
				"the Highlands, even in areas in which they are not the dominant ethnic group. ",&
				"They are very much a reflection of their Foyerian counterparts, sharing much in common."
			else if (race_info_request .EQ. 'Orieni Highlander') then
				print*, "The third traditional Highlander ethnic group, this race ",&
				"is particularly noted for their Orieni admixture. Despite this unique trait, ",&
				"they are grouped in with the general Highlander ethnicities and are not subject to ",&
				"discrimination or special treatment. Importantly, they have a wide rage of phenotypes."
			else if (race_info_request .EQ. 'Convent Auroran') then
				print*, "The Convent Auroran ethnic group is a closed-off, largely urban ",&
				"population that is ethnically Auroran. These pale-skinned, fair-haired people used ",&
				"to be found practically everywhere on the Continent. Nowadays, most live in the ",&
				"urban cities of the Southern Empire, where they dominate the capitals. ",&
				"Many assist in propping up the Imperial regime, as the Empire has allowed them to be ",&
				"disproportionally powerful. Many also serve in the military or become highly educated. ",&
				"Indeed, Convent Aurorans are often seen as particularly ambitious and are among the ",&
				"Richest and most educated races on the Continent."
			else if (race_info_request .EQ. 'Imperial') then
				print*, "The premier race of the Empire, this race heavily resembles ",&
				"that of other true Foyerian Auroran races, such as Foyerians and Vitalians. ",&
				"Imperials are often split between coming form the rich Provinces and the more humble Governorates.",&
				"Provincial Imperials are among the best educated and prestigous groups of the world ",&
				"while ones from the governorates tend to be in great positions of power and form the core ",&
				"of their local militaries."
			else if (race_info_request .EQ. 'Kathaic') then
				print*, "A broad classification for people from the Governorate of ",&
				"Kathay. Often looked down upon by their Convent Auroran and Imperial overlords, ",&
				"the Kathaic appear to be quite rugged, and many have claimed that they have a great ",&
				"martial potential. Regardless, the Kathaic now have major expectations, and are expected ",&
				"to become one of the major ethnic groups in the world. ",&
				"Most Kathaic are of average stature and have both brown hair and brown eyes."
			else if (race_info_request .EQ. 'Thurop') then
				print*, "Thurops are one of the few examples of an ethnic group in the Empire ",&
				"that manages to dominate affairs in its own governorate. Quite numerous, ",&
				"this coastal people maintains a powerful naval tradition in addition to being ",&
				"one of the most influential diaspora groups in the Empire."
			else if (race_info_request .EQ. 'Cstphene') then
				print*, "The people of Cstphon are proud of their martial backgrounds, and indeed, ",&
				"this race is often used for its military potential. They are another example of ",&
				"an ethnic group that rises above many others in the Empire. The Cstphene can ",&
				"often be ambitious and powerful, and those who do not take up military careers ",&
				"abroad often obtain formal education and serve governments otherwise."
			else if (race_info_request .EQ. 'Entranan') then
				print*, "A supposed close relative of Thurops, Entranans come from quite a similar ",&
				"environment to the Thurops. Entranans do have the common conception of being less ",&
				"civilized and more servile. Indeed, Entrana has been looked down upon for ",&
				"centuries while under the Imperial yoke, and its people are often seen as ",&
				"inferior Thurops."
			else if (race_info_request .EQ. 'Fetan') then
				print*, "The Fetan come from a harsh, volcanic environment with a long history of ",&
				"spiritualism and civilization. Indeed, the Imperial rule is seen as morally wrong to ",&
				"many Fetan, and they wish to see their constitutional theocracy restored to ",&
				"absolutism."
			else if (race_info_request .EQ. 'Ralois') then
				print*, "The Ralois are often split by their origins. Most live in rural, ",&
				"agricultural areas, but there is a notable amount who live in the capital of Ralaer: ",&
				"Novglatier, where the local Raloir maintain a strong tradition of legalism and ",&
				"philosophy."
			else if (race_info_request .EQ. 'Hanoir') then
				print*, "The Hanoir hail from a land that is all too used to military occupation. ",&
				"Their land is otherwise h arsh and not usable for large-scale farming or resource ",&
				"extraction. The Hanoir, however, remain close to the border with Oriena ",&
				"and thus there are a notable few who are well educated in Orieni topics. ",&
				"Otherise, many take up military careers elsewhere."
			else if (race_info_request .EQ. 'Nerhest') then
				print*, "The Nerhest are a largely poor, agrarian people. Largely seen as ",&
				"backwards, most that leave their governorate find work in the urban areas of ",&
				"the Empire, where they toil as unskilled laborers."
			else if (race_info_request .EQ. 'Aryan') then
				print*, "A desert people, composed of recently settled tribesmen, they still retain ",&
				"vague tribal structures, a stark contrast to the barriers that have been otherwise ",&
				"broken by Imperial colonization."
			else if (race_info_request .EQ. 'Tiblan') then
				print*, "A mountainous people that do their best to not be subjected to Imperial ",&
				"authority, this group does their best to keep isolated and alone from others. ",&
				"Very few leave their governorate, and those that do often have motives of ",&
				"furthering the cause of independence."
			else if (race_info_request .EQ. 'Sophene') then
				print*, "The Sophene are a coastal people, but unlike the Thurop or Entranans, ",&
				"the Sophene are used to jagged, rocky terrain on their coast. Indeed, ",&
				"this makes the Sophene notably backwards compared to others. Many move to the parts ",&
				"of the Empire to serve in the military."
			else if (race_info_request .EQ. 'Algus') then
				print*, "The Algus are seen as a forested people, where many live in isolated ",&
				"villages deep in the endless forests. This often brings them into conflict with ",&
				"their Imperial overlords, who look to exploit the material wealth of the area."
			else if (race_info_request .EQ. 'Anoch') then
				print*, "The vast majority of Anoch live in rural areas, and indeed, they are one ",&
				"of the least mixed populations on the continent. Unlike most other races, the Anoch ",&
				"have red hair as a common trait, and this curiosity causes many Anoch women ",&
				"to leave and find nobles to marry. The rural populations do their best to survive off ",&
				"of farming, surviving disease, and overcoming poor education."
			else if (race_info_request .EQ. 'Dessan') then
				print*, "Hardly notable, the Dessans are seen as quite average. Not troublesome, ",&
				"but hardly collaborators to the Empire. They remain mostly rural."
			else if (race_info_request .EQ. 'Rihden') then
				print*, "In the recent past, the Rihden were nomadic, and indeed, they have only ",&
				"recently settled. Now most are subsistence farmers, and there are often tensions ",&
				"between them and the local Imperials and Convent Aurorans."
			else if (race_info_request .EQ. 'Inden') then
				print*, "The Inden are adept at navigating the lake-filled governorate they ",&
				"live in. But otherwise, they are hardly notable, as they live in a rural area ",&
				"and very few leave to go elsewhere."
			else if (race_info_request .EQ. 'Sinias') then
				print*, "The last coastal people, the Sinias are only notable in that most refuse ",&
				"to eat fish, a holdover form the previous religion that occupied the area."
			else if (race_info_request .EQ. 'Mokven') then
				print*, "There are very few Mokven. Many leave their governorate to perform a ",&
				"wide variety of unskilled services in the Empire, including military service and labor."
			else if (race_info_request .EQ. 'Vanois') then
				print*, "The Vanois share a border with Oriena, and thus they are quite used to ",&
				"this strange ethnic group. Indeed, there is a small population of Vanois scholars who ",&
				"specialize in Orieni Studies."
			end if
		end do
		
		print*, "What is your race?"
		read (*,'(A)') player_race
		if (player_race .EQ. 'Foyerian') then
			race_value = .true.
		else if (player_race .EQ. 'Vitalian') then
			race_value = .true.
		else if (player_race .EQ. 'Orieni Highlander') then
			race_value = .true.
		else if (player_race .EQ. 'Convent Auroran') then
			do while (convent_type_value .EQV. .false.)
				print*, "Are you an Exile? Yes or No?"
				read (*,*) convent_exile_response
				if (convent_exile_response .EQ. 'Yes') then
					is_convent_exile = .true.
					convent_type_value = .true.
				else if (convent_exile_response .EQ. 'yes') then
					is_convent_exile = .true.
					convent_type_value = .true.
				else if (convent_exile_response .EQ. 'No') then
					is_convent_exile = .false.
					convent_type_value = .true.
				else if (convent_exile_response .EQ. 'no') then
					is_convent_exile = .false.
					convent_type_value = .true.
				end if
			end do
			race_value = .true.
		else if (player_race .EQ. 'Imperial') then
			race_value = .true.
		else if (player_race .EQ. 'Kathaic') then
			race_value = .true.
		else if (player_race .EQ. 'Thurop') then
			race_value = .true.
		else if (player_race .EQ. 'Cstphene') then
			race_value = .true.
		else if (player_race .EQ. 'Entranan') then
			race_value = .true.
		else if (player_race .EQ. 'Fetan') then
			race_value = .true.
		else if (player_race .EQ. 'Ralois') then
			race_value = .true.
		else if (player_race .EQ. 'Hanoir') then
			race_value = .true.
		else if (player_race .EQ. 'Nerhest') then
			race_value = .true.
		else if (player_race .EQ. 'Aryan') then
			race_value = .true.
		else if (player_race .EQ. 'Tiblan') then
			race_value = .true.
		else if (player_race .EQ. 'Sophene') then
			race_value = .true.
		else if (player_race .EQ. 'Algus') then
			race_value = .true.
		else if (player_race .EQ. 'Anoch') then
			race_value = .true.
		else if (player_race .EQ. 'Dessan') then
			race_value = .true.
		else if (player_race .EQ. 'Rihden') then
			race_value = .true.
		else if (player_race .EQ. 'Inden') then
			race_value = .true.
		else if (player_race .EQ. 'Sinias') then
			race_value = .true.
		else if (player_race .EQ. 'Mokven') then
			race_value = .true.
		else if (player_race .EQ. 'Vanois') then
			race_value = .true.
		else
			print*, "Invalid race. Pick from the given list."
		end if
	enddo
	
	print*, "Select a Background"
	
	print*, "Select a Career"
	
	print*, "Quest: The Doctor from the City"
end program testquest
