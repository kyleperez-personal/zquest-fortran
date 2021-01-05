program introduction
	use proper_cap ! lets you use the fixformat function
	!! Dialogue modules
	use action_one ! Encounter with Enora, Anten, and Centom
	use celene_dialogue ! 20 Questions with Celene
	
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
	
	!! End Character parameter declarations
		
	!!!!
	!! Temporary dialogue variables
	character(len=6) :: do_i_want_to_talk ! do I want to initiate dialogue?
	character(len=20) :: i_want_to_talk_to ! for talking to Enora, Anten, or Centom
	logical :: no_more_dialogue
	logical :: enora_dialogue_done, anten_dialogue_done, centom_dialogue_done
		! Assignments
	no_more_dialogue = .false.
	
	enora_dialogue_done = .false.
	anten_dialogue_done = .false.
	centom_dialogue_done = .false.
	
	!! End temporary dialogue variables
	!!!!
	
	!! Temporary character atributes
	!! For Atana Marwel
	player_gender = "Female"
	player_is_male = .false.
	player_is_female = .true.
	
	player_age = 38
	
	player_race = "Convent Auroran"
	
	player_citizenship = "The Empire"
	player_area = "Thurop" ! only for testing; Atana is from Kathay
	
	player_holy_order = "None"
	player_is_in_holy_order = .false.
	
	player_career = "Professor"
	player_is_doctor = .false.
	player_is_military = .false.
	
	player_background = "Urban"
	player_imperial_class = "None"
	
	player_is_convent_auroran = .true.
	player_is_convent_exile = .false.
	
	player_is_diasporic = .false.
	player_is_settler = .false.
	player_is_nobility = .true. ! only true for testing; Atana is not a noble.
	
	player_is_cstphene_guardsman = .false.
	player_served_in_banact_war = .false.
	
	player_forename = "Atana Biene"
	player_surname = "Marwel"
	player_nobilary = "None"
	player_short_name = "Atana"
	player_short_last_name = "Marwel"
	player_nickname = "Atana"
	
	player_has_driver_license = .true.
	
	!! End temporary character atributes
	
	
	
	! Exposition 1
	print*, "You find yourself waiting in the lobby of one of the many government buildings in the brilliant ",&
	"City of the Eternal Warlord."
	if(player_is_military .EQV. .true.) then
		print*, "You were not given a choice in coming here. Your military superiors ordered you here and you ",&
		"dutifully obeyed."
	else
		print*, "You were a bit unsure of the invitation to come, but you were coaxed by promises of great ",&
		"wealth in such abundance that only a fool would not come, especially with the upfront bonus provided."
	end if
	
	print*, "You don't know much about what exactly is going on other than whatever it is, it is certainly important, but ",&
	"you have no idea of the magnitude of importance."
	
	print*, "The building you are in is of a great shining white, with the blue sky pouring in through the windows, with a ",&
	"few white clouds visible. Indeed, it truly does seem like a great day. A few green potted plants decorate the ",&
	"quite clean and pure area."
	print*, "You were told to bring a few things with you, of note clothes and other personal times, packed in bags or suitcases. ",&
	"You have them ready, and seem to be ready to leave this area at any time. There is most certainly some suspicion into what ",&
	"is going on. Regardless, you are waiting to be called into an office to discuss the matters of business, and indeed to ",&
	"find out exactly what is going on."
	
	print*, "Besides you, there are three others present."
	print*, "There is Enora, the secretary manning the desk. She is a blonde-haired, ",&
	"blue-eyed Imperial, with a white blouse and black pants, and she looks quite professional. ",&
	"In particular, she exudes an aurea of professionalism, respect, and pleasantness."
	
	print*, "There is Anten, a military guard who is stationed here, an Imperial with brown hair and brown eyes. He seems ",&
	"quite gruff, and unapproachable. He has a rifle in his arms and a pistol on his side, though he does not look ",&
	"particularly hostile, and he seems focused on checking surroundings constantly."
	
	print*, "There is Centom of Renam, an Imperial noble aged around 50 who is currently waiting for an appointment. ",&
	"He seems to be quite tall, and has blonde hair and brown eyes. He seems to be particularly approachable, as he ",&
	"relaxes in the waiting room."
	
	! Do you want to talk to Enora, Anten, or Centom?
	do while (no_more_dialogue .EQV. .false.)
		print*, "Do you want to talk to anyone?"
		print*, "Yes or No."
		read (*,*) do_i_want_to_talk
		do_i_want_to_talk = fixformat(do_i_want_to_talk)
		if (do_i_want_to_talk .EQ. 'Yes') then
			exit
		else if (do_i_want_to_talk .EQ. 'No') then
			no_more_dialogue = .true.
		else
			print*, "Try again."
		end if
	end do
	
	! If you want to talk to them, pick who you want to talk to.
	do while (no_more_dialogue .EQV. .false.)
		print*, "Who do you want to talk to?"
		print*, "Enter Exit if done talking, or People to see who you can talk to."
		read (*,'(A)') i_want_to_talk_to
		i_want_to_talk_to = fixformat(i_want_to_talk_to)
		if (i_want_to_talk_to .EQ. 'Exit') then
			no_more_dialogue = .true.
		else if (i_want_to_talk_to .EQ. 'People') then
			if (enora_dialogue_done .EQV. .false.) then
				print*, "You can talk to Enora, the secratary."
			end if
			if (anten_dialogue_done .EQV. .false.) then
				print*, "You can talk to Anten, a guard."
			end if
			if (centom_dialogue_done .EQV. .false.) then
				print*, "You can talk to Centom of Renam, a noble."
			end if
		else if (i_want_to_talk_to .EQ. 'Enora') then
			if (enora_dialogue_done .EQV. .false.) then
				call enora_dialogue(enora_dialogue_done)
			else
				print*, "You have already talked to Enora."
			end if
		else if (i_want_to_talk_to .EQ. 'Anten') then
			if (anten_dialogue_done .EQV. .false.) then
				call anten_dialogue(anten_dialogue_done)
			else
				print*, "You have already talked to Anten."
			end if
		else if (i_want_to_talk_to .EQ. 'Centom') then
			if (centom_dialogue_done .EQV. .false.) then
				call centom_dialogue(player_is_nobility,centom_dialogue_done)
			else
				print*, "You have already talked to Centom of Renam."
			end if
		! This choice is here if you put in the Nobilary.
		else if (i_want_to_talk_to .EQ. 'Centom of Renam') then
			if (centom_dialogue_done .EQV. .false.) then
				call centom_dialogue(player_is_nobility,centom_dialogue_done)
			else
				print*, "You have already talked to Centom of Renam."
			end if
		else
			print*, "Try again."
		end if
		
		if (enora_dialogue_done .EQV. .true.) then
			if (anten_dialogue_done .EQV. .true.) then
				if (centom_dialogue_done .EQV. .true.) then
					print*, "You have talked to everybody."
					no_more_dialogue = .true.
				end if
			end if
		end if
	end do
	
	print*, "You wait about for a few more minutes, biding your time and hoping that this will all come to an end soon. ",&
	"As you wait, Enora walks up to you and speaks."
	print*, '(Enora): "Thank you for waiting patiently. If you would please follow me. I''ll take you ',&
	'where you need to go."'
	
	! Exposition 2a
	print*, "Enora leads you from the open waiting room into the interior of the building. The halls that she ",&
	"leads you through are lined with artwork, pictures, and framed papers. Indeed, these things set on the wall ",&
	"all seem to be quite important, with a great deal many pictures of nobles, the Warlord himself, spirits, ",&
	"and other important beings all present in a variety of fashions."
	
	! Asking Enora questions
	no_more_dialogue = .false.
	do while (no_more_dialogue .EQV. .false.)
		print*, "Do you want to talk to Enora while you walk with her?"
		print*, "Yes or No."
		read (*,*) do_i_want_to_talk
		do_i_want_to_talk = fixformat(do_i_want_to_talk)
		if (do_i_want_to_talk .EQ. 'Yes') then
			exit
		else if (do_i_want_to_talk .EQ. 'No') then
			no_more_dialogue = .true.
		else
			print*, "Try again."
		end if
	end do
	
	if (no_more_dialogue .EQV. .false.) then
		call enora_hall_dialogue
	end if
	
	! Exposition 2b
	print*, "You arrive at a luxurious office door. Enora knocks on the door, waits for a few moments, and then ",&
	"opens the door for you before leaving."
	print*, "You look inside and you are met with Celene Psonus of South Imperia, a middle aged Imperial woman. ",&
	"She has dyed blonde hair and blue eyes, and is wearing what appears to be a quite standard white blouse and ",&
	"black pants, with slightly raised blue shoes on her feet. She is additionally adorned with a few sparkling, yet ",&
	"humble, pieces of silver and diamond jewelry. She looks quite expectantly at you, and most certainly she was ",&
	"expecting you to be brought here."
	print*, "The atmosphere about her makes her seem quite powerful and commanding of respect."
	
	! Checks of character traits to get more information.
	if (player_citizenship .EQ. 'The Empire') then
		print*, "You know that Lady Psonus is the Minister of State for the Empire."
	end if
	if (player_is_nobility .EQV. .true.) then
		print*, "You know that Celene belongs to the House of Psonus, a powerful noble family in the Center State. ",&
		"In particular, you know that her father is the Lord of South Imperia."
	end if
	if (player_is_female .EQV. .true.) then
		print*, "Celene Psonus is the first female minister in the Empire and was also the first female ",&
		"Imperial Representative for a governorate."
	end if
	
	print*, "She has a variety of pictures scattered about her desk, with many of them from her service in ",&
	"Kathay, including a few pictures with the former Governor-General of Kathay."

	
	print*, "Psonus has a further variety of pictures scattered about her desk, with her looking quite formal and ",&
	"professional in many of them. No doubt that many of the pictures with her are important people."
	
	! Do you know about Altec?
	if (player_area .EQ. 'Kathay' .OR. player_career .EQ. 'Historian') then
		print*, "You recognize that the late Victor Altec, the last Governor-General of Kathay, and his wife ",&
		"Oriurora Altec are featured heavily in many of the pictures. A great deal of these pictures are not ",&
		"professional, and show the two of them alongside Celene Psonus having fun together and looking like friends."
	end if
	
	print*, "She also has a degree from the University of the Holy Order of the Eternal Lady of Weapons posted ",&
	"on the wall facing you, stating that one Mara Aurora Alania Zera Celene Carola Ranvon Psonus of ",&
	"South Imperia received a Bachelor's in Governance. The degree is held in a frame with a shining piece ",&
	"of glass covering its visage, still looking incredibly fresh and new despite it being issued over ",&
	"twenty years ago."
	
	print*, "Psonus offers you a set around a table in her office, motioning you towards it, all while she remains ",&
	"seated at her own desk."
	print*, '(Psonus): "Thank you for coming. Take a seat and let''s get started."'
	print*, "So, you go and sit down in a somewhat comfortable chair and prepare to listen."
	
	! Dialogue: Celene Psonus
	call dialogue_celene_psonus(player_citizenship,player_served_in_banact_war)


end program introduction
