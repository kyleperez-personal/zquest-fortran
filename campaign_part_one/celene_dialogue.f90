module celene_dialogue


	use proper_cap ! Lets you use the fixformat function
	use already_picked ! Lets you use the picked function for dialogue you have already chosen.

	implicit none
	
	CONTAINS
	
	subroutine dialogue_celene_psonus(player_citizenship,player_served_in_banact_war)
	
	! Input Variables
	character(len=30), intent(in) :: player_citizenship
	logical, intent(in) :: player_served_in_banact_war
	! Output Variables
	
	! Temporary variables
	integer :: times_i_said_no
	character(len=7) :: my_dialogue_choice
	logical :: dialogue_is_finished
	logical :: can_challenge_celene
	
	logical :: c1_already_picked,c1a_already_picked,c1b_already_picked
	logical :: c2_already_picked,c3_already_picked
	logical :: c4_already_picked,c4a_already_picked,c5_already_picked
	logical :: c6_already_picked,c6a_already_picked
	logical :: c6b_already_picked,c6c_already_picked,c6d_already_picked
	logical :: c6e_already_picked,c6f_already_picked,c6fa_already_picked
	logical :: c6g_already_picked,c6ga_already_picked,c6h_already_picked
	logical :: c6i_already_picked,c6j_already_picked,c6k_already_picked
	logical :: c7_already_picked, c8_already_picked,c8a_already_picked
	logical :: c9_already_picked,c9a_already_picked,c10_already_picked
	logical :: c11_already_picked,c12_already_picked
	
	times_i_said_no = 0
	
	dialogue_is_finished = .false.
	can_challenge_celene = .false.
	
	c1_already_picked = .false.
	c1a_already_picked = .false.
	c1b_already_picked = .false.
	c2_already_picked = .false.
	c3_already_picked = .false.
	c4_already_picked = .false.
	c4a_already_picked = .false.
	c5_already_picked = .false.
	c6_already_picked = .false.
	c6a_already_picked = .false.
	c6b_already_picked = .false.
	c6c_already_picked = .false.
	c6d_already_picked = .false.
	c6e_already_picked = .false.
	c6f_already_picked = .false.
	c6fa_already_picked = .false.
	c6g_already_picked = .false.
	c6ga_already_picked = .false.
	c6h_already_picked = .false.
	c6i_already_picked = .false.
	c6j_already_picked = .false.
	c6k_already_picked = .false.
	c7_already_picked = .false.
	c8_already_picked = .false.
	c8a_already_picked = .false.
	c9_already_picked = .false.
	c9a_already_picked = .false.
	c10_already_picked = .false.
	c11_already_picked = .false.
	c12_already_picked = .false.
	
	
	! L1
	print*, '(Psonus): "Well, I''m sure you want to know why you''ve been called here."'
	print*, '(Psonus): "So, you''ve been brought here to perform a service for your Warlord. Now, I''m not an expert in running ',&
	'these types of things; my expertise is in politics, not organizing search parties."'
	print*, '(Psonus): "So I imagine that you''re familiar with the war that happened twenty years ago?"'
	! Do you want to ask questions about the war?
	do while (dialogue_is_finished .EQV. .false.)
		print*, "Do you want to ask about the war?"
		print*, "Yes or No."
		read (*,*) my_dialogue_choice
		my_dialogue_choice = fixformat(my_dialogue_choice)
		if (my_dialogue_choice .EQ. 'Yes') then
			if (player_served_in_banact_war .EQV. .true.) then
				print*, '(Psonus): Don''t worry, it was a while ago for all of us.'
			end if
			exit
		else if (my_dialogue_choice .EQ. 'No') then
			if (player_served_in_banact_war .EQV. .true.) then
				print*, "You say that you served in the war, and are more than familiar with it."
				print*, '(Psonus): "That''s what I thought you''d say. Just making sure."'
			else
				print*, "You say that you are familiar enough with the war."
			end if
			dialogue_is_finished = .true.
		else
			print*, "Try again."
		end if
	end do
	
	! Questions 1a through 1aa
	! If you do, this dialogue (not in a subroutine) runs
	do while (dialogue_is_finished .EQV. .false.)
		print*, "What do you want to ask?"
		if (c1_already_picked .EQV. .false.) then
			print*, "1: Ask for information about the war."
		else
			print*, picked("1a: Ask for information about the war.")
		end if
		if (c1_already_picked .EQV. .true.) then
			print*, "1a: Ask for more details." ! note, option does not need to be greyed out after being picked.
		end if
		print*, "Exit: leave dialogue."
		read (*,*) my_dialogue_choice
		my_dialogue_choice = fixformat(my_dialogue_choice)
		if (my_dialogue_choice .EQ. 'Exit') then
			if (c1_already_picked .EQV. .true.) then
				print*, 'You seem satisfied with what you learned and stop asking questions.'
				dialogue_is_finished = .true.
			else
				print*, 'You seem satisfied with your own knowledge and choose not to ask questions.'
				if (player_served_in_banact_war .EQV. .true.) then
					print*, '(Psonus): "Got your memory jogging? Good."'
					dialogue_is_finished = .true.
				end if
			end if
		else if (my_dialogue_choice .EQ. '1') then
			print*, 'You ask Psonus for information about the war.'
			print*, '(Psonus): "So I won''t ask where you''ve been in the meantime, but something like ',&
			'twenty-five years ago there were these things, called the Banact. They were a lizard-like ',&
			'people who came from a world far away to attack us. So the Empire, alongside Aurora and ',&
			'the Highlands beat them back and defeated them."'
			c1_already_picked = .true.
		else if (my_dialogue_choice .EQ. '1a') then
			if (c1_already_picked .EQV. .true.) then
				print*, 'You ask for even more information about the war.'
				print*, '(Psonus): "If you want to know more, I can tell you some more, but let''s ',&
				'hold it until later. Sound good?"'
				dialogue_is_finished = .true.
			else
				print*, "Invalid option."
			end if
		else
			print*, "Invalid option."
		end if
	end do
	
	! L2
	print*, '(Psonus): "Well, as a result of this war, we have somewhere between ten and twenty thousand Banact, ',&
	'or Lizards, in the Empire, just running about. These were enemy combatants in the war, and under Imperial ',&
	'law, they are subject to punishment for their crimes against humanity. We have one in particular that ',&
	'we are looking for."'
	print*, '(Psonus): "Though I''m sure that you''re quite suspicious as to why you, of all people, were ',&
	'brought here today. I''ll be quite honest, if it was up to me, I''d have sent our military in to find and ',&
	'extract this thing, but it''s not my place to question the will of a millenia-old undead being."'
	print*, '(Psonus): "You may have guessed that this probably isn''t some kind of soldier that we''re looking for. ',&
	'We have it on good authority that there is a high ranking general who deserted in the closing months of ',&
	'the war, a general who is liable in the deaths of many Imperials, Imperial Subjects, Aurorans, and ',&
	'Highlanders. It is your task, if you so choose to accept it, to find him and bring him back to the City ',&
	'so he may stand for his crimes."'
	print*, '(Psonus): "As I already said, normally we would have your military do this, but...well, are ',&
	'you aware of the Empire''s current political issues?"'
	
	! Q2a Q2aa
	! Dialogue block for learning about the politics after the war.
	dialogue_is_finished = .false.
	do while (dialogue_is_finished .EQV. .false.)
		print*, "Do you want to ask about the Empire's current political issues?"
		print*, "Yes or No."
		read (*,*) my_dialogue_choice
		my_dialogue_choice = fixformat(my_dialogue_choice)
		if (my_dialogue_choice .EQ. 'Yes') then
			if (player_citizenship .EQ. 'The Highlands') then
				print*, '(Psonus): "I know you''re not from the Empire. It''s alright."'
			end if
			exit
		else if (my_dialogue_choice .EQ. 'No') then
			print*, "You say that you are familiar enough with the issues of the Empire."
			dialogue_is_finished = .true.
		else
			print*, "Try again."
		end if
	end do
	
	c1_already_picked = .false.
	do while (dialogue_is_finished .EQV. .false.)
		print*, "What do you want to ask?"
		if (c1_already_picked .EQV. .false.) then
			print*, "1: Ask about the issues the Empire faces."
		else
			print*, picked("1: Ask about the issues the Empire faces.")
		end if
		if (c1_already_picked .EQV. .true.) then
			print*, "1a: Tell her that her story is quite biased." ! doesn't need greyed out option choice.
		end if
		print*, "Exit: leave dialogue."
		read (*,*) my_dialogue_choice
		my_dialogue_choice = fixformat(my_dialogue_choice)
		
		if (my_dialogue_choice .EQ. 'Exit') then
			dialogue_is_finished = .true.
		else if (my_dialogue_choice .EQ. '1') then
			print*, 'You ask for more information about the issues of the Empire.'
			print*, '(Psonus): "Well, after the Lizard War, there was a bit of a leadership struggle. There''s ',&
			'quite a bit of political fire behind all this, trying to change the narrative, but the Governor-General ',&
			'of Kathay refused to resign his position when asked, and a rogue Imperial agent took this as a sign to ',&
			'assassinate him. Upon this occuring, the people took up arms against the IMperial government and attempted ',&
			'to secure independence. We were able to crush the uprising in Kathay, but allowed the rebels to form their own ',&
			'legitimate government, still subject to Imperial authority, in place of direct rule as a compromise to keep the ',&
			'peace. As it turns out, Kathay still proved themselves to be quite opposed to the Empire, and they constantly ',&
			'give us issues when attempting to assert proper Imperial authority in spite of their violation of the terms of ',&
			'the treaty they signed with us."'
			c1_already_picked = .true.
		else if (my_dialogue_choice .EQ. '1a') then
			if (c1_already_picked .EQV. .true.) then
				print*, 'You tell Psonus that her story is a very pro-Imperial stance of the situation.'
				print*, '(Psonus): "If you reall want to discuss this, let''s do that later, alright?"'
				can_challenge_celene = .true.
				dialogue_is_finished = .true.
			else
				print*, "Invalid option."
			end if
		else
			print*, "Invalid option."
		end if
	end do
	
	! L3
	print*, '(Psonus): "So, our leads are telling us that this Banact is in Kathay. Maybe that will let you know why we can''t ',&
	'send our military in without drawing suspicion from the local government. We''ve, and by we I mean the Warlord, hand-picked ',&
	'you to go into Kathay to handle this for us. You''ll be handsomely rewarded.'
	
	! Asking about rewards
	dialogue_is_finished = .false.
	do while (dialogue_is_finished .EQV. .false.)
		print*, "Do you want to ask about the rewards?"
		print*, "Yes or No."
		read (*,*) my_dialogue_choice
		my_dialogue_choice = fixformat(my_dialogue_choice)
		
		if (my_dialogue_choice .EQ. 'Yes') then
			exit
		else if (my_dialogue_choice .EQ. 'No') then
			dialogue_is_finished = .true.
		else
			print*, "Try again."
		end if
	end do
	
	do while (dialogue_is_finished .EQV. .false.)
		print*, "What do you want to ask?"
		print*, "1: Ask about the rewards."
		print*, "Exit: leave dialogue."
		read (*,*) my_dialogue_choice
		my_dialogue_choice = fixformat(my_dialogue_choice)
		if (my_dialogue_choice .EQ. 'Exit') then
			print*, 'You decide to not ask about the rewards.'
			dialogue_is_finished = .true.
		else if (my_dialogue_choice .EQ. '1') then
			print*, 'You ask about the rewards you can expect.'
			print*, '(Psonus): "Upon the capture of this general, we will be open to any generous requests for rewards. ',&
			'This includes being exempt from filing taxes, handsome rewards in cash, goods, or property, assistance ',&
			'for immigration, and quite a bit more that you will be able to think of."'
			dialogue_is_finished = .true.
			c10_already_picked = .true.
		else
			print*, "Invalid option."
		end if
	end do
	
	! L4
	print*, '(Psonus): "Here''s a bit of an issue though. We have it on good authority that he''s in Kathay, but we ',&
	'don''t know where in particular; it''s a very big place, and I really don''t think that searching piecemeal will ',&
	'get us very far."'
	print*, '(Psonus): "I''m willing to bet that you can find some people in Aurea who can help. You are free to collect ',&
	'information through whatever means possible.'
	
	! Asking questions about the rewards.
	dialogue_is_finished = .false.
	c1_already_picked = .false.
	do while (dialogue_is_finished .EQV. .false.)
		print*, "Do you want to ask about just what you are allowed to do?"
		print*, "Yes or No."
		read (*,*) my_dialogue_choice
		my_dialogue_choice = fixformat(my_dialogue_choice)
		if (my_dialogue_choice .EQ. 'Yes') then
			exit
		else if (my_dialogue_choice .EQ. 'No') then
			dialogue_is_finished = .true.
		else
			print*, 'Try again.'
		end if
	end do
	
	do while (dialogue_is_finished .EQV. .false.)
		print*, "What do you want to ask?"
		print*, "1: Ask about how free your hands are in the search."
		print*, "Exit: leave dialogue."
		read (*,*) my_dialogue_choice
		my_dialogue_choice = fixformat(my_dialogue_choice)
		if (my_dialogue_choice .EQ. 'Exit') then
			print*, "You think to yourself that maybe it's better not to know."
			dialogue_is_finished = .true.
		else if (my_dialogue_choice .EQ. '1') then
			print*, 'You ask about just open your means are to collect information.'
			print*, '(Psonus): "So long as you do not commit any particularly heinous crimes, we can ',&
			'expunge you of all crimes committed. Unfortunately, we cannot offer overt diplomatic immunity due ',&
			'to political complications with Kathay, though we can use our influence to get you set free in case of ',&
			'capture. Don''t take that has a sign to get yourself imprisoned all the time though."'
			c11_already_picked = .true.
			dialogue_is_finished = .true.
		else
			print*, "Invalid option."
		end if
	end do
	
	! L5
	print*, '(Psonus): "So, if you''re willing to accept, we''ll ship you out to Mercy, in Anoch, to get you ',&
	'prepared before you cross the border into Kathay."'
	print*, '(Psonus): I mean, you will accept, won''t you?"'
	
	! Are you going to go on the adventure?
	dialogue_is_finished = .false.
	do while (dialogue_is_finished .EQV. .false.)
		print*, "Are you willing to go on this dangerous, life-risking adventure?"
		print*, "Yes or No."
		read (*,*) my_dialogue_choice
		my_dialogue_choice = fixformat(my_dialogue_choice)
		if (my_dialogue_choice .EQ. 'Yes') then
			print*, '(Psonus): "That''s excellent. If you have any questions about anything, please let ',&
			'me know and I''ll do my best to answer them."'
			dialogue_is_finished = .true.
		else if (my_dialogue_choice .EQ. 'No') then
			if (times_i_said_no .EQ. 0) then
				print*, '(Psonus): "You know, maybe I should make it a bit more clear. I''m asking for your ',&
			'permission, but that doesn''t mean that we need it. Get what I''m saying?"'
				print*, '(Psonus): "Now, what was your answer again?"'
			else if (times_i_said_no .EQ. 1) then
				print*, '(Psonus): "Honey, this isn''t up to me. The Warlord''s ordering this. Nobody ',&
				'can change what he wants. Don''t do this please."'
			else if (times_i_said_no .EQ. 2) then
				print*, '(Psonus): "Come on, you know too much already. If you don''t accept, there will be ',&
				'consequences. You''re looking at imprisonment and possibly treason."'
			else
				print*, '(Psonus): "Look, you don''t have a choice in this. You''ll go there to Kathay, get ',&
				'this Lizard, and bring him back. If you don''t, you''ll be hunted down. I''ll be here to ',&
				'answer some questions if you need me.'
				dialogue_is_finished = .true.
			end if
			times_i_said_no = times_i_said_no + 1
		else
			print*, "Try again."
		end if
	end do
	
	! Twenty Questions with Celene
	dialogue_is_finished = .false.
	c1_already_picked = .false.
	do while (dialogue_is_finished .EQV. .false.)
		print*, "What do you want to ask Psonus about?"
		! 1
		if (c1_already_picked .EQV. .false.) then
			print*, "1: Ask about where to start your search."
		else
			print*, picked("1: Ask about where to start your search.")
		end if
		! 1a,b
		if (c1_already_picked .EQV. .true.) then
			! 1a
			if (c1a_already_picked .EQV. .false.) then
				print*, "1a: Ask for some more details."
			else
				print*, picked("1a: Ask for some more details.")
			end if
			! 1b
			if (c1b_already_picked .EQV. .false.) then
				print*, "1b: Ask about where to stay in Aurea."
			else
				print*, picked("1b: Ask about where to stay in Aurea.")
			end if
		end if
		! 2
		if (c2_already_picked .EQV. .false.) then
			print*, "2: Question about potential competition in the search."
		else
			print*, picked("2: Question about potential competition in the search.")
		end if
		! 3
		if (c3_already_picked .EQV. .false.) then
			print*, "3: Ask more about the target."
		else
			print*, picked("3: Ask more about the target.")
		end if
		! 4
		if (c4_already_picked .EQV. .false.) then
			print*, "4: Ask why you are being sent to Anoch instead of Kathay."
		else
			print*, picked("4: Ask why you are being sent to Anoch instead of Kathay.")
			! 4a
			if (c4a_already_picked .EQV. .false.) then
				print*, "4a: Ask why you are being sent to Mercy in particular."
			else
				print*, picked("4a: Ask why you are being sent to Mercy in particular.")
			end if
		end if
		! 5
		if (c5_already_picked .EQV. .false.) then
			print*, "5: Ask for the source of all of her information about the Lizard general."
		else
			print*, picked("5: Ask for the source of all of her information about the Lizard general.")
		end if
		! 6
		if (c6_already_picked .EQV. .false.) then
			print*, "6: Ask about Psonus's life."
		else
			print*, picked("6: Ask about Psonus's life.")
			! 6a
			if (c6a_already_picked .EQV. .false.) then
				print*, "6a: Ask about Ascalon."
			else
				print*, picked("6a: Ask about Ascalon.")
			end if
			! 6b
			if (c6b_already_picked .EQV. .false.) then
				print*, "6b: Ask about her father."
			else
				print*, picked("6b: Ask about her father.")
			end if
			! 6c
			if (c6c_already_picked .EQV. .false.) then
				print*, "6c: Ask about her mother."
			else
				print*, picked("6c: Ask about her mother.")
			end if
			! 6d
			if (c6d_already_picked .EQV. .false.) then
				print*, "6d: Ask about her siblings."
			else
				print*, picked("6d: Ask about her siblings.")
			end if
			! 6e
			if (c6e_already_picked .EQV. .false.) then
				print*, "6e: Ask about her education."
			else
				print*, picked("6e: Ask about her education.")
			end if
			! 6f
			if (c6f_already_picked .EQV. .false.) then
				print*, "6f: Ask about her affiliation to the Holy Order of the Eternal Lady of Weapons."
			else
				print*, picked("6f: Ask about her affiliation to the Holy Order of the Eternal Lady of Weapons.")
				! 6fa
				if (c6fa_already_picked .EQV. .false.) then
					print*, "6fa: Ask about the rumours and conspiracies she mentioned."
				else
					print*, picked("6fa: Ask about the rumours and conspiracies she mentioned.")
				end if
			end if
			! 6g
			if (c6g_already_picked .EQV. .false.) then
				print*, "6g: Ask about how it was to be a woman in a position of power back then."
			else
				print*, picked("6g: Ask about how it was to be a woman in a position of power back then.")
				! 6ga
				if (c6ga_already_picked .EQV. .false.) then
					print*, "6ga: Ask what Victor Altec did exactly."
				else
					print*, picked("6ga: Ask what Victor Altec did exactly.")
				end if
			end if
			! 6h
			if (c6h_already_picked .EQV. .false.) then
				print*, "6h: Ask about how Psonus felt being so young at her ",&
				"appointment as Imperial Representative for Kathay."
			else
				print*, picked("6h: Ask about how Psonus felt being so young at her "),&
				picked("appointment as Imperial Representative for Kathay.")
			end if
			! 6i
			if (c6i_already_picked .EQV. .false.) then
				print*, "6i: Ask if she has war stories."
			else
				print*, picked("6i: Ask if she has war stories.")
			end if
			! 6j
			if (c6j_already_picked .EQV. .false.) then
				print*, "6j: Ask about Victor Altec."
			else
				print*, picked("6j: Ask about Victor Altec.")
			end if
			! 6k
			if (c6k_already_picked .EQV. .false.) then
				print*, "6k: Ask about her thoughts on the revolution in Kathay."
			else
				print*, picked("6k: Ask about her thoughts on the revolution in Kathay.")
			end if
		end if
		! 7
		if (c7_already_picked .EQV. .false.) then
			print*, "7: Ask about the Lizard War."
		else
			print*, picked("7: Ask about the Lizard War.")
		end if
		! 8
		if (c8_already_picked .EQV. .false.) then
			print*, "8: Ask about Kathay."
		else
			print*, picked("8: Ask about Kathay.")
			! 8a
			if (c8a_already_picked .EQV. .false.) then
				print*, "8a: If asked about the laws in Kathay that are more strict."
			else
				print*, picked("8a: If asked about the laws in Kathay that are more strict.")
			end if
		end if
		! 9
		if (c9_already_picked .EQV. .false.) then
			print*, "9: Ask about the postwar years."
		else
			print*, picked("9: Ask about the postwar years.")
		end if
		! 9a
		if (c9_already_picked .EQV. .true.) then
			if (c9a_already_picked .EQV. .false.) then
				print*, "9a: Ask if she is afraid of the Empire collapsing."
			else
				print*, picked("9a: Ask if she is afraid of the Empire collapsing.")
			end if	
		end if
		! 10
		if (c10_already_picked .EQV. .false.) then
			print*, "10: Ask about the rewards you can expect for your service."
		else
			print*, picked("10: Ask about the rewards you can expect for your service.")
		end if
		! 11
		if (c11_already_picked .EQV. .false.) then
			print*, "11: Ask about your freedom to retrieve information."
		else
			print*, picked("11: Ask about your freedom to retrieve information.")
		end if
		! 12, only availible if you can challenge Celene
		if (can_challenge_celene .EQV. .true.) then
		 	if (c11_already_picked .EQV. .false.) then
		 		print*, "12: Challenge Psonus on her viewpoint of Kathay's rebellion."
		 	else
		 		print*, picked("12: Challenge Psonus on her viewpoint of Kathay's rebellion.")
		 	end if
		end if
		 
		print*, "Exit: leave dialogue."
		read (*,*) my_dialogue_choice
		my_dialogue_choice = fixformat(my_dialogue_choice)
		! Exit
		if (my_dialogue_choice .EQ. 'Exit') then
			dialogue_is_finished = .true.
		! 1
		else if (my_dialogue_choice .EQ. '1') then
			print*, 'You ask about where you should start looking for this Lizard general.'
			print*, '(Psonus): "I''d say that you should start in Aurea, the capital of Kathay. It''s a big ',&
			'city that''s sure to hold the location of this general."'
			c1_already_picked = .true.
		! 1a
		else if (my_dialogue_choice .EQ. '1a') then
			if (c1_already_picked .EQV. .true.) then
				print*, "You ask for more details about where to search."
				print*, '(Psonus): "I guess that you''re lucky that I''ve been there before. Maybe check out the ',&
				'University of Aurea. I know that there''s a large store of Banact data there that''s analyzed ',&
				'by experts from all around the world. Maybe that has some information."'
				c1a_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 1b
		else if (my_dialogue_choice .EQ. '1b') then
			if (c1_already_picked .EQV. .true.) then
				print*, "You ask about where to stay in Aurea."
				print*, '(Psonus): "The local Convent Aurorans in Aurea are going to be quite sympathetic to ',&
				'our cause; they will likely avoid giving you trouble. Just be aware of the fact that there ',&
				'has been a bit of unrest in those sections of the city. Maybe you can stay under the ',&
				'government''s nose by staying somewhere less suspicious, but maybe more hostile to you."'
				c1b_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 2
		else if (my_dialogue_choice .EQ. '2') then
			print*, "You ask about whether or not you can expect others to be looking for the same Lizard ",&
			"you are looking for."
			print*, '(Psonus): "So all I can guarantee is that we will not be sending anyone to compete with you. ',&
			'But I can''t control what Kathay does, and if I were to tell them to not impede our search, they ',&
			'would only be emboldened. As far as I know, they''re going to be the only ones likely to compete with ',&
			'you."'
			c2_already_picked = .true.
		! 3
		else if (my_dialogue_choice .EQ. '3') then
			print*, "You ask for more information about your target."
			print*, '(Psonus): "I''m sorry that you have to hear this, but we don''t know much about him; you''ll ',&
			'probably have to be the one to find out everything. Regardless, we do think that this general was ',&
			'highly decorated in the war and that he was responsible for the administration of Kathay ',&
			'during its occupation, before he led some more campaigns on the Continent. These rumors could mean that ',&
			'Kathay is quite motivated to find him."'
			c3_already_picked = .true.
		! 4
		else if (my_dialogue_choice .EQ. '4') then
			print*, "You ask about why you are being shipped to Anoch instead of Kathay."
			print*, '(Psonus): "We''re looking to minimize any attention from Kathay''s government. By sending you ',&
			'off to Anoch, which borders Kathay, you should be able to cross an unguarded section of the ',&
			'border, and thus minimize any trouble that you might encounter as a result of a recorded crossing."'
			c4_already_picked = .true.
		! 4a
		else if (my_dialogue_choice .EQ. '4a') then
			if (c4_already_picked .EQV. .true.) then
				print*, "You ask why you are being sent to Mercy in Anoch instead of any other city there."
				print*, '(Psonus): "Well, we have a military base there. It will serve to outfit you ',&
				'before your journey. Maybe there''s also information hiding out there too."'
				c4a_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 5
		else if (my_dialogue_choice .EQ. '5') then
			print*, "You ask Psonus for the source of her information on this Banact general."
			print*, '(Psonus): "All I''m allowed to say is that it''s from the Aurorans."'
			c5_already_picked = .true.
		! 6
		else if (my_dialogue_choice .EQ. '6') then
			print*, "You ask Psonus about her life."
			print*, '(Psonus): "Oh, well, I really don''t like to stroke my own life, but if you''re so ',&
			'interested then I might as well share. It''s surprisingly nice; not too many ask."'
			print*, '(Psonus): "So I was born in the City of Ascalon here in the Center State, but my family ',&
			'moved to the City when I was quite young, to raise my brothers and I. I like to think that I ',&
			'had quite a normal upbringing, at least as much as a normal one you can have when your father ',&
			'is high nobility."'
			print*, '(Psonus): "But after being schooled, I went to UHOLE, learned about governance, which ',&
			'was quite the strange thing for a woman at the time. I graduated, and within a few months, ',&
			'I suddenly found myselfl appointed as the Imperial Representative of Kathay. I was actually ',&
			'the first woman to ever hold such a position, and the youngest person to ever hold such a position, ',&
			'as I had really just graduated from college. It''s quite hilarious to look back on it now."'
			print*, '(Psonus): "After my appointment, I found out, maybe a week later, that there''s this ',&
			'big war that''s going to start. So I get to move to Kathay and start to handle that. I was ',&
			'actually hold that the war was the reason why I was appointed, since the government was ',&
			'much more interested in people who had the energy to run around for twelve hours a day, all ',&
			'week, putting out fires, and not so much old, experienced people who couldn''t do that ',&
			'for an extended period of time. I basically kept on that kind of work schedule for the ',&
			'whole war. I even got to flee Kathay and make the drive all the way to the City. You would ',&
			'think that a few weeks on the open road would be relaxing, but when you''re afraid that a ',&
			'bunch of lizards might bomb you, you''re just worried the whole time."'
			print*, '(Psonus): "I still remember the big battle that happened right outside the City. ',&
			'I was in the palace, army uniform on, rifle in hand, and trembling the whole time. As a ',&
			'reminder, women can''t serve in the army, I had no training, and I was told to help guard the ',&
			'Warlord himself, on the basis that it was an honorable position. I was quite scared that ',&
			'that we would lose, and the Lizards would barge on in. Turns out though, the Aurorans won ',&
			'the battle, with us alongside them, and so I never had to make a final stand."'
			print*, '(Psonus): "So we liberated the Continent and returned to Aurea, and began to administer ',&
			'the mess that was left behind after occupation."'
			print*, '(Psonus): "After the flames of war had been put out, Victor was suddenly getting ',&
			'asked to step down, as he was apparantly fanning nationalist tensions in the governorate. ',&
			'It might be a bit more accurate to say that he had created them. As it turns out, when ',&
			'you''re someone as groundbreaking as him, and lead the people through a time of great ',&
			'tribulation, they like you. The central government didn''t want someone as powerful as him, ',&
			'so they asked for him to stand down. He refused."'
			print*, '(Psonus): "I tried a few times to convince him otherwise. On my last time, we ',&
			'were there talking, and his wife and I were even sitting next to one another, ',&
			'trying to convince him to retire and go live a life away from the spotlight for his own ',&
			'good. He said that it was bigger than him. That''s when it happened. The agent came in ',&
			'from behind me, and Victor saw him pull out the gun, and when he went for his own, the ',&
			'agent pulled the trigger and shot him. One of the nobles with us managed to wrestle the guy ',&
			'to the ground, while Oriurora and I tried to see what we could do to help Victor. He ',&
			'was able to get out some last words to Oriurora, and then died."'
			print*, '(Psonus): "That''s when things went downhill. Rebellion broke out once news got out. ',&
			'I suddenly found myself being the only member of the government opposed to the ongoing ',&
			'revolution, so I got up and left to join up with the Imperials attempting to return ',&
			'order to Aurea."'
			print*, '(Psonus): "After the revolt was put down, there was about a month where I ruled ',&
			'alone, then we came to the peace that we''re at today. I resigned my position in ',&
			'agreement with the terms, and started to serve under the late Minister of State."'
			print*, '(Psonus): "I went from being considered one of the founders of the modern Kathay, ',&
			'honored by all, to being a hated opponent of independence, and being seen as a demonic ',&
			'figure. It really was a shame, for as much as they idolized Victor, I know that he never ',&
			'would have hated me for my actions against today''s Kathay."'
			print*, '(Psonus): "But unlike them, I moved on. I succeeded the previous Minister of ',&
			'State within a few months, and here I am now. It''s a bit embarrassing to say, but I am ',&
			'the most powerful woman in the world, and really, when I recant my story, it seems like ',&
			'a whole novel."'
			print*, '(Psonus): "At the very least, I hope it illuminates my biases towards ',&
			'everything. All this modern political turmoil, I was there, physically, when it ',&
			'started. Maybe that''s why I''m here today, but I guess that''s not really important."'
			print*, '(Psonus): "Anyways, any other questions, or anything you wanted more info about?"'
			c6_already_picked = .true.
		! 6a
		else if (my_dialogue_choice .EQ. '6a') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You ask about Psonus's hometown: Ascalon."
				print*, '(Psonus): "It''s a nice city, but not as nice as the City. I try to visit every ',&
				'year for vacation, so I can spend time with my family. Honestly, not much of it has really ',&
				'changed since the War. I''m not sure if that''s for the better or for the worse."'
				c6a_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6b
		else if (my_dialogue_choice .EQ. '6b') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You ask about her father."
				print*, '(Psonus): "My father is Alexen Psonus, the Twelfth. He did his best to raise ',&
				'his children, visiting us all the time. It''s quite a bit weird actually; many children ',&
				'in my situation were raised by nannies instead. Having my actual father as a father ',&
				'figure was great, and he''s really passed on his ambition and talent to his children. Really, ',&
				'he was the one to push me to be all I could be, even though I was a woman, and I thank him ',&
				'all the time for that, since it made me who I am today."'
				c6b_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6c
		else if (my_dialogue_choice .EQ. '6c') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You note Celene's distinct lack of mentioning her mother and inquire about her."
				print*, '(Psonus): "Oh, well, I guess she was there sometimes. I can''t really touch ',&
				'on that subject due to political concerns. But tell you what, I''ll have an autobiography ',&
				'out after I retire and before I die. You can read that for all the details."'
				c6c_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6d
		else if (my_dialogue_choice .EQ. '6d') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You ask about Psonus's siblings."
				print*, '(Psonus): "So my older brother is still waiting in the wings to succeed my father. ',&
				'He really just lives in Ascalon, helping my father out with the administration of the ',&
				'realm. Back before that, he served in the army during the Lizard War. I also have a ',&
				'younger brother. He serves with the Holy Order of the Eternal Lady of Weapons, and from ',&
				'what I hear, he''s quite happy with his service. Sorry, but I can''t provide too many ',&
				'details out of concerns of blackmail."'
				c6d_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6e
		else if (my_dialogue_choice .EQ. '6e') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You ask for more information about Psonus's education."
				print*, '(Psonus): "It was a good time. When I graduated, still wet behind the ears, I ',&
				'would have said that it was the most important accomplishment of my life. Turns out that ',&
				'within six months, I''d have about twenty more accomplishments more important than ',&
				'graduation."'
				c6e_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6f
		else if (my_dialogue_choice .EQ. '6f') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You ask just how closely affiliated Psonus is with the Holy Order of the ",&
				"Eternal Lady of Weapons."
				print*, '(Psonus): "I just went to their school. Don''t believe the rumours and ',&
				'conspiracies, alright?"'
				c6f_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6fa
		else if (my_dialogue_choice .EQ. '6fa') then
			if (c6f_already_picked .EQV. .true.) then
				print*, "Interested, you ask about these rumours and conspiracies surrounding ",&
				"her affiliation with this Holy Order."
				print*, '(Psonus): "Some detractors like to say that my family is in big with them ',&
				'because I went to school at their university and by brother serves with them. I''d ',&
				'like to preface with the fact that my father''s dear friend recommended the ',&
				'school to me, since he went there too. There are a few other rumours going around too, ',&
				'but I assure that it''s just table talk nonsense."'
				c6fa_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6g
		else if (my_dialogue_choice .EQ. '6g') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You ask Psonus about how it was being a woman in a position of power during ",&
				"a time when it was uncommon, and how it felt to be the first woman at the top."
				print*, '(Psonus): "It was something else, it really was. I didn''t really have any ',&
				'role models to look up to at the time, so I had to forge my own path. Thankfully, ',&
				'I had a really supportive cast of coworkers who didn''t really care about my gender. ',&
				'They never really gave me issues, but there were a few people below me who really ',&
				'didn''t like reporting to a woman, and I had to deal with sexual harrassment and ',&
				'the like, but thankfully my dear friend Victor Altec was able to handle a lot of ',&
				'the trouble that people would give me, even if done a bit brutishly. He always knew ',&
				'how to deal with those nobles in a way that they would never give me trouble again."'
				c6g_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6ga
		else if (my_dialogue_choice .EQ. '6ga') then
			if (c6g_already_picked .EQV. .true.) then
				print*, "You question about just what Victor Altec did to get people off of her case."
				print*, '(Psonus): "So let me say, I''m not a big fan of unnecessary violence, but he was. ',&
				'He would go, sometimes taking me or his future wife along, and pay a visit to the offending ',&
				'noble, and usually crack them over the head a few times with some kind of improvised ',&
				'weapon. It was violent and barbaric, but I guess that it worked in the end. Not to say ',&
				'that I would ever do such things, but I guess that it made them all stand in line within ',&
				'a few weeks instead of slowly adjusting to me."'
				c6ga_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6h
		else if (my_dialogue_choice .EQ. '6h') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You ask about how it felt to be given an important position at such a ",&
				"young age."
				print*, '(Psonus): "You can only imagine how it is when you''re a young, attractive ',&
				'woman appointed to a position of great power. There are a lot of words thrown ',&
				'about, some to build you up, and some to tear you down. You quickly learn to tune ',&
				'everything out and just keep the words from those who you need to work with. I must ',&
				'say that the inexperience with everything was quite detrimental at first, but Victor ',&
				'Altec was very supportive of me, and he taught me a great amount, even though I ',&
				'would have to go and say that he was quite new to everything too. Between him, the ',&
				'Warlord, and even my own father, I was able to adjust."'
				c6h_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6i
		else if (my_dialogue_choice .EQ. '6i') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You ask if Psonus has any war stories."
				print*, '(Psonus): "I really don''t have many war stories. I didn''t really fight, ',&
				'and juts went around talking to nobles and Aurorans the whole time. Victor Altec, on ',&
				'the other hand, served as a general in the war, and if he were still alive, he''d be ',&
				'able to tell a lot."'
				c6i_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6j
		else if (my_dialogue_choice .EQ. '6j') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You ask about Victor Altec."
				print*, '(Psonus): "He really was quite a dear friend. He could be quite abrasive, ',&
				'and his girlfriend, later wife, Oriurora, could be a bit distant at times, but they ',&
				'were really great people. We became quick friends, kind of out of necessity, and ',&
				'it probably wasn''t the best idea in the end. Really though, he took me under his ',&
				'wing and taught me a great amount about governing."'
				print*, '(Psonus): "I''m not a fan of all the immortalizing stories about him that ',&
				'come out of Kathay, but really, he was a larger-than-life person and it really was ',&
				'great to serve alongside him. I hope that the two of them are at rest."'
				c6j_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 6k
		else if (my_dialogue_choice .EQ. '6k') then
			if (c6_already_picked .EQV. .true.) then
				print*, "You ask Psonus for her thoughts on the revolution."
				print*, '(Psonus): "It wasn''t nice. I was fighting against the friends that ',&
				'I had grown close to, and that wasn''t fun. When they realized that while they ',&
				'were leading a revolt for justice, the common people were fighting for ',&
				'independence, not to highlight tyranny. So they put their arms down and joined me. ',&
				'Of course, this realization didn''t happen until after poor Oriurora died; I was ',&
				'never a fan of the indiscriminate bombings, or any of the other horrible things ',&
				'we did, but it was ultimately not I who issued any of those orders."'
				print*, '(Psonus): "But I can''t help but think that Victor wanted independence for ',&
				'Kathay. We had a great deal of internal conflicts about it. We were able to come to ',&
				'compromises, but with him gone, things got out of control."'
				c6k_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 7
		else if (my_dialogue_choice .EQ. '7') then
			print*, "You ask for some more details on the Lizard War."
			print*, '(Psonus): "So I really like to think about the war as the start of my life. Throughout ',&
			'the conflict, I was the Imperial Representative for Kathay. Really, the war came out of nowhere. ',&
			'I had just accepted my appointment when I, and really all of us in the government, found out ',&
			'about the war. We spent something like a week here in the City preparing, and in my case I had to ',&
			'balance this with relocating myself to Kathay. Imagine showing up to work on the first day and ',&
			'being told that some foreign people were coming from the sky to bring war."'
			print*, '(Psonus): "But after that little aside, the war began. It went well enough at first. There ',&
			'were a few million Aurorans who saw deployment in Kathay and in the south of the Empire as a ',&
			'whole, and they really were the ones who held the line against the Banact, not so much our ',&
			'Imperial and Highlander troops. We got periodic visits from Aurora; I really don''t know ',&
			'where she was during the whole time she wasn''t with us, but I have pictures with her ',&
			'and the like. Anyways, after about a year and a half, they hold us that they were pulling ',&
			'out and returning to Aurora. They refused to give a reason why, and I think they left us with ',&
			'something like a hundred thousand men as the rest of them went back home. At this, the ',&
			'Highlands pulled out and went back home to prepare for the defence of their realm as we tried ',&
			'to pick up the pieces and survive as long as we could."'
			print*, '(Psonus): "The Banact broke through quite quickly after the evacuation, and they ',&
			'poured into Kathay. We withdrew our forces the best we could, getting as many out of the ',&
			'governorate as we could, and delaying the enemy as long as was possible, in the vain hope that ',&
			'the Aurorans would return. The Banact steadily took over allied territory, taking the great ',&
			'cities that many people called home, and, well, we''re still piecing together things ',&
			'about their mysterious administration."'
			print*, '(Psonus): "But there was no end to being pushed back; before we knew it, they had ',&
			'taken over most of the Empire south of the Highlands. The Banact didn''t stop there; they ',&
			'pushed into the Highlands, taking a significant portion of the east, and then pushing up to ',&
			'the Provinces. Same story here, but by this time, we got notifications of the Aurorans returning."'
			print*, '(Psonus): "And return they did. We don''t really have accurate numbers for their ',&
			'deployments, but the esimate goes as something over ten million deployed. The Aurorans, with ',&
			'us supporting them, won a decisive battle against the Banact, right outside the City, and from ',&
			'there, the Banact collapsed and we started the reconquest of our Empire."'
			print*, '(Psonus): "Within about a year, we reoccupied our old territory and were collapsing ',&
			'in on the remaining Banact forces. We captured those that we could and entrusted them ',&
			'to Aurora, who took them away. We declared victory, and here we are today."'
			c7_already_picked = .true.
		! 8
		else if (my_dialogue_choice .EQ. '8') then
			print*, "You ask for some more details about Kathay."
			print*, '(Psonus): "That''s a bit of a touchy subject. It''s quite a large area, and very populous. ',&
			'Its capital is Aurea, which is a nice and modern city. That being said, the entire region is heavily ',&
			'centralized in Aurea, and it''s the only large city in the area, which isn''t exactly ',&
			'uncommon in the Empire. I mean, I know practically everything about Aurea from before ',&
			'the rebellion, but things have changed a lot. Still, I''ll try to let you know about it ',&
			'nowadays."'
			print*, '(Psonus): "So, if I''m honest, Kathay is quite an oppressive and authoritarian state. ',&
			'It is ruled by the Kathay Military Party, which maintains a military dictatorship headed up ',&
			'by the Altec of Kathay: Renami Yandava. Kathay is a threat to the stability of the Empire, as they ',&
			'constantly fund and enflame tensions in our governorates. Even worse, they''re a nationalist ',&
			'state, so they try to enforce ethnic uniformity in Kathay, meaning that the populations of Imperials ',&
			'and Settlers, many of whom had lived there for generations uncountable, have found themselves ',&
			'expelled. This includes friends of mine, one of whom served in Kathay as a noble, even supporting ',&
			'their rebellion for some time, before finding him and his own family expelled for being Thurop."'
			print*, '(Psonus): "The Convent Auroran minority, however, stands as a stark contrast to the Kathaic ',&
			'majority. They initially supported the rebellion, but after realizing that life under the oppressive ',&
			'regime wasn''t all that it was made out to be, they quickly turned to us for support. We''ve ',&
			'given them it. They''ve been a thorn in Kathay''s side, as not only do they make a sizable ',&
			'portion of Aurea''s population, but they are also well over-represented in educated professions, ',&
			'and they hold a large portion of the tax base. Increasingly, they are causing social ',&
			'upheaval through protests, and supposedly starting riots."'
			print*, '(Psonus): "I would like to add that Kathay, especially Aurea, is a police state. I''m ',&
			'not entirely sure as to how far it pervades, but it''s a heavily patrolled area with outright ',&
			'draconian laws. When visiting, I strongly suggest not getting into more trouble than ',&
			'absolutely necessary, and they are not receptive to Imperial authority. Also, mind you, ',&
			'laws in Kathay are, on average, more strict than here in the Center State."'
			c8_already_picked = .true.
		! 8a
		else if (my_dialogue_choice .EQ. '8a') then
			if (c8_already_picked .EQV. .true.) then
				print*, "You ask about the laws that are more strict in Kathay than in the rest of ",&
				"the Empire."
				print*, '(Psonus): "Well, Kathay very much mirrors the Highlands in many ways, no ',&
				'doubt a trait brought by Victor Altec. They usually handle crime through imprisonment ',&
				'or execution. Mind you, imprisonment usually translates to indentured ',&
				'servitude to the state. They maintain codes that regulate foreign ethnicities, an ',&
				'unfortunate result of centuries of Imperial rule, and they retain notably strict ',&
				'moral codes; don''t drink if you''re under 25, obey curfew, don''t get caught with ',&
				'drugs, and so on. You''ve been warned, which is more than you''ll get from them."'
				c8a_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 9
		else if (my_dialogue_choice .EQ. '9') then
			print*, "You ask about how the postwar in the Empire."
			print*, '(Psonus): "It could certaintly be better. The Empire has been undergoing quite a ',&
			'bit of turmoil for one. Even though we came out victorious in the wawr, there was a lot of ',&
			'rebuilding that we had to do, and we''re still not done with it. We''ve been expected to ',&
			'reconstruct cities that we built over the centuries in just a few years. We''ve done our ',&
			'best, reviving the largest urban centers first, which is where most of our tax base is located, ',&
			'at a great cost to how our smaller cities see us. Mind you, the south of the Empire saw the ',&
			'most destruction, and even before the war it wasn''t the most finanically viable location. ',&
			'As a result, the rural areas down south hold a lot of anger at us."'
			print*, '(Psonus): "Of course, this is completely ignoring the Kathay Spring and the ',&
			'revolution in Thurop. Kathay saw a significant amount of development during the War; ',&
			'it came out of the fire better than it went in, and we were betting on Aurea staying in ',&
			'proper Imperial hands, so that Kathay could become as well developed as the Provinces. ',&
			'It''s on its way, no doubt, but we don''t have any real control over what the government ',&
			'does there. Aurea has the second largest industrial base in the Empire, the fourth ',&
			'largest in the world, and the rest of Kathay is one of the areas for agriculture. One ',&
			'of the largest economies on the Continent is a rogue, hostile state that commands a ',&
			'larger army than the Center State itself."'
			print*, '(Psonus): "On the other hand, we had the jewel of the South, Thurop, suffer ',&
			'a revolution that saw the Imperial government tossed out, and we were forced to ',&
			'recognize the new Naval Dictatorship as a legitimate Imperial subject. Mind you, the ',&
			'Governorate of Thurop was the primary naval power in the Empire, nad was the second ',&
			'largest economy in the Empire before the war, and we were very proud of it being a ',&
			'well-developed, modern land."'
			print*, '(Psonus): "In addition to these two things, we have to deal with our government in ',&
			'Fetedal facing increasing pressure to restore the theocracy to absolutism. That means that the ',&
			'spirit we''ve had subjugated there is looking to reclaim power and declare independence. As if ',&
			'all that wasn''t enough, we''re getting reports that there''s a growing fundamentally ',&
			'conservative section of the Cstphene population looking to impose a national state."'
			print*, '(Psonus): "Even better, the Highlands is eyeing up our weaknesses, and Foyer and his ',&
			'whole structure are certainly planning something. So, not going to lie, things aren''t going ',&
			'all too well outside of the true core of the Empire."'
			c9_already_picked = .true.
		! 9a
		else if (my_dialogue_choice .EQ. '9a') then
			if (c9_already_picked .EQV. .true.) then
				print*, "You ask Psonus if she is afraid of the Empire collapsing."
				print*, '(Psonus): "Look, I''m not afraid of the Empire losing territory as much ',&
				'as I am worried about what will happen if the aftermath of us leaving. If our ',&
				'authority falls apart, our former territories may devolve into unorganized, ',&
				'warring states akin to uncivilized societies, undoing centuries of effort on our ',&
				'part. And quite frankly, the two nations that have risen up have given us good ',&
				'reason to fear that our collapse would bring conflict. The collapse of the Empire ',&
				'alone is not to strike fear, but rather the wars that will be fought to control ',&
				'our legacy are to be feared."'
				print*, '(Psonus): "Don''t let me get you all scared. The Provinces of the Empire will ',&
				'survive unscathed. I just can''t make any guarantees beyond that."'
				c9a_already_picked = .true.
			else
				print*, "Invalid option."
			end if
		! 10
		else if (my_dialogue_choice .EQ. '10') then
			print*, 'You ask about the rewards you can expect.'
			print*, '(Psonus): "Upon the capture of this general, we will be open to any generous requests for rewards. ',&
			'This includes being exempt from filing taxes, handsome rewards in cash, goods, or property, assistance ',&
			'for immigration, and quite a bit more that you will be able to think of."'
			c10_already_picked = .true.
		! 11
		else if (my_dialogue_choice .EQ. '11') then
			print*, 'You ask about just open your means are to collect information.'
			print*, '(Psonus): "So long as you do not commit any particularly heinous crimes, we can ',&
			'expunge you of all crimes committed. Unfortunately, we cannot offer overt diplomatic immunity due ',&
			'to political complications with Kathay, though we can use our influence to get you set free in case of ',&
			'capture. Don''t take that has a sign to get yourself imprisoned all the time though."'
			c11_already_picked = .true.
		! 12
		else if (my_dialogue_choice .EQ. '12') then
			if (can_challenge_celene .EQV. .true.) then
				print*, "You challenge Psonus on her viewpoint of Kathay's rebellion."
				print*, '(Psonus): "Look, I was there for the whole thing, alright? I saw Victor ',&
				'get gunned down in front of me, I walked the streets that were turned into ',&
				'rubble by bombs, I met the heroes and the villians of both sides. You can''t ',&
				'convince me that I''m wrong."'
				c12_already_picked = .true.
			else
				print*, "Invalid option."
			end if	
		else
			print*, "Invalid option."
		end if
	end do
	
	print*, '(Psonus): "Well that''s all there is I suppose. Do the Empire proud. We won''t be in contact with one ',&
	'another, but maybe we''ll see one another once you get back."'
	print*, '(Psonus): "Now, someone will be arriving momentarily to take you to the City''s airbase. ',&
	'They''ll be here any moment."'
	
	



end subroutine dialogue_celene_psonus

end module celene_dialogue
