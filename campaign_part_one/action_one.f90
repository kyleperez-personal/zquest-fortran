module action_one
	use proper_cap ! lets you use the fixformat function
	use ansi_colors ! lets you use the color function
	use already_picked ! lets you use the picked function which colors text gray.
	
	implicit none
	
	CONTAINS
	
	! Enora the secratary's dialogue
	subroutine enora_dialogue(enora_dialogue_done)
		!! Input variables
		! None
		!! Output variables
		logical, intent(out) :: enora_dialogue_done
	
		!! temporary variables
		character(len=7) :: my_dialogue_choice
		logical :: dialogue_is_finished
		
		logical :: c1_already_picked, c1a_already_picked, c2_already_picked
		c1_already_picked = .false.
		c1a_already_picked = .false.
		c2_already_picked = .false.
	
		enora_dialogue_done = .false.
		dialogue_is_finished = .false.
	
		print*, "You go up to Enora at her desk. As you stand there, she looks up at you and seems to await a question."
		print*, "(HINT: type in the characters before the colon exactly how they appear.)"
		do while (dialogue_is_finished .EQV. .false.)
		
			print*, "What do you want to do?"
			if (c1_already_picked .EQV. .false.) then
				print*, "1: Ask why you are here."
			else
				print*, picked("1: Ask why you are here.")
			end if
			if (c1_already_picked .EQV. .true.) then
				if (c1a_already_picked .EQV. .false.) then
					print*, "1a: Prod more on why you are here."
				else
					print*, picked("1a: Prod more on why you are here.")
				end if
			end if
			if (c2_already_picked .EQV. .false.) then
				print*, "2: Ask Enora a personal question."
			else
				print*, picked("2: Ask Enora a personal question.")
			end if
			print*, "Exit: leave dialogue"
			read(*,*) my_dialogue_choice
			my_dialogue_choice = fixformat(my_dialogue_choice)
			
			if (my_dialogue_choice .EQ. 'Exit') then
				print*, "You walk away from Enora."
				dialogue_is_finished = .true.
			else if (my_dialogue_choice .EQ. '1') then
				print*, "You ask Enora why you were asked to be here."
				print*, '(Enora): "I''m not sure. All that I really know is that you''re supposed to be here."'
				c1_already_picked = .true.
			else if (my_dialogue_choice .EQ. '1a') then
				if (c1_already_picked .EQV. .true.) then
					print*, "You prod Enora more about why you are here."
					print*, '(Enora): "Alright. All I know is that you''re scheduled to see the Minister of State. ',&
					 'That''s all I know. Please just wait patiently until I''m allowed to escort you to her."'
					 c1a_already_picked = .true.
				else
					print*, "Invalid option."
				end if
			
			else if (my_dialogue_choice .EQ. '2') then
				print*, "You attempt to ask Enora about herself, but as soon as you ",&
				"loose a question, she interrupts you."
				print*, '(Enora): "Sorry, but I''m not supposed to talk while on the job."'
				c2_already_picked = .true.
			else
				print*, "Invalid option."
			end if
			
			if (c1_already_picked .EQV. .true.) then
				if (c1a_already_picked .EQV. .true.) then
					if (c2_already_picked .EQV. .true.) then
						print*, "Having nothing else to ask Enora, you walk away."
						enora_dialogue_done = .true.
						dialogue_is_finished = .true.
					end if
				end if
			end if
		end do
	
		end subroutine enora_dialogue
	
		! Anten the security guard's dialogue
		subroutine anten_dialogue(anten_dialogue_done)
			! Input Variables
			! None
			! Output Variables
			logical, intent(out) :: anten_dialogue_done
		
			!! temporary variables
			character(len=7) :: my_dialogue_choice
			logical :: dialogue_is_finished
		
			logical :: c1_already_picked
			c1_already_picked = .false.
	
			anten_dialogue_done = .false.
			dialogue_is_finished = .false.
			
			print*, "You go to approach Anten. Upon getting closer, he seems quite unwilling to talk,",&
			"but he manages to muster out the effort to speak."
			print*, '(Anten): "Sorry. Please address everything to the secratary Enora."'
			print*, "(HINT: type in the characters before the colon exactly how they appear.)"
			do while (dialogue_is_finished .EQV. .false.)
			print*, "What do you want to do?"
			if (c1_already_picked .EQV. .false.) then
				print*, "1: Bother him some more."
			else
				print*, picked("1: Bother him some more.")
			end if
			print*, "Exit: Leave Dialogue"
			
			read (*,*) my_dialogue_choice
			my_dialogue_choice = fixformat(my_dialogue_choice)
			
			if (my_dialogue_choice .EQ. 'Exit') then
				print*, "You walk away from Anten."
			else if (my_dialogue_choice .EQ. '1') then
				print*, "You both Anten a bit more, trying to get some kind of information out of him. In response, he speaks up."
				print*, '(Anten): "Please sit back down; there''s no need to start any issues."'
				c1_already_picked = .true.
			else
				print*, "Invalid Option"
			end if
			
			if (c1_already_picked) then
				print*, "Choosing to heed Anten, you walk away."
				anten_dialogue_done = .true.
				dialogue_is_finished = .true.
			end if
			
			end do
			
		end subroutine anten_dialogue
	
		! Centom of Renam's Dialogue
		subroutine centom_dialogue(player_is_nobility,centom_dialogue_done)
			! Input Variables
			logical, intent(in) :: player_is_nobility
			! Output Variables
			logical, intent(out) :: centom_dialogue_done
		
			!! temporary variables
			character(len=7) :: my_dialogue_choice
			logical :: dialogue_is_finished
		
			logical :: c1_already_picked
			logical :: c2_already_picked,c2a_already_picked
			logical :: c3_already_picked,c3a_already_picked
			logical :: centom_prodded_by_noble
			logical :: c3aba_already_picked, c3abaa_already_picked
			
			c1_already_picked = .false.
			c2_already_picked = .false.
			c2a_already_picked = .false.
			c3_already_picked = .false.
			c3a_already_picked = .false.
			centom_prodded_by_noble = .false.
			c3aba_already_picked = .false.
			c3abaa_already_picked = .false.
	
			centom_dialogue_done = .false.
			dialogue_is_finished = .false.
			
			print*, "You approach Centom of Renam. As you get closer, he looks up at you and waves his hand in a ",&
			"jovial fashion, clearly welcoming you."
			print*, '(Centom): "Good morning. Is there anything that I can help you with?"'
			print*, "(HINT: type in the characters before the colon exactly how they appear.)"	
			do while (dialogue_is_finished .EQV. .false.)
				print*, "What do you want to do?"
				! Dialogue Choices
				if (c1_already_picked .EQV. .false.) then
					print*, "1: Ask the man his name."
				else
					print*, picked("1: Ask the man his name.")
				end if
				if (c2_already_picked .EQV. .false.) then
					print*, "2: Ask him what he his doing here."
				else
					print*, picked("2: Ask him what he his doing here.")
				end if
				if (c2_already_picked .EQV. .true.) then
					if (c2a_already_picked .EQV. .false.) then
						print*, "2a: Ask for more details about what he is doing here."
					else
						print*, picked("2a: Ask for more details about what he is doing here.")
					end if
				end if
				if (c3_already_picked .EQV. .false.) then
					print*, "3: Ask him about himself."
				else
					print*, picked("3: Ask him about himself.")
				end if
				if (c3_already_picked .EQV. .true.) then
					if (c3a_already_picked .EQV. .false.) then
						print*, "3a: Prod for more information about himself."
					else
						print*, picked("3a: Prod for more information about himself.")
					end if
					! Can be either prodded by a noble or a non noble
				end if
				
				if (centom_prodded_by_noble .EQV. .true.) then ! note, non-alignment with normal conventions
					if (c3aba_already_picked .EQV. .false.) then
						print*, "3aa: Ask about his experiences in the army."
					else
						print*, picked("3aa: Ask about his experiences in the army.")
					end if
				end if	
				if (centom_prodded_by_noble .EQV. .true.) then
					if (c3aba_already_picked .EQV. .true.) then
						if (c3abaa_already_picked .EQV. .false.) then
							print*, "3aaa: Ask for more information about his time in the army."
						else
							print*, picked("3aaa: Ask for more information about his time in the army.")
						end if
					end if
				end if
				print*, "Exit: Leave Dialogue"
				!! End dialogue choices
			
				read (*,*) my_dialogue_choice
				my_dialogue_choice = fixformat(my_dialogue_choice)
			
				if (my_dialogue_choice .EQ. 'Exit') then
					print*, "You walk away from Centom."
					dialogue_is_finished = .true.
				else if (my_dialogue_choice .EQ. '1') then
					print*, "You ask the man his name."
					print*, '(Centom): "I am Centom of Renam."'
					c1_already_picked = .true.
				else if (my_dialogue_choice .EQ. '2') then
					print*, "You ask Centom what he is doing here."
					print*, '(Centom): "I have a meeting with one of the people who work in the Ministry of State. ',&
					'It''s not with the Minister herself, but that''s fine, as it''s not important enough to concern her."'
					c2_already_picked = .true.
				else if (my_dialogue_choice .EQ. '2a') then
					if (c2_already_picked .EQV. .true.) then
						print*, "You Centom for more details on why he is here."
						print*, '(Centom): "Sorry, but I can''t speak about it."'
						c2a_already_picked = .true.
					else
						print*, "Invalid option."
					end if
				else if (my_dialogue_choice .EQ. '3') then
					print*, "You ask Centom about just where he came from."
					print*, '(Centom): I shouldn''t say. There''s not really much of a need for me to ',&
					'go on about my past."'
					c3_already_picked = .true.
				else if (my_dialogue_choice .EQ. '3a') then
					if (c3_already_picked .EQV. .true.) then
						print*, "You push Centom for more details about his life, unsatisfied with the previous answer."
						if (player_is_nobility .EQV. .false.) then
							print*, '(Centom): "I''m just a small noble; it''s nothing important."'
							c3a_already_picked = .true.
						else
							print*, '(Centom): "Alright, for my fellow noble. I hail from Petral, where I serve ',&
							'as an admittedly unimpressive landholder. I delegate my duties to others so ',&
							'that I may serve in the Center State''s army."'
							c3a_already_picked = .true.
							centom_prodded_by_noble = .true. ! allows you to go and trigger additional dialogue options as a noble.
						end if
					else
						print*, "Invalid option."
					end if
				else if (my_dialogue_choice .EQ. '3aa') then
					if (centom_prodded_by_noble .EQV. .true. .and. c3a_already_picked .EQV. .true.) then
						print*, "You ask Centom for more details about his time in the army."
						print*, '(Centom): "Well, for one, I served in the Lizard War. I only served for about ',&
						'two years, and during that time I was stationed in Kathay. We were stationed on the border ',&
						'with the Great Desert and I fought alongside men sourced from all corners of the continent, ',&
						'not just from the Empire, but also from the Highlands and Aurora. Unfortunately, I found ',&
						'myself wounded during an attempted raid on a column of supplies trying to be delivered ',&
						'to the Lizard frontline. I got a shot square in my chest, and the world went black as I ',&
						'held onto life as I felt my chest burn and my heart grow weak. Suddenly, I awoke and ',&
						'found myself in a triage center a few kilometers back from the frontline, being treated. ',&
						'After that I was deemed unfit for frontline service and was sent back to the City to ',&
						'coordinate the response here. I''ve been serving here ever since."'
						c3aba_already_picked = .true.
					else
						print*, "Invalid option."
					end if
				else if (my_dialogue_choice .EQ. '3aaa') then
					if (c3aba_already_picked .EQV. .true.) then
						print*, "You ask Centom to tell more about his experiences in war."
						print*, '(Centom): "This isn''t about my experiences, but let me make it clear: I ',&
						'haven''t forgiven those vile lizards for their actions against the Empire. ',&
						'There are some of them who fled combat and have been living in the wild for ',&
						'twenty years, and I think that it''s our job to hunt them down and ',&
						'try them for their crimes against us."'
						c3abaa_already_picked = .true.
					else
						print*, "Invalid option."
					end if	
				else
					print*, "Invalid option."
				end if
			
				! conditions for finishing dialogue
				if (player_is_nobility .EQV. .true.) then
					if (c1_already_picked .EQV. .true.) then
						if (c2a_already_picked .EQV. .true.) then
							if (c3abaa_already_picked .EQV. .true.) then
								print*, "Having nothing else to ask Centom of Renam, you walk away."
								centom_dialogue_done = .true.
								dialogue_is_finished = .true.
							end if
						end if
					end if	
				else
					if (c1_already_picked .EQV. .true.) then
						if (c2a_already_picked .EQV. .true.) then
							if (c3a_already_picked.EQV. .true.) then
								print*, "Having nothing else to ask Centom of Renam, you walk away."
								centom_dialogue_done = .true.
								dialogue_is_finished = .true.
							end if
						end if
					end if	
				end if
			end do
		end subroutine centom_dialogue
		
		
		subroutine enora_hall_dialogue
			! Input variables
			! None
			! Output variables
			! None
			
			!! Temporary variables
			character(len=7) :: my_dialogue_choice
			logical :: dialogue_is_finished
			
			logical :: c1_already_picked,c2_already_picked
			
			dialogue_is_finished = .false.
			
			c1_already_picked = .false.
			c2_already_picked = .false.
			
			print*, "As you walk throughout the halls, following Enora, you decide to speak up and ask a question."
			
			do while (dialogue_is_finished .EQV. .false.)
				print*, "What do you want to do?"
				if (c1_already_picked .EQV. .false.) then
					print*, "1: Ask about the architecture and artwork of the building."
				else
					print*, picked("1: Ask about the architecture and artwork of the building.")
				end if
				if (c2_already_picked .EQV. .false.) then
					print*, "2: Ask about where you are headed to."
				else
					print*, picked("2: Ask about where you are headed to.")
				end if
				print*, "Exit: Leave Dialogue"
				
				read (*,*) my_dialogue_choice
				my_dialogue_choice = fixformat(my_dialogue_choice)
				
				if (my_dialogue_choice .EQ. 'Exit') then
					print*, "You decide to quit asking questions."
					dialogue_is_finished = .true.
				else if (my_dialogue_choice .EQ. '1') then
					print*, "You ask Enora about the particulars of the pictures and and the idea behind ",&
					"the architecture choices of the building."
					c1_already_picked = .true.
					print*, '(Enora): "I don''t think that I''m qualified to speak about this. I just work here."'
				else if (my_dialogue_choice .EQ. '2') then
					print*, "You ask Enora about where you are headed to."
					print*, '(Enora): "Come on, it''s just a few more moments until we get there. Just hold on."'
					c2_already_picked = .true.
				else
					print*, "Invalid option."
				end if
				
				if (c1_already_picked .EQV. .true.) then
					if (c2_already_picked .EQV. .true.) then
						print*, "You decide that if you're so close to where you are headed to, it's ",&
						"time to stop asking questions to Enora."
						dialogue_is_finished = .true.
					end if
				end if
			end do
			
		end subroutine enora_hall_dialogue


end module action_one
