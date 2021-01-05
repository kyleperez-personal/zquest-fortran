module career_selector
	use proper_cap ! use fixformat function

	implicit none
	
	CONTAINS
	subroutine career_select(player_is_male,player_citizenship,player_is_in_holy_order,&
	player_career,player_is_doctor,player_is_military)
	!! Input variables
	logical, intent(in) :: player_is_male
	character(len=*), intent(in) :: player_citizenship
	logical, intent(in) :: player_is_in_holy_order
	
	!! Output variables
	character(len=*), intent(out) :: player_career
	logical, intent(out) :: player_is_doctor,player_is_military ! Assigns based on career.
	
	!! Temporary Variables
	character(len=5) :: do_i_want_career_info,my_career_is_okay
	character(len=20) :: job_i_want_to_know_about
	logical :: i_want_career_info,career_is_confirmed,no_more_career_info,career_is_selected
	

	
	
	i_want_career_info = .false.
	no_more_career_info = .false.
	career_is_selected = .false.
	career_is_confirmed = .false.
	
	do while (career_is_confirmed .EQV. .false.)
		
		print*, "Select a career from the following list:"
		!! Job List
		print*, "   Working Class:"
		print*, "Farmer"
		print*, "Laborer"
		print*, "Factory Worker"
		print*, "Driver"
		print*, "   Academic:"
		print*, "Student"
		print*, "Graduate Student"
		print*, "Professor"
		print*, "   Professional:"
		print*, "Doctor"
		print*, "Engineer"
		print*, "Archaeologist"
		print*, "Anthropologist"
		print*, "Historian"
		print*, "Linguist"
		print*, "Merchant"
		print*, "   Military and Law Enforcement:"
		print*, "Police"
		! If a woman is not a citizen of the Highlands or in a Holy Order, she cannot be in a military career other than a doctor.
		if ((player_is_male .EQV. .true.) .or. &
		(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
			print*, "Sailor"
			print*, "Airman"
			print*, "Infantry"
			print*, "Calvary"
			print*, "Ordinance"
			print*, "Medic"
			print*, "Sniper"
			print*, "Officer"
		end if
		print*, "Military Doctor"
		print*, "Covert Operations"
		
		! Asks if player wants to know about their career.
		do while (i_want_career_info .EQV. .false.)
			print*, "Do you want information about the career options? Yes or No?"
			read (*,*) do_i_want_career_info
			do_i_want_career_info = fixformat(do_i_want_career_info)
			if (do_i_want_career_info .EQ. 'Yes') then
				i_want_career_info = .true.
			else if (do_i_want_career_info .EQ. 'No') then
				print*, "Okay."
				exit
			else
				print*, "Try again."
			end if
		end do
		
		! If player wants to know about careers, this activates. Can also print job list.
		if (i_want_career_info .EQV. .true.) then
			do while (no_more_career_info .EQV. .false.)
				print*, "What career do you want to learn about?"
				print*, "Type Careers to see the list of careers again."
				print*, "Type Exit to stop learning."
				read (*,'(A)') job_i_want_to_know_about
				job_i_want_to_know_about = fixformat(job_i_want_to_know_about)
				if (job_i_want_to_know_about .EQ. 'Exit') then
					no_more_career_info = .true.
				else if (job_i_want_to_know_about .EQ. 'Careers') then
					!! Job List
					print*, "   Working Class:"
					print*, "Farmer"
					print*, "Laborer"
					print*, "Factory Worker"
					print*, "Driver"
					print*, "   Academic:"
					print*, "Student"
					print*, "Graduate Student"
					print*, "Professor"
					print*, "   Professional:"
					print*, "Doctor"
					print*, "Engineer"
					print*, "Archaeologist"
					print*, "Anthropologist"
					print*, "Historian"
					print*, "Linguist"
					print*, "Merchant"
					print*, "   Military and Law Enforcement:"
					print*, "Police"
					if ((player_is_male .EQV. .true.) .or. (player_citizenship .EQ. 'The Highlands' &
					.or. player_is_in_holy_order .EQV. .true.)) then
						print*, "Sailor"
						print*, "Airman"
						print*, "Infantry"
						print*, "Calvary"
						print*, "Ordinance"
						print*, "Medic"
						print*, "Sniper"
						print*, "Officer"
					end if
					print*, "Military Doctor"
					print*, "Covert Operations"
				else if (job_i_want_to_know_about .EQ. 'Farmer') then
					print*, "A Farmer is somewhat broad category that varies from being a subsistence farmer to running a small-scale ",&
					"farming operation. Such people usually come from rural areas, though occasionally urbanites pick ",&
					"up the career."
				else if (job_i_want_to_know_about .EQ. 'Laborer') then
					print*, "A Laborer is a broad classification for a group that works jobs that do not require university educations, ",&
					"but may be of various skill and pay. It can vary from being a grunt day laborer to being a skilled tradesman."
				else if (job_i_want_to_know_about .EQ. 'Factory Worker') then
					print*, "A Factory Worker works in one of the large industrial workplaces scattered across the ",&
					"world. While most jobs are unskilled, involving assembly line workplaces, some can be remarkably well paid."
				else if (job_i_want_to_know_about .EQ. 'Driver') then
					print*, "A Driver is someone who operates a vehicle for a living. For most it involves trucking loads ",&
					"of goods across the continent. Such people tend to be adept drivers."
				else if (job_i_want_to_know_about .EQ. 'Student') then
					print*, "A Student is a person who is currently attending an undergraduate institution. They come in many ",&
					"varieties and most certainly cannot all be grouped into a well-defined category."
				else if (job_i_want_to_know_about .EQ. 'Graduate Student') then
					print*, "A Graduate Student is a person who is currently attending graduate school. They have already obtained ",&
					"an undergraduate degree and are pursuing higher education. There are a variety of topics that one may ",&
					"choose to study."
				else if (job_i_want_to_know_about .EQ. 'Professor') then
					print*, "A Professor has obtained a graduate education and currently does research and teaches at ",&
					"a university. They are well-educated, intelligent, and most certainly have a variety of topics that they ",&
					"may choose to study."
				else if (job_i_want_to_know_about .EQ. 'Doctor') then
					print*, "A Doctor has obtained a graduate education in some type of medicine and works somewhere in ",&
					"the medical field. Experts on human anatomy and able to diagnose and treat sickness, they are vital ",&
					"to the health of a population."
				else if (job_i_want_to_know_about .EQ. 'Engineer') then
					print*, "An Engineer has an education that varies from an undergradute to a postgraduate education. ",&
					"They may work in a variety of subdisciplines and are often well-compensated and hold important practical skills."
				else if (job_i_want_to_know_about .EQ. 'Archaeologist') then
					print*, "An Archaeologist holds expertise examining artifacts from times past. They may hold interests in a ",&
					"variety of fields, but most have a graduate education and have a good survival skills."
				else if (job_i_want_to_know_about .EQ. 'Anthropologist') then
					print*, "An Anthropologist is an expert in the study of sentient beings. While most study humans, ",&
					"a select few study Banact and other more alien peoples. Some choose to study outside the standard ",&
					"Foyerian-Auroran people groups and are experts in the Orieni or Ghenima. Most tend to be well educated."
				else if (job_i_want_to_know_about .EQ. 'Historian') then
					print*, "A Historian is a scholar of times past, assembling past narratives and building them into ",&
					"something digestible and understandable by the people today. Many are well-educated and specialize in a ",&
					"variety of fields."
				else if (job_i_want_to_know_about .EQ. 'Merchant') then
					print*, "A Merchant practices the selling of goods. This is a diverse profession, ranging from operating ",&
					"a small business to owning an entire empire of businesses. Most Merchants tend to be well-balanced ",&
					"and independent individuals who can operate without external help."
				else if (job_i_want_to_know_about .EQ. 'Linguist') then
					print*, "A Linguist is an expert in languages. Most commonly, they are scholars of Foyerian Auroran, but ",&
					"some are experts in the Orieni or Banact languages, and are certainly of an impressive pedigree."
				else if (job_i_want_to_know_about .EQ. 'Police') then
					print*, "Someone who is a member of the Police has training and expertise in patrolling cities and ",&
					"enforcing laws on a largely unarmed civilian populace. Some patrol urban areas, while others police ",&
					"rural land. Often their location dictates their ability to interact with others."
				else if (job_i_want_to_know_about .EQ. 'Sailor') then
					if ((player_is_male .EQV. .true.) .or. &
					(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
						print*, "A Sailor is an enlisted member of the naval branch of some military. While many do not step ",&
						"into the ocean, many serve on ships while others stay on land. They tend to be experienced in the ",&
						"upkeep and maintainence of large ships and maintaining supplies to large armies. They also know how ",&
						"to work as a part of a team to accomplish greater goals."	
					else
						print*, "A woman is not allowed to serve in the Empire's armed forces."
					end if
				else if (job_i_want_to_know_about .EQ. 'Airman') then
					if ((player_is_male .EQV. .true.) .or. &
					(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
						print*, "An Airman is an enlisted member of the air branch of a military. Most do not fly aircraft, but are ",&
						"usually adept in the maintainence and logistics of aircraft, alongside a military discipline of working ",&
						"with others to accomplish great things."
					else
						print*, "A woman is not allowed to serve in the Empire's armed forces."
					end if
				else if (job_i_want_to_know_about .EQ. 'Infantry') then
					if ((player_is_male .EQV. .true.) .or. &
					(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
						print*, "A member of the Infantry has experience in armed land forces, and has training in small arms ",&
						"and basic explosives. Often trained to be a part of a larger team and coordinating with others, they ",&
						"serve in more than just national militaries, but also in smaller local forces and militia. They form ",&
						"the backbone of any fighting force."
					else
						print*, "A woman is not allowed to serve in the Empire's armed forces."
					end if
				else if (job_i_want_to_know_about .EQ. 'Calvary') then
					if ((player_is_male .EQV. .true.) .or. &
					(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
						print*, "A member of the Calvary has experience in operating military vehicles, be it driving, shooting ",&
						"or maintenence. The term is a holdover from an older time, but still a relevant description. Some are ",&
						"enlisted, others officers, and all work in a team to accomplish their goals."
					else
						print*, "A woman is not allowed to serve in the Empire's armed forces."
					end if
				else if (job_i_want_to_know_about .EQ. 'Ordinance') then
					if ((player_is_male .EQV. .true.) .or. &
					(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
						print*, "A member of the Ordinance sector of the military has military experience in handling explosives. This may ",&
						"span to assembling bombs, disarming them, operating cannons, and so on. In particular, most are well-versed ",&
						"on the safe handling of such dangerous weapons."
					else
						print*, "A woman is not allowed to serve in the Empire's armed forces."
					end if
				else if (job_i_want_to_know_about .EQ. 'Medic') then
					if ((player_is_male .EQV. .true.) .or. &
					(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
						print*, "A Medic is not a doctor, but a trained soldier with an additional expertise in treating ",&
						"battlefield injuries. While lacking the education of a true medical professional, they can certainly ",&
						"hold their own in helping others through basic treatment in addition to being able to perform admirably ",&
					"in combat."
					else
						print*, "A woman is not allowed to serve in the Empire's armed forces."
					end if	
				else if (job_i_want_to_know_about .EQ. 'Sniper') then
					if ((player_is_male .EQV. .true.) .or. &
					(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
						print*, "A Sniper is a specialist position, with special training in stealthily eliminating targets from ",&
						"after, often when in isolated positions in the midst of dangerous territory. Most have excellent perception ",&
						"and are skilled hunters of man."
					else
						print*, "A woman is not allowed to serve in the Empire's armed forces."
					end if
				else if (job_i_want_to_know_about .EQ. 'Officer') then
					if ((player_is_male .EQV. .true.) .or. &
					(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
						print*, "An Officer is a well-educated, prestigious position in the military. Most command others, but are ",&
						"themselves sandwiched in the command structure and report to others. They lead troops, coordinate logistics, ",&
						"administer bases, and represent their men and military."		
					else
						print*, "A woman is not allowed to serve in the Empire's armed forces."
					end if
				else if (job_i_want_to_know_about .EQ. 'Military Doctor') then
					if ((player_is_male .EQV. .true.) .or. &
					(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
						print*, "A Military Doctor serves as a trained doctor in a military. They possess the same qualifications ",&
						"as a civilian doctor, but with a small amount of military training and discipline instilled in them. Most ",&
						"are not as adept as working with civilians and instead are well aware of working with other military personel."
					else
						print*, "This is one of the few military careers open to women in the Empire."
						print*, "A Military Doctor serves as a trained doctor in a military. They possess the same qualifications ",&
						"as a civilian doctor, but with a small amount of military training and discipline instilled in them. Most ",&
						"are not as adept as working with civilians and instead are well aware of working with other military personel."
					end if
				else if (job_i_want_to_know_about .EQ. 'Covert Operations') then
					print*, "Someone involved in Covert Operations specializes in a more discrete form of armed service. Many are ",&
					"data scientists who sift through sensative data, while others are deployed in field service, where these spies ",&
					"are experts in infiltration, espionage, and detective work."
				!! End Job Descriptions
				else
					print*, "Try again."	
				end if
			end do
		end if
		
		! Selects career. Checks for typos.
		do while (career_is_selected .EQV. .false.)
			print*, "What career are you selecting?"
			read (*,'(A)') player_career
			player_career = fixformat(player_career)
			! Player careers
			if (player_career .EQ. 'Farmer') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Laborer') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Factory Worker') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Driver') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Student') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Graduate Student') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Professor') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Doctor') then
				player_is_doctor = .true. ! Doctors are considered Doctors
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Engineer') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Archaeologist') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Anthropologist') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Historian') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Merchant') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Linguist') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Police') then
				player_is_doctor = .false.
				player_is_military = .false.
				career_is_selected = .true.
			else if (player_career .EQ. 'Sailor') then
				if ((player_is_male .EQV. .true.) .or. &
				(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
					player_is_doctor = .false.
					player_is_military = .true.
					career_is_selected = .true.
				else
					print*, "A woman is not allowed to serve in the Empire's armed forces."
				end if
			else if (player_career .EQ. 'Airman') then
				if ((player_is_male .EQV. .true.) .or. &
				(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
					player_is_doctor = .false.
					player_is_military = .true.
					career_is_selected = .true.
				else
					print*, "A woman is not allowed to serve in the Empire's armed forces."
				end if
			else if (player_career .EQ. 'Infantry') then
				if ((player_is_male .EQV. .true.) .or. &
				(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
					player_is_doctor = .false.
					player_is_military = .true.
					career_is_selected = .true.
				else
					print*, "A woman is not allowed to serve in the Empire's armed forces."
				end if
			else if (player_career .EQ. 'Calvary') then
				if ((player_is_male .EQV. .true.) .or. &
				(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
					player_is_doctor = .false.
					player_is_military = .true.
					career_is_selected = .true.
				else
					print*, "A woman is not allowed to serve in the Empire's armed forces."
				end if
			else if (player_career .EQ. 'Ordinance') then
				if ((player_is_male .EQV. .true.) .or. &
				(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
					player_is_doctor = .false.
					player_is_military = .true.
					career_is_selected = .true.
				else
					print*, "A woman is not allowed to serve in the Empire's armed forces."
				end if
			else if (player_career .EQ. 'Medic') then
				if ((player_is_male .EQV. .true.) .or. &
				(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
					player_is_doctor = .false. ! medics are not considered doctors
					player_is_military = .true.
					career_is_selected = .true.
				else
					print*, "A woman is not allowed to serve in the Empire's armed forces."
				end if
			else if (player_career .EQ. 'Sniper') then
				if ((player_is_male .EQV. .true.) .or. &
				(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
					player_is_doctor = .false.
					player_is_military = .true.
					career_is_selected = .true.
				else
					print*, "A woman is not allowed to serve in the Empire's armed forces."
				end if
			else if (player_career .EQ. 'Officer') then
				if ((player_is_male .EQV. .true.) .or. &
				(player_citizenship .EQ. 'The Highlands' .or. player_is_in_holy_order .EQV. .true.)) then
					player_is_doctor = .false.
					player_is_military = .true.
					career_is_selected = .true.
				else
					print*, "A woman is not allowed to serve in the Empire's armed forces."
				end if
			else if (player_career .EQ. 'Military Doctor') then
				! this is the only path that a woman from the Empire who is not from a Holy Order can take to be in the military.
				player_is_doctor = .true. ! Military Doctors are considered Doctors
				player_is_military = .true.
				career_is_selected = .true.
			else if (player_career .EQ. 'Covert Operations') then
				player_is_doctor = .false.
				player_is_military = .false. ! Covert Operations are not considered military (although closely affiliated)
				career_is_selected = .true.
			! End careers
			else
				print*, "Try again."
			end if
		end do
		
		! Confirms player's career.
		do while (career_is_confirmed .EQV. .false.)
			! This IF statement prints the proper phrase for the player's career.
			if (player_career .EQ. 'Archaeologist') then
				print*, "You are an ",TRIM(player_career),"."
			else if (player_career .EQ. 'Anthropologist') then
				print*, "You are an ",TRIM(player_career),"."
			else if (player_career .EQ. 'Engineer') then
				print*, "You are an ",TRIM(player_career),"."
			else if (player_career .EQ. 'Officer') then
				print*, "You are an ",TRIM(player_career),"."
			else if (player_career .EQ. 'Airman') then
				print*, "You are an ",TRIM(player_career),"."
			else if (player_career .EQ. 'Ordinance') then
				print*, "You are an ",TRIM(player_career)," Specialist."
			else if (player_career .EQ. 'Police') then
				print*, "You serve in the Police."
			else if (player_career .EQ. 'Infantry') then
				print*, "You serve in the Infantry."
			else if (player_career .EQ. 'Calvary') then
				print*, "You serve in the Calvary."
			else if (player_career .EQ. 'Covert Operations') then
				print*, "You work in ",TRIM(player_career),"."
			else
				print*, "You are a ",TRIM(player_career),"."
			end if
			print*, "Is this okay? Yes or No."
			read(*,*) my_career_is_okay
			my_career_is_okay = fixformat(my_career_is_okay)
			if (my_career_is_okay .EQ. 'Yes') then
				career_is_confirmed = .true.
			else if (my_career_is_okay .EQ. 'No') then
				i_want_career_info = .false. !! Completely reset variables in question
				no_more_career_info = .false.
				career_is_selected = .false.
				print*, "Let's try again then."
				exit ! Exits loop without leaving overall do loop.
			else
				print*, "Try again."
			end if
		end do
	end do
	
	! If statement prints proper title.
	if (player_career .EQ. 'Archaeologist') then
		print*, "You are an ",TRIM(player_career),"."
	else if (player_career .EQ. 'Anthropologist') then
		print*, "You are an ",TRIM(player_career),"."
	else if (player_career .EQ. 'Engineer') then
		print*, "You are an ",TRIM(player_career),"."
	else if (player_career .EQ. 'Officer') then
		print*, "You are an ",TRIM(player_career),"."
	else if (player_career .EQ. 'Airman') then
		print*, "You are an ",TRIM(player_career),"."
	else if (player_career .EQ. 'Ordinance') then
		print*, "You are an ",TRIM(player_career)," Specialist."
	else if (player_career .EQ. 'Police') then
		print*, "You serve in the Police."
	else if (player_career .EQ. 'Infantry') then
		print*, "You serve in the Infantry."
	else if (player_career .EQ. 'Calvary') then
		print*, "You serve in the Calvary."
	else if (player_career .EQ. 'Covert Operations') then
		print*, "You work in ",TRIM(player_career),"."
	else
		print*, "You are a ",TRIM(player_career),"."
	end if
	end subroutine career_select
end module career_selector
