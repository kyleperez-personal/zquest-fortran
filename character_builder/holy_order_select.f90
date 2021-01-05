module holy_order_selector
	use proper_cap ! lets you use the fixformat function
	
	implicit none
	
	CONTAINS
	subroutine holy_order_select(player_race,player_holy_order,player_is_in_holy_order)
	!! Inputs
	character(len=*), intent(in) :: player_race
	!! Holy Order outputs
	character(len=*), intent(out) :: player_holy_order
	logical, intent(out) :: player_is_in_holy_order
	
	!! Temporary variables
	character(len=5) :: i_want_to_be_in_an_order,i_want_to_know_more,is_my_order_choice_correct
	character(len=50) :: order_i_want_to_know_about
	logical :: i_want_holy_order_info,order_decision_is_confirmed
	
	
	
	i_want_holy_order_info = .false.
	order_decision_is_confirmed = .false.
	
	!! Asks if a party member wants to be a part of a Holy Order.
	!! If yes, then assign a role based on career.
	!! And add tag to player's background.
	do while (order_decision_is_confirmed .EQV. .false.)
		! Checking to see if player wants to be in an order.
		do while (order_decision_is_confirmed .EQV. .false.)
			print*, "Do you want to be a member of a Holy Order? Yes or No."
			print*, "Type Info if you want to know what a Holy Order is."
			read (*,*) i_want_to_be_in_an_order
			i_want_to_be_in_an_order = fixformat(i_want_to_be_in_an_order)
			if (i_want_to_be_in_an_order .EQ. 'Info') then
				print*, "A Holy Order is an organization that dedicates itself to servitude of a particular ",&
				"spirit. They are, however, entirely independent of spiritual authority and they are left to themselves ",&
				"to interpret the words of a spirit. These Orders can be found all across the Continent, with members coming ",&
				"from all walks of life to serve their most favored spirit in the form of voluntary service in a Holy Order."
				print*, "Holy Orders are headed by a Grandmaster who is responsible for acting as a diplomatic ",&
				"representative and the commander-in-chief of military forces. The Grandmaster is usually appointed ",&
				"by the Synod of Masters, which is where the vast majority of the Holy Order's power is located; composed of ",&
				"high-ranking members and military officials from all across the Order, the Synod of Masters passes ",&
				"legislation, approves purchases, and makes final judicial decisions and acts as the primary administrator of ",&
				"the Holy Order. Masters themselves are usually elected by a local group of Seras, or directly elected ",&
				"by local branches of a Holy Order."
				print*, "Most Holy Orders are extremely wealthy organizations, owning territory, businesses, and charities within ",&
				"the sovereign territory of other realms. These Holy Orders share legal authority in the territory that they own."
				print*, "Holy Orders are officially recognized by all realms on the Continent, and have the rights to ",&
				"issue their own currency, legislate and enforce laws on their shared territory, and operate independent ",&
				"military forces and deploy them as they see necessary."
				print*, "The Holy Orders maintain garrisons, meeting places, businesses, and religious sites in the ",&
				"major spiritual centers of the Empire and the Highlands, and are often seen as a welcome sight to the ",&
				"realms of the land, who enjoy having militaristic orders ready to serve as mercenary troops."
			else if (i_want_to_be_in_an_order .EQ. 'Yes') then
				print*, "You will be in a Holy Order."
				player_is_in_holy_order = .true.
				exit
			else if (i_want_to_be_in_an_order .EQ. 'No') then
				print*, "You won't be in a Holy Order"
				player_is_in_holy_order = .false.
				player_holy_order = 'None'
				exit
			else
				print*, "Try again."
			end if
		end do
		
		! If player wants to be in an order, they can choose the order and learn about them.
		if (player_is_in_holy_order .EQV. .true.) then
			print*, "List of Holy Orders:"
			print*, "The Holy Order of the Commander of Weapons"
			print*, "The Holy Order of the Eternal Lady of Weapons"
			print*, "The Holy Order of the Queen of the Ice"
			print*, "The Holy Order of the Golden Warrior"
			print*, "The Holy Order of the Spirit of Life"
			print*, "The Holy Order of the Aspect of Fire"
			print*, "The Holy Order of the Practitioner of Discipline"
			print*, "The Holy Order of the Fair Essence"
			!! Add more that come up.
			! Asks if player wants to know more about individual Holy Orders
			do while (i_want_holy_order_info .EQV. .false.)
				print*, "Do you want any information about the Holy Orders? Yes or No."
				read (*,*) i_want_to_know_more
				i_want_to_know_more = fixformat(i_want_to_know_more)
				if (i_want_to_know_more .EQ. 'Yes') then
					print*, "Let's learn more then."
					i_want_holy_order_info = .true.
				else if (i_want_to_know_more .EQ. 'No') then
					print*, "Alright."
					exit
				else
					print*, "Try again."
				end if
			end do
		end if
		
		! If a player is in a Holy Order and wants to know more, this triggers.
		! Placed outside of the original if statement to make everything look nicer.
		if (i_want_holy_order_info .EQV. .true.) then
			do while (i_want_holy_order_info .EQV. .true.)
				print*, "Which Holy Order would you like to learn about?"
				print*, "Type in its full name or its patron spirit to see information."
				print*, "Type 'Orders' for the list of Holy Orders."
				print*, "Type Exit to stop learning."
				read(*,'(A)') order_i_want_to_know_about
				order_i_want_to_know_about = fixformat(order_i_want_to_know_about)
				if (order_i_want_to_know_about .EQ. 'Exit') then
					i_want_holy_order_info = .false.
				else if (order_i_want_to_know_about .EQ. 'Orders') then
					print*, "List of Holy Orders:"
					print*, "The Holy Order of the Commander of Weapons"
					print*, "The Holy Order of the Eternal Lady of Weapons"
					print*, "The Holy Order of the Queen of the Ice"
					print*, "The Holy Order of the Golden Warrior"
					print*, "The Holy Order of the Spirit of Life"
					print*, "The Holy Order of the Aspect of Fire"
					print*, "The Holy Order of the Practitioner of Discipline"
					print*, "The Holy Order of the Fair Essence"
					!! Add more that come up.
				! Order Info Section
				else if (order_i_want_to_know_about .EQ. 'The Holy Order of the Commander of Weapons') then
					print*, "Patron Spirit: Foyer"
					print*, "The Holy Order of the Commander of Weapons is among the most popular and most powerful ",&
					"Holy Orders. The members primarily come from either the Highlands or the Imperial provinces, with ",&
					"few who come from the southern Empire. This Order is well known for its militarism, and it retains ",&
					"among the most elite military forces among not just the Orders, but in the entire world."
					print*, "Headquarters: The City of the Eternal Warlord, Imperial Center State (secondary ",&
					"headquarters are located in the City of Foyer in the Highlands.)"
					print*, "Members: 434,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'Foyer') then
					print*, "Patron Spirit: Foyer"
					print*, "The Holy Order of the Commander of Weapons is among the most popular and most powerful ",&
					"Holy Orders. The members primarily come from either the Highlands or the Imperial provinces, with ",&
					"few who come from the southern Empire. This Order is well known for its militarism, and it retains ",&
					"among the most elite military forces among not just the Orders, but in the entire world."
					print*, "Headquarters: The City of the Eternal Warlord, Imperial Center State (secondary ",&
					"headquarters are located in the City of Foyer in the Highlands.)"
					print*, "Members: 434,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'The Holy Order of the Eternal Lady of Weapons') then
					print*, "Patron Spirit: Eternity"
					print*, "The Holy Order of the Eternal Lady of Weapons often comes into soft conflict with the Holy Order ",&
					"of the Commander of Weapons due to the spiritual tensions between Foyer and Eternity; this often ",&
					"bleeds into the Orders' relationships. Members of this Holy Order come from all around the continent, ",&
					"with a suspiciously large number from the Highlands. This Order houses a small military force that often ",&
					"sees service all around the southern Empire."
					print*, "Headquarters: The City of the Eternal Warlord, Imperial Center State"
					print*, "Members: 212,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'Eternity') then
					print*, "Patron Spirit: Eternity"
					print*, "The Holy Order of the Eternal Lady of Weapons often comes into soft conflict with the Holy Order ",&
					"of the Commander of Weapons due to the spiritual tensions between Foyer and Eternity; this often ",&
					"bleeds into the Orders' relationships. Members of this Holy Order come from all around the continent, ",&
					"with a suspiciously large number from the Highlands. This Order houses a small military force that often ",&
					"sees service all around the southern Empire."
					print*, "Headquarters: The City of the Eternal Warlord, Imperial Center State"
					print*, "Members: 212,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'The Holy Order of the Queen of the Ice') then
					print*, "Patron Spirit: Aurora"
					print*, "The Holy Order of the Queen of the Ice is without a doubt the largest, most powerful, and ",&
					"richest Holy Order. The ranks of the Order are dominated by Convent Aurorans, both exiles and communed. ",&
					"Still, there is a notable minority of members who come from other races. This Holy Order retains an ",&
					"awkward state of communion with the Foyer-Auroran pantheon due to the inherent religious differences ",&
					"between Convent Aurorans and other Foyerian Aurorans, but the Order still tenuously agrees to be in ",&
					"proper communion with the pantheon unless given direction from Aurora herself."
					print*, "Headquarters: The City of the Eternal Warlord, Imperial Center State"
					print*, "Members: 781,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'Aurora') then
					print*, "Patron Spirit: Aurora"
					print*, "The Holy Order of the Queen of the Ice is without a doubt the largest, most powerful, and ",&
					"richest Holy Order. The ranks of the Order are dominated by Convent Aurorans, both exiles and communed. ",&
					"Still, there is a notable minority of members who come from other races. This Holy Order retains an ",&
					"awkward state of communion with the Foyer-Auroran pantheon due to the inherent religious differences ",&
					"between Convent Aurorans and other Foyerian Aurorans, but the Order still tenuously agrees to be in ",&
					"proper communion with the pantheon unless given direction from Aurora herself."
					print*, "Headquarters: The City of the Eternal Warlord, Imperial Center State"
					print*, "Members: 781,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'The Holy Order of the Golden Warrior') then
					print*, "Patron Spirit: Vility"
					print*, "The Holy Order of the Golden Warrior is dominated by Highlanders and is one of the few Holy Orders ",&
					"that is based out of the Highlands. This Order usually cooperates closely with the Holy Order of ",&
					"the Spirit of Life, given their common location in the Highlands, alongside a more distant cooperation ",&
					"with the more distant Orders dedicated to spirit with a close relationship to Vility."
					print*, "Headquarters: City of Vility, Province of Vility, the Highlands"
					print*, "Members: 351,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'Vility') then
					print*, "Patron Spirit: Vility"
					print*, "The Holy Order of the Golden Warrior is dominated by Highlanders and is one of the few Holy Orders ",&
					"that is based out of the Highlands. This Order usually cooperates closely with the Holy Order of ",&
					"the Spirit of Life, given their common location in the Highlands, alongside a more distant cooperation ",&
					"with the more distant Orders dedicated to spirit with a close relationship to Vility."
					print*, "Headquarters: City of Vility, Province of Vility, the Highlands"
					print*, "Members: 351,000"
				else if (order_i_want_to_know_about .EQ. 'The Holy Order of the Spirit of Life') then
					print*, "Patron Spirit: Zerixa"
					print*, "The Holy Order of the Spirit of Life is traditionally dominated by Highlanders, in ",&
					"particular Orieni Highlanders, but an increasing number of members come from beyond the Highlands. This ",&
					"Holy Order is home to a sizable medical contingent and is second only to Aurora's Order in terms of ",&
					"hospitals and other medical facilities in Order ownership. Zerixa's Order completely lacks a formal ",&
					"military component, and instead has a substitute, informal militia. Indeed, while many other Holy Orders ",&
					"receive payment to deploy military forces, The Holy Order of the Spirit of Life often sees payment for ",&
					"deploying their medical teams around the world."
					print*, "Headquarters: City of Zerixa, Province of Zarata, the Highlands"
					print*, "Members: 198,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'Zerixa') then
					print*, "Patron Spirit: Zerixa"
					print*, "The Holy Order of the Spirit of Life is traditionally dominated by Highlanders, in ",&
					"particular Orieni Highlanders, but an increasing number of members come from beyond the Highlands. This ",&
					"Holy Order is home to a sizable medical contingent and is second only to Aurora's Order in terms of ",&
					"hospitals and other medical facilities in Order ownership. Zerixa's Order completely lacks a formal ",&
					"military component, and instead has a substitute, informal militia. Indeed, while many other Holy Orders ",&
					"receive payment to deploy military forces, The Holy Order of the Spirit of Life often sees payment for ",&
					"deploying their medical teams around the world."
					print*, "Headquarters: City of Zerixa, Province of Zarata, the Highlands"
					print*, "Members: 198,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'The Holy Order of the Aspect of Fire') then
					print*, "Patron Spirit: Farar"
					print*, "The Holy Order of the Aspect of Fire is indeed an internationally recognized Holy Order, but ",&
					"its isolated location, along with its charter coming from a definitively regional spirit, means that it ",&
					"has a lower member count than all of the major Holy Orders. The ranks of the Order are dominated by Fetan, ",&
					"with a small Imperial contingent; this is due to the fact that Farar is the well-known patron spirit of ",&
					"Fetedal and the Fetan in general. This Holy Order largely sees its deployment in the Highlands, which keeps ",&
					"them away from their fiery patron and away from the prying eyes of the Imperial state, who look to curb the ",&
					"power of this Order due to their support for an absolute theocracy in Fetedal, which directly counters ",&
					"Imperial authority."
					print*, "Headquarters: Psonus, Governorate of Fetedal"
					print*, "Members: 43,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'Farar') then
					print*, "Patron Spirit: Farar"
					print*, "The Holy Order of the Aspect of Fire is indeed an internationally recognized Holy Order, but ",&
					"its isolated location, along with its charter coming from a definitively regional spirit, means that it ",&
					"has a lower member count than all of the major Holy Orders. The ranks of the Order are dominated by Fetan, ",&
					"with a small Imperial contingent; this is due to the fact that Farar is the well-known patron spirit of ",&
					"Fetedal and the Fetan in general. This Holy Order largely sees its deployment in the Highlands, which keeps ",&
					"them away from their fiery patron and away from the prying eyes of the Imperial state, who look to curb the ",&
					"power of this Order due to their support for an absolute theocracy in Fetedal, which directly counters ",&
					"Imperial authority."
					print*, "Headquarters: Psonus, Governorate of Fetedal"
					print*, "Members: 43,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'The Holy Order of the Practitioner of Discipline') then
					print*, "Patron Spirit: Anylsa"
					print*, "The Holy Order of the Practitioner of Discipline is quite fitting for an Order dedicated to an ",&
					"elemental, being small in comparison to the major Orders. Still, its ranks are filled mostly by the ",&
					"many coastal peoples, with a select few Imperials. This small Order is well known for its notably ",&
					"more conservative and outright prohibitionist politics, which it officially professes as its ",&
					"interpretation of Anylsan Discipline. This Order is also known for its martial arts, which were famous ",&
					"in ancient times, but nowadays are little other than a curiousity of the modern world."
					print*, "Headquarters: City of Anylsa, Governorate of Entrana"
					print*, "Members: 61,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'Anylsa') then
					print*, "Patron Spirit: Anylsa"
					print*, "The Holy Order of the Practitioner of Discipline is quite fitting for an Order dedicated to an ",&
					"elemental, being small in comparison to the major Orders. Still, its ranks are filled mostly by the ",&
					"many coastal peoples, with a select few Imperials. This small Order is well known for its notably ",&
					"more conservative and outright prohibitionist politics, which it officially professes as its ",&
					"interpretation of Anylsan Discipline. This Order is also known for its martial arts, which were famous ",&
					"in ancient times, but nowadays are little other than a curiousity of the modern world."
					print*, "Headquarters: City of Anylsa, Governorate of Entrana"
					print*, "Members: 61,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'The Holy Order of the Fair Essence') then
					print*, "Patron Spirit: Enteie"
					print*, "The Holy Order of the Fair Essence is unique for its practice of legalism as a charitable service. ",&
					"The Order operates one of the world's most renown law schools, and the Order itself has the largest ",&
					"ratio of lawyers in its rank. These lawyers are often deployed to assist the poor as charity or ",&
					"to support other Holy Orders in legal battles. The Order is composed mostly of Imperials and Convent Aurorans ",&
					"and is one of the few Holy Orders that a Convent Auroran can join without becoming an exile, due to the ",&
					"Convent Auroran origins of Enteie and official recognition of this from Aurora herself."
					print*, "Headquarters: Novglaitier, Governorate of Ralaer"
					print*, "Members: 58,000"
					! print*, "Founded: DATE"
				else if (order_i_want_to_know_about .EQ. 'Enteie') then
					print*, "Patron Spirit: Enteie"
					print*, "The Holy Order of the Fair Essence is unique for its practice of legalism as a charitable service. ",&
					"The Order operates one of the world's most renown law schools, and the Order itself has the largest ",&
					"ratio of lawyers in its rank. These lawyers are often deployed to assist the poor as charity or ",&
					"to support other Holy Orders in legal battles. The Order is composed mostly of Imperials and Convent Aurorans ",&
					"and is one of the few Holy Orders that a Convent Auroran can join without becoming an exile, due to the ",&
					"Convent Auroran origins of Enteie and official recognition of this from Aurora herself."
					print*, "Headquarters: Novglaitier, Governorate of Ralaer"
					print*, "Members: 58,000"
					! print*, "Founded: DATE"
				! End Order Info
				else
					print*, "Try again."
				end if
			end do
		end if
		
		! Pick the Order
		if (player_is_in_holy_order .EQV. .true.) then
			do while (player_is_in_holy_order .EQV. .true.)
				print*, "What Holy Order do you want to join?"
				print*, "Type in the Full Name of the Order or its patron spirit."
				print*, "Type Orders to see what orders are availible."
				read (*,'(A)') player_holy_order
				player_holy_order = fixformat(player_holy_order)
				if (player_holy_order .EQ. 'Orders') then
					print*, "List of Holy Orders:"
					print*, "The Holy Order of the Commander of Weapons"
					print*, "The Holy Order of the Eternal Lady of Weapons"
					print*, "The Holy Order of the Queen of the Ice"
					print*, "The Holy Order of the Golden Warrior"
					print*, "The Holy Order of the Spirit of Life"
					print*, "The Holy Order of the Aspect of Fire"
					print*, "The Holy Order of the Practitioner of Discipline"
					print*, "The Holy Order of the Fair Essence"
				else if (player_holy_order .EQ. 'The Holy Order of the Commander of Weapons') then
					exit
				else if (player_holy_order .EQ. 'Foyer') then
					player_holy_order = 'The Holy Order of the Commander of Weapons'
					exit
				else if (player_holy_order .EQ. 'The Holy Order of the Eternal Lady of Weapons') then
					exit
				else if (player_holy_order .EQ. 'Eternity') then
					player_holy_order = 'The Holy Order of the Eternal Lady of Weapons'
					exit
				else if (player_holy_order .EQ. 'The Holy Order of the Queen of the Ice') then
					exit
				else if (player_holy_order .EQ. 'Aurora') then
					player_holy_order = 'The Holy Order of the Queen of the Ice'
					exit
				else if (player_holy_order .EQ. 'The Holy Order of the Golden Warrior') then
					exit
				else if (player_holy_order .EQ. 'Vility') then
					player_holy_order = 'The Holy Order of the Golden Warrior'
					exit
				else if (player_holy_order .EQ. 'The Holy Order of the Spirit of Life') then
					exit
				else if (player_holy_order .EQ. 'Zerixa') then
					player_holy_order = 'The Holy Order of the Spirit of Life'
					exit
				else if (player_holy_order .EQ. 'The Holy Order of the Aspect of Fire') then
					exit
				else if (player_holy_order .EQ. 'Farar') then
					player_holy_order = 'The Holy Order of the Aspect of Fire'
					exit
				else if (player_holy_order .EQ. 'The Holy Order of the Practitioner of Discipline') then
					exit
				else if (player_holy_order .EQ. 'Anylsa') then
					player_holy_order = 'The Holy Order of the Practitioner of Discipline'
					exit
				else if (player_holy_order .EQ. 'The Holy Order of the Fair Essence') then
					exit
				else if (player_holy_order .EQ. 'Enteie') then
					player_holy_order = 'The Holy Order of the Fair Essence'
					exit
				else
					print*, "Try again."
				end if
			end do
		end if
		
		! Confirmation
		do while (order_decision_is_confirmed .EQV. .false.)
			if (player_is_in_holy_order .EQV. .false.) then
				print*, "You will not be a member of a Holy Order."
			else
				print*, "You will be a member of ",TRIM(player_holy_order),"."
				! Checks to see if the player is a Convent Auroran.
				! Convent Aurorans may face exile if they don't serve a special set of spirits.
				if (player_race .EQ. 'Convent Auroran') then
					if (player_holy_order .EQ. 'The Holy Order of the Queen of the Ice') then
						print*, "Your Convent Auroran community will approve of your choice."
					else if (player_holy_order .EQ. 'The Holy Order of the Fair Essence') then
						print*, "Your Convent Auroran community will accept your decision."
					else
						print*, "This choice will make you a Convent Auroran Exile!" ! If the player goes through with the choice, they will be made an exile elsewhere.
					end if
				end if
			end if
			print*, "Is this okay? Yes or No."
			read (*,*) is_my_order_choice_correct
			is_my_order_choice_correct = fixformat(is_my_order_choice_correct)
			if (is_my_order_choice_correct .EQ. 'Yes') then
				if (player_is_in_holy_order .EQV. .false.) then
					print*, "You will not be in a Holy Order."
					order_decision_is_confirmed = .true.
				else
					print*, "You are a member of ",TRIM(player_holy_order),"."
					order_decision_is_confirmed = .true.
				end if
			else if (is_my_order_choice_correct .EQ. 'No') then
				print*, "Let's try again then."
				exit
			else
				print*, "Try again."
			end if
		end do
	end do
	end subroutine holy_order_select
end module holy_order_selector
