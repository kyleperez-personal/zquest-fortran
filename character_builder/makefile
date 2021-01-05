# Creating the executable
character_build : character_build.f90 \
		gender_select.f90 \
		age_select.f90 \
		race_select.f90 \
		location_select.f90 \
		holy_order_select.f90 \
		career_select.f90 \
		background_select.f90 \
		name_select.f90 \
		proper_cap.f90 \
		lower_cap.f90 \
		cap_functions.f90
	# calls gnu furtran, naming execultable followed by files used.
	gfortran -o character_build \
	character_build.f90 \
	gender_select.f90 \
	age_select.f90 \
	race_select.f90 \
	location_select.f90 \
	holy_order_select.f90 \
	career_select.f90 \
	background_select.f90 \
	name_select.f90 \
	proper_cap.f90 \
	lower_cap.f90 \
	cap_functions.f90 \
	ansi.f90
	# End file compilation list
	
#gender_select.o: gender_select.f90
#	gfortran -c gender_select.f90
	
#age_select.o: age_select.f90
#	gfortran -c age_select.f90
	
#race_select.o: race_select.f90
#	gfortran -c race_select.f90
	
#location_select.o: location_select.f90
#	gfortran -c location_select.f90
	
#holy_order_select.o: holy_order_select.f90
#	gfortran -c holy_order_select.f90
	
#career_select.o: career_select.f90
#	gfortran -c career_select.f90
	
#background_select.o: background_select.f90
#	gfortran -c background_select.f90
	
#name_select.o: name_select.f90
#	gfortran -c name_select.f90
	
#proper_cap.o: proper_cap.f90
#	gfortran -c proper_cap.f90
	
#lower_cap.o: lower_cap.f90
#	gfortran -c lower_cap.f90
	
#cap_functions.o: cap_functions.f90
#	gfortran -c cap_functions.f90

clean:
	rm character_build
