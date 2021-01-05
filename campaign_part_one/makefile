# Creating the executable
introduction : introduction.f90 \
		action_one.f90 \
		proper_cap.f90
	# calls gnu fortran, naming execultable followed by files used.
	gfortran -o introduction \
	introduction.f90 \
	action_one.f90 \
	celene_dialogue.f90 \
	proper_cap.f90 \
	ansi.f90 \
	already_picked.f90
	# End file compilation list
	
clean:
	rm introduction
