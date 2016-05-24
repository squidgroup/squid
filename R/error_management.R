# error_management: apply the right check method to test if input has the right format.
#
# Args:
#   obj:      the object to test.
#   obj_name: string; the name of the object to test.
#   type:     
#   minimum:  the minimum value of the input value. If NULL the input does not have a minimum.
#   maximum:  the maximum value of the input value. If NULL the input does not have a maximum.
#   nb_col:   integer; number of columns that the matrix should have.
#   nb_row:   integer; number of rows that the matrix should have.
#
# Returns:
#   the object inputed if it is one positively checked otherwise generates an error.

error_management <- function(obj, obj_name="", type=NULL, minimum=NULL, maximum=NULL, nb_col=NULL, nb_row=NULL){
	
  longcalling<-but.not.seen.in.Error<-NULL
  
	obj_name <- paste0("input[[",obj_name,"]]")
	
	# check if input if one boolean value
	if(type == "check_one_boolean"){
		try(check_one_boolean(obj=obj, 
													obj_name=obj_name, 
													minimum=minimum, 
													maximum=maximum,
													longcalling, 
													expression, 
													but.not.seen.in.Error))
	}
	
	# check if input is one numeric value
	if(type == "check_one_numeric"){
		try(check_one_numeric(obj=obj, 
													obj_name=obj_name,
													minimum=minimum, 
													maximum=maximum,
													longcalling, 
													expression, 
													but.not.seen.in.Error))
	}
	
	# check if input is one integer value
	if(type == "check_one_integer"){
		try(check_one_integer(obj=obj, 
													obj_name=obj_name,
													minimum=minimum, 
													maximum=maximum,
													longcalling, 
													expression, 
													but.not.seen.in.Error))
	}
	
	# check if input is a matrix object
	if(type == "check_matrix"){
	  try(check_matrix(obj=obj, 
                     obj_name=obj_name,
                     nb_col=nb_col, 
	                   nb_row=nb_row,
                     longcalling, 
                     expression, 
                     but.not.seen.in.Error))
	}
	
	
	return(obj)
}