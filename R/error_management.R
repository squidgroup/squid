error_management <- function(obj, obj_name="", type=NULL, minimum=NULL, maximum=NULL, nb_col=NULL, nb_row=NULL){
	
	obj_name <- paste0("input[[",obj_name,"]]")
	
	if(type == "check_one_boolean"){
		try(check_one_boolean(obj=obj, 
													obj_name=obj_name, 
													minimum=minimum, 
													maximum=maximum,
													longcalling, 
													expression, 
													but.not.seen.in.Error))
	}
	
	if(type == "check_one_numeric"){
		try(check_one_numeric(obj=obj, 
													obj_name=obj_name,
													minimum=minimum, 
													maximum=maximum,
													longcalling, 
													expression, 
													but.not.seen.in.Error))
	}
	
	if(type == "check_one_integer"){
		try(check_one_integer(obj=obj, 
													obj_name=obj_name,
													minimum=minimum, 
													maximum=maximum,
													longcalling, 
													expression, 
													but.not.seen.in.Error))
	}
	
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