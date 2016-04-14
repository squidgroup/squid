check_one_boolean <- function(obj, obj_name="", ...){
	if(!is.logical(obj) || !is.vector(obj) || length(obj) != 1)
			stop(paste(obj_name, "must be one logical."), call. = FALSE)
	
	return(obj)
} 