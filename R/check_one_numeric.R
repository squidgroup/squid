check_one_numeric <- function(obj, obj_name="", ...){
	if(!is.numeric(obj) || !is.vector(obj) || length(obj) != 1)
		stop(paste(obj_name, "must be one numeric."), call. = FALSE)
	
	return(obj)
} 