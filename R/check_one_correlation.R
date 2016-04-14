check_one_correlation <- function(obj, obj_name="", ...){
	if(!is.numeric(obj) | length(obj) != 1 | obj < 0 | obj > 1)
		stop(paste(obj_name, "must be one numeric between 0 and 1."), call. = FALSE)
	
	return(obj)
} 