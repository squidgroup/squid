check_one_variance <- function(obj, obj_name="", ...){
	if(!is.numeric(obj) || !is.vector(obj) || length(obj) != 1 || obj < 0)
		stop(paste(obj_name, "must be one positive numeric."), call. = FALSE)
	
	return(obj)
} 