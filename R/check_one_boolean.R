# check_one_boolean: test if an input is one logical object
#
# Args:
#   obj:      the object to test.
#   obj_name: string; the name of the object to test.
#
# Returns:
#   the object inputed if it is one logical object otherwise generates an error.

check_one_boolean <- function(obj, obj_name="", ...){
	if(!is.logical(obj) || !is.vector(obj) || length(obj) != 1)
			stop(paste(obj_name, "must be one logical."), call. = FALSE)
	return(obj)
} 