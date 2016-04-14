check_one_integer <- function(obj, obj_name="", minimum=NULL, maximum=NULL, ...){

	is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){  
		if(is.numeric(x)){
			return(abs(x - round(x)) < tol)
		}else{
			return(FALSE)
		}
		return(FALSE)
	}
	
	if(is.null(minimum) && is.null(maximum)){
		if(!is.wholenumber(obj) || !is.vector(obj) || length(obj) != 1)
			stop(paste(obj_name, "must be one integer."), call. = FALSE)
	}else{
		if(!is.null(minimum) && is.null(maximum)){
			if(!is.wholenumber(obj) || !is.vector(obj) || length(obj) != 1 || obj < minimum)
				stop(paste(obj_name, "must be one integer >= ",minimum,"."), call. = FALSE)
		}else{
			if(is.null(minimum) && !is.null(maximum)){
				if(!is.wholenumber(obj) || !is.vector(obj) || length(obj) != 1 || obj > maximum)
					stop(paste(obj_name, "must be one integer <= ",maximum,"."), call. = FALSE)
			}else{
				if(!is.null(minimum) && !is.null(maximum)){
					if(!is.wholenumber(obj) || !is.vector(obj) || length(obj) != 1 || obj < minimum || obj > maximum)
						stop(paste0(obj_name, " must be one integer between ",minimum," and ",maximum,"."), call. = FALSE)
				}
			}
		}
	}

	return(obj)
} 
