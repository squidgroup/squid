check_one_numeric <- function(obj, obj_name="", minimum=NULL, maximum=NULL, ...){
	
	if(is.null(minimum) && is.null(maximum)){
		if(!is.numeric(obj) || !is.vector(obj) || length(obj) != 1)
			stop(paste(obj_name, "must be one numeric."), call. = FALSE)
	}else{
		if(!is.null(minimum) && is.null(maximum)){
			if(!is.numeric(obj) || !is.vector(obj) || length(obj) != 1 || obj < minimum)
				stop(paste(obj_name, "must be one numeric >= ",minimum,"."), call. = FALSE)
		}else{
			if(is.null(minimum) && !is.null(maximum)){
				if(!is.numeric(obj) || !is.vector(obj) || length(obj) != 1 || obj > maximum)
					stop(paste(obj_name, "must be one numeric <= ",maximum,"."), call. = FALSE)
			}else{
				if(!is.null(minimum) && !is.null(maximum)){
					if(!is.numeric(obj) || !is.vector(obj) || length(obj) != 1 || obj < minimum || obj > maximum)
						stop(paste0(obj_name, " must be one numeric between ",minimum," and ",maximum,"."), call. = FALSE)
				}
			}
		}
	}

	return(obj)
} 