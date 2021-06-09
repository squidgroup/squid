# check_one_numeric: test if an input is one numeric object
#
# Args:
#   obj:      the object to test.
#   obj_name: string; the name of the object to test.
#   minimum:  the minimum value of the input value. If NULL the input does not have a minimum.
#   maximum:  the maximum value of the input value. If NULL the input does not have a maximum
#
# Returns:
#   the object inputed if it is one numeric object otherwise generates an error.

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


# check_one_integer: test if an input is one integer object
#
# Args:
#   obj:      the object to test.
#   obj_name: string; the name of the object to test.
#   minimum:  the minimum value of the input value. If NULL the input does not have a minimum.
#   maximum:  the maximum value of the input value. If NULL the input does not have a maximum.
#
# Returns:
#   the object inputed if it is one integer object otherwise generates an error.

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


# check_matrix: test if an input is a matrix object and if it has the right dimension
#
# Args:
#   obj:      the object to test.
#   obj_name: string; the name of the object to test.
#   nb_col:   integer; number of columns that the matrix should have.
#   nb_row:   integer; number of rows that the matrix should have.
#
# Returns:
#   the object inputed if it is a matrix otherwise generates an error.

check_matrix <- function(obj, obj_name="", nb_col=NULL, nb_row=NULL, ...){
  if(!is.matrix(obj) || ncol(obj) != nb_col || nrow(obj) != nb_row || !is.numeric(obj))
    stop(paste(obj_name, "must be a numeric matrix with",nb_col,"columns and",nb_row,"rows."), call. = FALSE)
  return(obj)
} 



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