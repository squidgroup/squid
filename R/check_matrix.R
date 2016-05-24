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