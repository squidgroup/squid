check_matrix <- function(obj, obj_name="", nb_col=NULL, nb_row=NULL, ...){
  if(!is.matrix(obj) || ncol(obj) != nb_col || nrow(obj) != nb_row || !is.numeric(obj))
    stop(paste(obj_name, "must be a numeric matrix with",nb_col,"columns and",nb_row,"rows."), call. = FALSE)
  return(obj)
} 