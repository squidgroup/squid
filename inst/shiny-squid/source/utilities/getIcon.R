# create an icon into input label
getIcon <- function(id_){ 
  return(
    tagList(tags$i(class = "fa fa-info-circle fa-fw info", id=id_))
  ) 
}