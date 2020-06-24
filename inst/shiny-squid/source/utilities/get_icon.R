# create an icon into input label
getIcon <- function(title="", placement="top", trigger="hover"){ 
  return(
  	tagList(HTML(paste0("<i class = 'fa fa-info-circle fa-fw info' 
  								  data-toggle='tooltip'
  									title='",title,"'
  									data-placement='",placement,"'
  									trigger='",trigger,"'></i>")))
  ) 
}