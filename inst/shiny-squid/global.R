library(shinyMatrix)
library(plotly)

#### Variables ####
source("./source/variables/general/var_general.R",local=TRUE)
source("./source/variables/fullmodel/var_fullmodel.R",local=TRUE)
source("./source/variables/modules/var_modules.R",local=TRUE)


#### Utilities ####
source("./source/utilities/all_msg.R",local=TRUE)
source("./source/utilities/get_table.R",local=TRUE)
source("./source/utilities/get_icon.R",local=TRUE)


#### Inputs ####
source("./source/inputs/get_numeric_input.R",local=TRUE)

#### ggplot theme ####

ggplot2::theme_set(ggplot2::theme_classic())