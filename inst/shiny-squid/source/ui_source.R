# INCLUDE

#### Text ####
source("./source/text/portal/txt_portal.R",local=TRUE)
source("./source/text/modules/txt_module1.R",local=TRUE)
source("./source/text/modules/txt_module3.R",local=TRUE)
source("./source/text/modules/txt_module6.R",local=TRUE)
source("./source/text/fullmodel/txt_full_model.R",local=TRUE)

#### Inputs ####
source("./source/inputs/get_slider_input.R",local=TRUE)
source("./source/inputs/get_environment_input.R",local=TRUE)
source("./source/inputs/get_checkbox_input.R",local=TRUE)
source("./source/inputs/get_select_input.R",local=TRUE)

#### Utilities ####

source("./source/utilities/running_indicator.R",local=TRUE)
source("./source/utilities/get_label.R",local=TRUE)
source("./source/utilities/display_rcode.R",local=TRUE)
source("./source/utilities/citations.R",local=TRUE)

#### Pages ####
source("./source/pages/full_model/ui_environment.R",local=TRUE)
source("./source/pages/full_model/ui_specific_environment.R",local=TRUE)

######
c()