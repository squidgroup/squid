# INCLUDE

suppressPackageStartupMessages(library(data.table))


#### Server ####
source("./source/server/fullmodel/svr_full_model.R",local=TRUE)
source("./source/server/fullmodel/svr_dispay_model_equation.R",local=TRUE)
source("./source/server/fullmodel/svr_get_model_equation.R",local=TRUE)
source("./source/server/fullmodel/svr_get_vind_matrix.R",local=TRUE)
source("./source/server/fullmodel/svr_get_b_matrix.R",local=TRUE)
source("./source/server/fullmodel/svr_get_summary_variances.R",local=TRUE)
source("./source/server/fullmodel/svr_create_rcode.R",local=TRUE)

#### Utilities ####
source("./source/utilities/matrix_input_2.R",local=TRUE)
source("./source/utilities/matrix_input_b.R",local=TRUE)
source("./source/utilities/matrix_input_vind.R",local=TRUE)
source("./source/utilities/test_input.R",local=TRUE)
source("./source/utilities/test_integer.R",local=TRUE)
source("./source/utilities/test_input_b_matrix.R",local=TRUE)
source("./source/utilities/test_input_vind_matrix.R",local=TRUE)

source("./source/utilities/run_power_analysis.R",local=TRUE)
source("./source/utilities/multiplot.R",local=TRUE)
source("./source/utilities/cor2cov_matrix.R",local=TRUE)
source("./source/utilities/disable_action_button.R",local=TRUE)
source("./source/utilities/plot_design.R",local=TRUE)