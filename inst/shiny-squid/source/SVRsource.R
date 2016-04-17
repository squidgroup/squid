#################################################################
#################################################################
##################### INCLUDE PACKAGES ##########################
#################################################################
#################################################################

suppressPackageStartupMessages(library(data.table))

#### Variables ####
source("./source/variables/general/VARgeneral.R",local=TRUE)
source("./source/variables/fullmodel/VARfullmodel.R",local=TRUE)
source("./source/variables/modules/VARmodules.R",local=TRUE)

#### Server ####
source("./source/server/fullmodel/SVRFullModel.R",local=TRUE)
source("./source/server/fullmodel/SVRDispayModelEquation.R",local=TRUE)
source("./source/server/fullmodel/SVRGetModelEquation.R",local=TRUE)
source("./source/server/fullmodel/SVRGetVindMatrix.R",local=TRUE)
source("./source/server/fullmodel/SVRGetBMatrix.R",local=TRUE)
source("./source/server/fullmodel/SVRGetSummaryVariances.R",local=TRUE)

#### Utilities ####
source("./source/utilities/matrixInput2.R",local=TRUE)
source("./source/utilities/matrixInputB.R",local=TRUE)
source("./source/utilities/matrixInputVind.R",local=TRUE)
source("./source/utilities/testInput.R",local=TRUE)
source("./source/utilities/getIcon.R",local=TRUE)
source("./source/utilities/testInteger.R",local=TRUE)
source("./source/utilities/testInputBMatrix.R",local=TRUE)
source("./source/utilities/testInputVindMatrix.R",local=TRUE)
source("./source/utilities/getTable.R",local=TRUE)
source("./source/utilities/runPowerAnalysis.R",local=TRUE)
source("./source/utilities/error_msg.R",local=TRUE)
source("./source/utilities/multiplot.R",local=TRUE)
source("./source/utilities/Cor2CovMatrix.R",local=TRUE)
source("./source/utilities/disableActionButton.R",local=TRUE)

#### Inputs ####
source("./source/inputs/getNumericInput.R",local=TRUE)



