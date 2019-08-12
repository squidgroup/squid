# INCLUDE


#### Text ####
source("./source/text/portal/TXTportal.R",local=TRUE)
source("./source/text/modules/TXTmodule1.R",local=TRUE)
source("./source/text/modules/TXTmodule3.R",local=TRUE)
source("./source/text/modules/TXTmodule6.R",local=TRUE)
source("./source/text/fullmodel/TXTfullmodel.R",local=TRUE)

#### Inputs ####
source("./source/inputs/getSliderInput.R",local=TRUE)
source("./source/inputs/getEnvironmentInput.R",local=TRUE)
source("./source/inputs/getCheckboxInput.R",local=TRUE)
source("./source/inputs/getSelectInput.R",local=TRUE)

#### Utilities ####

source("./source/utilities/runningIndicator.R",local=TRUE)
source("./source/utilities/getLabel.R",local=TRUE)
source("./source/utilities/displayRCode.R",local=TRUE)
source("./source/utilities/citations.R",local=TRUE)

#### Pages ####
source("./source/pages/fullModel/UIenvironment.R",local=TRUE)
source("./source/pages/fullModel/UIspecificEnvironment.R",local=TRUE)


#### Bibliography #####
# bib             <- RefManageR::ReadBib("./inst/shiny-squid/source/bibliography/bibliography.bib")
bib             <- RefManageR::ReadBib("./source/bibliography/bibliography.bib")
bibPrintOptions <- list(bib.style = "authoryear", style = "html", first.inits = TRUE,
                        no.print.fields = c("ISSN", "URL", "ISBN", "file", "keywords", "mendeley-tags", "editor"))
bibCiteOptions  <- bibPrintOptions
bibPrintOptions[["max.names"]]   <- 10
bibCiteOptions[["max.names"]]    <- 2


######
c()