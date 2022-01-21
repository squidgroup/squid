## Test environments
* local Windows 10, R 4.1.2
* win-builder (devel and release)
* Fedora Linux, R-devel, clang, gfortran
* macOS 10.13.6 High Sierra, R-release, brew


## R CMD check results
There were no ERRORs or WARNINGs. 

There are 2 NOTES:
1- The size of the directory (./inst/shiny-squid/) that stores the shinyapp is larger than 1Mb.
2- The packages (‘arm’ ‘brms’ ‘lme4’ ‘plotly’ ‘shinyMatrix’) are only used in the shinyapp.
These notes are related to the shinyapp that our package includes. We therefore conclude that they are not relevant for the well-functioning of the package itself. 
  
## Downstream dependencies
squid package does not have any downstream dependencies. 
