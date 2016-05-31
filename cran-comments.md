## Test environments
* local Windows 10, R 3.2.5
* local OS X El Capitan V10.11.4, R 3.3.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Hassen Allegue <hassen.allegue@gmail.com>'

New submission

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2016
  COPYRIGHT HOLDER: SQuID
  
## Downstream dependencies
I have also run R CMD check on downstream dependencies of squid 
(https://github.com/hallegue/squid/revdep). 
All packages that I could install passed.