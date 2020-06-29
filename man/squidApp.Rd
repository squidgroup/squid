\name{squidApp}
\alias{squidApp}
\title{SQuID application}
\usage{
squidApp(...)
}
\arguments{
\item{...}{any argument that could be passed to the function \code{\link[shiny]{runApp}} from the \href{https://cran.r-project.org/package=shiny}{shiny} package.}
}
\description{
\code{squidApp} runs SQuID application as a browser interface that has been developed with the package shiny.

SQuID web interface is made to help researchers to become familiar with multilevel phenotypic variation, and to design optimal sampling schemes for their studies. SQuID is built up as a series of modules that guide the user into situations of increasing complexity to explore the dynamics between the way phenotypes are sampled and the estimation of parameters of specific interest. The complete simulations the full model simulation tab provides functionality that allows the user to generate data sets that can then be used to run analyses in the statistical package of their choice and address specific research questions.

SQuID is based on a mathematical model that creates a group of individuals (i.e. study population) repeatedly expressing phenotypes, for one or two different traits, through time. Phenotypic values of traits are generated following the general principle of the phenotypic equation (\href{http://onlinelibrary.wiley.com/doi/10.1111/1365-2656.12013/abstract}{Dingemanse & Dochtermann 2013, Journal of Animal Ecology}): phenotypic variance (Vp) is assumed to be the sum of a series of components (see the full model tab on the web interface). The user has the flexibility to add different causal variance components that will form the phenotype of the individual at each time step, and to set up the relative importance of each component through the definition of environmental effects. SQuID then allows the user to collect a subsample of phenotypes for each simulated individual (i.e. operational data set), according to a specific sampling design. For most of the modules, the simulated data set is automatically fed into a statistical model in R and the main results of the analysis shown in an output. For the full model the user has the opportunity to download the operational data set for further analyses.
}
\details{
SQuID application will by default show up on your default web browser (we recommend to update your web browser to its latest version). In order to use properly the SQuID application, we encourage you to start by reading the documentation on the portal page. From there you will be guided through SQuID application according to your experience and knowledge of multilevel modelling.

For more advanced and efficient simulations, SQuID is also available as an R function \code{\link{squidR}}.

Note that while the SQuID application is running, your \code{R} session will be busy. Then, in order to exit the SQuID application mode, you have to press the "Esc" key while you are on your \code{R} session window.
}
\section{References}{
Allegue, H., Araya-Ajoy, Y.G., Dingemanse, N.J., Dochtermann N.A., Garamszegi, L.Z., Nakagawa, S., Reale, D., Schielzeth, H. and Westneat, D.F. (2016) SQuID - Statistical Quantification of Individual Differences: an educational and statistical tool for understanding multi-level phenotypic data in linear mixed models. \emph{Methods in Ecology and Evolution}, \bold{8}:257-267.

Dingemanse, N.J. and Dochtermann N.A. (2013) Quantifying individual variation in behaviour: mixed-effect modelling approaches. \emph{Journal of Animal Ecology}, \bold{82}:39-54.
}
\seealso{
\code{\link{squidR}}
}
\examples{
# run SQuID application
\dontrun{squidApp()}

# press the "Esc" key to exit SQuID application mode. 
}