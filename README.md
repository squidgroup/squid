### Statistical Quantification of Individual Differences: an educational and statistical tool for understanding multi-level phenotypic data in linear mixed models

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/squid)](https://cran.r-project.org/package=squid)
[![Build
Status](https://travis-ci.org/squid-group/squid.svg?branch=master)](https://travis-ci.org/squid-group/squid)
[![Downloads](https://cranlogs.r-pkg.org/badges/squid?color=brightgreen)](https://cran.r-project.org/package=squid)
[![total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/squid)](http://cranlogs.r-pkg.org/badges/grand-total/squid)

#### Brief description

<img id='logo' src='./man/pictures/logo_2.png' align='left' alt='' style='padding-right:20px;'>
**SQuID** stands for **S**tatistical **Qu**antification of
**I**ndividual **D**ifferences and is the product of the SQuID working
group. The package aims to help scholars who, like us, are interested in
understanding patterns of phenotypic variance. Individual differences
are the raw material for natural selection to act on and hence the basis
of evolutionary adaptation. Understanding the sources of phenotypic
variance is thus a most essential feature of biological investigation
and mixed effects models offer a great, albeit challenging tool.
Disseminating the properties, potentials and interpretational challenges
in the research community is thus a foremost goal of SQuID.

The `squid` package has two main objectives: First, it provides an
educational tool useful for students, teachers and researchers who want
to learn to use mixed-effects models. Users can experience how the
mixed-effects model framework can be used to understand distinct
biological phenomena by interactively exploring simulated multilevel
data. Second, `squid` offers research opportunities to those who are
already familiar with mixed-effects models, as `squid` enables the
generation of datasets that users may download and use for a range of
simulation-based statistical analyses such as power and sensitivity
analysis of multilevel and multivariate data.

#### Install squid package

To install the latest released version from CRAN:

    install.packages("squid")

To install the development version from GitHub:

    # install.packages("devtools")
    devtools::install_github("hallegue/squid")

Get more information about the installation of the
[devtools](https://cran.r-project.org/package=devtools/readme/README.html)
package.

#### Background

The phenotype of a trait in an individual results from a sum of genetic
and environmental influences. Phenotypic variation is structured in a
hierarchical way and the hierarchical modelling in mixed effect models
is great tool to analyze and decompose such variation. Phenotypes vary
across species, across populations of the same species, across
individuals of the same population, and across repeated observations of
the same individual. We focused on the individual level because it
represents one of the most important biological levels to both
ecological and evolutionary processes. Different sources of variation
are at the origin of the phenotype of an individual. Individuals may
differ in their phenotypes because they carry different gene variants
(i.e. alleles). But individuals also experience different environments
during their lifetime. Some environmental influences impose a lasting
mark on the phenotype, while others are more ephemerous. The former tend
to produce long-lasting, among-individual variation, while the latter
causes within-individual variation. However, this depends on the time
scale at which the measurements of the phenotypes are done relative that
of the environmental influences. Furthermore, individuals differ not
only in their average phenotypes but also in how they respond to changes
in their environment (i.e. differences in individual phenotypic
plasticity). This represents an interaction between the among- and the
within-individual levels of variation. The patterns of variation can,
thus, be very complex. Selection can act differently on these different
components of variance in the phenotypes of a trait, and this is why it
is important to quantify their magnitude.

Mixed models are very flexible statistical tools that provide a way to
estimate the variation at these different levels, and represent the
general statistical framework for evolutionary biology. Because of the
progress in computational capacities mixed models have become
increasingly popular among ecologists and evolutionary biologists over
the last decade. However, fitting mixed model is not a straightforward
exercise, and the way data are sampled among and within individuals can
have strong implications on the outcome of the model. This is why we
created the `squid` simulation tool that could help new users interested
in decomposing phenotypic variance to get more familiar with the concept
of hierarchical organization of traits, with mixed models and to avoid
pitfalls caused by inappropriate sampling.

#### squid package description

`squid` is a simulation-based tool that can be used for research and
educational purposes. `squid` creates a world inhabited by individuals
whose phenotypes are generated by a user-defined phenotypic equation,
which allows easy translation of biological hypotheses into
mathematically quantifiable parameters. The framework is suitable for
performing simulation studies, determining optimal sampling designs for
user-specific biological problems, and making simulation based
inferences to aid in the interpretation of empirical studies. `squid` is
also a teaching tool for biologists interested in learning, or teaching
others, how to implement and interpret mixed-effects models, when
studying the processes causing phenotypic variation. `squid` is based on
a mathematical model that creates a group of individuals (i.e. study
population) repeatedly expressing phenotypes, for one or two different
traits, in uniform time. Phenotypic values of traits are generated
following the general principle of the phenotypic equation ([Dingemanse
& Dochtermann 2013, Journal of Animal
Ecology](https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/1365-2656.12013)):
phenotypes are assumed to be the summed effects of a series of
components and the phenotypic variance (Vp) is the sum of the respective
variances in theses causal components. The user has thus the flexibility
to add different variance components that will form the phenotype of the
individual at each time step, and to set up the relative importance of
each component through the definition of environmental effects. `squid`
then allows the user to collect a sub-sample of phenotypes for each
simulated individual (i.e. operational data set), according to a
specific sampling design. The major difference between `squid` and other
R packages that also allow performance analysis through data simulation
(e.g. [`pamm`](https://cran.r-project.org/package=pamm),
[`odprism`](https://cran.r-project.org/package=odprism),
[`simr`](https://cran.r-project.org/package=simr)), is that only `squid`
allows separate steps for generating the world first and then model a
sampling process from it. `squid` is subject to evolution and is
designed to adapt to more complex scenarios in the future.

`squid` has two main functions; `squidApp()` and `squidR()`:

-   **`squidApp()`:** runs the SQuID application which is a
    browser-based interface created with the
    [`shiny`](https://shiny.rstudio.com) package (we recommend to update
    your default web browser to its latest version). SQuID is built up
    as a series of modules that guide the user into situations of
    increasing complexity to explore the phenotypic equation model and
    the dynamics between the way phenotypes are sampled and the
    estimation of parameters of specific interest; The last module is
    the full model simulation that allows the user to generate data sets
    that can then be used to run analyses in the statistical package of
    their choice for specific research questions. For most of the
    modules, the simulated data set is automatically fed into a
    statistical model in R and the main results of the analysis shown in
    an output. For the full model the user has the opportunity to
    download the operational data set for further analyses. The SQuID
    application also has a tab (Full model (Step by step)) describing in
    details the SQuID full model.

<!-- -->

    # run SQuID application
    library(squid)
    squidApp()

-   **`squidR()`:** is a traditional R function that allows data
    generation and sampling without the browser-based interface. This
    function can be used for more advanced and efficient simulations
    once you understand how SQuID works. `squidR()` can be easily
    included in R scripts.

#### History of the project

It all started in Hannover in November 2013 at the occasion of a
workshop on personality organised by Susanne Foitzik, Franjo Weissing,
and Niels Dingemanse and funded by the Volkswagen Foundation. During
this workshop, a group of researchers discussed the potential issues
related to sampling designs on the estimation of components of the
phenotypic variance and covariance. It became obvious that there was an
urgent need to develop a simulation package to help anyone interested in
using a mixed model approach at getting familiar with this methods and
avoiding the pitfalls related to the interpretation of the results. A
first model and a working version of the package were created in January
2014, during a meeting at Université du Québec à Montréal. The current
version was produced during a workshop in November 2014, at the Max
Plank Institute for Ornithology in Seewiesen.

#### SQuID team

-   Hassen Allegue (Université du Québec À Montréal, Montreal, Canada)
-   Yimen G. Araya-Ajoy (Norwegian University of Science and Technology,
    Trondheim, Norway)
-   Niels J. Dingemanse (Max Planck Institute for Ornithology, Seewiesen
    & University of Munich, Germany)
-   Ned A. Dochtermann (North Dakota State University, Fargo, USA)
-   Laszlo Z. Garamszegi (Estación Biológica de Doñana-CSIC, Seville,
    Spain)
-   Shinichi Nakagawa (University of New South Wales, Sydney, Australia)
-   Denis Réale (Université du Québec À Montréal, Montreal, Canada)
-   Holger Schielzeth (University of Bielefeld, Bielefeld, Germany)
-   David F. Westneat (University of Kentucky, Lexington, USA)

#### References

Allegue, H., Araya-Ajoy, Y.G., Dingemanse, N.J., Dochtermann N.A.,
Garamszegi, L.Z., Nakagawa, S., Réale, D., Schielzeth, H. and Westneat,
D.F. (2016). SQuID - Statistical Quantification of Individual
Differences: an educational and statistical tool for understanding
multi-level phenotypic data in linear mixed models. Methods in Ecology
and Evolution, 8:257-267. [DOI:
10.1111/2041-210X.12659](https://doi.org/10.1111/2041-210X.12659)

Dingemanse, N.J. and Dochtermann N.A. (2013). Quantifying individual
variation in behaviour: mixed-effect modelling approaches. Journal of
Animal Ecology, 82:39-54. [DOI:
10.1111/1365-2656.12013](https://doi.org/10.1111/1365-2656.12013)
