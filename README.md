A repository to store all my useful R functions.
Who knows a (few) package(s) in the future.

Functions include

CO2
   CO2 anim.R
      an animation from online atmospheric CO2 concentration data
   headspace_pCO2.R
      a translation of an equation produced by Yves Prairie @ UQAM
         status: complete
   CO2 calc
      a translation of a BASIC program written by Ernie Lewis
         status: work in progress


Data import functions
   read_tps.R
      to read .tps files produced by tpsDIG for morphometrics
         status: complete
         in need of generalization for inclusion in a package
   Microplates
      to read data produced by micro well plate readers
         status: complete
         in need of generalization for inclusion in a package

Growth curve fitting functions recovering ecologicaly relavant parameters
   example data.RData
      example data
   exponential_growth_nls.R
      exponetial growth using non linear least squares (nls)
   logistic_growth_nls.R
      logistic growth using nls
   logistic_growth_mle.R
      logistic growth using maximum likelihood estimation (mle)
      currently only works with normal error distribution
      needs to be implemented using either logitnormal distribution or normal distribution of the logit
      both cases use the ratio of Nt over a maximum value slightly higher than K
   in need of generalization for inclusion in a package

Meta analysis
   crossref_get_doi.R
      recover digital object identifier (DOI) given complete bibliographic data
   crossref_get_meta_from_doi.R
      get bibliographic data given DOI

Education
   bolds sequence recovery.R
   Creation of a list of random sequences from the BOLDS database

Chemtax
   chemtax.R
   A wrapper to the Chemtax approach provided in Chemtax in the limSolve package

R packages
   Export list of installed packages for re-installation after update