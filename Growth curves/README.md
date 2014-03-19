Needs for improvments
- []  start values should be calculated automaticly based on readings
   - [] needs implementaiton in nls
   - [] needs implementation for r in all (current start is 1)
   - [] sd in mle should probably be based on spread of data

Growth curve fitting functions recovering ecologicaly relavant parameters
   - example data.RData
      - example data
   - exponential_growth_nls.R
      - exponetial growth using non linear least squares (nls)
   - logistic_growth_nls.R
      - logistic growth using nls
   - logistic_growth_mle.R
     - logistic growth using maximum likelihood estimation (mle)
      -  currently only works with normal error distribution
      needs to be implemented using either logitnormal distribution or normal distribution of the logit
      both cases use the ratio of Nt over a maximum value slightly higher than K
