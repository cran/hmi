
<!-- README.md is generated from README.Rmd. Please edit that file -->
The hmi package allows user to run single level and multilevel imputation models.

The user just has to pass the data to the main function and, optionally, his analysis model. Basically the package then translates this analysis model into commands to impute the data according to it with functions from `mice`, `MCMCglmm` or routines build for this package.

As a brief example try:

``` r
example_2 <- CO2
#standardizing the variable:
example_2$conc <- (example_2$conc - mean(example_2$conc))/sd(example_2$conc)
#adding an intercept variable:
example_2$Intercept <- 1

#running the imputation:
library("hmi")
result_multi <- hmi(data = example_2, model_formula = uptake ~ 1 + conc + (1 + conc | Plant))
```

    ## [1] "We interprete Intercept as the intercept variable and set its value to 1."
    ## [1] "We interprete Plant as the cluster indicator and treat it as a factor."

For more details, please read the Vignette.
