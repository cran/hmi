% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hmi_wrapper.R
\name{hmi}
\alias{hmi}
\title{hmi: Hierarchical Multilevel Imputation.}
\usage{
hmi(
  data,
  model_formula,
  family,
  additional_variables,
  list_of_types,
  m = 5,
  maxit,
  nitt = 22000,
  burnin = 2000,
  pvalue = 1,
  mn = 1,
  k,
  spike,
  heap,
  rounding_degrees,
  rounding_formula = ~.,
  pool_with_mice = TRUE
)
}
\arguments{
\item{data}{A \code{data.frame} with all variables appearing in \code{model_formula}.}

\item{model_formula}{A \code{\link[stats]{formula}} used for the analysis model.
Currently the package is designed to handle formula used in
\code{lm}, \code{glm}, \code{lmer} and \code{glmer}. The formula is also used for the default pooling.}

\item{family}{To improve the default pooling, a family object supported by \code{glm} (resp. \code{glmer}) an be given.
See \code{family} for details.}

\item{additional_variables}{A character with names of variables (separated by "+", like "x8+x9")
that should be included in the imputation model as fixed effects variables,
but not in the analysis model.
An alternative would be to include such variable names into the \code{model_formula}
and run a reduced analysis model with \code{hmi_pool} or the functions provide by \code{mice}.}

\item{list_of_types}{a list where each list element has the name of a variable
in the data.frame. The elements have to contain a single character denoting the type of the variable.
See \code{get_type} for details about the variable types.
With the function \code{list_of_types_maker}, the user can get the framework for this object.
In most scenarios this is should not be necessary.
One example where it might be necessary is when only two observations
of a continuous variable are left - because in this case \code{get_type}
interpret is variable to be binary. Wrong is it in no case.}

\item{m}{An integer defining the number of imputations that should be made.}

\item{maxit}{An integer defining the number of times the imputation cycle
(imputing \eqn{x_1|x_{-1}} then \eqn{x_2|x_{-2}}, ... and finally \eqn{x_p|x_{-p}}) shall be repeated.
The task of checking convergence is left to the user, by evaluating the chainMean and chainVar!}

\item{nitt}{An integer defining number of MCMC iterations (see \code{MCMCglmm}).}

\item{burnin}{An integer for the desired number of
Gibbs samples that shall be regarded as burnin.}

\item{pvalue}{A numeric between 0 and 1 denoting the threshold of p-values a variable in the imputation
model should not exceed. If they do, they are excluded from the imputation model.}

\item{mn}{An integer defining the minimum number of individuals per cluster.}

\item{k}{An integer defining the allowed maximum of levels in a factor covariate.}

\item{spike}{A numeric value saying which value in the semi-continuous data might be the spike.
Or a list with with such values and names identical to the variables with spikes
(see \code{list_of_spikes_maker} for details.) In versions earlier to 0.9.0 it was called \code{heap}.}

\item{heap}{Use spike instead. \code{heap} is only included due to backwards compatibility
and will be removed with version 1.0.0}

\item{rounding_degrees}{A numeric vector with the presumed rounding degrees of rounded variables.
Or a list with rounding degrees,  where each list element has the name of a rounded continuous variable.
Such a list can be generated using \code{list_of_rounding_degrees_maker(data)}.
Note: it is presupposed that the rounding degrees include 1 meaning that there
is a positive probability that e.g. 3500 was only rounded to the nearest integer
(and not rounded to the nearest multiple of 100 or 500).}

\item{rounding_formula}{A formula with the model formula for the latent rounding tendency G.
Or a list with model formulas for G, where each list element has the name of a rounded continuous variable.
Such a list can be generated
using \code{list_of_rounding_formulas_maker(data)}}

\item{pool_with_mice}{A Boolean indicating whether the user wants to pool the \code{m} data sets by mice
using his \code{model_formula}. The default value is \code{FALSE} because this tampers the
\code{mids} object as it adds an argument \code{pooling} not found in "normal" \code{mids} objects
generated by \code{mice}.}
}
\value{
The function returns a \code{mids} object. See \code{mice} for further information.
}
\description{
The user has to pass his data to the function.
Optionally he passes his analysis model formula so that \code{hmi} runs the imputation model
in line with his analysis model formula.\cr
And of course he can specify some parameters for the imputation routine
(like the number of imputations and iterations and the number of iterations
within the Gibbs sampling).\cr
}
\examples{
\dontrun{
 data(Gcsemv, package = "hmi")

 model_formula <- written ~ 1 + gender + coursework + (1 + gender|school)

 set.seed(123)
 dat_imputed <- hmi(data = Gcsemv, model_formula = model_formula, m = 2, maxit = 2)
 #See ?hmi_pool for how to pool results.
}
}
\references{
Matthias Speidel, Joerg Drechsler and Shahab Jolani (2020):
"The R Package hmi: A Convenient Tool for Hierarchical Multiple Imputation and Beyond",
Journal of Statistical Software, Vol. 95, No. 9, p. 1--48, \url{http://dx.doi.org/10.18637/jss.v095.i09}
}
