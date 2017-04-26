#' The function to impute rounded continuous variables
#'
#' For example the income in surveys is often reported rounded by the respondents.
#' See Drechsler, Kiesl and Speidel (2015) for more details.
#' @param y_imp A Vector with the variable to impute.
#' @param X_imp A data.frame with the fixed effects variables.
#' @param intercept_varname A character denoting the name of the intercept variable.
#' @references Joerg Drechsler, Hans Kiesl, Matthias Speidel (2015):
#' "MI Double Feature: Multiple Imputation to Address Nonresponse and Rounding Errors in Income Questions".
#' Austrian Journal of Statistics Vol. 44, No. 2, http://dx.doi.org/10.17713/ajs.v44i2.77
#' @return A n x 1 data.frame with the original and imputed values.
imp_roundedcont <- function(y_imp, X_imp,
                                  intercept_varname = NULL){

  # ----------------------------- preparing the X data ------------------
  # remove excessive variables
  X_imp <- remove_excessives(X_imp)

  # standardize X
  X_imp_stand <- stand(X_imp)

  missind <- is.na(y_imp)

  # ph has to be numeric, so it must only consists of precise observations
  decomposed <- decompose_interval(interval = y_imp)

  if(any(decomposed$lower > decomposed$upper, na.rm = TRUE)){
    stop("in your interval covariate, some values in the lower bound exceed the upper bound.")
  }

  y_precise_template <- decomposed$precise
  #if there are imprecise values only...
  if(all(is.na(y_precise_template))){
    #... the template will be sut up, be the row wise means of the imprecise data
    y_precise_template <- rowMeans(decomposed[, 2:3], na.rm = TRUE)
  }

  #define a place holder
  ph <- sample_imp(y_precise_template)
  tmp_1 <- data.frame(y = ph)

  # run a linear model to get the suitable model.matrix for imputation of the NAs
  lmstart <- stats::lm(ph ~ 0 + . , data = X_imp_stand)
  X_model_matrix_1 <- stats::model.matrix(lmstart)
  xnames_1 <- paste("X", 1:ncol(X_model_matrix_1), sep = "")
  tmp_1[, xnames_1] <- X_model_matrix_1


  fixformula_1 <- stats::formula(paste("y ~ 0 +", paste(xnames_1, collapse = "+"), sep = ""))


  reg_1 <- stats::lm(fixformula_1, data = tmp_1)

  # remove unneede variables with an NA coefficient
  unneeded <- is.na(stats::coefficients(reg_1))
  tmp_2 <- data.frame(y = ph)

  xnames_2 <- xnames_1[!unneeded]

  tmp_2[, xnames_2] <- X_model_matrix_1[, !unneeded, drop = FALSE]

  fixformula_2 <- stats::formula(paste("y ~ 0 +", paste(xnames_2, collapse = "+"), sep = ""))
  reg_2 <- stats::lm(fixformula_2, data = tmp_2)
  X_model_matrix_2 <- stats::model.matrix(reg_2)

  max.se <- abs(stats::coef(reg_2) * 3)
  coef.std <- sqrt(diag(stats::vcov(reg_2)))


  includes_unimportants <- any(coef.std > max.se)
  counter <- 0
  while(includes_unimportants & counter <= ncol(X_model_matrix_2)){
    counter <- counter + 1

    X_model_matrix_2 <- as.data.frame(X_model_matrix_2[, coef.std <= max.se, drop = FALSE])
    lm_less_variables <- stats::lm(ph ~ 0 + . , data = X_model_matrix_2)
    #remove regression parameters which have a very high standard error
    max.se <- abs(stats::coef(lm_less_variables) * 3)
    coef.std <- sqrt(diag(stats::vcov(lm_less_variables)))

    includes_unimportants <- any(coef.std > max.se)
  }

  MM_1 <- as.data.frame(X_model_matrix_2)

  # standardise the data
  y_precise <- y_precise_template
  n <- length(y_precise_template)
  mean_y_precise <- mean(y_precise, na.rm = TRUE)
  sd_y_precise <- stats::sd(y_precise, na.rm = TRUE)

  y_imp_std <- (y_imp - mean_y_precise)/sd_y_precise
  if(is_interval(y_imp_std)){
    y_imp_std <- decompose_interval(y_imp_std)$precise
  }

  # --preparing the ml estimation
  # -define rounding intervals

  round_base <- c(1, 5, 10, 50, 100, 500, 1000)
  intervals <- round_base/2

  #check if which observation are rounded
  #Calculate the rounding degree only for those with not an missing value in inc

  p1 <- y_precise_template %% 5    ==  0  # divisable by 5
  p1[is.na(p1)] <- FALSE

  p2 <- y_precise_template %% 10   ==  0  # divisable by 10
  p2[is.na(p2)] <- FALSE

  p3 <- y_precise_template %% 50   ==  0  # etc
  p3[is.na(p3)] <- FALSE

  p4 <- y_precise_template %% 100  ==  0  #
  p4[is.na(p4)] <- FALSE

  p5 <- y_precise_template %% 500  ==  0  #
  p5[is.na(p5)] <- FALSE

  p6 <- y_precise_template %% 1000 ==  0  #
  p6[is.na(p6)] <- FALSE

  p <- factor(p1 + p2 + p3 + p4 + p5 + p6, levels = c("0", "1", "2", "3", "4", "5", "6"), ordered = TRUE)
   ###indicator which variables need to be imputed #MS: because they are rounded (and not because they are missing)
  rounded <- p != 0


  #####maximum likelihood estimation using starting values
  ####estimation of the parameters

  # estimation of the starting values for eta and the thresholds on the x-axis:
  # ordered probit maximum possible rounding on the rounded in income data


  tryCatch(
    {

      probitstart <- MASS::polr(p[!missind] ~ y_imp_std[!missind],
                                contrasts = NULL, Hess = TRUE, model = TRUE,
                                method = "probit")

    },
    error = function(cond) {
      message("We assume that perfect separation occured in your rounded continuous variable,
              because of too few observations.
              Consider specifying the variable to be continuous via list_of_types (see ?hmi).")
      message("Here's the original error message:")
      message(cond)

      return(NULL)
    },
    warning = function(cond) {
      message("We assume that perfect separation occured in your rounded continuous variable,
              because of too few observations.
              Consider specifying the variable to be continuous via list_of_types (see ?hmi).")
      message("Here's the original warning message:")
      message(cond)

      return(NULL)
    },
    finally = {

    }
  )

  gammastart <- as.vector(probitstart$coefficients) # the fix effect(s)
  kstart <- as.vector(probitstart$zeta) # the tresholds (in the summary labeled "Intercepts")
  #explaining the tresholds:
  #0 (rounding degree 1), 0|1 (reounding degree 5),  1|2 (10),  2|3 (50),  3|4 (100),   4|5 (500),   5|6 (1000)


  ph <- sample_imp(decompose_interval(y_imp_std)$precise)
  MM_2 <- data.frame(target = ph)
  xnames <- colnames(MM_1)
  MM_2[, xnames] <- MM_1
  lmstart2 <- stats::lm(ph ~ 0 + ., data = MM_2) # it might be more practical to run the model
  #only based on the observed data, but this could cause some covariates in betastart2 to be dropped
  betastart2 <- as.vector(lmstart2$coef)
  sigmastart2 <- summary(lmstart2)$sigma


  #####maximum likelihood estimation using the starting values

  function_generator <- function(para, X, y_in_negloglik, lower, upper,
                                 my_p, mean_y_precise, sd_y_precise){
    ret <- function(para){
      ret_tmp <- negloglik2(para = para, X = X, y_in_negloglik = y_in_negloglik,
                            lower = lower, upper = upper,
                            my_p = my_p,
                            mean_y_precise = mean_y_precise, sd_y_precise = sd_y_precise)
      return(ret_tmp)
    }
    return(ret)
  }


  starting_values <- c(kstart, betastart2, gammastart, sigmastart2)

  ###exclude obs below (above) the 0.5% (99.5%) income quantile before maximizing
  ###the likelihood. Reason: Some extrem outliers cause problems during the
  ###maximization

  quants <- stats::quantile(y_precise_template, c(0.005, 0.995), na.rm = TRUE)

  # in X and y_in_negloglik only those observations that are no outliers shall be included.
  # Observations with a missing Y are to be included as well even if they could be an outlier.
  # Therefore w
  keep <- (y_precise_template >= quants[1] & y_precise_template <= quants[2]) |
    is.na(y_precise_template)


  #the interval data have to be standardised as well:
  lower_std <- (decomposed$lower - mean_y_precise)/sd_y_precise
  upper_std <- (decomposed$upper - mean_y_precise)/sd_y_precise

  negloglik2_generated <- function_generator(para = starting_values,
                                             X = MM_2[keep, , drop = FALSE],
                                             y_in_negloglik = y_precise_template[keep],
                                             lower = lower_std[keep],
                                             upper = upper_std[keep],
                                             my_p = as.numeric(as.character(p[keep])),
                                             mean_y_precise = mean_y_precise,
                                             sd_y_precise = sd_y_precise)

  m2 <- stats::optim(par = starting_values, negloglik2_generated, method = "BFGS",
                   control = list(maxit = 10000), hessian = TRUE)

  par_ml2 <- m2$par
  hess <- m2$hessian

  # link about nearest covariance matrix:
  # http://quant.stackexchange.com/questions/2074/what-is-the-best-way-to-fix-a-covariance-matrix-that-is-not-positive-semi-defi
  # nearPD(hess)$mat
  # isSymmetric(Sigma_ml2)


  Sigma_ml2 <- tryCatch(
    {

      Sigma_ml2 <- solve(hess)

    },
    error = function(cond) {
      message("Hessian matrix couldn't be inverted (in the imputation function of the rounded continuous variable).")
      message("Here's the original error message:")
      message(cond)

      Sigma_ml2 <- diag(ncol(hess))

    },
    warning = function(cond) {
      message("There seems to be a problem with the Hessian matrix in the imputation of the rounded continuous variable")
      message("Here's the original warning message:")
      message(cond)

      Sigma_ml2 <- solve(hess)

    },
    finally = {
    }
  )


  ###set starting values equal to the observed income
  ###rounded income will be replaced by imputations later
 	y_std_tmp <- decompose_interval(y_imp_std)$precise

 	imp_tmp <- y_precise_template


 	####draw new parameters (because it is a Bayesian imputation)
  check <- TRUE
  #  counter <- 0
  while(check){
      pars <- mvtnorm::rmvnorm(1, mean = par_ml2, sigma = Sigma_ml2)
      #first eq on page 63 in Drechsler, Kiesl, Speidel (2015)
      #Can we work with a diagonal matrix as well, or is this too far from the posterior?

      ####test if drawn parameters for the thresholds are in increasing order
      ####and if the standard deviation of the residuals is<0
      ####if yes, draw again
      # pars takes the starting values c(kstart, betastart2, gammastart, sigmastart2)
      test <- c(pars[2:6] - pars[1:5], pars[length(pars)])

      check <- any(test < 0)
  }


  # derive imputation model parameters from previously drawn parameters
  beta_hat <- as.matrix(pars[7:(length(pars) - 2)], ncol = 1)
  gamma1_hat <- pars[length(pars) - 1]
  sigma_hat <- pars[length(pars)]
  mu_g <- gamma1_hat * as.matrix(MM_2) %*% beta_hat
  mu_y <- as.matrix(MM_2) %*% beta_hat
  mymean <- cbind(mu_g, mu_y)

  #The covariance matrix from equation (3)
  Sigma <- matrix(c(1 + gamma1_hat^2 * sigma_hat^2,
                      gamma1_hat * sigma_hat^2, gamma1_hat * sigma_hat^2,
                      sigma_hat^2), nrow = 2)


  ###################################
  #BEGIN IMPUTING INTERVALL-DATA AND COMPLETELY MISSING DATA
  #for this purpose we have to replace the lower and upper bounds
  # of those observations with an NA in y_imp by -Inf and Inf

  expanded_lower <- decomposed$lower

  expanded_lower[is.na(expanded_lower)] <- -Inf

  expanded_upper <- decomposed$upper
  expanded_upper[is.na(expanded_upper)] <- Inf


  expanded_lower_std <- (expanded_lower - mean_y_precise)/sd_y_precise
  expanded_upper_std <- (expanded_upper - mean_y_precise)/sd_y_precise
  #draw values from the truncated normal distributions
  # the bounds are straight forward for the interval data.
  # for the missing data, the bounds are -Inf and +Inf,
  # which is equivalent to draw from a unbounded normal distribution.
  # for precise observations, the bounds are here set to be NA,
  # resulting in NA draws for those observations.
  # The imputation for precise but rounded data follows in the next section.
  # precise and not rounded data need no impuation at all.

  mytry <- msm::rtnorm(n = n, lower = expanded_lower_std,
                        upper = expanded_upper_std,
                        mean = as.matrix(MM_2) %*% beta_hat,
                        sd = sigma_hat)

  # proposed values for imputation
  #do the backtransformation from standardised to unstandardised
  imp_tmp_imprecise <- mytry * sd_y_precise + mean_y_precise


  imp_tmp[is.na(decomposed$precise)] <- imp_tmp_imprecise[is.na(decomposed$precise)]


  ##############################
  # BEGIN UNROUNDING-IMPUTATION#
  ###define bounds for the rounding basis
  bounds_hat <- c(-Inf, pars[1:6], Inf)
  ###define interval bounds for maximum possible rounding intervals
  # Needed in the following imputation loop
  # but only for those observations who answered this question
  # I cannot write 'log.inc - log(intervalls)' etc. because we calculate log(inv - intervals)

  #Notice: currently we expect y_precise_template to be (rounded) income data with
  # values >= 1
  y_lower <- (y_precise_template - intervals[as.numeric(as.character(p)) + 1] -
                  mean_y_precise)/sd_y_precise

  y_upper <- (y_precise_template + intervals[as.numeric(as.character(p)) + 1] -
                  mean_y_precise)/sd_y_precise

  g_lower <- bounds_hat[as.numeric(as.character(p)) + 1]
  g_upper <- bounds_hat[as.numeric(as.character(p)) + 2]


  ###loop over all observations that need to be unrounded
  for (i in which(rounded)){
    test <- TRUE
    while(test){

      ###draw from truncated multivariate normal
      ###drawn y must be between y_lower and y_upper
      ###drawn g must be smaller than g_upper (g > g_upper is not consistent with
      ###rounding observed in the data)
      mytry <- tmvtnorm::rtmvnorm(1,
                          mean = mymean[i, ],
                          sigma = Sigma,
                          lower = c(-Inf, y_lower[i]),
                          upper = c(g_upper[i], y_upper[i]),
                          algorithm = "gibbs", burn.in.samples = 1000)

      ###draws with rejection sampling. If intervall restrictions are not fulfilled after 1 mio
      ###draws, NA is given back. In this case, the rounded data will stay unmodified
      if(is.na(mytry[1])){
        print(paste("corrected imputation not possible for record:", i))
        print(paste("observed income for that record:", y_precise_template[i]))
        ##generate a rounding indicater that is always consistent with the observed data
        g_temp <- bounds_hat[2] - 1

        mytry <- c(g_temp, y_std_tmp[i])
      }
      ####get imputed rounding indicator
      round_int <- sum(mytry[1] > bounds_hat)

      ###get imputed income on original scale
      imp_precise_temp <- mytry[2] * sd_y_precise + mean_y_precise

      ###test if imputed income after rounding is equal to observed rounded income given the
      ###imputed rounding indicator. Is the imputation plausible given the observed data?
      ###if not(test=TRUE), draw again
      test <- round(imp_precise_temp/round_base[round_int]) * round_base[round_int] !=
                 y_precise_template[i]
    }

    imp_tmp[i] <- imp_precise_temp

  }
  y_ret <- data.frame(y_imp = imp_tmp)

  return(y_ret)
}


