#' The function for hierarchical imputation of contious variables.
#'
#' The function is called by the wrapper.
#' @param y_imp A Vector with the variable to impute.
#' @param X_imp A data.frame with the fixed effects variables.
#' @param Z_imp A data.frame with the random effects variables.
#' @param clID A vector with the cluster ID.
#' @param nitt An integer defining number of MCMC iterations (see MCMCglmm).
#' @param thin An integer defining the thinning interval (see MCMCglmm).
#' @param burnin An integer defining the percentage of draws from the gibbs sampler
#' that should be discarded as burn in (see MCMCglmm).
#' @return A n x 1 matrix with the original and imputed values.
imp_cont_multi <- function(y_imp,
                      X_imp,
                      Z_imp,
                      clID,
                      nitt = 3000,
                      thin = 10,
                      burnin = 1000){

  # -----------------------------preparing the data ------------------
  # -- standardise the covariates in X (which are numeric and no intercept)
  # ----------------------------- preparing the X and Z data ------------------

  # remove excessive variables
  X_imp <- remove_excessives(X_imp)

  # standardise the covariates in X (which are numeric and no intercept)
  X_imp_stand <- stand(X_imp)

  # -- standardise the covariates in Z (which are numeric and no intercept)

  Z_imp_stand <- stand(Z_imp)


  missind <- is.na(y_imp)
  n <- nrow(X_imp_stand)


  # Get the number of random effects variables
  n.par.rand <- ncol(Z_imp_stand)
  length.alpha <- length(table(clID)) * n.par.rand


  #define a place holder (ph)
  ph <- sample_imp(y_imp)


  tmp_0_sub <- data.frame(target = ph, X_imp_stand)[!missind, , drop = FALSE]
  tmp_0_all <- data.frame(target = ph, X_imp_stand)


  X_model_matrix_1_sub <- stats::model.matrix(target ~ 0 + ., data = tmp_0_sub)
  X_model_matrix_1_all <- stats::model.matrix(target ~ 0 + ., data = tmp_0_all)
  colnames(X_model_matrix_1_sub) <- gsub("`", "", colnames(X_model_matrix_1_sub))
  colnames(X_model_matrix_1_all) <- gsub("`", "", colnames(X_model_matrix_1_all))

  # remove unneeded variables/categories from X_model_matrix_1
  # model to determine unnneeded variables
  reg_1_sub <- stats::lm(target ~ 0 + ., data = tmp_0_sub)
  unneeded <- is.na(stats::coefficients(reg_1_sub))

  #data, where the needed variables should be stored


  xnames_1 <- paste("X", 1:ncol(X_model_matrix_1_all), sep = "")
  znames_1 <- paste("Z", 1:ncol(Z_imp_stand), sep = "")

  xnames_2 <- xnames_1[!unneeded]
  znames_2 <- znames_1
  X_model_matrix_2_all <- X_model_matrix_1_all[, !unneeded, drop = FALSE]
  tmp_2_all <- data.frame(target = ph)
  tmp_2_all[, xnames_2] <- X_model_matrix_2_all
  tmp_2_all[, znames_2] <- Z_imp_stand
  tmp_2_all[, "ClID"] <- clID

  tmp_2_sub <- tmp_2_all[!missind, , drop = FALSE]
  # -------------- calling the gibbs sampler to get imputation parameters----

  fixformula <- stats::formula(paste("target~ 0 + ", paste(xnames_2, collapse = "+"), sep = ""))
  randformula <- stats::as.formula(paste("~us(", paste(znames_1, collapse = "+"), "):ClID", sep = ""))


  prior <- list(R = list(V = 1e-07, nu = -2),
                G = list(G1 = list(V = diag(ncol(Z_imp_stand)), nu = 0.002)))

  MCMCglmm_draws <- MCMCglmm::MCMCglmm(fixformula, random = randformula, data = tmp_2_sub,
                             verbose = FALSE, pr = TRUE, prior = prior,
                             saveX = TRUE, saveZ = TRUE,
                             nitt = 3000,
                             thin = 10,
                             burnin = 1000)

  pointdraws <- MCMCglmm_draws$Sol
  xdraws <- pointdraws[, 1:ncol(X_model_matrix_2_all), drop = FALSE]
  zdraws <- pointdraws[, ncol(X_model_matrix_2_all) + 1:length.alpha, drop = FALSE]
  variancedraws <- MCMCglmm_draws$VCV
  # the last column contains the variance (not standard deviation) of the residuals

  number_of_draws <- nrow(pointdraws)
  select.record <- sample(1:number_of_draws, 1, replace = TRUE)

  # -------------------- drawing samples with the parameters from the gibbs sampler --------
  ###start imputation


  rand.eff.imp <- matrix(zdraws[select.record, ], ncol = n.par.rand)

  fix.eff.imp <- matrix(xdraws[select.record, ], nrow = ncol(X_model_matrix_2_all))

  sigma.y.imp <- sqrt(variancedraws[select.record, ncol(variancedraws)])

  y_temp <- stats::rnorm(n, X_model_matrix_2_all %*% fix.eff.imp +
                      apply(Z_imp_stand * rand.eff.imp[clID,], 1, sum), sigma.y.imp)

  y_ret <- matrix(ifelse(missind, y_temp, y_imp), ncol = 1)


  # --------- returning the imputed data --------------
  return(y_ret)
}


# Generate documentation with devtools::document()
# Build package with devtools::build() and devtools::build(binary = TRUE) for zips
