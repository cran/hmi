## ------------------------------------------------------------------------
## $Plant
## [1] "ordered_categorical"
## 
## $Type
## [1] "binary"
## 
## $Treatment
## [1] "binary"
## 
## $conc
## [1] "roundedcont"
## 
## $uptake
## [1] "cont"

## ----echo = TRUE---------------------------------------------------------
ex <- CO2
set.seed(1)
ex[sample(1:nrow(CO2), size = 20), "uptake"] <- NA
head(ex) # e.g. in line 5 there is a NA now.

## ----echo = FALSE, message = FALSE, cache = TRUE-------------------------
library("mice")
source('C:/Users/Matthias/Documents/hmi/R/hmi_wrapper_2017-04-20.R')
source('C:/Users/Matthias/Documents/hmi/R/hmi_imp_cont_single_2017-04-11.R')

## ----eval = FALSE, message = FALSE, cache = TRUE-------------------------
#  library("hmi")
#  result <- hmi(ex)

## ----eval = TRUE, results = "hide", message = FALSE, cache = TRUE--------
library("hmi")
set.seed(1)
result <- hmi(ex)

## ----eval = FALSE, message = FALSE, cache = TRUE, fig.width = 8----------
#  plot(result, layout = c(2, 1))

## ----eval = TRUE, message = FALSE, cache = TRUE--------------------------
head(complete(result, 1))

## ----eval = TRUE, message = TRUE, cache = TRUE---------------------------
library("lme4")
lmer(uptake ~ 1 + conc + (1 + conc | Plant), data = ex)

## ----eval = TRUE, message = FALSE, cache = TRUE--------------------------
library("lme4")
ex_2 <- ex
ex_2$conc <- (ex_2$conc - mean(ex_2$conc))/sd(ex_2$conc)
mod <- lmer(uptake ~ 1 + conc + (1 + conc | Plant), data = ex_2)

## ----eval = TRUE, message = FALSE, cache = TRUE--------------------------
fixef(mod)

## ----eval = TRUE, message = FALSE, cache = TRUE--------------------------
vcov(mod)

## ----eval = FALSE, message = FALSE, cache = TRUE-------------------------
#  set.seed(1)
#  result_multi <- hmi(data = ex_2, model_formula = uptake ~ 1 + conc + (1 + conc | Plant), maxit = 1)

## ----eval = FALSE, message = FALSE, cache = TRUE-------------------------
#  ex_2$Intercept <- 1
#  set.seed(1)
#  result_multi <- hmi(data = ex_2, model_formula = uptake ~ 1 + conc + (1 + conc |Plant), maxit = 1)

## ----eval = TRUE, echo = FALSE, results = "hide", message = FALSE, cache = TRUE----
source('C:/Users/Matthias/Documents/hmi/R/hmi_imp_cont_multi_2017-04-11.R')
source('C:/Users/Matthias/Documents/hmi/R/hmi_smallfunctions_2017-04-20.R')
ex_2$Intercept <- 1
set.seed(1)
result_multi <- hmi(data = ex_2, model_formula = uptake ~ 1 + conc + (1 + conc |Plant), maxit = 1)

## ----eval = TRUE, echo = TRUE, message = FALSE, cache = TRUE-------------
pool(with(data = result_multi, expr = lmer(uptake ~ 1 + conc + (1 + conc |Plant))))

## ----eval = FALSE, echo = TRUE, message = FALSE, cache = TRUE------------
#   my_analysis <- function(complete_data){
#    # In this list, you can write all the parameters you are interested in.
#    # Those will be averaged.
#    # So make sure that averaging makes sense and that you only put in single numeric values.
#    parameters_of_interest <- list()
#  
#    # ---- write in the following lines, what you are interested in to do with your complete_data
#    # the following lines are an example where the analyst is interested in the fixed intercept
#    # and fixed slope and the random intercepts variance,
#    # the random slopes variance and their covariance
#    my_model <- lmer(uptake ~ 1 + conc + (1 + conc |Plant), data = complete_data)
#  
#    parameters_of_interest[[1]] <- fixef(my_model)[1]
#    parameters_of_interest[[2]] <- fixef(my_model)[2]
#    parameters_of_interest[[3]] <- VarCorr(my_model)[[1]][1, 1]
#    parameters_of_interest[[4]] <- VarCorr(my_model)[[1]][1, 2]
#    parameters_of_interest[[5]] <- VarCorr(my_model)[[1]][2, 2]
#    names(parameters_of_interest) <- c("beta_0", "beta_1", "sigma0", "sigma01", "sigma1")
#  
#    # ---- do not change this function below this line.
#    return(parameters_of_interest)
#   }
#  
#  hmi_pool(mids = result_multi, analysis_function = my_analysis)

## ----eval = TRUE, echo = FALSE, message = FALSE, cache = TRUE------------
source('C:/Users/Matthias/Documents/hmi/R/hmi_smallfunctions_2017-04-20.R')
 my_analysis <- function(complete_data){
  # In this list, you can write all the parameters you are interested in.
  # Those will be averaged.
  # So make sure that averaging makes sense and that you only put in single numeric values.
  parameters_of_interest <- list()

  # ---- write in the following lines, what you are interetest in to do with your complete_data
  # the following lines are an example where the analyst is interested in the fixed intercept
  # and fixed slope and the random intercepts variance,
  # the random slopes variance and their covariance
  my_model <- lmer(uptake ~ 1 + conc + (1 + conc |Plant), data = complete_data)

  parameters_of_interest[[1]] <- fixef(my_model)[1]
  parameters_of_interest[[2]] <- fixef(my_model)[2]
  parameters_of_interest[[3]] <- VarCorr(my_model)[[1]][1, 1]
  parameters_of_interest[[4]] <- VarCorr(my_model)[[1]][1, 2]
  parameters_of_interest[[5]] <- VarCorr(my_model)[[1]][2, 2]
  names(parameters_of_interest) <- c("beta_0", "beta_1", "sigma0", "sigma01", "sigma1")

  # ---- do change this function below this line.
  return(parameters_of_interest)
 }

hmi_pool(mids = result_multi, analysis_function = my_analysis)

