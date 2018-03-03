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
library("lme4")
ex <- sleepstudy
set.seed(1)
ex[sample(1:nrow(CO2), size = 20), "Reaction"] <- NA
head(ex) # e.g. in line 5 there is a NA now.

## ----echo = FALSE, message = FALSE, cache = TRUE-------------------------
library("mice")
library("hmi")

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
lmer(formula = Reaction~1+Days+(1+Days|Subject), data = ex, na.action = na.omit)

## ----eval = FALSE, message = FALSE, cache = TRUE-------------------------
#  set.seed(1)
#  result_multi <- hmi(data = ex, model_formula = Reaction ~ 1 + Days + (1 + Days | Subject), maxit = 1)

## ----eval = FALSE, message = FALSE, cache = TRUE-------------------------
#  set.seed(1)
#  result_multi <- hmi(data = ex, model_formula = Reaction ~ 1 + Days + (1 + Days | Subject), maxit = 1)

## ----eval = TRUE, echo = FALSE, results = "hide", message = FALSE, cache = TRUE----
library("hmi")
ex
set.seed(1)
result_multi <- hmi(data = ex, model_formula = Reaction ~ 1 + Days + (1 + Days | Subject), maxit = 1)

## ----eval = TRUE, echo = TRUE, message = FALSE, cache = TRUE-------------
pool(with(data = result_multi, expr = lmer(Reaction ~ 1 + Days + (1 + Days | Subject))))

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
#    my_model <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), data = complete_data)
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
library("hmi")
 my_analysis <- function(complete_data){
  # In this list, you can write all the parameters you are interested in.
  # Those will be averaged.
  # So make sure that averaging makes sense and that you only put in single numeric values.
  parameters_of_interest <- list()

  # ---- write in the following lines, what you are interetest in to do with your complete_data
  # the following lines are an example where the analyst is interested in the fixed intercept
  # and fixed slope and the random intercepts variance,
  # the random slopes variance and their covariance
  my_model <- lmer(Reaction ~ 1 + Days + (1 + Days | Subject), data = complete_data)

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

