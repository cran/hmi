context("hmi")
library(testthat)
library(hmi)
library(mice)

#set up the data
set.seed(123)
y_cat <- sample(c("A", "B", "C", NA), size = 150, replace = TRUE)
y_catord <- factor(sample(c("A", "B", "C", NA), size = 150, replace = TRUE), ordered = TRUE)
y_binary <-  sample(c(0, 1, NA), size = 150, replace = TRUE)
y_cont <- rnorm(150)
y_cont[sample(1:length(y_cont), size = 20)] <- NA

y_semicont <- rnorm(150)
y_semicont[sample(1:length(y_semicont), size = 60)] <- 0
y_semicont[sample(1:length(y_semicont), size = 20)] <- NA
y_count <- ifelse(runif(150) > 0.8, NA, rpois(150, lambda = 5))

y_low <- ifelse(runif(150) > 0.8, NA, rpois(150, lambda = 5))
y_up <- rpois(150, lambda = 25)

y_interval <- generate_interval(y_low, y_up)


age <- round(rnorm(150, mean = 55, sd = 10))
tmp <- sample(1:length(age), size = ceiling(length(age)*0.4))#get a 40 % sample
age[tmp] <- 5 * floor(age[tmp]/5 + 0.5)
y_low <- age - sample(0:3, size = 150, replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.1))
y_up <- age + sample(0:3, size = 150, replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.1))
y_roundandinterval <- generate_interval(y_low, y_up)


X_imp <- iris[, 1:4]

clID <- iris$Species

datacatsingle <- cbind(y_cat, X_imp)
datacatmulti <- cbind(y_cat, X_imp, clID)

datacatordsingle <- cbind(y_catord, X_imp)
datacatordmulti <- cbind(y_catord, X_imp, clID)

databinarysingle <- cbind(y_binary, X_imp)
databinarymulti <- cbind(y_binary, X_imp, clID)

datacontsingle <- cbind(y_cont, X_imp)
datacontmulti <- cbind(y_cont, X_imp, clID)

datasemicontsingle <- cbind(y_semicont, X_imp)
datasemicontmulti <- cbind(y_semicont, X_imp, clID)

datacountsingle <- cbind(y_count, X_imp)
datacountmulti <- cbind(y_count, X_imp, clID)

datainterval <- cbind(y_interval, X_imp)

dataroundandinterval <- cbind(y_roundandinterval, X_imp)

#Run the imputations
midscatsingle <- hmi(datacatsingle,
                     model_formula = formula(y_cat ~ 1 + .))



midscatmulti <- hmi(datacatmulti,
                    model_formula = formula(y_cat ~ 1 + . + (1 + Sepal.Length|clID)))


midscatordsingle <- hmi(datacatordsingle,
                        model_formula = formula(y_catord ~ 1 + .))


#test multinomial logistic multilevel model
#library("brms")
#brms::brm (Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
#     data=iris, family="categorical",
#     prior=c(set_prior ("normal (0, 8)")))



midscatordmulti <- hmi(datacatordmulti,
                       model_formula = formula(y_catord ~ 1 + . + (1 + Sepal.Length|clID)))


midsbinarysingle <- hmi(databinarysingle,
                        model_formula = formula(y_binary ~ 1 + .))


midsbinarymulti <- hmi(databinarymulti,
                       model_formula = formula(y_binary ~ 1 + . + (1 + Sepal.Length|clID)))


midscontsingle <- hmi(datacontsingle,
                      model_formula = formula(y_cont ~ 1 + .))

midscontmulti <- hmi(datacontmulti,
                     model_formula = formula(y_cont ~ 1 + . + (1 + Sepal.Length|clID)))


midssemicontsingle <- hmi(datasemicontsingle,
                      model_formula = formula(y_semicont ~ 1 + .))


midssemicontmulti <- hmi(datasemicontmulti,
                     model_formula = formula(y_semicont ~ 1 + . + (1 + Sepal.Length|clID)))


midscountsingle <- hmi(datacountsingle,
                      model_formula = formula(y_count ~ 1 + .))


midscountmulti <- hmi(datacountmulti,
                     model_formula = formula(y_count ~ 1 + . + (1 + Sepal.Length|clID)))

midsinterval <- hmi(datainterval,
                      model_formula = formula(y_interval ~ 1 + .))

midsroundandinterval <- hmi(dataroundandinterval,
                    model_formula = formula(y_roundandinterval ~ 1 + .))

# more detailed, elaborated setting for direct comparison of hmi and mice
set.seed(123)
x <- rnorm(10000)
y <- as.matrix(cbind(1, x)) %*% matrix(c(-3, 2)) + rnorm(10000)

# induce MAR
library(boot)
misprob <- inv.logit(x)
y_obs <- ifelse(misprob > runif(10000), NA, y)

dataHMIvsMICE <- cbind(y_obs, x)
set.seed(4567)
midshmi <- hmi(dataHMIvsMICE)


set.seed(456)
midsmice <- mice(dataHMIvsMICE, method = "norm", maxit = 1)

differences_cont <- pool(with(midshmi, expr = lm(y_obs ~ 1 + x)))[[3]] -
  pool(with(midsmice, expr = lm(y_obs ~ 1 + x)))[[3]]

# another direct comparison in the case ouf categorical data
pB <- as.matrix(cbind(1, x)) %*% matrix(c(-3, 2))
pC <- as.matrix(cbind(1, x)) %*% matrix(c(1.1, -0.8))
probB <- exp(pB)/(1 + exp(pB) + exp(pC))
probC <- exp(pC)/(1 + exp(pB) + exp(pC))
probA <- 1 - (probB + probC)
probs <- matrix(c(probA, probB, probC), ncol = 3)
ycat <- array(dim = 10000)
for(i in 1:10000){
  ycat[i] <- sample(LETTERS[1:3], size = 1, prob = probs[i, ])
}
y_obs <- ifelse(misprob > runif(10000), NA, ycat)
dataHMIvsMICE <- data.frame(y_obs, x)

set.seed(1234)
midshmi <- hmi(dataHMIvsMICE)


set.seed(1234)
midsmice <- mice(dataHMIvsMICE, method = "polyreg", maxit = 1)


# run and pool models
poolhmi <- pool(with(midshmi, expr = nnet::multinom(y_obs ~ 1 + x, trace = FALSE)))[[3]]
poolmice <- pool(with(midsmice, expr = nnet::multinom(y_obs ~ 1 + x, trace = FALSE)))[[3]]
differences_cat <- (poolhmi$estimate - poolmice$estimate)/ poolmice$estimate


#test_check("hmi")

test_that("hmi returns plausible values", {
  # cat
  expect_true(sum(! apply(midscatsingle$imp$y_cat, 2, unique) %in% c("A", "B", "C")) == 0)
  expect_equal(class(mice::complete(midscatsingle, 1)), "data.frame")
  expect_equal(class(mice::complete(midscatsingle, 1)$y_cat), "factor")
  expect_equal(class(midscatsingle$pooling), c("mipo", "data.frame"))
  expect_true(sum(! apply(midscatmulti$imp$y_cat, 2, unique) %in% c("A", "B", "C")) == 0)
  expect_equal(class(mice::complete(midscatmulti, 1)), "data.frame")
  expect_equal(class(mice::complete(midscatmulti, 1)$y_cat), "factor")
  #expect_equal(class(midscatmulti$pooling), c("mipo", "data.frame")) # Currently no multilevel ordered model is available in R.
  #catord
  expect_true(sum(! apply(midscatordsingle$imp$y_cat, 2, unique) %in% c("A", "B", "C")) == 0)
  expect_equal(class(mice::complete(midscatordsingle, 1)), "data.frame")
  expect_equal(class(mice::complete(midscatordsingle, 1)$y_cat), c("ordered", "factor"))
  expect_equal(class(midscatordsingle$pooling), c("mipo", "data.frame"))
  expect_true(sum(! apply(midscatordmulti$imp$y_cat, 2, unique) %in% c("A", "B", "C")) == 0)
  expect_equal(class(mice::complete(midscatordmulti, 1)), "data.frame")
  expect_equal(class(mice::complete(midscatordmulti, 1)$y_cat), c("ordered", "factor"))
  expect_equal(class(midscatordmulti$pooling), c("mipo", "data.frame"))
  #binary
  expect_true(sum(! apply(midsbinarysingle$imp$y_binary, 2, unique) %in% 0:1) == 0)
  expect_equal(class(mice::complete(midsbinarysingle, 1)), "data.frame")
  expect_equal(class(mice::complete(midsbinarysingle, 1)$y_binary), "numeric")
  expect_equal(class(midsbinarysingle$pooling), c("mipo", "data.frame"))
  expect_true(sum(! apply(midsbinarymulti$imp$y_binary, 2, unique) %in% 0:1) == 0)
  expect_equal(class(mice::complete(midsbinarymulti, 1)), "data.frame")
  expect_equal(class(mice::complete(midsbinarymulti, 1)$y_binary), "numeric")
  expect_equal(class(midsbinarymulti$pooling), c("mipo", "data.frame"))
  #cont:
  expect_true(sum(is.na(midscontsingle$imp$y_cont)) == 0)
  expect_equal(class(mice::complete(midscontsingle, 1)), "data.frame")
  expect_equal(class(mice::complete(midscontsingle, 1)$y_cont), "numeric")
  expect_equal(class(midscontsingle$pooling), c("mipo", "data.frame"))
  expect_true(sum(is.na(midscontmulti$imp$y_cont)) == 0)
  expect_equal(class(mice::complete(midscontmulti, 1)), "data.frame")
  expect_equal(class(mice::complete(midscontmulti, 1)$y_cont), "numeric")
  expect_equal(class(midscontmulti$pooling), c("mipo", "data.frame"))
  #semicont:
  expect_true(sum(is.na(midssemicontsingle$imp$y_semicont)) == 0)
  expect_equal(class(mice::complete(midssemicontsingle, 1)), "data.frame")
  expect_equal(class(mice::complete(midssemicontsingle, 1)$y_semicont), "numeric")
  expect_equal(class(midssemicontsingle$pooling), c("mipo", "data.frame"))
  expect_true(sum(is.na(midssemicontmulti$imp$y_semicont)) == 0)
  expect_equal(class(mice::complete(midssemicontmulti, 1)), "data.frame")
  expect_equal(class(mice::complete(midssemicontmulti, 1)$y_semicont), "numeric")
  expect_equal(class(midssemicontmulti$pooling), c("mipo", "data.frame"))
  #count:
  expect_true(sum(is.na(midscountsingle$imp$y_count)) == 0)
  expect_equal(class(mice::complete(midscountsingle, 1)), "data.frame")
  expect_equal(class(mice::complete(midscountsingle, 1)$y_count), "integer")
  expect_equal(class(midscountsingle$pooling), c("mipo", "data.frame"))
  expect_true(sum(is.na(midscountmulti$imp$y_cont)) == 0)
  expect_equal(class(mice::complete(midscountmulti, 1)), "data.frame")
  expect_equal(class(mice::complete(midscountmulti, 1)$y_count), "integer")
  expect_equal(class(midscountmulti$pooling), c("mipo", "data.frame"))
  #interval:
  expect_true(sum(is.na(midsinterval$imp$y_interval)) == 0)
  expect_equal(class(mice::complete(midsinterval, 1)), "data.frame")
  expect_equal(class(mice::complete(midsinterval, 1)$y_interval), "numeric")
  expect_equal(class(midsinterval$pooling), c("mipo", "data.frame"))
  #round and interval
  expect_true(sum(is.na(midsroundandinterval$imp$y_interval)) == 0)
  expect_equal(class(mice::complete(midsroundandinterval, 1)), "data.frame")
  expect_equal(class(mice::complete(midsroundandinterval, 1)$y_roundandinterval), "numeric")
  expect_equal(class(midsroundandinterval$pooling), c("mipo", "data.frame"))
  #direct comparison
  expect_true(all(abs(differences_cont$estimate) < 0.01))
  expect_true(all(abs(differences_cat) < 0.1))
})
