#' National Health and Nutrition Examination Survey (2015 - 2016) - imputed
#'
#' The Income data set from the US American National Health and Nutrition Examination Survey (NHANES)
#' collected by the Centers for Disease Control and Prevention (CDC) and
#' the National Center for Health Statistics (NCHS) for 2015-2016 - imputed
#' The NHANES data are included into the package for illustration. The original data are stored in
#' \code{nhanes_org}. The modified data set \code{nhanes} is imputed by running \code{hmi(nhanes)}.
#' The Website (\url{https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/INQ_I.htm}) gives the following Analytic notes:
#' "The income questions were asked as part of household interview,
#' the interview sample weights may be used in the analysis for data in this section.
#' However, if the data is joined with other data from the Mobile Examination Center (MEC),
#' the MEC exam weights should be used. Please refer to the NHANES Analytic Guidelines
#' and the on-line NHANES Tutorial for further details on the use of sample weights
#' and other analytic issues. Both of these are available on the NHANES website."
#'
#' @format A data frame with 9971 rows and 12 variables:
#' \describe{
#' \item{inq020}{Income from wages/salaries? 1 = Yes, 2 = No}
#' \item{inq012}{Income from self employment? 1 = Yes, 2 = No}
#' \item{inq030}{Income from Social Security or Railroad Retirement? 1 = Yes, 2 = No}
#' \item{inq060}{Income from other disability pension? 1 = Yes, 2 = No}
#' \item{inq080}{Income from retirement/survivor pension? 1 = Yes, 2 = No}
#' \item{inq090}{Income from Supplemental Security Income? 1 = Yes, 2 = No}
#' \item{inq132}{Income from state/county cash assistance? 1 = Yes, 2 = No}
#' \item{inq140}{Income from interest/dividends or rental? 1 = Yes, 2 = No}
#' \item{inq150}{Income from other sources? 1 = Yes, 2 = No}
#' \item{ind235}{Monthly family income?}
#' \item{ind310}{Total savings/cash assets for the family?}
#' \item{inq320}{How do you get to the grocery store?, 1 = In my car, 2 = In a car that belongs to someone I live with, 3 = In a car that belongs to someone who lives elsewhere, 4 = Walk, 5 = Ride bicycle, 6 = Bus, subway or other public transit, 7 = Taxi or other paid driver, 8 = Someone else delivers groceries, 9 = Other, 66 = No usual mode of traveling to store, 77 = Refused, 99 = Don't know}
#' }
#' @references
#' Centers for Disease Control and Prevention (CDC).
#' National Center for Health Statistics (NCHS).
#' National Health and Nutrition Examination Survey Data.
#' Hyattsville, MD: U.S. Department of Health and Human Services, Centers for Disease Control and Prevention.
#' Variables descriptions at \url{https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/INQ_I.htm}
#' @source Website of the Centers for Disease Control and Prevention:
#' \url{https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/INQ_I.XPT}
"nhanes_imp"
