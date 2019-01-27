#' The UCI "German Credit Data" Dataset
#'
#' This dataset classifies people described by a set of attributes as good or bad credit risks.
#'
#' @format A data frame with 1000 rows and 20 variables:
#' \describe{
#' \item{account_status}{Factor. Status of existing checking account}
#' \item{duration}{Numeric. Duration in month}
#' \item{purpose}{Factor. Purpose}
#' \item{credit_history}{Factor. Credit history}
#' \item{amount}{Numeric. Credit amount}
#' \item{savings}{Numeric. Savings account/bonds }
#' \item{employment}{Factor Present employment since}
#' \item{installment_rate}{Integer. Installment rate in percentage of disposable income}
#' \item{status_gender}{Factor. Personal status and gender}
#' \item{guarantors}{Factor. Other debtors / guarantors}
#' \item{resident_since}{Numeric. Present residence since}
#' \item{property}{Factor. Property}
#' \item{age}{Numeric. Age in years}
#' \item{other_plans}{Factor. Other installment plans}
#' \item{housing}{Factor. Housing}
#' \item{num_credits}{Numeric. num_credits}
#' \item{job}{Factor. Job}
#' \item{people_maintenance}{Numeric. Number of people being liable to provide maintenance for}
#' \item{phone}{Factor. Telephone }
#' \item{foreign}{Factor. foreign worker }
#' \item{BAD}{Factor. Target feature. 1 = BAD}
#'
#'
#' ...
#' }
#' @source Professor Dr. Hofmann, Hans (1994).
#' UCI Machine Learning Repository
#' \url{[https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)]}.
#' Hamburg, Germany: Universitaet Hamburg, Institut fuer Statistik und "Oekonometrie.
"german_credit"
