#' Calculates fertility indicators based on the Demographic and Health Surveys (DHS).
#'
#' \code{fert} returns fertility indicators such as Total Fertility Rate (TFR),
#' General Fertility Rate (GFR) and Age Specific Fertility Rate (ASFR)
#' \code{fert} returns the Standrad Error (SE), the fertility exposure (N), the weighted exposure (WN),
#' the Design Effect (DEFT), the Relative Standard Error (RSE) and the Confidence Interval (CI).
#'
#' @author Mahmoud Elkasabi.
#'
#' @param Data.Name DHS data women files (IR).
#'
#' @param Indicator Type of indicator to be calculated ("tfr", "gfr", "asfr").
#'
#' @param JK "Yes" to estimate Jackknife SE for TFR.
#'
#' @param Strata The stratification variable name if other than "v022".
#'
#' @param Cluster The sampling clusters variable name if other than "v021".
#'
#' @param Weight The sampling weight variable name if other than "v005".
#'
#' @param EverMW "Yes" for ever-married women data.
#'
#' @param AWFact The all-women-factor variable name; only for ever-married women data.
#'
#' @param Period The study period for fertility; default is 3 years.
#'
#' @param Class Allow for domain level indicators.
#'
#' @examples
#' # Calculate TFR and estimate Jackknife SE based on all women AWIR70 data
#'
#' data("AWIR70")
#' Total_Fertility_Rate <- fert(
#'  AWIR70,
#'  Indicator="tfr",
#'  JK="Yes"
#' )
#'
#' @examples
#' # Calculate GFR and estimate SE based on ever-married women EMIR70 data
#'
#' data("EMIR70")
#' General_Fertility_Rate <- fert(
#'  EMIR70,
#'  Indicator="gfr",
#'  EverMW="YES",
#'  AWFact="awfactt"
#' )
#'
#' @examples
#' # Calculate Urban/Rural level ASFR and estimate SE based on all women AWIR70 data
#'
#' data("AWIR70")
#' Age_Specific_Fertility_Rate <- fert(
#'  AWIR70,
#'  Indicator="asfr",
#'  Class="v025"
#' )
#'
#' @return Fertility indicators (TFR, GFR or ASFR), and precision indicators (SE, DEFT, RSE and CI).
#'
#' @export
fert <- function (Data.Name, Indicator, JK=NULL,Strata=NULL,Cluster=NULL,Weight=NULL,EverMW=NULL,AWFact=NULL,Period=NULL,Class=NULL)
{
  if (!Indicator %in% c("tfr", "gfr", "asfr")) stop("Please specify a valid fertility indicator, such as tfr, gfr, or asfr")

  if (!is.null(Strata)){
    Data.Name$strata = Data.Name[[Strata]]
    Data.Name$v022 = NULL
    names(Data.Name)[names(Data.Name)==c("strata")] <- c("v022")
  }

  if (!is.null(Cluster)){
    Data.Name$cluster = Data.Name[[Cluster]]
    Data.Name$v021 = NULL
    names(Data.Name)[names(Data.Name)==c("cluster")] <- c("v021")
  }

  if (!is.null(Weight)){
    Data.Name$weight = Data.Name[[Weight]]
    Data.Name$v005 = NULL
    names(Data.Name)[names(Data.Name)==c("weight")] <- c("v005")
  }

  if (!("v021" %in% names(Data.Name))) {message("Error: v021 is missing")}
  if (!("v005" %in% names(Data.Name))) {message("Error: v005 is missing")}
  if (!("v008" %in% names(Data.Name))) {message("Error: v008 is missing")}
  if (!("v011" %in% names(Data.Name))) {message("Error: v011 is missing")}
  if (!("v022" %in% names(Data.Name))) {message("Error: v022 is missing")}

    else{

    if (("TRUE" %in% (!(paste("b3_0", 1:9, sep="") %in% names(Data.Name)))) | ("TRUE" %in% (!(paste("b3_", 10:20, sep="") %in% names(Data.Name))))) {message("Warning Message: Check the Birth History variables b3_01:b3_20")}
  }

  if (Indicator == "tfr"){
    TFR(Data.Name, JK,EverMW,AWFact,Period,Class)
  }
  else if (Indicator == "gfr"){
    GFR(Data.Name, EverMW,AWFact,Period,Class)

  }
  else if (Indicator == "asfr"){
    ASFR(Data.Name, EverMW,AWFact,Period,Class)

  }
}
