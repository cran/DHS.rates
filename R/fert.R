#' Calculates fertility indicators based on survey data.
#'
#' \code{fert} returns fertility indicators such as the Total Fertility Rate (TFR),
#' General Fertility Rate (GFR), and Age Specific Fertility Rate (ASFR)
#' \code{fert} returns the Standard Error (SE), fertility exposure (N), weighted exposure (WN),
#' Design Effect (DEFT), Relative Standard Error (RSE), and Confidence Interval (CI).
#'
#' @author Mahmoud Elkasabi.
#'
#' @param Data.Name The DHS women (IR) dataset or data from other survey with the same format.
#'
#' @param Indicator Type of indicator to be calculated ("tfr", "gfr", "asfr").
#'
#' @param JK "Yes" to estimate Jackknife SE for TFR.
#'
#' @param CL Confidence level to calculate the Confidence Coefficient Z of the Confidence Intervals; default if 95.
#'
#' @param Strata Stratification variable if other than "v022".
#'
#' @param Cluster Sample cluster variable if other than "v021".
#'
#' @param Weight Survey weight variable if other than "v005".
#'
#' @param Date_of_interview Date of Interview (CMC) variable if other than "v008".
#'
#' @param Woman_DOB Woman date of birth (CMC) variable if other than "v011".
#'
#' @param EverMW "Yes" for ever-married women data.
#'
#' @param AWFact All-women factor variable in case of EverMW = “Yes”.
#'
#' @param PeriodEnd The end of the exposure period in YYYY-MM format; default is the date of the survey.
#'
#' @param Period The study period for fertility in months; default is 36 months (3 years).
#'
#' @param Class Allow for domain level indicators.
#'
#' @examples
#' # Calculate TFR and estimate Jackknife SE based on all women AWIR70 data
#'
#' data("AWIR70")
#' Total_Fertility_Rate <- fert(
#'  AWIR70,
#'  Indicator = "tfr",
#'  JK = "Yes"
#' )
#'
#' @examples
#' # Calculate GFR and estimate SE based on ever-married women EMIR70 data
#'
#' data("EMIR70")
#' General_Fertility_Rate <- fert(
#'  EMIR70,
#'  Indicator = "gfr",
#'  EverMW = "YES",
#'  AWFact = "awfactt"
#' )
#'
#' @examples
#' # Calculate Urban/Rural level ASFR and estimate SE based on all women AWIR70 data
#'
#' data("AWIR70")
#' Age_Specific_Fertility_Rate <- fert(
#'  AWIR70,
#'  Indicator = "asfr",
#'  Class = "v025"
#' )
#'
#' @return Fertility indicators (TFR, GFR, or ASFR), and precision indicators (SE, DEFT, RSE, and CI).
#'
#' @export
fert <- function(Data.Name, Indicator, JK = NULL, CL = NULL, Strata = NULL, Cluster = NULL, Weight = NULL,
                 Date_of_interview = NULL, Woman_DOB = NULL, EverMW = NULL,
                 AWFact = NULL, PeriodEnd = NULL, Period = NULL, Class = NULL){

  if (!Indicator %in% c("tfr", "gfr", "asfr"))
    stop("Please specify a valid fertility indicator, such as tfr, gfr, or asfr")

  if (!is.null(Strata)){
    Data.Name$strata = Data.Name[[Strata]]
    Data.Name$v022 = NULL
    names(Data.Name)[names(Data.Name) == c("strata")] <- c("v022")
  }

  if (!is.null(Cluster)){
    Data.Name$cluster = Data.Name[[Cluster]]
    Data.Name$v021 = NULL
    names(Data.Name)[names(Data.Name) == c("cluster")] <- c("v021")
  }

  if (!is.null(Weight)){
    Data.Name$weight = Data.Name[[Weight]]
    Data.Name$v005 = NULL
    names(Data.Name)[names(Data.Name) == c("weight")] <- c("v005")
  }

    if (!is.null(Date_of_interview)){
    Data.Name$DOI = Data.Name[[Date_of_interview]]
    Data.Name$v008 = NULL
    names(Data.Name)[names(Data.Name) == c("DOI")] <- c("v008")
  }

  if (!is.null(Woman_DOB)){
    Data.Name$WDOB = Data.Name[[Woman_DOB]]
    Data.Name$v011 = NULL
    names(Data.Name)[names(Data.Name) == c("WDOB")] <- c("v011")
  }

  if (!("v021" %in% names(Data.Name))) stop({message("Error: v021/Primary-sampling-unit is missing")})
  if (!("v005" %in% names(Data.Name))) stop({message("Error: v005/Sample-weight is missing")})
  if (!("v008" %in% names(Data.Name))) stop({message("Error: v008/Date-of-Interview is missing")})
  if (!("v011" %in% names(Data.Name))) stop({message("Error: v011/Woman-date-of-birth is missing")})
  if (!("v022" %in% names(Data.Name))) stop({message("Error: v022/Sample-strata is missing")})

    else{

    if (("TRUE" %in% (!(paste("b3_0", 1:9, sep = "") %in% names(Data.Name)))) |
        ("TRUE" %in% (!(paste("b3_", 10:20, sep = "") %in% names(Data.Name)))))
      {warning("Birth History variables b3_01:b3_20 are not complete; the missing variables were created")}
  }

  for (i in 1:9){
    if ("TRUE" %in% (!(paste("b3_0", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("b3_0", i, sep = "")]] <- NA
  }

  for (i in 10:20){
    if ("TRUE" %in% (!(paste("b3_", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("b3_", i, sep = "")]] <- NA
  }

  Data.Name <- as.data.frame(Data.Name)

  if (Indicator == "tfr"){
    TFR(Data.Name, JK, CL, EverMW, AWFact, PeriodEnd, Period, Class)[[1]]
  }
  else if (Indicator == "gfr"){
    GFR(Data.Name, CL, EverMW, AWFact, PeriodEnd, Period, Class)[[1]]

  }
  else if (Indicator == "asfr"){
    ASFR(Data.Name, CL, EverMW, AWFact, PeriodEnd, Period, Class)[[1]]

  }
}
