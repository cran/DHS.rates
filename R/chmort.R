#' Calculates childhood mortality rates based on the Demographic and Health Surveys (DHS).
#'
#' \code{chmort} returns childhood mortality rates such as Neonatal Mortality Rate (NNMR),
#' Post-neonatal Mortality Rate (PNNMR), Infant Mortality Rate (IMR), Child Mortality Rate (CMR),
#' and Under-5 Mortality Rate (U5MR)
#' \code{chmort} returns the Standrad Error (SE), Number of births contributed to the death exposure (N),
#' the Relative Standard Error (RSE) and the Confidence Interval (CI).
#'
#' @author Mahmoud Elkasabi.
#'
#' @param Data.Name DHS data birth files (BR).
#'
#' @param JK "Yes" to estimate Jackknife SE.
#'
#' @param Strata The stratification variable name if other than "v022".
#'
#' @param Cluster The sampling clusters variable name if other than "v021".
#'
#' @param Weight The sampling weight variable name if other than "v005".
#'
#' @param Year_of_survey Year of survey variable if other than "v007".
#'
#' @param Date_of_interview Date of Interview variable if other than "v008".
#'
#' @param Date_of_birth Child date of birth variable if other than "b3".
#'
#' @param Date_of_death Child date of death variable if other than "b7".
#'
#' @param PeriodEnd The end year of the exposure period; default is the year of the survey.
#'
#' @param Period The study period for mortality; default is 5 years.
#'
#' @param Class Allow for domain level indicators.
#'
#' @examples
#' # Calculate five-year children mortality rates based on ADBR70 data
#'
#' data("ADBR70")
#' chmort(
#'  ADBR70,
#'  JK = "Yes"
#' )
#'
#' @examples
#' # Calculate ten-year children mortality rates based on ADBR70 data
#'
#' data("ADBR70")
#' chmort(
#'  ADBR70,
#'  JK = "Yes",
#'  Period = 10
#' )
#'
#' @examples
#' # The exposure period ends in 2011
#'
#' data("ADBR70")
#' chmort(
#'  ADBR70,
#'  PeriodEnd = 2011
#' )
#'
#' @return Childhood mortality rates (NNMR, PNNMR, IMR, CMR and U5MR), and precision indicators (SE, RSE and CI).
#'
#' @export
chmort <- function(Data.Name, JK = NULL, Strata = NULL, Cluster = NULL, Weight = NULL, Year_of_survey = NULL,
                   Date_of_interview = NULL, Date_of_birth = NULL, Date_of_death = NULL, PeriodEnd = NULL,
                   Period = NULL, Class = NULL){

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

  if (!is.null(Year_of_survey)){
    Data.Name$YOS = Data.Name[[Year_of_survey]]
    Data.Name$v007 = NULL
    names(Data.Name)[names(Data.Name) == c("YOS")] <- c("v007")
  }

  if (!is.null(Date_of_interview)){
    Data.Name$DOI = Data.Name[[Date_of_interview]]
    Data.Name$v008 = NULL
    names(Data.Name)[names(Data.Name) == c("DOI")] <- c("v008")
  }

  if (!is.null(Date_of_birth)){
    Data.Name$BDOB = Data.Name[[Date_of_birth]]
    Data.Name$b3 = NULL
    names(Data.Name)[names(Data.Name) == c("BDOB")] <- c("b3")
  }

  if (!is.null(Date_of_death)){
    Data.Name$BDOD = Data.Name[[Date_of_death]]
    Data.Name$b7 = NULL
    names(Data.Name)[names(Data.Name) == c("BDOD")] <- c("b7")
  }

  if (!("v021" %in% names(Data.Name))) stop({message("Error: v021/Primary-sampling-unit is missing")})
  if (!("v005" %in% names(Data.Name))) stop({message("Error: v005/Sample-weight is missing")})
  if (!("v007" %in% names(Data.Name))) stop({message("Error: v007/Year-of-survey is missing")})
  if (!("v008" %in% names(Data.Name))) stop({message("Error: v008/Date-of-Interview is missing")})
  if (!("v022" %in% names(Data.Name))) stop({message("Error: v022/Sample-strata is missing")})
  if (!("b3" %in% names(Data.Name))) stop({message("Error: b3/Date-of-birth is missing")})
  if (!("b7" %in% names(Data.Name))) stop({message("Error: b7/Date-of-death is missing")})

 Data.ready <- as.data.frame(Data.Name[!Data.Name$v005 == 0,])

  if (is.null(Period)){Data.ready$period = 60}
   else {Data.ready$period = Period * 12}   # refence period in years

  if (is.null(PeriodEnd)){Data.ready$periodend = 0}
  else {Data.ready$periodend = (Data.ready$v007 - PeriodEnd) * 12}  # End of refence period in years;
  #0 when the end is the DOI

  Data.ready$rweight = Data.ready$v005/1000000

  #Time period
  Data.ready$tu <- (Data.ready$v008 - Data.ready$periodend)
  Data.ready$tl <- (Data.ready$v008 - Data.ready$periodend) - Data.ready$period

  #######For Overall Indicators; no Class ########################################
  if (is.null(Class)){

  chmortdat<- Data.ready[, c("v021", "v022", "rweight", "v007", "v008", "b3", "b7", "tu", "tl","periodend")]
  chmortdat$id <- c(as.factor(chmortdat$v021))

    if (is.null(JK)){PSU <- 0} else {PSU <- max(chmortdat$id)}
    CHMORT = CHMORT5(chmortdat,PeriodEnd = PeriodEnd)[[1]]
    NBIRTH = NBIRTHS(chmortdat)[[1]]

    JKres <- matrix(0, nrow = PSU, ncol = 5)
    dimnames(JKres) <- list(NULL, c("NNMR", "PNNMR", "IMR", "CMR", "U5MR"))

    if (is.null(JK)){

      RESULTcmr <- matrix(0, nrow = 5, ncol = 2)
      dimnames(RESULTcmr) <- list(c("NNMR", "PNNMR", "IMR", "CMR", "U5MR" ),c("Rate","N"))
      for (i in 1:(nrow(RESULTcmr))) {
        RESULTcmr[i,1] = CHMORT[i]
        RESULTcmr[i,2] = NBIRTH[i]
        }
      list(round(RESULTcmr,2))

    } else {

      for (i in unique(chmortdat$id))
        {
        chmortdatJ <- chmortdat[which(!chmortdat$id == i),]
        JKres[i,] <- CHMORT5(chmortdatJ,PeriodEnd = PeriodEnd)[[1]]
        }

      RESULTcmr <- matrix(0, nrow = 5, ncol = 7)
      dimnames(RESULTcmr) <- list(c("NNMR", "PNNMR", "IMR", "CMR", "U5MR"),
                                  c("Rate", "SE", "N", "RSE", "LCI", "UCI", "iterations"))
      for (i in 1:(nrow(RESULTcmr))) {
        RESULTcmr[i, 1] = CHMORT[i]
        RESULTcmr[i, 2] = sqrt(sum(((PSU * CHMORT[i] - (PSU-1) * JKres[, i])-CHMORT[i])^2)/(PSU * (PSU-1)))
        RESULTcmr[i, 3] = NBIRTH[i]
        RESULTcmr[i, 4] = RESULTcmr[i, 2] / RESULTcmr[i, 1]
        RESULTcmr[i, 5] = RESULTcmr[i, 1] - (2 * RESULTcmr[i, 2])
        RESULTcmr[i, 6] = RESULTcmr[i, 1] + (2 * RESULTcmr[i, 2])
        RESULTcmr[i, 7] = PSU
        }
      list(round(RESULTcmr, 2))
      }
  }

  #######For Class Indicators; #################################################################################################
  else{

    chmortdat<- Data.ready[, c("v021", "v022", "rweight", "v007", "v008", "b3", "b7", "tu", "tl",
                               "periodend", Class)]

    chmortdat$DomID  <- c(as.factor(chmortdat[[Class]]))

    RESULTS <- matrix(0, 0, ncol = 3)
    dimnames(RESULTS) <- list(NULL, c("Class", "R", "N") )
    RESULTS <- as.data.frame(RESULTS)

    RESULTSJK <- matrix(0, 0, ncol = 8)
    dimnames(RESULTSJK) <- list(NULL, c("Class", "R", "SE", "N", "RSE", "LCI", "UCI", "iterations"))
    RESULTSJK <- as.data.frame(RESULTSJK)

    for (j in 1:(max(chmortdat$DomID))) {

      DatD = as.data.frame(chmortdat[chmortdat$DomID == j, ])
      DatD$id <- NULL
      DatD$id <- c(as.factor(DatD$v021))
      DatD[[Class]] = haven::as_factor(DatD[[Class]])

      CHMORT = CHMORT5(DatD, PeriodEnd = PeriodEnd)[[1]]
      NBIRTH = NBIRTHS(DatD)[[1]]

        if (is.null(JK)){

            RESULTS0 <- cbind.data.frame(attributes(DatD[[Class]])$levels[[j]],
                                         round(CHMORT, 2), NBIRTH)

            names(RESULTS0) <- c("Class", "R", "N")
            RESULTS = rbind(RESULTS, RESULTS0)

        }else {

          if (is.null(JK)){PSU <- 0} else {PSU <- max(DatD$id)}
          if  (PSU == 1)  {warning("Error: a single cluster Class")}

          JKres <- matrix(0, nrow = 5, ncol = 0)
          dimnames(JKres) <- list(c("NNMR", "PNNMR", "IMR", "CMR", "U5MR"), NULL)

            for (i in unique(DatD$id))
            {
              DatDJ <- DatD[which(!DatD$id == i), ]
              CHMORTj = CHMORT5(DatDJ, PeriodEnd = PeriodEnd)[[1]]
              JKres0 <- cbind.data.frame(CHMORTj)
              JKres = cbind(JKres, JKres0)
            }

          RateSE <- numeric(length = 5)
          names(RateSE) <- c("NNMR", "PNNMR", "IMR", "CMR", "U5MR")

            for (i in seq_along (RateSE)) {
            JKSE = ((PSU * CHMORT[[i]] - (PSU-1) * JKres[i, ])-CHMORT[[i]])^2
            RateSE[names(RateSE)[i]] = sqrt(sum(JKSE)/(PSU * (PSU-1)))
            }

          RESULTS0 <- cbind.data.frame(attributes(DatD[[Class]])$levels[[j]], round(CHMORT, 2),
                                       round(RateSE, 2), NBIRTH, round(RateSE/CHMORT, 2),
                                       round(CHMORT - 2 * RateSE, 2), round(CHMORT + 2 * RateSE, 2),
                                       PSU)

          names(RESULTS0) <- c("Class", "R", "SE", "N", "RSE", "LCI", "UCI", "iterations")
          RESULTSJK = rbind(RESULTSJK, RESULTS0)
      }
    }
    if (is.null(JK)){list(RESULTS)} else {list(RESULTSJK)}
  }
}
