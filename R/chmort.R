#' Calculates childhood mortality rates based on the Demographic and Health Surveys (DHS).
#'
#' \code{chmort} returns childhood mortality rates such as Neonatal Mortality Rate (NNMR),
#' Post-neonatal Mortality Rate (PNNMR), Infant Mortality Rate (IMR), Child Mortality Rate (CMR),
#' and Under-5 Mortality Rate (U5MR)
#' \code{chmort} returns the Standrad Error (SE), the mortality exposure (N), the weighted exposure (WN),
#' the Design Effect (DEFT), the Relative Standard Error (RSE) and the Confidence Interval (CI).
#'
#' @author Mahmoud Elkasabi.
#'
#' @param Data.Name DHS data birth files (BR).
#'
#' @param JK "Yes" to estimate Jackknife SE.
#'
#' @param CL The confidence level to calculate the Confidence Intervals; default if 95.
#'
#' @param Strata The stratification variable name if other than "v022".
#'
#' @param Cluster The sampling clusters variable name if other than "v021".
#'
#' @param Weight The sampling weight variable name if other than "v005".
#'
#' @param Date_of_interview Date of Interview variable if other than "v008".
#'
#' @param Date_of_birth Child date of birth variable if other than "b3".
#'
#' @param Age_at_death Child age at death variable if other than "b7".
#'
#' @param PeriodEnd The end of the exposure period in YYYY-MM format; default is the time of the survey.
#'
#' @param Period The study period for mortality; default is 60 months (5 years).
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
#'  Period = 120
#' )
#'
#' @examples
#' # The exposure period ends in June 2011
#'
#' data("ADBR70")
#' chmort(
#'  ADBR70,
#'  PeriodEnd = "2011-06"
#' )
#'
#' @return Childhood mortality rates (NNMR, PNNMR, IMR, CMR and U5MR), and precision indicators (SE, RSE and CI).
#'
#' @export
chmort <- function(Data.Name, JK = NULL, CL = NULL, Strata = NULL, Cluster = NULL, Weight = NULL,
                   Date_of_interview = NULL, Date_of_birth = NULL, Age_at_death = NULL,
                   PeriodEnd = NULL, Period = NULL, Class = NULL){

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

      if (!is.null(Date_of_birth)){
        Data.Name$BDOB = Data.Name[[Date_of_birth]]
        Data.Name$b3 = NULL
        names(Data.Name)[names(Data.Name) == c("BDOB")] <- c("b3")
      }

      if (!is.null(Age_at_death)){
        Data.Name$BDOD = Data.Name[[Age_at_death]]
        Data.Name$b7 = NULL
        names(Data.Name)[names(Data.Name) == c("BDOD")] <- c("b7")
      }

      if (!("v021" %in% names(Data.Name))) stop({message("Error: v021/Primary-sampling-unit is missing")})
      if (!("v005" %in% names(Data.Name))) stop({message("Error: v005/Sample-weight is missing")})
      if (!("v008" %in% names(Data.Name))) stop({message("Error: v008/Date-of-Interview is missing")})
      if (!("v022" %in% names(Data.Name))) stop({message("Error: v022/Sample-strata is missing")})
      if (!("b3" %in% names(Data.Name))) stop({message("Error: b3/Date-of-birth is missing")})
      if (!("b7" %in% names(Data.Name))) stop({message("Error: b7/Age-at-death is missing")})

      Data.ready <- as.data.frame(Data.Name[!Data.Name$v005 == 0,])

      if (is.null(Period)){Data.ready$period = 60}
      else {Data.ready$period = Period}   # refence period in months

      if (is.null(PeriodEnd)){Data.ready$periodend = Data.ready$v008}
      else {
      dates <- paste(PeriodEnd, "01", sep = "-")
      PeriodEndm <- as.numeric(format(as.Date(dates), "%m"))
      PeriodEndy <- as.numeric(format(as.Date(dates), "%Y"))
      PeriodEndcmc <- ((PeriodEndy - 1900) * 12) + PeriodEndm
      Data.ready$periodend = PeriodEndcmc
      }  # End of refence period;

      # The CI confidence level
      if (is.null(CL)) {
        Z <- stats::qnorm(.025,lower.tail=FALSE)
      } else {
        Z <- stats::qnorm((100-CL)/200,lower.tail=FALSE)
      }

      Data.ready$rweight = Data.ready$v005/1000000

      #Time period
      Data.ready$tu <- Data.ready$periodend
      Data.ready$tl <- Data.ready$periodend - Data.ready$period

      ## Title for the results #########################
      if (is.null(Period)){Periodmsg = 60} else {Periodmsg = Period}

      if (is.null(PeriodEnd)){
        PeriodEndy_ <- as.integer((mean(Data.ready$v008) - 1)/12)+1900
        PeriodEndm_ <- round(mean(Data.ready$v008) - ((PeriodEndy_ - 1900) * 12),0)

        PeriodEndm_m <- round(min(Data.ready$v008) - ((PeriodEndy_ - 1900) * 12),0)
        PeriodEndm_x <- round(max(Data.ready$v008) - ((PeriodEndy_ - 1900) * 12),0)
      }
      else {
        dates <- paste(PeriodEnd, "01", sep = "-")
        PeriodEndm_ <- as.numeric(format(as.Date(dates), "%m"))
        PeriodEndy_ <- as.numeric(format(as.Date(dates), "%Y"))

        if (PeriodEndm_ >=  round(mean(Data.ready$v008) - (((as.integer((mean(Data.ready$v008) - 1)/12)+1900) - 1900) * 12),0) &
            PeriodEndy_ >= as.integer((mean(Data.ready$v008) - 1)/12)+1900)

        message(crayon::bold("Note:", "\n",
                       "You specified a reference period that ends after the survey fieldwork dates....."), "\n",
                  "1. Make sure the dates in the survey are coded according to the Gregorian calendar.", "\n",
                  "2. If the dates are coded according to the Gregorian calendar, use a proper PeriodEnd that came before the time of the survey.", "\n",
                  "3. If the dates are not coded according to the Gregorian calendar, use a PeriodEnd according to the used calendar.")
      }

    if (is.null(PeriodEnd)){
        cat("\n", crayon::white$bgBlue$bold("The current function calculated Childhood Mortality Rates based on a reference period of"),
            crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), crayon::red$bold$underline(month.abb[PeriodEndm_m]), "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), crayon::red$bold$underline(PeriodEndy_), "\n" )
      }
      else {
        cat("\n", crayon::white$bgBlue$bold("The current function calculated Childhood Mortality Rates based on a reference period of"),
            crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended in"), crayon::red$bold$underline(month.abb[PeriodEndm_]), crayon::red$bold$underline(PeriodEndy_), "\n" )

      }

      #######For Overall Indicators; no Class ########################################
      if (is.null(Class)){

        chmortdat<- Data.ready[, c("v021", "v022", "rweight", "v008", "b3", "b7", "tu", "tl","periodend")]
        chmortdat$id <- c(as.factor(chmortdat$v021))

        if (is.null(JK)){PSU <- 0} else {PSU <- max(chmortdat$id)}
        CHMORT = CHMORT5(chmortdat,PeriodEnd = PeriodEnd)[[1]]
        NBIRTH = NBIRTHS(chmortdat)[[1]]
        NBIRTHW= NBIRTHSW(chmortdat)[[1]]
        DEFF = DEFT(chmortdat)[[1]]

        JKres <- matrix(0, nrow = PSU, ncol = 5)
        dimnames(JKres) <- list(NULL, c("NNMR", "PNNMR", "IMR", "CMR", "U5MR"))

        if (is.null(JK)){

          RESULTcmr <- matrix(0, nrow = 5, ncol = 3)
          dimnames(RESULTcmr) <- list(c("NNMR", "PNNMR", "IMR", "CMR", "U5MR" ),c("Rate","N","WN"))
          for (i in 1:(nrow(RESULTcmr))) {
            RESULTcmr[i,1] = round(CHMORT[i],2)
            RESULTcmr[i,2] = round(NBIRTH[i],0)
            RESULTcmr[i,3] = round(NBIRTHW[i],0)
          }
          list(RESULTcmr)[[1]]

        } else {

          for (i in unique(chmortdat$id))
          {
            chmortdatJ <- chmortdat[which(!chmortdat$id == i),]
            JKres[i,] <- CHMORT5(chmortdatJ,PeriodEnd = PeriodEnd)[[1]]
          }

          RESULTcmr <- matrix(0, nrow = 5, ncol = 9)
          dimnames(RESULTcmr) <- list(c("NNMR", "PNNMR", "IMR", "CMR", "U5MR"),
                                      c("Rate", "SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI", "iterations"))
          for (i in 1:(nrow(RESULTcmr))) {
            RESULTcmr[i, 1] = round(CHMORT[i],2)
            RESULTcmr[i, 2] = round(sqrt(sum(((PSU * CHMORT[i] - (PSU-1) * JKres[, i])-CHMORT[i])^2)/(PSU * (PSU-1))),2)
            RESULTcmr[i, 3] = round(NBIRTH[i],0)
            RESULTcmr[i, 4] = round(NBIRTHW[i],0)
            RESULTcmr[i, 5] = round(DEFF[i],2)
            RESULTcmr[i, 6] = round(RESULTcmr[i, 2] / RESULTcmr[i, 1],2)
            RESULTcmr[i, 7] = round(RESULTcmr[i, 1] - (Z * RESULTcmr[i, 2]),2)
            RESULTcmr[i, 7] [RESULTcmr[i, 7] <= 0]= 0
            RESULTcmr[i, 8] = round(RESULTcmr[i, 1] + (Z * RESULTcmr[i, 2]),2)
            RESULTcmr[i, 9] = PSU
          }
          list(RESULTcmr)[[1]]
        }
      }

      #######For Class Indicators; #################################################################################################
      else{

        chmortdat<- Data.ready[, c("v021", "v022", "rweight", "v008", "b3", "b7", "tu", "tl",
                                   "periodend", Class)]

        chmortdat$DomID  <- c(as.factor(chmortdat[[Class]]))

        RESULTS <- matrix(0, 0, ncol = 4)
        dimnames(RESULTS) <- list(NULL, c("Class", "R", "N", "WN") )
        RESULTS <- as.data.frame(RESULTS)

        RESULTSJK <- matrix(0, 0, ncol = 10)
        dimnames(RESULTSJK) <- list(NULL, c("Class", "R", "SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI", "iterations"))
        RESULTSJK <- as.data.frame(RESULTSJK)

        for (j in 1:(max(chmortdat$DomID))) {

          DatD = as.data.frame(chmortdat[chmortdat$DomID == j, ])
          DatD$id <- NULL
          DatD$id <- c(as.factor(DatD$v021))
          DatD[[Class]] = haven::as_factor(DatD[[Class]])

          CHMORT = CHMORT5(DatD, PeriodEnd = PeriodEnd)[[1]]
          NBIRTH = NBIRTHS(DatD)[[1]]
          NBIRTHW = NBIRTHSW(DatD)[[1]]
          DEFF = DEFT(DatD)[[1]]

          if (is.null(JK)){

            RESULTS0 <- cbind.data.frame(attributes(DatD[[Class]])$levels[[j]],
                                         round(CHMORT, 2), round(NBIRTH,0), round(NBIRTHW,0))

            names(RESULTS0) <- c("Class", "R", "N", "WN")
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

            LCI = CHMORT - (Z * RateSE)
            LCI[LCI <= 0] = 0

            RESULTS0 <- cbind.data.frame(attributes(DatD[[Class]])$levels[[j]], round(CHMORT, 2),
                                         round(RateSE, 2), round(NBIRTH,0), round(NBIRTHW,0), round(DEFF,2), round(RateSE/CHMORT, 2),
                                         round(LCI, 2), round(CHMORT + Z * RateSE, 2),
                                         PSU)

            names(RESULTS0) <- c("Class", "R", "SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI", "iterations")
            RESULTSJK = rbind(RESULTSJK, RESULTS0)
          }
        }
        if (is.null(JK)){list(RESULTS)[[1]]} else {list(RESULTSJK)[[1]]}
      }
    }
