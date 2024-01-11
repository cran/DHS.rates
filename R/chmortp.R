#' Calculates the childhood component death probabilities based on survey data.
#'
#' \code{chmortp} returns weighted childhood component death probabilities for 8 age segments 0, 1-2, 3-5, 6-11,
#' 12-23, 24-35, 36-47, and 48-59 months
#' \code{chmort} returns weighted and unweighted  number of deaths and children-years exposure.
#'
#' @author Mahmoud Elkasabi.
#'
#' @param Data.Name The DHS births (BR) dataset or data from other survey with the same format.
#'
#' @param Weight Survey weight variable if other than "v005".
#'
#' @param Date_of_interview Date of Interview (CMC) variable if other than "v008".
#'
#' @param Date_of_birth Child date of birth (CMC) variable if other than "b3".
#'
#' @param Age_at_death Child age at death (in months) variable if other than "b7".
#'
#' @param PeriodEnd The end of the exposure period in YYYY-MM format; default is the date of the survey.
#'
#' @param Period The study period for mortality in months; default is 60 months (5 years).
#'
#' @param Class Allow for domain level indicators.
#'
#' @examples
#' # Calculate childhood component death probabilities based on ADBR70 data
#'
#' data("ADBR70")
#' chmortp(
#'  ADBR70
#' )
#'
#' @return Childhood component death probabilities.
#'
#' @export
chmortp <- function(Data.Name, Weight = NULL, Date_of_interview = NULL,
                   Date_of_birth = NULL, Age_at_death = NULL,
                   PeriodEnd = NULL, Period = NULL, Class = NULL){

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

      if (!("v005" %in% names(Data.Name))) stop({message("Error: v005/Sample-weight is missing")})
      if (!("v008" %in% names(Data.Name))) stop({message("Error: v008/Date-of-Interview is missing")})
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
        cat("\n", crayon::white$bgBlue$bold("The current function calculated the childhood component death probabilities based on a reference period of"),
            crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_m]), "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), crayon::red$bold$underline(PeriodEndy_), "\n" ,
            crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")

      }
      else {
        cat("\n", crayon::white$bgBlue$bold("The current function calculated the childhood component death probabilities based on a reference period of"),
            crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_]), crayon::red$bold$underline(PeriodEndy_), "\n" ,
            crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")


      }

      #######For Overall Indicators; no Class ########################################
      if (is.null(Class)){

        chmortdat<- Data.ready[, c("rweight", "v008", "b3", "b7", "tu", "tl","periodend")]

        CHMORT8 = CHMORTp(chmortdat,PeriodEnd = PeriodEnd)
        list(CHMORT8)[[1]]
      }

      #######For Class Indicators; #################################################################################################
      else{

        chmortdat<- Data.ready[, c("rweight", "v008", "b3", "b7", "tu", "tl", "periodend", Class)]

        chmortdat[[Class]] <- haven::as_factor(chmortdat[[Class]])
        #chmortdat$DomID  <- c(as.factor(chmortdat[[Class]]))
        chmortdat$DomID  <- c(as.numeric(chmortdat[[Class]]))

        RESULTS <- matrix(0, 0, ncol = 6)
        dimnames(RESULTS) <- list(NULL, c("Class", "PROBABILITY", "W.DEATHS", "W.EXPOSURE", "DEATHS", "EXPOSURE") )
        RESULTS <- as.data.frame(RESULTS)

        for (j in 1:(max(as.numeric(chmortdat$DomID)))) {

          DatD = as.data.frame(chmortdat[chmortdat$DomID == j, ])
          DatD[[Class]] = haven::as_factor(DatD[[Class]])

          CHMORT8 = CHMORTp(DatD,PeriodEnd = PeriodEnd)

            RESULTS0 <- cbind.data.frame(attributes(DatD[[Class]])$levels[[j]],
                                         CHMORT8)

            names(RESULTS0) <- c("Class", "PROBABILITY", "W.DEATHS", "W.EXPOSURE","DEATHS", "EXPOSURE")
            RESULTS = rbind(RESULTS, RESULTS0)

        }
        list(RESULTS)[[1]]
      }

    }
