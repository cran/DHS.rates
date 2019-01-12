# ASFR function: calculate ASFR
# Mahmoud Elkasabi
# 03/02/2018
# Edited on 09/12/2018
# Edited on 01/05/2019

ASFR <- function(Data.Name, CL = NULL, EverMW = NULL, AWFact = NULL,
                 PeriodEnd = NULL, Period = NULL, Class = NULL) {

  Data.Name <- Data.Name[!Data.Name$v005 == 0, ]
  Data.Name$ID <- seq.int(nrow(Data.Name))

  # The CI confidence level
  if (is.null(CL)) {
    Z <- stats::qnorm(.025,lower.tail=FALSE)
  } else {
    Z <- stats::qnorm((100-CL)/200,lower.tail=FALSE)
  }

  ## Title for the results #########################
  if (is.null(Period)){Periodmsg = 36} else {Periodmsg = Period}

  if (is.null(PeriodEnd)){
    PeriodEndy_ <- as.integer((mean(Data.Name$v008) - 1)/12)+1900
    PeriodEndm_ <- round(mean(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12),0)

    PeriodEndm_m <- round(min(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12),0)
    PeriodEndm_x <- round(max(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12),0)
  }
  else {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm_ <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy_ <- as.numeric(format(as.Date(dates), "%Y"))

    if (PeriodEndm_ >=  round(mean(Data.Name$v008) - (((as.integer((mean(Data.Name$v008) - 1)/12)+1900) - 1900) * 12),0) &
        PeriodEndy_ >= as.integer((mean(Data.Name$v008) - 1)/12)+1900)

    message(crayon::bold("Note:", "\n",
            "You specified a reference period that ends after the survey fieldwork dates....."), "\n",
            "1. Make sure the dates in the survey are coded according to the Gregorian calendar.", "\n",
            "2. If the dates are coded according to the Gregorian calendar, use a proper PeriodEnd that came before the time of the survey.", "\n",
            "3. If the dates are not coded according to the Gregorian calendar, use a PeriodEnd according to the used calendar.")

  }


  if (is.null(PeriodEnd)){
    cat("\n", crayon::white$bgBlue$bold("The current function calculated ASFR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), crayon::red$bold$underline(month.abb[PeriodEndm_m]), "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), crayon::red$bold$underline(PeriodEndy_), "\n" )
  }
  else {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated ASFR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended in"), crayon::red$bold$underline(month.abb[PeriodEndm_]), crayon::red$bold$underline(PeriodEndy_), "\n" )

  }

  ####### For Overall Indicators; no Class #############################################
  if (is.null(Class)) {
    if (is.null(EverMW)) {
      Data.Name$allwoment <- 1
    } else {
      Data.Name$allwoment <- Data.Name$awfactt / 100
    }
    BirthEx <- DataPrepare(Data.Name, PeriodEnd, Period)
    BirthEx$exposure <- BirthEx$allwoment * BirthEx$exposure

    dstrat <- survey::svydesign(id = ~ v021, strata = ~ v022, weights = ~ rweight, data = BirthEx)

    AGE <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

    ASFR <- (survey::svyby(~ birth, by = ~ age5, denominator = ~ exposure,
                           design = dstrat, survey::svyratio))[, 2]

    SE <- (survey::svyby(~ birth, by = ~ age5, denominator = ~ exposure,
                         design = dstrat, survey::svyratio))[, 3]

    N <- stats::aggregate(BirthEx$exposure, list(BirthEx$age5), sum)$x

    WN <- (survey::svyby(~ exposure, by = ~ age5, design = dstrat, survey::svytotal))$exposure

    DEFT <- sqrt(survey::svyby(~ birth, by = ~ age5, denominator = ~ exposure,
                               design = dstrat, deff = "replace", survey::svyratio)$DEff)

    RSE <- survey::cv(survey::svyby(~ birth, by = ~ age5, denominator = ~ exposure,
                                    design = dstrat, survey::svyratio))
    LCI = ASFR - (Z * SE)
    LCI[LCI <= 0] = 0
    UCI <- ASFR + (Z * SE)

    RESULTS <- cbind.data.frame(AGE, round(ASFR, 3), round(SE, 3), round(N, 0), round(WN, 0),
                                round(DEFT, 3), round(RSE, 3), round(LCI, 3), round(UCI, 3))

    names(RESULTS) <- c("AGE", "ASFR", "SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI")
    list(RESULTS)

} else {

    ####### For Class Indicators; #############################################

    Data.Name[[Class]] <- haven::as_factor(Data.Name[[Class]])
    Data.Name$DomID <- c(as.factor(Data.Name[[Class]]))

    if (is.null(EverMW)) {
      Data.Name$allwoment <- 1
    } else if (is.null(AWFact)) {
      Data.Name$allwoment <- Data.Name$awfactt / 100
    } else {
      Data.Name$allwoment <- Data.Name[[AWFact]] / 100
    }
    BirthEx <- DataPrepare(Data.Name, PeriodEnd, Period)
    BirthEx$exposure <- BirthEx$allwoment * BirthEx$exposure

    Data.class <- Data.Name[, c("ID", "DomID", Class)]
    Dat <- merge(BirthEx, Data.class, by = "ID", all.x = TRUE)

    RESULTS <- matrix(0, 1, ncol = 10)
    dimnames(RESULTS) <- list(NULL, c("Class", "AGE", "ASFR", "SE", "N", "WN",
                                      "DEFT", "RSE", "LCI", "UCI"))
    RESULTS <- as.data.frame(RESULTS)

    for (j in 1:(max(Dat$DomID))) {
      DatD <- Dat[Dat$DomID == j, ]

      dstrat <- survey::svydesign(id = ~ v021, strata = ~ v022, weights = ~ rweight, data = DatD)

      AGE <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

      ASFR <- (survey::svyby(~ birth, by = ~ age5, denominator = ~ exposure,
                             design = dstrat, survey::svyratio))[, 2]

      SE <- (survey::svyby(~ birth, by = ~ age5, denominator = ~ exposure,
                           design = dstrat, survey::svyratio))[, 3]

      N <- stats::aggregate(DatD$exposure, list(DatD$age5), sum)$x

      WN <- (survey::svyby(~ exposure, by = ~ age5, design = dstrat, survey::svytotal))$exposure

      DEFT <- sqrt(survey::svyby(~ birth, by = ~ age5, denominator = ~ exposure,
                                 design = dstrat, deff = "replace", survey::svyratio)$DEff)

      RSE <- survey::cv(survey::svyby(~ birth, by = ~ age5, denominator = ~ exposure,
                                      design = dstrat, survey::svyratio))

      LCI = ASFR - (Z * SE)
      LCI[LCI <= 0] = 0
      UCI <- ASFR + (Z * SE)

      RESULTS0 <- cbind.data.frame(attributes(Dat[[Class]])$levels[[j]], AGE, round(ASFR, 3),
                                   round(SE, 3), round(N, 0), round(WN, 0), round(DEFT, 3),
                                   round(RSE, 3), round(LCI, 3), round(UCI, 3))

      names(RESULTS0) <- c("Class", "AGE", "ASFR", "SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI")
      RESULTS <- rbind(RESULTS, RESULTS0)
    }

    list(RESULTS)
  }
}
