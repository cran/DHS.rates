# ASMR function: calculate ASMR
# Mahmoud Elkasabi
# 12/08/2021

ASMR <- function(Data.Name, CL = NULL,
                 PeriodEnd = NULL, Period = NULL) {

  Data.Name <- Data.Name[!Data.Name$v005 == 0, ]
  Data.Name$ID <- seq.int(nrow(Data.Name))

  # The CI confidence level
  if (is.null(CL)) {
    Z <- stats::qnorm(.025,lower.tail=FALSE)
  } else {
    Z <- stats::qnorm((100-CL)/200,lower.tail=FALSE)
  }

  ## Title for the results #########################
  if (is.null(Period)){Periodmsg = 84} else {Periodmsg = Period}

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
    cat("\n", crayon::white$bgBlue$bold("The current function calculated ASMR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_m]), "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), crayon::red$bold$underline(PeriodEndy_), "\n",
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")

  }
  else {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated ASMR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_]), crayon::red$bold$underline(PeriodEndy_), "\n" ,
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")

  }

    Data.Name$allwoment = 1
    DeathEx <- DataPrepareM(Data.Name, PeriodEnd, Period)

    options(survey.lonely.psu = "adjust")
    dstrat <- survey::svydesign(id = ~ v021, strata = ~ v022, weights = ~ rweight, data = DeathEx)

    AGE <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

    SEX <- c(rep("FEMALES",7),rep("MALES",7))

    WN <- (survey::svyby(~ exposure, by = ~ agegrp+sex, design = dstrat, survey::svytotal))$exposure

    asmr <- (survey::svyby(~ death, by = ~ agegrp+sex, denominator = ~ exposure,
                           design = dstrat, survey::svyratio))[, 3]

    deaths <- WN*(asmr/1000)

    SE <- (survey::svyby(~ death, by = ~ agegrp+sex, denominator = ~ exposure,
                         design = dstrat, survey::svyratio))[, 4]

    N <- stats::aggregate(DeathEx$exposure, list(DeathEx$agegrp, DeathEx$sex), sum)$x

    DEFT <- sqrt(survey::svyby(~ death, by = ~ agegrp+sex, denominator = ~ exposure,
                               design = dstrat, deff = "replace", survey::svyratio)$DEff)

    RSE <- survey::cv(survey::svyby(~ death, by = ~ agegrp+sex, denominator = ~ exposure,
                                    design = dstrat, survey::svyratio))
    LCI = asmr - (Z * SE)
    LCI[LCI <= 0] = 0
    UCI <- asmr + (Z * SE)

    RESULTS <- cbind.data.frame(AGE, SEX, round(deaths, 0), round(WN, 0), round(asmr, 3), round(SE, 3), round(N, 0),
                                round(DEFT, 3), round(RSE, 3), round(LCI, 3), round(UCI, 3), row.names = NULL)

    names(RESULTS) <- c("AGE", "SEX", "Deaths","Exposure_years", "ASMR", "SE", "N",  "DEFT", "RSE", "LCI", "UCI")
    list(RESULTS)

}

