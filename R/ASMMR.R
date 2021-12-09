# ASMMR function: calculate ASMMR
# Mahmoud Elkasabi
# 12/08/2021

ASMMR <- function(Data.Name, CL = NULL,
                 PeriodEnd = NULL, Period = NULL) {
  mm1 <- NULL

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
    cat("\n", crayon::white$bgBlue$bold("The current function calculated ASMMR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_m]), "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), crayon::red$bold$underline(PeriodEndy_), "\n",
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")

  }
  else {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated ASMMR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_]), crayon::red$bold$underline(PeriodEndy_), "\n" ,
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")

  }

    DeathEx <- DataPrepareM(Data.Name, PeriodEnd, Period)

    DeathEx$mm9[is.na(DeathEx$mm9)] <- 0
    DeathEx$prm_death = ifelse(DeathEx$mm1 ==2 &  DeathEx$mm9 >= 2 & DeathEx$mm9 <= 6, DeathEx$death, 0)

    PRMMRT = 0
    PRMMRT <- ifelse("TRUE" %in% ("PRMMRT" %in% names(Data.Name)), 0, 1)

    if (PRMMRT==0) {

      stop({message("Error: mm16 is missing; calculating Maternal Mortality Rate is not possible. Try Indicator = ASPRMR for Pregnancy Related Mortality Rate.")})

    } else {
      DeathEx$mm_death = 0
      DeathEx$mm_death = ifelse(((DeathEx$mm12 >= 100 &  DeathEx$mm12 <= 141) |
                                   (DeathEx$mm12 == 198 | DeathEx$mm12 == 199 | is.na(DeathEx$mm12))) &
                                  (!DeathEx$mm16 == 1 & !DeathEx$mm16 == 2),
                                DeathEx$prm_death, 0)

      DeathEx$mm_death = ifelse(is.na(DeathEx$mm_death),
                                0, DeathEx$mm_death)
    }

    options(survey.lonely.psu = "adjust")
    dstrat <- survey::svydesign(id = ~ v021, strata = ~ v022, weights = ~ rweight, data = DeathEx)

    dsub <- subset(dstrat, mm1==2)

    AGE <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

    asmmr <- (survey::svyby(~ mm_death, by = ~ agegrp, denominator = ~ exposure,
                           design = dsub, survey::svyratio))[, 2]

    PMDF <- 100*(survey::svyby(~ mm_death, by = ~ agegrp, denominator = ~ death,
                           design = dsub, survey::svyratio))[, 2]

    SE <- (survey::svyby(~ mm_death, by = ~ agegrp, denominator = ~ exposure,
                         design = dsub, survey::svyratio))[, 3]

    N <- stats::aggregate(DeathEx$exposure, list(DeathEx$agegrp, DeathEx$sex), sum)$x[1:7]

    WN <- (survey::svyby(~ exposure, by = ~ agegrp, design = dsub, survey::svytotal))$exposure

    deaths <- WN*(asmmr/1000)

    DEFT <- sqrt(survey::svyby(~ mm_death, by = ~ agegrp, denominator = ~ exposure,
                               design = dsub, deff = "replace", survey::svyratio)$DEff)

    RSE <- survey::cv(survey::svyby(~ mm_death, by = ~ agegrp, denominator = ~ exposure,
                                    design = dsub, survey::svyratio))
    LCI = asmmr - (Z * SE)
    LCI[LCI <= 0] = 0
    UCI <- asmmr + (Z * SE)

    RESULTS <- cbind.data.frame(AGE, round(PMDF, 2), round(deaths, 0), round(WN, 0), round(asmmr, 3), round(SE, 3), round(N, 0),
                                round(DEFT, 3), round(RSE, 3), round(LCI, 3), round(UCI, 3), row.names = NULL)

    names(RESULTS) <- c("AGE", "Maternal_Deaths%", "Deaths", "Exposure_years", "ASMMR", "SE", "N", "DEFT", "RSE", "LCI", "UCI")
    list(RESULTS)
}
