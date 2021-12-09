# AAMR function: calculate AAMR
# Mahmoud Elkasabi
# 12/08/2021

AAMR <- function (Data.Name, JK = NULL, CL = NULL,
                  PeriodEnd = NULL, Period = NULL)
{
  v013 <- rweight <- sex <- exposure <- agegrp <- death <- NULL

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
  } else {
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
    cat("\n", crayon::white$bgBlue$bold("The current function calculated AAMR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_m]), "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), crayon::red$bold$underline(PeriodEndy_), "\n",
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")

  } else {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated AAMR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_]), crayon::red$bold$underline(PeriodEndy_), "\n",
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")
  }


  Data.Name$id <- c(as.factor(Data.Name$v021))
  Data.Name$rweight = Data.Name$v005 / 1000000

  DeathEx <- DataPrepareM(Data.Name, PeriodEnd, Period)

  if (is.null(JK)){PSU <- 0} else {PSU <- max(as.numeric(DeathEx$id))}

  options(dplyr.summarise.inform = FALSE)
  AGEDIST <- (dplyr::group_by(Data.Name, v013) %>% summarise(x = sum(rweight)))$x/sum(Data.Name$rweight)

  options(survey.lonely.psu = "adjust")
  dstrat <- survey::svydesign(id = ~ v021, strata = ~ v022, weights = ~ rweight, data = DeathEx)
  ASMR <- (survey::svyby(~ death, by = ~ agegrp+sex, denominator = ~ exposure,
                         design = dstrat, survey::svyratio))[, 3]

  aamr <- c(sum(ASMR[1:7] * AGEDIST), sum(ASMR[8:14] * AGEDIST))

  N     =  (dplyr::group_by(DeathEx, sex) %>% summarise(x = sum(exposure)))$x
  WN    = (survey::svyby(~ exposure, by = ~ sex, design = dstrat, survey::svytotal))$exposure
  deaths <- WN*(aamr/1000)
  AAMR_DEFT = sqrt(survey::svyby(~ death, by = ~ sex, denominator = ~ exposure,
                                 design = dstrat, deff = "replace", survey::svyratio)$DEff)
  SEX <- c("FEMALES","MALES")

  JKres <- matrix(0, nrow = PSU, ncol = 2)
  dimnames(JKres) <- list(NULL, c("AAMRj_f","AAMRj_m"))

  if (is.null(JK)){

    RESULTS <- cbind.data.frame(SEX, round(deaths, 0), round(WN, 0), aamr, round(N, 0), row.names = NULL)
    names(RESULTS) <- c("SEX", "Deaths", "Exposure_years", "AAMR", "N")
    list(RESULTS)

  } else {

    for (i in unique(as.numeric(DeathEx$id)))
    {
      Data.NameJ <- Data.Name[which(!Data.Name$id == i), ]

      AGEDISTj <- (dplyr::group_by(Data.NameJ, v013) %>% summarise(x = sum(rweight)))$x/sum(Data.NameJ$rweight)

      DeathExJ <- DeathEx[which(!DeathEx$id == i), ]

      ASMRj <- (dplyr::group_by(DeathExJ, sex, agegrp) %>% summarise(x = sum(death*rweight)))$x/
        (dplyr::group_by(DeathExJ, sex, agegrp) %>% summarise(x = sum(exposure*rweight)))$x

      JKres[i,1] <- sum(ASMRj[1:7] * AGEDISTj)
      JKres[i,2] <- sum(ASMRj[8:14] * AGEDISTj)
    }
    AAMRjf = JKres[1:PSU, 1]
    AAMRjm = JKres[1:PSU, 2]
    JKSEf = ((PSU * aamr[1] - (PSU-1) * AAMRjf)-aamr[1])^2
    JKSEm = ((PSU * aamr[2] - (PSU-1) * AAMRjm)-aamr[2])^2
    SE = c(sqrt(sum(JKSEf) / (PSU * (PSU-1))), sqrt(sum(JKSEm) / (PSU * (PSU-1))))
    RSE = SE / aamr
    LCI = aamr - (Z * SE)
    LCI[LCI <= 0] = 0
    UCI = aamr + (Z * SE)
    PSUs = c(PSU,PSU)

    RESULTS <- cbind.data.frame(SEX, round(deaths, 0), round(WN, 0), round(aamr,3), round(SE,3), round(N, 0), round(AAMR_DEFT,3), round(RSE,3), round(LCI,3), round(UCI,3), PSUs, row.names = NULL)
    names(RESULTS) <- c("SEX","Deaths", "Exposure_years", "AAMR", "SE", "N", "DEFT", "RSE", "LCI", "UCI", "iterations")
    list(RESULTS)

  }

}
