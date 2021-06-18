# GFR function: calculate GFR
# Mahmoud Elkasabi
# 03/02/2018
# Edited on 09/12/2018
# Edited on 01/05/2019
# Edited on 04/02/2019
# Edited on 05/18/2020
# Edited on 10/06/2020
# Edited on 05/23/2021

GFR <- function (Data.Name, CL = NULL, EverMW = NULL, AWFact = NULL,
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
    cat("\n", crayon::white$bgBlue$bold("The current function calculated GFR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR",crayon::red$bold$underline(month.abb[PeriodEndm_m]), "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), crayon::red$bold$underline(PeriodEndy_), "\n" ,
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")

  }
  else {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated GFR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_]), crayon::red$bold$underline(PeriodEndy_), "\n" ,
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")


  }

  #######For Overall Indicators; no Class ####################################################
  if (is.null(Class)){

    if (is.null(EverMW)){Data.Name$allwoment = 1} else {Data.Name$allwoment = Data.Name$awfactt / 100}
    BirthEx <- DataPrepare(Data.Name, PeriodEnd, Period)
    BirthEx <- BirthEx[BirthEx$age5 >= 0 & BirthEx$age5 <= 6, ]
    BirthEx$exposureg = BirthEx$allwoment * BirthEx$exposureg

    options(survey.lonely.psu = "adjust")
    dstrat<-survey::svydesign(id = ~v021, strata = ~v022, weights = ~rweight, data = BirthEx)
    GFR   <- (survey::svyratio(~birth, ~exposureg, dstrat, deff = "replace"))$ratio[1, 1]
    SE    <- survey::SE(survey::svyratio(~birth, ~exposureg, dstrat, deff = "replace"))
    RSE   <- survey::cv(survey::svyratio(~birth, ~exposureg, dstrat, deff = "replace"))
    DEFT  <- sqrt(survey::deff(survey::svyratio(~birth, ~exposureg, dstrat, deff = "replace")))
    N     <- sum(BirthEx$exposureg)
    WN    <- survey::svytotal(BirthEx$exposureg, dstrat)[[1]]
    LCI = GFR - (Z * SE)
    LCI[LCI <= 0] = 0
    UCI = GFR + (Z * SE)

    RESULTS <- matrix(0, nrow = 1, ncol = 8)
    dimnames(RESULTS) <- list(NULL, c("GFR", "SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI") )
    RESULTS[1, ] <- c(round(GFR, 3), round(SE, 3), round(N, 0), round(WN, 0), round(DEFT, 3),
                      round(RSE, 3), round(LCI, 3), round(UCI, 3))
    list(RESULTS)

  } else {

  #######For Class Indicators; ###################################################################

    Data.Name[[Class]] <- haven::as_factor(Data.Name[[Class]])
    #Data.Name$DomID  <- c(as.factor(Data.Name[[Class]]))
    Data.Name$DomID  <- c(as.numeric(Data.Name[[Class]]))

    if (is.null(EverMW)){Data.Name$allwoment = 1}
      else if (is.null(AWFact)) {Data.Name$allwoment = Data.Name$awfactt / 100}
        else {Data.Name$allwoment = Data.Name[[AWFact]] / 100}

    BirthEx <- DataPrepare(Data.Name, PeriodEnd, Period)
    BirthEx <- BirthEx[BirthEx$age5 >= 0 & BirthEx$age5 <= 6, ]
    BirthEx$exposureg = BirthEx$allwoment * BirthEx$exposureg

    Data.class <- Data.Name[, c("ID", "DomID", Class)]
    Dat <- merge(BirthEx, Data.class, by = "ID", all.x = TRUE)

    RESULTS <- matrix(0, nrow = max(as.numeric(Dat$DomID)), ncol = 9)
    dimnames(RESULTS) <- list(NULL, c("Class", "GFR", "SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI"))
    RESULTS <- as.data.frame(RESULTS)

    for (j in 1:(max(as.numeric(Dat$DomID)))) {

      DatD = Dat[Dat$DomID == j, ]

      options(survey.lonely.psu = "adjust")
      dstrat<-survey::svydesign(id = ~v021, strata = ~v022, weights = ~rweight, data = DatD)
      GFR   <- (survey::svyratio(~birth, ~exposureg, dstrat, deff = "replace"))$ratio[1, 1]
      SE    <- survey::SE(survey::svyratio(~birth, ~exposureg, dstrat, deff = "replace"))
      RSE   <- survey::cv(survey::svyratio(~birth, ~exposureg, dstrat, deff = "replace"))
      DEFT  <- sqrt(survey::deff(survey::svyratio(~birth, ~exposureg, dstrat, deff = "replace")))
      N     <- sum(DatD$exposureg)
      WN    <- survey::svytotal(DatD$exposureg, dstrat)[[1]]
      #####################################################################################################
      LCI = GFR - (Z * SE)
      LCI[LCI <= 0] = 0
      UCI= GFR + (Z * SE)

      RESULTS[j, ] <- c(attributes(Dat[[Class]])$levels[[j]], round(GFR, 3), round(SE, 3),
                        round(N, 0), round(WN, 0), round(DEFT, 3), round(RSE, 3),
                        round(LCI, 3), round(UCI, 3))
    }

    list(RESULTS)
  }
}
