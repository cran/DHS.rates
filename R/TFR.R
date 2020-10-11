# TFR function: calculate TFR
# Mahmoud Elkasabi
# 03/02/2018
# Edited on 09/12/2018
# Edited on 01/05/2019
# Edited on 04/02/2019
# Edited on 05/17/2020
# Edited on 10/06/2020

TFR <- function (Data.Name, JK = NULL, CL = NULL, EverMW = NULL, AWFact = NULL,
                 PeriodEnd = NULL, Period = NULL, Class = NULL)
{
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
    cat("\n", crayon::white$bgBlue$bold("The current function calculated TFR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_m]), "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), crayon::red$bold$underline(PeriodEndy_), "\n",
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")

  } else {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated TFR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_]), crayon::red$bold$underline(PeriodEndy_), "\n",
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")
  }

  #######For Overall Indicators; no Class #####################################################

  if (is.null(Class)){

    if (is.null(EverMW)){Data.Name$allwoment = 1} else {Data.Name$allwoment = Data.Name$awfactt/100}
    BirthEx <- DataPrepare(Data.Name, PeriodEnd, Period)
    BirthEx$exposure = BirthEx$allwoment * BirthEx$exposure

    if (is.null(JK)){PSU <- 0} else {PSU <- max(as.numeric(BirthEx$id))}

    #ASFR-TFR
    options(survey.lonely.psu = "adjust")
    dstrat<-survey::svydesign(id = ~v021, strata = ~v022, weights = ~rweight, data = BirthEx)
    ASFR  = (survey::svyby(~birth, by = ~age5, denominator = ~exposure,
                           design = dstrat, survey::svyratio))[, 2]
    TFR   = 5 * sum(ASFR) / 1000
    N     = sum(BirthEx$exposure)
    WN    = sum(BirthEx$exposure * BirthEx$rweight)
    TFR_DEFT = sqrt(survey::deff(survey::svyratio(~birth, ~exposure, dstrat, deff = "replace")))

    JKres <- matrix(0, nrow = PSU, ncol = 1)
    dimnames(JKres) <- list(NULL, c("TFRj"))

    if (is.null(JK)){
      RESULTtfr <- matrix(0, nrow = 1, ncol = 3)
      dimnames(RESULTtfr) <- list(NULL, c("TFR", "N", "WN") )
      RESULTtfr[1, ] <- c(TFR, round(N, 0), round(WN, 0))
      res2 <- round(RESULTtfr, 3)

      list(res2)

    } else {

      for (i in unique(as.numeric(BirthEx$id)))
      {
        BirthExJ <- BirthEx[which(!BirthEx$id == i), ]

        #ASFR-TFR
        ASFRj  = (stats::aggregate(BirthExJ$birth * BirthExJ$rweight, list(BirthExJ$age5), sum)$x) / (stats::aggregate(BirthExJ$exposure * BirthExJ$rweight, list(BirthExJ$age5), sum)$x)
        JKres[i, ] <- (5 * sum(ASFRj) / 1000)
      }
      TFRj = JKres[1:PSU, 1]
      JKSE = ((PSU * TFR - (PSU-1) * TFRj)-TFR)^2
      SE = sqrt(sum(JKSE) / (PSU * (PSU-1)))
      RSE = SE / TFR
      LCI = TFR - (Z * SE)
      LCI[LCI <= 0] = 0
      UCI = TFR + (Z * SE)
      RESULTS <- matrix(0, nrow = 1, ncol = 9)
      dimnames(RESULTS) <- list(NULL, c("TFR", "SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI", "iterations"))
      RESULTS[1, ] <- c(TFR, SE, round(N, 0), round(WN, 0), TFR_DEFT, RSE, LCI, UCI, PSU)
      RESULTS <- round(RESULTS, 3)

      list(RESULTS)
    }

  } else {

    #######For Class Indicators; #########################################################################

    Data.Name[[Class]] <- haven::as_factor(Data.Name[[Class]])
    #Data.Name$DomID  <- c(as.factor(Data.Name[[Class]]))
    Data.Name$DomID  <- c(as.numeric(Data.Name[[Class]]))

    if (is.null(EverMW)){
      Data.Name$allwoment = 1
    } else if (is.null(AWFact)){
      Data.Name$allwoment = Data.Name$awfactt / 100
    } else {
      Data.Name$allwoment = Data.Name[[AWFact]] / 100}

    BirthEx <- DataPrepare(Data.Name, PeriodEnd, Period)
    BirthEx$exposure = BirthEx$allwoment * BirthEx$exposure

    Data.class <- Data.Name[, c("ID", "DomID", Class)]
    Dat <- merge(BirthEx, Data.class, by = "ID", all.x = TRUE)

    RESULTS <- matrix(0, nrow = max(as.numeric(Dat$DomID)), ncol = 10)
    dimnames(RESULTS) <- list(NULL, c("Class", "TFR", "SE", "N", "WN", "DEFT", "RSE",
                                      "LCI", "UCI", "iterations"))
    RESULTtfr <- matrix(0, nrow = max(as.numeric(Dat$DomID)), ncol = 4)
    dimnames(RESULTtfr) <- list(NULL, c("Class", "TFR", "N", "WN"))

    RESULTS <- as.data.frame(RESULTS)
    RESULTtfr <- as.data.frame(RESULTtfr)

    for (j in 1:(max(as.numeric(Dat$DomID)))) {

      DatD = Dat[Dat$DomID == j, ]
      DatD$id <- NULL
      DatD$id <- c(as.factor(DatD$v021))

      if (is.null(JK)){
        PSU <- 0
      } else {
        PSU <- max(as.numeric(DatD$id))
      }

      JKres <- matrix(0, nrow = PSU, ncol = 1)
      dimnames(JKres) <- list(NULL, c("TFRj"))

      #ASFR-TFR
      options(survey.lonely.psu = "adjust")
      dstrat<-survey::svydesign(id = ~v021, strata = ~v022, weights = ~rweight, data = DatD)
      ASFR  = (survey::svyby(~birth, by = ~age5, denominator = ~exposure,
                             design = dstrat, survey::svyratio))[, 2]
      TFR   = 5 * sum(ASFR) / 1000
      N     = sum(DatD$exposure)
      WN    = sum(DatD$exposure * DatD$rweight)
      TFR_DEFT = sqrt(survey::deff(survey::svyratio(~birth, ~exposure, dstrat, deff = "replace")))

      if  (PSU == 1)  {
        warning("Error: a single cluster Class")
      } else {

        if (is.null(JK)){
          RESULTtfr[j, ] <- c(attributes(Dat[[Class]])$levels[[j]], round(TFR, 3),
                              round(N, 0), round(WN, 0))
        } else {

          for (i in unique(as.numeric(DatD$id)))
          {
            BirthExJ <- DatD[which(!DatD$id == i), ]

            #ASFR-TFR
            ASFRj  = (stats::aggregate(BirthExJ$birth * BirthExJ$rweight, list(BirthExJ$age5), sum)$x) / (stats::aggregate(BirthExJ$exposure * BirthExJ$rweight, list(BirthExJ$age5), sum)$x)
            JKres[i, ] <- (5 * sum(ASFRj) / 1000)
          }

          TFRj = JKres[1:PSU, 1]
          JKSE = ((PSU * TFR - (PSU-1) * TFRj)-TFR)^2
          SE = sqrt(sum(JKSE) / (PSU * (PSU-1)))
          RSE = SE / TFR
          LCI = TFR - (Z * SE)
          LCI[LCI <= 0] = 0
          UCI = TFR + (Z * SE)
          RESULTS[j, ] <- c(attributes(Dat[[Class]])$levels[[j]], round(TFR, 3), round(SE, 3),
                            round(N, 0), round(WN, 0), round(TFR_DEFT, 3), round(RSE, 3), round(LCI, 3),
                            round(UCI, 3), PSU)
        }
      }
    }

    if (is.null(JK)){list(RESULTtfr)} else {list(RESULTS)}
  }
}
