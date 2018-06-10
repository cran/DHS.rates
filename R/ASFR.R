# ASFR function: calculate ASFR
# Mahmoud Elkasabi
# 03/02/2018
# Edited on 06/10/2018

ASFR <- function(Data.Name, EverMW = NULL, AWFact = NULL,
                 PeriodEnd = NULL, Period = NULL, Class = NULL) {

  Data.Name <- Data.Name[!Data.Name$v005 == 0, ]
  Data.Name$ID <- seq.int(nrow(Data.Name))

  ####### For Overall Indicators; no Class #############################################
  if (is.null(Class)) {
    if (is.null(EverMW)) {
      Data.Name$allwoment <- 1
    } else {
      Data.Name$allwoment <- Data.Name$awfactt / 100
    }
    BirthEx <- DataPrepare(Data.Name, PeriodEnd, Period)
    BirthEx$exposureg <- BirthEx$allwoment * BirthEx$exposureg

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
    LCI <- ASFR - (2 * SE)
    UCI <- ASFR + (2 * SE)

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
    BirthEx$exposureg <- BirthEx$allwoment * BirthEx$exposureg

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

      LCI <- ASFR - (2 * SE)
      UCI <- ASFR + (2 * SE)

      RESULTS0 <- cbind.data.frame(attributes(Dat[[Class]])$levels[[j]], AGE, round(ASFR, 3),
                                   round(SE, 3), round(N, 0), round(WN, 0), round(DEFT, 3),
                                   round(RSE, 3), round(LCI, 3), round(UCI, 3))

      names(RESULTS0) <- c("Class", "AGE", "ASFR", "SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI")
      RESULTS <- rbind(RESULTS, RESULTS0)
    }

    list(RESULTS)
  }
}
