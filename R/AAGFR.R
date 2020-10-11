# AAGFR function: calculate AAGFR for MMR calculation
# Mahmoud Elkasabi
# 10/05/2020

AAGFR <- function (Data.Name,
                 PeriodEnd = NULL, Period = NULL) {

  v013 <- rweight <- age5 <- birth <- exposure <- NULL

  Data.Name <- Data.Name[!Data.Name$v005 == 0, ]
  Data.Name$ID <- seq.int(nrow(Data.Name))

    Data.Name$id <- c(as.factor(Data.Name$v021))
    Data.Name$rweight = Data.Name$v005 / 1000000

    BirthEx <- DataPrepareM_GFR(Data.Name, PeriodEnd, Period)

    options(dplyr.summarise.inform = FALSE)
    AGEDIST <- (dplyr::group_by(Data.Name, v013) %>% summarise(x = sum(rweight)))$x/sum(Data.Name$rweight)

    ASFR <- (dplyr::group_by(BirthEx, age5) %>% summarise(x = sum(birth*rweight)))$x/
      (dplyr::group_by(BirthEx, age5) %>% summarise(x = sum(exposure*rweight)))$x

    gfr <- ceiling(sum(ASFR[1:7] * AGEDIST))

    list(gfr)[[1]]
}
