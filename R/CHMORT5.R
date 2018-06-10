# CHMORT5 function: calculate Children death probabilities
# NBIRTHS function: calculate total births contributed to the exposure (full or partial contribution)
# Mahmoud Elkasabi
# 04/03/2018
# Edited on 06/10/2018

### Child mortality rates
CHMORT5 <- function(Data.Name, PeriodEnd = NULL) {
  ageseg <- list(c(0, 1), c(1, 3), c(3, 6), c(6, 12), c(12, 24), c(24, 36), c(36, 48), c(48, 60))
  names(ageseg) <- c(1, 2, 3, 4, 5, 6, 7, 8)

  deathprob <- numeric(length = 8)
  names(deathprob) <- names(ageseg)

  for (i in seq_along(ageseg)) {
    segdata <- Data.Name[which((Data.Name$b7 >= ageseg[[i]][1]) | is.na(Data.Name$b7)), ]

    segdata$exposure[segdata$b3 >= (segdata$tl - ageseg[[i]][2]) &
                       segdata$b3 < (segdata$tl - ageseg[[i]][1]) ] <- 0.5

    segdata$exposure[segdata$b3 >= (segdata$tl - ageseg[[i]][1]) &
                       segdata$b3 < (segdata$tu - ageseg[[i]][2]) ] <- 1

    segdata$exposure[segdata$b3 >= (segdata$tu - ageseg[[i]][2]) &
                       segdata$b3 < (segdata$tu - ageseg[[i]][1]) ] <- 0.5

    segdata$death[segdata$b3 >= (segdata$tl - ageseg[[i]][2]) &
                    segdata$b3 < (segdata$tl - ageseg[[i]][1]) &
                    (segdata$b7 >= ageseg[[i]][1] & segdata$b7 < ageseg[[i]][2])] <- 0.5

    segdata$death[segdata$b3 >= (segdata$tl - ageseg[[i]][1]) &
                    segdata$b3 < (segdata$tu - ageseg[[i]][2]) &
                    (segdata$b7 >= ageseg[[i]][1] & segdata$b7 < ageseg[[i]][2])] <- 1

    segdata$death[segdata$b3 >= (segdata$tu - ageseg[[i]][2]) &
                    segdata$b3 < (segdata$tu - ageseg[[i]][1]) &
                    (segdata$b7 >= ageseg[[i]][1] & segdata$b7 < ageseg[[i]][2])] <- ifelse(is.null(PeriodEnd) , 1, 0.5)

    segdata$death[is.na(segdata$b7)] <- 0

    deathprob[names(ageseg)[i]] <- sum(segdata$death * segdata$rweight, na.rm = TRUE) / (sum(segdata$exposure * segdata$rweight, na.rm = TRUE))
  }

  mort_rates <- list(
    c(data.frame(dpro = deathprob[1:1])),
    c(data.frame(dpro = deathprob[1:4])),
    c(data.frame(dpro = deathprob[5:8])),
    c(data.frame(dpro = deathprob[1:8]))
  )
  names(mort_rates) <- c("NNMR", "IMR", "CMR", "U5MR")

  mort_res <- numeric(length = 5)
  names(mort_res) <- c("NNMR", "PNNMR", "IMR", "CMR", "U5MR")

  for (i in seq_along(mort_rates)) {
    mort <- mort_rates[[i]]
    mort_label <- names(mort_rates)[i]
    mort$dpro <- (1 - mort$dpro)

    dpromat <- data.matrix(mort$dpro)
    product <- matrixStats::colProds(dpromat)

    mort_res[mort_label] <- (abs(1 - product) * 1000)
  }
  mort_res[2] <- mort_res[3] - mort_res[1]

  list(mort_res)
}

### N: Total births contributed to the calculations
NBIRTHS <- function(Data.Name) {
  BirthNg <- list(c(0, 1), c(1, 12), c(0, 12), c(12, 60), c(0, 60))
  names(BirthNg) <- c("NNMR", "PNNMR", "IMR", "CMR", "U5MR")

  BirthN <- numeric(length = 5)
  names(BirthN) <- names(BirthNg)

  for (i in seq_along(BirthNg)) {
    segdata <- Data.Name[which((Data.Name$b7 >= BirthNg[[i]][1]) | is.na(Data.Name$b7)), ]

    segdata$exposure[segdata$b3 >= (segdata$tl - BirthNg[[i]][2])
                     & segdata$b3 < (segdata$tu - BirthNg[[i]][1]) ] <- 1

    BirthN[names(BirthNg)[i]] <- sum(segdata$exposure, na.rm = TRUE)
  }

  list(BirthN)
}
