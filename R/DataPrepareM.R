# DataPrepareM function: prepare Death and Exposure data
# Mahmoud Elkasabi
# 09/21/2020

DataPrepareM <- function(Dat, PeriodEnd = NULL, Period = NULL)
{
  Dat$rweight = Dat$v005 / 1000000

  if (!is.null(PeriodEnd)) {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy <- as.numeric(format(as.Date(dates), "%Y"))
    PeriodEndcmc <- ((PeriodEndy - 1900) * 12) + PeriodEndm
  }

  # 1. Construct a siblings data ##########################################################
  myvarsid <- c("ID", "v021", "v005", "v008", "v022", "rweight")

  myvars <- c(myvarsid,
              paste("mmidx_0", 1:9, sep = ""), paste("mmidx_", 10:20, sep = ""),
              paste("mm1_0", 1:9, sep = ""), paste("mm1_", 10:20, sep = ""),
              paste("mm2_0", 1:9, sep = ""), paste("mm2_", 10:20, sep = ""),
              paste("mm4_0", 1:9, sep = ""), paste("mm4_", 10:20, sep = ""),
              paste("mm8_0", 1:9, sep = ""), paste("mm8_", 10:20, sep = ""),
              paste("mm9_0", 1:9, sep = ""), paste("mm9_", 10:20, sep = ""),
              paste("mm12_0", 1:9, sep = ""), paste("mm12_", 10:20, sep = ""),
              paste("mm16_0", 1:9, sep = ""), paste("mm16_", 10:20, sep = ""))

  Dat <- as.data.frame(Dat[myvars])

  def <- stats::reshape(Dat,
                 direction = "long",
                 varying = list(mmidx = 7:26, mm1 = 27:46, mm2 = 47:66, mm4 = 67:86, mm8 = 87:106,
                                mm9 = 107:126, mm12 = 127:146, mm16 = 147:166),
                 v.names = c("mmidx", "mm1", "mm2", "mm4", "mm8", "mm9", "mm12", "mm16"),
                 timevar = "alt")

  #################################################################################

  def <- def[stats::complete.cases(def$mmidx), ]
  def <- def[stats::complete.cases(def$mm1), ]
  def <- def[stats::complete.cases(def$mm2), ]
  def <- def[!def$mm1 == 8 & !def$mm1 == 9 & !def$mm2 == 8 & !def$mm2 == 9 , ]

  #################################################################################
  # 2. Deaths and exposure for each age group  #################################
  if (is.null(PeriodEnd)){def$periodend = def$v008} else {def$periodend = PeriodEndcmc}

  if (is.null(Period)){def$period = 84} else {def$period = Period}

  def$upplim <- ifelse(def$mm2 == 0, def$mm8, def$v008-1)
  def$lowlim = def$v008-def$period
  def$exposure <- ifelse(def$upplim-def$lowlim+1 < 0, 0, def$upplim - def$lowlim + 1)

  def$agegrp1 = as.integer((def$upplim-def$mm4)/60)
  def$expo1 = ifelse(def$exposure < def$upplim - (def$mm4 + def$agegrp1*60) + 1, def$exposure, def$upplim - (def$mm4 + def$agegrp1*60) + 1)
  def$death1 = ifelse(def$mm2==0 & def$expo1 > 0, 1, 0)
  def$exposure = def$exposure - def$expo1

  def$agegrp2 = def$agegrp1 - 1
  def$expo2 = ifelse(def$exposure < 60, def$exposure, 60)
  def$death2 = 0
  def$exposure = def$exposure - def$expo2

  def$agegrp3 = def$agegrp2 - 1
  def$expo3 = def$exposure
  def$death3 = 0

  myvarsid <- c("ID", "v021", "v005", "v008", "v022", "rweight",
                "mmidx","mm1","mm2","mm4","mm8","mm9","mm12", "mm16")

  myvars <- c(myvarsid,
              paste("agegrp", 1:3, sep = ""),
              paste("expo", 1:3, sep = ""),
              paste("death", 1:3, sep = ""))

  def <- as.data.frame(def[myvars])

  rdef <- stats::reshape(def,
                 direction = "long",
                 varying = list(agegrp = 15:17, expo = 18:20, death = 21:23),
                 v.names = c("agegrp", "expo", "death"),
                 timevar = "alt")

  DeathEx <- rdef[rdef$agegrp >= 3 & rdef$agegrp <= 9, ]

  DeathEx$sex = 2 - DeathEx$mm1
  DeathEx$exposure = DeathEx$expo/12
  DeathEx$death = DeathEx$death*1000

  DeathEx$id <- c(as.factor(DeathEx$v021))

  return(DeathEx)
}
