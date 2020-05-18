# DataPrepare function: prepare Birth and Exposure data
# Mahmoud Elkasabi
# 03/02/2018
# Edited on 06/10/2018
# Edited on 09/12/2018
# Edited on 01/05/2019
# Edited on 05/17/2020

DataPrepare <- function(Dat, PeriodEnd = NULL, Period = NULL)
{
  Dat$rweight = Dat$v005 / 1000000

  if (!is.null(PeriodEnd)) {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy <- as.numeric(format(as.Date(dates), "%Y"))
    PeriodEndcmc <- ((PeriodEndy - 1900) * 12) + PeriodEndm
  }

  # 1. Construct a children data ##########################################################
  myvars <- c(paste("ID"), paste("v021"), paste("v005"), paste("v008"), paste("v011"),
              paste("v022"), paste("allwoment"), paste("rweight"),
              paste("b3_0", 1:9, sep = ""), paste("b3_", 10:20, sep = ""))

  def <- reshape::melt(as.data.frame(Dat[myvars]), id = c("ID", "v021", "v005", "v008", "v011",
                                           "v022", "rweight", "allwoment"))

  names(def)[names(def) == c("value")] <- c("B3")
  def$variable <- NULL

  # 2. Briths to women 15-49 during the reference period  #################################
  if (is.null(PeriodEnd)){def$periodend = def$v008} else {def$periodend = PeriodEndcmc}

  if (is.null(Period)){def$period = 36} else {def$period = Period}
  def$age5 = as.integer((def$B3 - def$v011) / 60) - 3
  def$birth <- 0
  def$birth[def$periodend - def$B3 > 0 &
              def$periodend - def$B3 <= def$period & def$age5 >= 0] <- 1
  def$B3 <- NULL
  def$exposure = 0
  def$exposureg = 0
  def <- def[stats::complete.cases(def$age5), ]

  # 3. Exposure of women 15-49  ###########################################################
  newdata <- c("ID", "v021", "v005", "v008", "v011", "v022", "rweight", "allwoment")
  def2 <- Dat[newdata]

  if (is.null(PeriodEnd)){def2$periodend = def2$v008} else {def2$periodend = PeriodEndcmc}

  if (is.null(Period)){def2$period = 36} else {def2$period = Period}
  def2$agem   = def2$periodend - def2$v011 - 1 #age at the end of the period
  def2$age5   = as.integer(def2$agem / 60) #age group at the end of the period
  def2$higexp = def2$agem - (def2$age5 * 60) + 1  #Exposure (number of months) in current age group
  def2$higexp <- ifelse(def2$higexp >= def2$period, def2$period, def2$higexp)
  def2$age5   = def2$age5 - 3
  def2 <- def2[def2$age5 >= 0, ]

  ## Exposure in previous age group #####
  def2$lowexp <- ifelse(def2$higexp < def2$period & def2$age5 >= 1 , def2$period - def2$higexp, 0)
  def2$birth = 0
  def2$agem <- NULL
  def2l <- def2
  def2$lowexp <- NULL
  def2l$higexp <- NULL
  names(def2)[names(def2) == c("higexp")] <- c("exposure")
  names(def2l)[names(def2l) == c("lowexp")] <- c("exposure")
  def2l$age5 = def2l$age5 -1
  def3 <- rbind(def2, def2l)
  def3$exposure = def3$exposure / 12
  def3$exposureg <- ifelse(def3$age5 == 6, 0, def3$exposure)

  def4 <- rbind(def, def3)
  def4$birth = def4$birth * 1000

  BirthEx <- merge(stats::aggregate(list(def4$birth, def4$exposure, def4$exposureg),
                                    list(def4$ID, def4$v021, def4$v022, def4$age5, def4$allwoment), sum),
                   stats::aggregate(def4$rweight, list(def4$ID), mean), by = "Group.1")

  names(BirthEx) <- c("ID", "v021", "v022", "age5", "allwoment", "birth", "exposure",
                      "exposureg", "rweight")
  BirthEx <- BirthEx[BirthEx$birth != 0 | BirthEx$exposure != 0, ]
  BirthEx$id <- c(as.factor(BirthEx$v021))

  return(BirthEx)
}
