# GFR function: calculate GFR
# Mahmoud Elkasabi
# 03/02/2018

GFR <- function (Data.Name, EverMW=NULL,AWFact=NULL,Period=NULL,Class=NULL)
{
  Data.Name <- Data.Name[!Data.Name$v005==0,]
  Data.Name$ID <- seq.int(nrow(Data.Name))

  #######For Overall Indicators; no Class ######################################################################################
  if (is.null(Class)){

    if (is.null(EverMW)){Data.Name$allwoment = 1} else {Data.Name$allwoment = Data.Name$awfactt/100}
    BirthEx <- DataPrepare(Data.Name,Period)
    BirthEx$exposureg = BirthEx$allwoment*BirthEx$exposureg

    dstrat<-survey::svydesign(id=~v021,strata=~v022, weights=~rweight, data=BirthEx)
    GFR   <- (survey::svyratio(~birth,~exposureg, dstrat,deff="replace"))$ratio[1,1]
    SE    <- survey::SE(survey::svyratio(~birth,~exposureg, dstrat,deff="replace"))
    RSE   <- survey::cv(survey::svyratio(~birth,~exposureg, dstrat,deff="replace"))
    DEFT  <- sqrt(survey::deff(survey::svyratio(~birth,~exposureg, dstrat,deff="replace")))
    N     <- sum(BirthEx$exposureg)
    WN    <- survey::svytotal(BirthEx$exposureg, dstrat)[[1]]
    LCI= GFR - (2*SE)
    UCI= GFR + (2*SE)

    RESULTS <- matrix(0, nrow = 1, ncol = 8)
    dimnames(RESULTS) <- list(NULL, c("GFR","SE", "N", "WN", "DEFT","RSE", "LCI", "UCI") )
    RESULTS[1,] <- c(round(GFR,3),round(SE,3),round(N,0),round(WN,0),round(DEFT,3),round(RSE,3),round(LCI,3),round(UCI,3))
    list(RESULTS)

  }
  #######For Class Indicators; #################################################################################################
  else{

    Data.Name$DomID  <- c(as.factor(Data.Name[[Class]]))

    if (is.null(EverMW)){Data.Name$allwoment = 1} else if (is.null(AWFact)){Data.Name$allwoment = Data.Name$awfactt/100} else {Data.Name$allwoment = Data.Name[[AWFact]]/100}
    BirthEx <- DataPrepare(Data.Name,Period)
    BirthEx$exposureg = BirthEx$allwoment*BirthEx$exposureg

    Data.class <- Data.Name[,c("ID","DomID",Class)]
    Dat <- merge(BirthEx,Data.class,by="ID", all.x=TRUE)

    RESULTS <- matrix(0, nrow = max(Dat$DomID), ncol = 9)
    dimnames(RESULTS) <- list(NULL, c("Class","GFR","SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI") )
    RESULTS <- as.data.frame(RESULTS)

    for (j in 1:(max(Dat$DomID))) {

      DatD = Dat[Dat$DomID == j,]

      dstrat<-survey::svydesign(id=~v021,strata=~v022, weights=~rweight, data=DatD)
      GFR   <- (survey::svyratio(~birth,~exposureg, dstrat,deff="replace"))$ratio[1,1]
      SE    <- survey::SE(survey::svyratio(~birth,~exposureg, dstrat,deff="replace"))
      RSE   <- survey::cv(survey::svyratio(~birth,~exposureg, dstrat,deff="replace"))
      DEFT  <- sqrt(survey::deff(survey::svyratio(~birth,~exposureg, dstrat,deff="replace")))
      N     <- sum(DatD$exposureg)
      WN    <- survey::svytotal(DatD$exposureg, dstrat)[[1]]
      #####################################################################################################
      LCI= GFR - (2*SE)
      UCI= GFR + (2*SE)

      RESULTS[j,] <- c(attributes(Dat[[Class]])$levels[[j]],round(GFR,3),round(SE,3),round(N,0),round(WN,0),round(DEFT,3),round(RSE,3),round(LCI,3),round(UCI,3))
    }

    list(RESULTS)
  }
}
