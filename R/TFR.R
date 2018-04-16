# TFR function: calculate TFR
# Mahmoud Elkasabi
# 03/02/2018

TFR <- function (Data.Name, JK=NULL,EverMW=NULL,AWFact=NULL,PeriodEnd=NULL,Period=NULL,Class=NULL)
{
  Data.Name <- Data.Name[!Data.Name$v005==0,]
  Data.Name$ID <- seq.int(nrow(Data.Name))

  #######For Overall Indicators; no Class ######################################################################################
  if (is.null(Class)){

    if (is.null(EverMW)){Data.Name$allwoment = 1} else {Data.Name$allwoment = Data.Name$awfactt/100}
    BirthEx <- DataPrepare(Data.Name,PeriodEnd,Period)
    BirthEx$exposure = BirthEx$allwoment*BirthEx$exposure

    if (is.null(JK)){PSU <- 0} else {PSU <- max(BirthEx$id)}

    #ASFR-TFR
    dstrat<-survey::svydesign(id=~v021,strata=~v022, weights=~rweight, data=BirthEx)
    ASFR  = (survey::svyby(~birth, by=~age5, denominator=~exposure, design=dstrat, survey::svyratio))[,2]
    TFR   = 5* sum(ASFR)/1000
    N     = sum(BirthEx$exposure)
    WN    = sum(BirthEx$exposure*BirthEx$rweight)
    TFR_DEFT = sqrt(survey::deff(survey::svyratio(~birth,~exposure, dstrat,deff="replace")))

    JKres <- matrix(0, nrow = PSU, ncol = 1)
    dimnames(JKres) <- list(NULL, c("TFRj"))

    if (is.null(JK)){
      RESULTtfr <- matrix(0, nrow = 1, ncol = 3)
      dimnames(RESULTtfr) <- list(NULL, c("TFR","N", "WN") )
      RESULTtfr[1,] <- c(TFR,round(N,0),round(WN,0))
      res2 <- round(RESULTtfr,3)
      list(res2)

    } else {

      for (i in unique(BirthEx$id))
      {
        BirthExJ <- BirthEx[which(!BirthEx$id == i),]

        #ASFR-TFR
        ASFRj  = (stats::aggregate(BirthExJ$birth*BirthExJ$rweight, list(BirthExJ$age5), sum)$x)/(stats::aggregate(BirthExJ$exposure*BirthExJ$rweight, list(BirthExJ$age5), sum)$x)
        JKres[i,] <- as.numeric(5* sum(ASFRj)/1000)
      }
      TFRj= JKres[1:PSU,1]
      JKSE= ((PSU*TFR - (PSU-1)*TFRj)-TFR)^2
      SE= sqrt(sum(JKSE)/(PSU*(PSU-1)))
      RSE= SE/TFR
      LCI= TFR - (2*SE)
      UCI= TFR + (2*SE)
      RESULTS <- matrix(0, nrow = 1, ncol = 9)
      dimnames(RESULTS) <- list(NULL, c("TFR","SE", "N", "WN", "DEFT","RSE", "LCI", "UCI", "iterations") )
      RESULTS[1,] <- c(TFR,SE,round(N,0),round(WN,0),TFR_DEFT, RSE,LCI,UCI,PSU)
      RESULTS <- round(RESULTS,3)
      list(RESULTS)
    }
  }
  #######For Class Indicators; #################################################################################################
  else{
    Data.Name[[Class]] <- haven::as_factor(Data.Name[[Class]])
    Data.Name$DomID  <- c(as.factor(Data.Name[[Class]]))

    if (is.null(EverMW)){Data.Name$allwoment = 1} else if (is.null(AWFact)){Data.Name$allwoment = Data.Name$awfactt/100} else {Data.Name$allwoment = Data.Name[[AWFact]]/100}

    BirthEx <- DataPrepare(Data.Name,PeriodEnd,Period)
    BirthEx$exposure = BirthEx$allwoment*BirthEx$exposure

    Data.class <- Data.Name[,c("ID","DomID",Class)]
    Dat <- merge(BirthEx,Data.class,by="ID", all.x=TRUE)

    RESULTS <- matrix(0, nrow = max(Dat$DomID), ncol = 10)
    dimnames(RESULTS) <- list(NULL, c("Class","TFR","SE", "N", "WN", "DEFT", "RSE", "LCI", "UCI", "iterations") )
    RESULTtfr <- matrix(0, nrow = max(Dat$DomID), ncol = 4)
    dimnames(RESULTtfr) <- list(NULL, c("Class","TFR","N", "WN") )

    RESULTS <- as.data.frame(RESULTS)
    RESULTtfr <- as.data.frame(RESULTtfr)

    for (j in 1:(max(Dat$DomID))) {

      DatD = Dat[Dat$DomID == j,]
      DatD$id <- NULL
      DatD$id <- c(as.factor(DatD$v021))

      if (is.null(JK)){PSU <- 0} else {PSU <- max(DatD$id)}

      JKres <- matrix(0, nrow = PSU, ncol = 1)
      dimnames(JKres) <- list(NULL, c("TFRj"))

      #ASFR-TFR
      dstrat<-survey::svydesign(id=~v021,strata=~v022, weights=~rweight, data=DatD)
      ASFR  = (survey::svyby(~birth, by=~age5, denominator=~exposure, design=dstrat, survey::svyratio))[,2]
      TFR   = 5* sum(ASFR)/1000
      N     = sum(DatD$exposure)
      WN    = sum(DatD$exposure*DatD$rweight)
      TFR_DEFT = sqrt(survey::deff(survey::svyratio(~birth,~exposure, dstrat,deff="replace")))

      if  (PSU == 1)  {warning("Error: a single cluster Class")}
      else {

        if (is.null(JK)){
          RESULTtfr[j,] <- c(attributes(Dat[[Class]])$levels[[j]],round(TFR,3),round(N,0),round(WN,0))
        } else {

          for (i in unique(DatD$id))
          {
            BirthExJ <- DatD[which(!DatD$id == i),]

            #ASFR-TFR
            ASFRj  = (stats::aggregate(BirthExJ$birth*BirthExJ$rweight, list(BirthExJ$age5), sum)$x)/(stats::aggregate(BirthExJ$exposure*BirthExJ$rweight, list(BirthExJ$age5), sum)$x)
            JKres[i,] <- as.numeric(5* sum(ASFRj)/1000)
          }

          TFRj= JKres[1:PSU,1]
          JKSE= ((PSU*TFR - (PSU-1)*TFRj)-TFR)^2
          SE= sqrt(sum(JKSE)/(PSU*(PSU-1)))
          RSE= SE/TFR
          LCI= TFR - (2*SE)
          UCI= TFR + (2*SE)
          RESULTS[j,] <- c(attributes(Dat[[Class]])$levels[[j]],round(TFR,3),round(SE,3),round(N,0),round(WN,0), round(TFR_DEFT,3), round(RSE,3),round(LCI,3),round(UCI,3),PSU)
        }
      }
    }

    if (is.null(JK)){list(RESULTtfr)[[1]]} else {list(RESULTS)[[1]]}
  }
}
