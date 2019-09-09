source (file = 'GetMainContract.R')
BuildLinearData <-
  function (data ,
            morning = T,
            open.int = F,
            delay = 20,
            lags = 5,
            functions = NULL) {
    library (zoo)
    library (TTR)
    # declare constants
    day.start <- 33300 # 9:15
    AM.start <- 33300 # 9:15
    AM.open <- 33360 # 9:16 - trade open
    AM.close <- 40800 # 11:20 - trade close
    AM.end <- 41280 # 11:28

    PM.start <- 46800 # 13:00
    PM.open <- 46860 # 13:01 - trade open
    PM.close <- 54000 # 15:00 - trade close
    PM.end <- 54780 # 15:13
    start.time <- ifelse (morning , AM.start , PM.start) # - data start
    open.time <- ifelse (morning , AM.open , PM.open) # - trade open
    close.time <- ifelse (morning , AM.close , PM.close) # - trade close
    end.time <- ifelse (morning , AM.end , PM.end) # - data end

    # get main contract
    instrument <- GetMainContract (data , open.int)

    ind <-
      which (
        data$InstrumentID == instrument &
          data$SecondOfDay >= start.time &
          data$SecondOfDay < end.time
      )
    main.data <- data [ind , ]
    n <- nrow (main.data)

    time.secs <- main.data$SecondOfDay + main.data$UpdateMillisec / 1000
    ind.open <- head (which (time.secs >= open.time) , 1)
    ind.close <- head (which (time.secs >= close.time) , 1)

    # calculate variables
    mid.price <- (main.data$BidPrice1 + main.data$AskPrice1) / 2
    spread <- main.data$AskPrice1 - main.data$BidPrice1

    OIR.array <-
      (main.data$BidVolume1 - main.data$AskVolume1) / (main.data$BidVolume1
                                                       + main.data$AskVolume1)
    dBid.price <- c(0, diff (main.data$BidPrice1))
    dAsk.price <- c(0, diff (main.data$AskPrice1))

    ## build order imbalance signal according to Spec
    bid.CV <-
      (main.data$BidVolume1 - ifelse (dBid.price == 0 , c(0, main.data$BidVolume1 [-n]) , rep (0, n))) *
      as.integer (dBid.price >= 0)
    ask.CV <-
      (main.data$AskVolume1 - ifelse (dAsk.price == 0 , c(0, main.data$AskVolume1 [-n]) , rep (0, n))) *
      as.integer (dAsk.price <= 0)
    VOI.array <- bid.CV - ask.CV

    dVol <- c(NA , diff (main.data$Volume))
    dTO <- c(NA , diff (main.data$Turnover))
    AvgTrade.price <- dTO / dVol / 300
    AvgTrade.price [which (is.nan(AvgTrade.price))] <- NA
    AvgTrade.price <- na.locf (na.locf (AvgTrade.price , na.rm = F), fromLast = T)
    MPB.array <- (AvgTrade.price - c(mid.price [1] , rollmean (mid.price , k = 2)))

    k <- delay
    p <- lags
    new.ind <- (p + 1):(n - k)

    ## arithmetic average of future k midprices minus current midprice
    if (k > 0) {
      library (zoo)
      fpc <- rollmean (mid.price , k = k)[-1] - mid.price [1:(n - k)]
      dMid.Response <- c(fpc , rep (NA , k))
    } else {
      dMid.Response <- rep (0, n)
    }

    # build VOI , dMid , OIR - first p entries are useless
    VOI <- cbind (VOI.array)
    OIR <- cbind (OIR.array)
    MPB <- cbind (MPB.array)
    if (p > 0) {
      for (j in 1:p) {
        VOI <- cbind (VOI , c(rep (NA , j), VOI.array [1:(n - j)]))
        OIR <- cbind (OIR , c(rep (NA , j), OIR.array [1:(n - j)]))
        MPB <- cbind (MPB , c(rep (NA , j), MPB.array [1:(n - j)]))
      }
    }

    # trim the variables
    dMid.Response <- dMid.Response [new.ind]
    VOI <- VOI [new.ind , , drop = FALSE]
    OIR <- OIR [new.ind , , drop = FALSE]
    MPB <- MPB [new.ind , , drop = FALSE]

    colnames(VOI) <- paste('VOI.t', seq (0, p), sep = '')
    colnames(OIR) <- paste('OIR.t', seq (0, p), sep = '')
    colnames(MPB) <- paste('MPB.t', seq (0, p), sep = '')

    # trim the other supporting data
    mid.price <- mid.price[new.ind]
    spread <- spread[new.ind]
    AvgTrade.price <- AvgTrade.price[new.ind]
    main.data <- main.data[new.ind , ]
    time.secs <- time.secs[new.ind]

    ind.open <- ind.open - p
    ind.close <- ind.close - p

    # return an R object
    value <- {

    }
    value$data <- main.data
    value$dMid.Response <- dMid.Response
    value$VOI <- VOI
    value$OIR <- OIR
    value$MPB <- MPB

    value$time.secs <- time.secs
    value$ind.open <- ind.open
    value$ind.close <- ind.close

    value$mid.price <- mid.price
    value$spread <- spread
    value$AvgTrade.price <- AvgTrade.price

    return (value)
  }
