source (file ='BuildLinearData.R')
BuildLinearModel <-
  function (key,
            data,
            full.day = T,
            morning = T,
            open.int = F,
            delay = 20,
            lags = 5,
            strategy ='',
            functions = NULL) {
    # check if we need a full - day linear model or for a single trading session
    if (full.day == T) {
      morning.data <-
        BuildLinearData (
          data ,
          morning = T,
          open.int = open.int ,
          delay = delay ,
          lags = lags ,
          functions = functions
        )
      evening.data <-
        BuildLinearData (
          data ,
          morning = F,
          open.int = open.int ,
          delay = delay ,
          lags = lags ,
          functions = functions
        )
      dMid.Response <-
        c(morning.data$dMid.Response , evening.data$dMid.Response)
      VOI <- rbind (morning.data$VOI , evening.data$VOI)
      OIR <- rbind (morning.data$OIR , evening.data$OIR)
      time.secs <-
        c(morning.data$time.secs , evening.data$time.secs)
      mid.price <-
        c(morning.data$mid.price , evening.data$mid.price)
      spread <- c(morning.data$spread , evening.data$spread)
      AvgTrade.price <-
        c(morning.data$AvgTrade.price ,
          evening.data$AvgTrade.price)
      MPB <- rbind (morning.data$MPB , evening.data$MPB)
      trading.data <-
        rbind (morning.data$data , evening.data$data)
    } else {
      trading.data <-
        BuildLinearData (
          data ,
          morning = morning ,
          open.int = open.int ,
          delay = delay ,
          lags = lags ,
          functions = functions
        )
      dMid.Response <- trading.data$dMid.Response
      VOI <- trading.data$VOI
      OIR <- trading.data$OIR
      time.secs <- trading.data$time.secs
      mid.price <- trading.data$mid.price

      spread <- trading.data$spread
      AvgTrade.price <- trading.data$AvgTrade.price
      MPB <- trading.data$MPB
      trading.data <- trading.data$data
    }


    ## build the features matrix (x- variable ) based on strategy
    ## transform the variables if necessary
    identity <- function (x)
      x
    inverse <- function (x)
      1 / x
    f.VOI <-
      if (is.null (functions [['VOI']]))
        identity
    else
      functions [['VOI']]
    f.OIR <-
      if (is.null (functions [['OIR']]))
        identity
    else
      functions [['OIR']]

    ## build the explanatory variables
    x <- list ()
    x[['A']] <- data.frame (y = dMid.Response , VOI = f.VOI(VOI))
    x[['B']] <-
      data.frame (
        y = dMid.Response,
        VOI = f.VOI(VOI) / spread,
        OIR = f.OIR(OIR) / spread,
        MPB = MPB[,1] / spread
      )

    value <- {}
    # build the linear model using OLS
    if (strategy != '') {
      s <- strategy
      value$model <- lm(y ~ ., data = x[[s]])
    }

    ## return values
    value$dMid.Response <- dMid.Response ## y- value
    value$VOI <- VOI
    value$OIR <- OIR
    value$spread <- spread
    value$y <- dMid.Response
    value$x <- x
    value$data <- trading.data
    value$AvgTrade.price <- AvgTrade.price
    value$mid.price <- mid.price
    value$MPB <- MPB
    value$time.secs <- time.secs

    return (value)
  }
