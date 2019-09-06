# ###################################################
## LINEAR MODEL STRATEGY :
## BUY SIGNAL (at t)
## * E[ FPC (t)] >= 0.2
##
## SELL SIGNAL (at t)
## * E[ FPC (t)] <= -0.2
##
## if signal hits , buy or sell maximum position
# ###################################################
source (file = ' GetMainContract.R')
source (file = ' BuildLinearData.R')
LinearStrategy <-
  function (data ,
            coefs ,
            lags ,
            strategy = 'A',
            threshold = 0.2 ,
            morning = T,
            open.int = F,
            trade.at.mid = F,
            functions = NULL) {
    ## get all the market data ( this would be a data - stream in a real - time system )
    TR.COST <- 2.5 * 1e - 5
    value <-
      BuildLinearData (
        data ,
        morning = morning ,
        open.int = open.int ,
        delay = 0,
        lags = lags
      )
    main.data <- value$data
    n <- nrow (main.data)
    mid.price <- value$mid.price
    spread <- value$spread
    time.secs <- value$time.secs
    ind.open <- value$ind.open
    ind.close <- value$ind.close
    own <- F
    pos <- 0
    strat <- rep (0, n)
    realized.pnl <- rep(NA , n)
    total.trade.pnl <- c()
    returns <- c()
    pnl <- 0
    trade.costs <- 0
    buy.price <- 0
    sell.price <- 0
    entry <- 0
    trade.volume <- 0
    sharpes <- c()
    # get the vector of bid/ ask prices ( this will be scalar in data stream )
    ask <- if (trade.at.mid)
      mid.price
    else
      main.data$AskPrice1
    bid <- if (trade.at.mid)
      mid.price
    else
      main.data$BidPrice1
    # Set the x- values to be used in prediction depending on strategy
    # these would be scalar in a data stream
    VOI <- value$VOI
    OIR <- value$OIR
    MPB <- value$MPB
    identity <- function (x)
      x
    f.VOI <-
      if (is.null (functions [['VOI ']]))
        identity
    else
      functions [['VOI ']]
    f.OIR <-
      if (is.null (functions [['OIR ']]))
        identity
    else
      functions [['OIR ']]
    x <- cbind (rep (1, n))
    if (strategy == 'A') {
      x <- cbind (x, f.VOI (VOI))
    } else if (strategy == 'B') {
      x <-
        cbind (x, f.VOI (VOI) / spread , f.OIR(OIR) / spread , MPB [, 1] / spread)
    } else {
      stop (paste ('Missing Linear Strategy :', strategy))
    }
    # this is where we assume we get a data stream instead of looping through the
    dataset
    # multiply the coefficients with the factors and check if it 's above / below
    threshold
    # and trade if the signal is good
    # in an actual trading system , the decision would be calculated by a strategy
    engine
    # having the real - time data fed into the engine via a data stream
    # but in this simulation , we just assume we have the full dataset and the
    # strategy engine is the coefficient multiplication on the next line
    efpc.vec <-
      rowSums (x * matrix (rep (coefs , n), byrow = T, nrow = n))
    # each k = 500 ms;
    for (k in trade.ind) {
      efpc <- efpc.vec [k]
      ## check if we are within trading hours
      if (k >= ind.open &
          k < ind.close & own == F & efpc >= threshold) {
        ## BUY to OPEN
        strat [k] <- 1
        own = T
        pos <- 1
        buy.price <- ask[k]
        entry <- k
        tc <- buy.price * TR.COST
        trade.costs <- trade.costs + tc
        trade.volume <- trade.volume + 1
      } else if (k >= ind.open &
                 k < ind.close & own == F & efpc <= -threshold) {
        ## SELL to OPEN
        strat [k] <- -1
        own = T
        pos <- -1
        sell.price <- bid[k]
        entry <- k
        tc <- sell.price * TR.COST
        trade.costs <- trade.costs + tc
        trade.volume <- trade.volume + 1
      } else if (own == T & pos == 1 & efpc <= -threshold) {
        ## SELL to CLOSE
        strat [k] <- -1
        own <- F
        pos <- 0
        sell.price <- bid[k]
        tc <- tc + sell.price * TR.COST
        trade.costs <- trade.costs + tc
        trade.pnl <- sell.price - buy.price - tc
        pnl <- pnl + trade.pnl
        trade.volume <- trade.volume + 1
        total.trade.pnl <- c(total.trade.pnl , trade.pnl)
        if (k >= ind.open & k < ind.close) {
          ## SELL to OPEN
          strat [k] <- -2
          own <- T
          pos <- -1
          sell.price <- bid[k]
          entry <- k
          tc <- sell.price * TR.COST
          trade.costs <- trade.costs + tc
          trade.volume <- trade.volume + 1
        }
      } else if (own == T & pos == -1 & efpc >= threshold) {
        ## BUY to CLOSE
        strat [k] <- 1
        own = F
        pos <- 0
        buy.price <- ask[k]
        tc <- tc + buy.price * TR.COST
        trade.costs <- trade.costs + tc
        trade.pnl <- sell.price - buy.price - tc
        pnl <- pnl + trade.pnl
        trade.volume <- trade.volume + 1
        total.trade.pnl <- c(total.trade.pnl , trade.pnl)
        if (k >= ind.open & k < ind.close) {
          ## BUY to OPEN
          strat [k] <- 2
          own <- T
          pos <- 1
          buy.price <- ask[k]
          entry <- k
          tc <- buy.price * TR.COST
          trade.costs <- trade.costs + tc
          trade.volume <- trade.volume + 1
        }
      }
      realized.pnl [k] <- pnl
    }
    # check if we have a left - over position at end -of - day and close it
    if (sum (strat) == 1) {
      if (strat [n] == 1) {
        strat [n] <- 0
        trade.volume <- trade.volume - 1
      } else {
        strat [n] <- -1
        sell.price <- bid[n]
        tc <- tc + sell.price * TR.COST
        trade.costs <- trade.costs + tc
        trade.pnl <- sell.price - buy.price - tc
        pnl <- pnl + trade.pnl
        realized.pnl [n] <- pnl
        total.trade.pnl <- c(total.trade.pnl , trade.pnl)
        trade.volume <- trade.volume + 1
      }
    } else if (sum (strat) == -1) {
      if (strat [n] == -1) {
        strat [n] <- 0
        trade.volume <- trade.volume - 1
      } else {
        strat [n] <- 1
        buy.price <- ask[n]
        tc <- tc + buy.price * TR.COST
        trade.costs <- trade.costs + tc
        trade.pnl <- (sell.price - buy.price) - tc
        pnl <- pnl + trade.pnl
        realized.pnl [n] <- pnl
        total.trade.pnl <- c(total.trade.pnl , trade.pnl)
        trade.volume <- trade.volume + 1
      }
    }
    # return stats
    realized.pnl <- na.locf (c(0, realized.pnl))[-1]
    value <- {
      
    }
    value$time <- time.secs
    value$pnl <- realized.pnl
    value$strategy <- strat
    value$trade.volume <- trade.volume
    value$trade.pnl <- total.trade.pnl
    value$trade.costs <- trade.costs
    return (value)
  }