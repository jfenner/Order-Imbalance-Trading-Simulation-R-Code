load (file = 'DataSet.RData ')
source ('BuildLinearModel.R')
source ('LinearStrategy.R')
# ### AVERAGED LAG LINEAR STRATEGY ####
## set trading and model parameters
threshold <- 0.2
period <- 20
lags <- 5
strategy <- 'A'
coefs <- c()
## build the linear models and store their coefficients
for (i in 1:length (data)) {
  key <- names (data)[i]
  # full -day coefficients
  value <-
    BuildLinearModel (
      key ,
      data [[i]],
      full.day = T,
      delay = period ,
      lags = lags,
      strategy = strategy
    )
  model <- value$model
  coefs <- rbind (coefs , model$coefficients)
  print (names (data)[i])
}
## set the lagged coefficient weights
coef.weights <- c (1)
trade.volume <- c()
trade.costs <- c()
pnl.name <-
  paste ('pnl -', threshold , '-', strategy , period , 'F- lag ', lags , sep =
           '')
assign (pnl.name , matrix (nrow = length (data), ncol = 2))
pnl.matrix <- get (pnl.name)
pnl.matrix [1 ,] <- 0
trade.pnl <- c()
## apply the trading strategy to each trading day using historical linear model
coefficients
for (i in 1:length (data)) {
  key <- names (data)[i]
  if (i > 1) {
    coef <- 0
    w <- coef.weights [1:min (length (coef.weights), i - 1)]
    w <- w / sum (w)
    for (j in 1:length (w)) {
      coef <- coef + coefs [i - j ,] * w[j]
    }
    # morning trading using the weighted coefficients from T -1, T -2 ,...
    strat <-
      LinearStrategy (
        data [[i]],
        coef ,
        lags = lags ,
        strategy = strategy ,
        morning =
          T,
        threshold = threshold
      )
    pnl.matrix [i , 1] <- tail (strat$pnl , 1)
    trade.pnl <- c(trade.pnl , strat$trade.pnl)
    tv <- strat$trade.volume
    tc <- strat$trade.costs
    # afternoon trading using the weighted coefficients from T -1, T -2 ,...
    strat <-
      LinearStrategy (
        data [[i]],
        coef ,
        lags = lags ,
        strategy = strategy ,
        morning =
          F,
        threshold = threshold
      )
    pnl.matrix [i , 2] <- tail (strat$pnl , 1)
    trade.pnl <- c(trade.pnl , strat$trade.pnl)
    tv <- tv + strat$trade.volume
    trade.volume <- c(trade.volume , tv)
    tc <- tc + strat$trade.costs
    trade.costs <- c(trade.costs , tc)
  }
  print (
    paste (
      key ,
      strategy ,
      period ,
      threshold ,
      'P&L =',
      pnl.matrix [i , 1] ,
      pnl.matrix [i , 2] ,
      '
Total =',
      sum(pnl.matrix [1:i ,])
    )
  )
}
assign (pnl.name , pnl.matrix)
sharpe.ratio <-
  mean (rowSums (pnl.matrix)) * sqrt (nrow (pnl.matrix)) / sd(rowSums (pnl.matrix))
write.table (
  pnl.matrix ,
  paste (pnl.name , '.txt ', sep = ''),
  sep = '\t',
  row.names = F,
  col.names = F
)
