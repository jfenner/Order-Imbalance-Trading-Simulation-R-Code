GetMainContract <- function (data , open.int = F) {
  if (open.int == T) {
    temp <- data [which (data$SecondOfDay < 33240) , ]
    temp <-
      temp [temp$OpenInterest == max (temp$OpenInterest) , ]
  } else {
    temp <- data [which (data$SecondOfDay == 33300) , ]
    temp <- temp [temp$Volume == max(temp$Volume) , ]
  }
  return (head (temp$InstrumentID , 1))
}

