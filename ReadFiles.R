# function to read in the market data from CSV files
ReadFiles <- function (path , contract) {
  files <- list.files (path = path)
  data <- list ()
  for (f in files) {
    key <- substr (f , 7 , 14)
    temp <-
      read.csv (paste (path , f, sep = ''),
                  header = T,
                  stringsAsFactors = F)
    data [[key]] <-
      temp [which (substr (temp$InstrumentID , 1 , 2) == contract), c (1 , 3:11 , 18)]
  }
  save.image (data , file = paste (path , '/ DataSet . RData ', sep = ''))
  return (filepath)
}
