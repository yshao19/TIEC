# Download entire price history of Russell 3000
library (tidyquant)   # Data download and manipulation
library(tidyverse)
library(quantmod)     # To do quanty-type financial stuff
library(tidyr)        # Basically just for spread() function
library(timetk)       # To manipulate the data series

df <- read.csv('Russell3000_holdings.csv',
               skip = 9,
               header = TRUE)
df$Ticker

price_data_single = c()
adj_data = c()
tick_name = c()
log_adj_data = c()

for (i in 1:length(df$Ticker)) {
  # Attempt to get price data and handle errors
  price_data_single <- tryCatch({
    tq_get(df$Ticker[i],
           get = 'stock.prices',
           from = '2004-01-01',
           to = '2023-12-31')
  }, error = function(e) {
    warning(paste("Ticker", df$Ticker[i], "does not exist. Skipping to next."))
    return(NULL)  # Return NULL to signal failure
  })
  
  # Check if data retrieval was successful
  if (is.null(price_data_single)) {
    next  # Skip the rest of the loop and proceed with the next ticker
  }# Further processing if data is retrieved
  else if (length(price_data_single) == 1) {
    next
  }
  else if (nrow(price_data_single) > 0 &&
           price_data_single$date[1] != "2004-01-02") {
    next
  } else{
    # Combine adjusted data if column exists
    if ("adjusted" %in% names(price_data_single)) {
      adj_data <- cbind(adj_data, price_data_single$adjusted)
      
      log_ret_single <- price_data_single %>%
        tq_transmute(
          select = adjusted,
          mutate_fun = periodReturn,
          period = 'daily',
          col_rename = 'ret',
          type = 'log'
        )
      
      log_adj_data = cbind(log_adj_data, log_ret_single$ret)
      
      tick_name <- append(tick_name, price_data_single$symbol[1])
      
      print(paste(
        ncol(adj_data),
        i,
        df$Ticker[i],
        length(price_data_single$adjusted)
      ))
    }
  }
}

# If adj_data is not null, convert it to a dataframe
if (!is.null(adj_data)) {
  adj_data <- as.data.frame(adj_data)
  log_adj_data <- as.data.frame(log_adj_data)
}

colnames(adj_data) = tick_name
write.csv(adj_data, file = "Russell_3000_adj_close_040101_231231.csv", row.names = FALSE)

colnames(log_adj_data) = tick_name
write.csv(log_adj_data, file = "Russell_3000_log_adj_return_040101_231231.csv", row.names = FALSE)
