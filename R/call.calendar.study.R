# title: "Call Calendar Study"
# author: "Jason Taylor"

# Todos:
# - Change the custom open dates from csv to save maybe?

call.calendar <- function(progress.int, t) {
  shiny::withProgress(message = "Progress Bar", detail = "Opening Trades", {
    l.positions <- data.frame()
    s.positions <- data.frame()
    short.results <- data.frame()
    long.results <- data.frame()
    results <- data.frame()
    
    # Set the first possible opening date based on the stock loaded
    min.date <- min(complete.data$date)
    
    # Set the last day for expiration dates to keep us from opening trades
    # we can't close
    max.date <- max(complete.data$date)
    trade.data <- dplyr::filter(complete.data, call.put == "C",
                                expiration <= max.date)
    
    # Filter first day month/week data to dates we have options chains for
    first.day <- dplyr::filter(first.day, date >= min.date)
    
    for (i in unique(first.day$date))  {
      shiny::incProgress(progress.int)
      t = t + 1
      start.data <- dplyr::filter(trade.data, date == i) # Set open date
      # Check for data set and that iv rank is within range
      if (nrow(start.data) > 0 && start.data[1, iv.rank] >= low.iv &&
          start.data[1, iv.rank] <= high.iv) {
        short.data <- dplyr::mutate(start.data, dte.diff = abs(s.dte - dte))
        # Closest to DTE
        short.data <- dplyr::filter(short.data, dte.diff == min(dte.diff)) 
        # Min DTE if there are two same diff from target DTE
        short.data <- dplyr::filter(short.data, dte == min(dte)) 
        # Short must be OTM
        short.data <- dplyr::filter(short.data, strike >= price) 
        # Short strike selection
        short.trade <- dplyr::filter(short.data, strike == min(strike)) 
        short.trade <- dplyr::filter(short.trade, rank(date, ties.method = "first") == 1)
        long.data <- dplyr::mutate(start.data, long.dte.diff = abs(o.dte - dte))
        # Closest to DTE
        long.data <- dplyr::filter(long.data, long.dte.diff == min(long.dte.diff)) 
        # Max DTE if there are two same diff from target DTE
        long.data <- dplyr::filter(long.data, dte == max(dte)) 
        # Long strike selection
        long.trade <- dplyr::filter(long.data, strike == short.trade$strike) 
        long.trade <- dplyr::filter(long.trade, rank(date, ties.method = "first") == 1)
        if (nrow(long.trade) > 0 && nrow(short.trade) > 0)  {
          long.trade <- dplyr::mutate(long.trade, trade.num = t, type = "long")
          short.trade <- dplyr::mutate(short.trade, trade.num = t, type = "short")
          l.positions <- rbind(l.positions, long.trade, fill = TRUE)
          s.positions <- rbind(s.positions, short.trade, fill = TRUE)
        }
      }
    }
  }) # End opening trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Closing Trades", value = .5, {
    # Loop through all trades and find the results
    for (i in 1:nrow(s.positions))  {
      shiny::incProgress(.001)
      ps <- s.positions[i, mid.price]
      o <- s.positions[i, date]
      es <- s.positions[i, expiration]
      s <- s.positions[i, strike]
      j <- s.positions[i, trade.num]
      pl <- l.positions[i, mid.price]
      el <- l.positions[i, expiration]
      profit.target <- (pl - ps) + ((pl - ps) * prof.targ)  # Calc profit target
      
      # Can we close this calendar at the profit target prior to expiration?
      possible.close <- dplyr::filter(trade.data, date >= o, date <= es)
      possible.close <- dplyr::filter(possible.close, strike == s)
      possible.close <- dplyr::filter(possible.close, expiration == es | expiration == el)
      
      # Set the short's price to negative to calculate the calendar close price
      possible.close <- dplyr::mutate(possible.close,
                                      mid.price = ifelse(expiration == es,
                                                         -1 * mid.price, mid.price))
      possible.close <- dplyr::group_by(possible.close, date)
      possible.close <- dplyr::mutate(possible.close, combined = sum(mid.price))
      possible.close <- dplyr::filter(possible.close, combined >= profit.target)
      if (nrow(possible.close) > 0) {
        short.close <- dplyr::filter(possible.close, expiration == es)
        short.close <- dplyr::filter(short.close, date == min(date))
        short.close <- dplyr::mutate(short.close, trade.num = j,
                                     exit.reason = "Profit target")
        # Reset close to positive num
        short.close <- dplyr::mutate(short.close, mid.price = -1 * mid.price) 
        short.results <- rbind(short.results, short.close, fill = TRUE)
        long.close <- dplyr::filter(possible.close, expiration == el)
        long.close <- dplyr::filter(long.close, date == min(date))
        long.close <- dplyr::mutate(long.close, trade.num = j,
                                    exit.reason = "Profit target")
        long.results <- rbind(long.results, long.close, fill = TRUE)
      }
      
      # Close at expiration
      short.close <- dplyr::filter(trade.data, date == es, expiration == es, strike == s)
      short.close <- dplyr::mutate(short.close, trade.num = j,
                                   exit.reason = "Expiration")
      short.results <- rbind(short.results, short.close, fill = TRUE)
      long.close <- dplyr::filter(trade.data, date == es, expiration == el, strike == s)
      long.close <- dplyr::mutate(long.close, trade.num = j,
                                  exit.reason = "Expiration")
      long.results <- rbind(long.results, long.close, fill = TRUE)
    }
    
    # Filter down results to the first exit for each trade as multiple targets could be hit
    ord.short.results <- dplyr::group_by(short.results, trade.num)
    short.results <- dplyr::filter(ord.short.results,
                                   rank(date, ties.method = "first") == 1)
    short.results <- dplyr::ungroup(short.results)
    
    ord.long.results <- dplyr::group_by(long.results, trade.num)
    long.results <- dplyr::filter(ord.long.results,
                                  rank(date, ties.method = "first") == 1)
    long.results <- dplyr::ungroup(long.results)
    
    # Merge the opening and closing data frames to calculate profit loss
    merge.long.results <- merge(long.results, l.positions, by = "trade.num")
    long.results <- dplyr::mutate(merge.long.results,
                                  profit.loss = 100 * (mid.price.x - mid.price.y))
    long.results <- dplyr::mutate(long.results, year = year(date.y))
    long.results <- dplyr::select(long.results,
                                  long.expiration = expiration.x,
                                  trade.num,
                                  long.close.date = date.x,
                                  long.strike = strike.x,
                                  long.close.price = mid.price.x,
                                  long.close.rsi = rsi.14.x,
                                  long.close.ivrank = iv.rank.x,
                                  long.open.date = date.y,
                                  long.price = price.y,
                                  long.open.price = mid.price.y,
                                  long.delta = delta.y,
                                  long.dte = dte.y,
                                  long.open.rsi = rsi.14.y,
                                  long.open.ivrank = iv.rank.y,
                                  long.exp_type = exp_type.y,
                                  long.profit = profit.loss,
                                  long.year = year,
                                  type,
                                  exit.reason)
    # Add in commission at a rate of $1.5 per trade excluding closing for .05 or less
    long.results <- dplyr::mutate(long.results, long.profit = long.profit - 3)
    
    # Merge the opening and closing data frames to calculate profit loss
    merge.short.results <- merge(short.results, s.positions, by = "trade.num")
    short.results <- dplyr::mutate(merge.short.results,
                                   profit.loss = 100 * (mid.price.y - mid.price.x))
    short.results <- dplyr::mutate(short.results, year = year(date.y))
    short.results <- dplyr::select(short.results,
                                   short.expiration = expiration.x,
                                   trade.num,
                                   short.close.date = date.x,
                                   short.strike = strike.x,
                                   short.close.price = mid.price.x,
                                   short.close.rsi = rsi.14.x,
                                   short.close.ivrank = iv.rank.x,
                                   short.open.date = date.y,
                                   short.price = price.y,
                                   short.open.price = mid.price.y,
                                   short.delta = delta.y,
                                   short.dte = dte.y,
                                   short.open.rsi = rsi.14.y,
                                   short.open.ivrank = iv.rank.y,
                                   short.exp_type = exp_type.y,
                                   short.profit = profit.loss,
                                   short.year = year,
                                   type,
                                   exit.reason)
    # Add in commission at a rate of $1.5 per trade excluding closing for .05 or less
    short.results <- dplyr::mutate(short.results,
                                   short.profit = ifelse(short.close.price <= .05,
                                                         short.profit - 1.5,
                                                         short.profit - 3))
    
    # Combine short and long results to enable calculation of totals
    for (i in 1:nrow(short.results))  {
      s <- short.results[i, short.strike]
      y <- short.results[i, short.open.date]
      close <- short.results[i, short.close.date]
      d <- short.results[i, short.dte]
      cred <- short.results[i, short.open.price]
      iv <- short.results[i, short.open.ivrank]
      rsi <- short.results[i, short.open.rsi]
      ext <- short.results[i, short.exp_type]
      p <- short.results[i, short.profit]
      t <- short.results[i, trade.num]
      df1 <- dplyr::filter(long.results, trade.num == t)
      if (nrow(df1) > 0) {
        long.profit <- df1$long.profit
        long.debit <- df1$long.open.price
        debit <- long.debit - cred
        profit <- p + long.profit
        er <- df1$exit.reason
        this.case <- as.data.frame(matrix(nrow = 1, c(t, s, y, close, d, debit,
                                                      iv, rsi, ext, profit, er)),
                                   stringsAsFactors = FALSE)
        results <- rbind(results, this.case)
      }
    }
    colnames(results) <- c("trade.num", "strike", "open.date", "close.date",
                           "dte", "debit", "open.ivrank",
                           "open.rsi", "exp_type", "profit", "exit.reason")
    results <- dplyr::mutate(results, trade.num = as.integer(trade.num),
                             call.strike = as.numeric(strike),
                             put.strike = "NA",
                             open.date = as.Date(as.numeric(open.date),
                                                 origin = "1970-01-01"),
                             close.date = as.Date(as.numeric(close.date),
                                                  origin = "1970-01-01"),
                             year = year(open.date),
                             dte = as.numeric(dte),
                             debit = as.numeric(debit) * 100,
                             open.ivrank = as.numeric(open.ivrank),
                             open.rsi = as.numeric(open.rsi),
                             days.held = as.numeric(close.date) - as.numeric(open.date),
                             profit = as.numeric(profit),
                             has_profit = ifelse(profit > 0, "Yes", "No"))
  }) # End closing trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Creating Plot", value = .95, {
    # Send results to global data frame for viewing
    results.table <- dplyr::select(results, open.date, close.date, call.strike,
                                   dte, days.held, open.ivrank, debit, exp_type,
                                   exit.reason, profit)
    
    # Write out closing dates to csv so can be used as the open dates with the
    # custom open option
    close.dates <- dplyr::select(results, close.date, exit.reason)
    utils::write.csv(close.dates, file = "Data/customDates.csv")
    
    # Calculate totals for display
    assign("num_t", nrow(results), envir = .GlobalEnv)
    assign("tot_profit", sum(results$profit), envir = .GlobalEnv)
    assign("avg_profit", tot_profit/num_t, envir = .GlobalEnv)
    assign("tot_days", sum(results$days.held), envir = .GlobalEnv)
    assign("avg_days", tot_days/num_t, envir = .GlobalEnv)
    assign("avg_prof_day", avg_profit/avg_days, envir = .GlobalEnv)
    assign("maximum_loss",
           ifelse(min(results$profit) >= 0, 0, min(results$profit)), envir = .GlobalEnv)
    assign("percent_winners",
           scales::percent(length(which(results$profit > 0)) / num_t), .GlobalEnv)
    assign("exit.expiration",
           ifelse(scales::percent(length(which(results$exit.reason == "Expiration")) / num_t) == "NaN%",
                  0, scales::percent(length(which(results$exit.reason == "Expiration")) / num_t)),
           envir = .GlobalEnv)
    assign("exit.profit.target",
           ifelse(scales::percent(length(which(results$exit.reason == "Profit target")) / num_t) == "NaN%",
                  0, scales::percent(length(which(results$exit.reason == "Profit target")) / num_t)),
           envir = .GlobalEnv)
    assign("exit.loss.limit",
           ifelse(scales::percent(length(which(results$exit.reason == "Loss limit")) / num_t) == "NaN%",
                  0, scales::percent(length(which(results$exit.reason == "Loss limit")) / num_t)),
           envir = .GlobalEnv)
    assign("exit.gamma.risk",
           ifelse(scales::percent(length(which(results$exit.reason == "Gamma risk")) / num_t) == "NaN%",
                  0, scales::percent(length(which(results$exit.reason == "Gamma risk")) / num_t)),
           envir = .GlobalEnv)
    assign("exit.earnings",
           ifelse(scales::percent(length(which(results$exit.reason == "Earnings")) / num_t) == "NaN%",
                  0, scales::percent(length(which(results$exit.reason == "Earnings")) / num_t)),
           envir = .GlobalEnv)
    
  }) # End creating plot progress bar
  # Return results to global environment for further processing in main script
  assign("results", results, envir = .GlobalEnv)
  assign("results.table", results.table, envir = .GlobalEnv)
}
