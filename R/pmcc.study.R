# title: "Poor Man's Covered Call Study"
# author: "Jason Taylor"

# Todos:
# - Change the custom open dates from csv to save maybe?
pmcc <- function(progress.int, t) {
shiny::withProgress(message = "Progress Bar", detail = "Opening Trades", {
  l.positions <- data.frame()
  s.positions <- data.frame()
  long.results <- data.frame()
  short.results <- data.frame()
  results <- data.frame()
  
  # Set the first possible opening date based on the stock loaded
  min.date <- min(complete.data$date)
  
  # Set the last day for expiration dates to keep us from opening trades
  # we can't close
  max.date <- max(complete.data$date)
  trade.data <- dplyr::filter(complete.data, expiration <= max.date)
  
  # Filter first day month/week data to dates we have options chains for
  first.day <- dplyr::filter(first.day, open.date >= min.date)
  
for (i in unique(first.day$open.date))  {
  incProgress(progress.int)
  t = t + 1
  start.data <- dplyr::filter(trade.data, date == i)
  # Check for data set and that iv rank is within range
  if (nrow(start.data) > 0 && start.data[1, iv.rank] >= low.iv &&
      start.data[1, iv.rank] <= high.iv) {
    long.data <- dplyr::mutate(start.data, dte.diff = abs(o.dte - dte))
    long.data <- dplyr::filter(long.data, dte.diff == min(dte.diff))
    long.data <- dplyr::filter(long.data, strike < price)
    long.trade <- dplyr::filter(long.data, strike == max(strike))
    if (nrow(long.trade) > 0)  {
          long.trade <- dplyr::mutate(long.trade, trade.num = t, type = "long")
          long.trade.exp <- long.trade$expiration
          l.positions <- rbind(l.positions, long.trade, fill = TRUE)
          # Long trade was made so now we open the covered call
          short.data <- dplyr::mutate(start.data, s.dte.diff = abs(s.dte - dte))
          short.data <- dplyr::filter(short.data, s.dte.diff == min(s.dte.diff))
          short.data <- dplyr::filter(short.data, delta <= c.delta)
          short.data <- dplyr::filter(short.data, delta >= c.delta.lim)
          short.trade <- dplyr::filter(short.data, delta == max(delta))
          if (nrow(short.trade) > 0)  {
            s.trade.exp <- short.trade$expiration
            s.trade.date <- short.trade$date
            while (s.trade.exp <= long.trade.exp)  {
              start.data <- dplyr::filter(trade.data, date == s.trade.date)
              if (nrow(start.data) > 0) {
                short.data <- dplyr::mutate(start.data,
                                            s.dte.diff = abs(s.dte - dte))
                short.data <- dplyr::filter(short.data,
                                            s.dte.diff == min(s.dte.diff))
                short.data <- dplyr::filter(short.data, delta <= c.delta)
                short.data <- dplyr::filter(short.data, delta >= c.delta.lim)
                short.trade <- dplyr::filter(short.data, delta == max(delta))
                if (nrow(short.trade) > 0 )  {
                  short.trade <- dplyr::mutate(short.trade, trade.num = t,
                                               type = "short")
                  s.trade.exp <- short.trade$expiration
                  s.trade.date <- short.trade$expiration
                  if (s.trade.exp <= long.trade.exp)  {
                    s.positions <- rbind(s.positions, short.trade, fill = TRUE)
                  }
                } else {
                  break
                }
              } else  {
                break
              }
            } # End while
          }
        }
      } # End the if by stock
    }
}) # End opening trades progress bar
shiny::withProgress(message = "Progress Bar", detail = "Closing Trades", value = .5, {
  # Loop through all long trades and find the results
  for (i in 1:nrow(l.positions))  {
    incProgress(progress.int)
    p <- l.positions[i, mid.price]
    o <- l.positions[i, date]
    e <- l.positions[i, expiration]
    s <- l.positions[i, strike]
    j <- l.positions[i, trade.num]
    xl <- p - (p * l.loss.lim) # Limit loss to % of the debit paid
    possible.close <- dplyr::filter(trade.data, expiration == e, date >= o,
                                    date <= e, strike == s)
    # Find the close if the limit loss target is hit
    possible.xl.close <- dplyr::filter(possible.close, mid.price <= xl)
    if (nrow(possible.xl.close) > 0)  {
      xl.close <- dplyr::filter(possible.xl.close, date == min(date))
      xl.close <- dplyr::mutate(xl.close, trade.num = j, open.date = o,
                                exit.reason = "Loss limit")
      long.results <- rbind(long.results, xl.close)
    }
    # Close the long at expiration
    exp.close <- dplyr::filter(trade.data, date == e, expiration == e,
                               strike == s)
    exp.close <- dplyr::mutate(exp.close, trade.num = j, open.date = o,
                               exit.reason = "Expiration")
    long.results <- rbind(long.results, exp.close)
  }
  # Filter down results to the first exit for each trade as multiple targets
  # could be hit
  ord.long.results <- dplyr::group_by(long.results, open.date)
  long.results <- dplyr::filter(ord.long.results,
                                rank(date, ties.method = "first") == 1)
  long.results <- dplyr::ungroup(long.results)
  # Loop through all the short trades and close
  for (i in 1:nrow(s.positions))  {
    incProgress(progress.int)
    p <- s.positions[i, mid.price]
    o <- s.positions[i, date]
    e <- s.positions[i, expiration]
    s <- s.positions[i, strike]
    j <- s.positions[i, trade.num]
    xc <- p - (p * prof.targ) # Profit target x percent
    xl <- p * loss.lim # Limit loss to x times credit received
    check.long <- dplyr::filter(long.results, trade.num == j)
    elc <- check.long$date
    if (o < elc) {
      possible.close <- dplyr::filter(trade.data, expiration == e, date >= o,
                                      date <= e, strike == s)
      # Find the close if percent profit target is hit
      possible.xc.close <- dplyr::filter(possible.close, mid.price <= xc)
      if (nrow(possible.xc.close) > 0) {
        xc.close <- dplyr::filter(possible.xc.close, date == min(date))
        xc.close <- dplyr::mutate(xc.close, trade.num = j, open.date = o,
                                  exit.reason = "Profit target")
        if (xc.close$date < elc) {
          short.results <- rbind(short.results, xc.close)
        }
      }
      # Find the close if the limit loss target is hit
      possible.xl.close <- dplyr::filter(possible.close, mid.price >= xl)
      if (nrow(possible.xl.close) > 0) {
        xl.close <- dplyr::filter(possible.xl.close, date == min(date))
        xl.close <- dplyr::mutate(xl.close, trade.num = j, open.date = o,
                                  exit.reason = "Loss limit")
        if (xl.close$date < elc) {
          short.results <- rbind(short.results, xl.close)
        }
      }
      # Find the close at the long close date in the case that happens first
      el.close <- dplyr::filter(trade.data, date == elc, expiration == e,
                                strike == s)
      if (nrow(el.close) > 0) {
        el.close <- dplyr::mutate(el.close, trade.num = j, open.date = o,
                                  exit.reason = "Expiration")
        short.results <- rbind(short.results, el.close)
      }
      # Find the close at expiration
      exp.close <- dplyr::filter(trade.data, date == e, expiration == e,
                                 strike == s)
      if (nrow(exp.close) > 0) {
        exp.close <- dplyr::mutate(exp.close, trade.num = j, open.date = o,
                                   exit.reason = "Expiration")
        if (exp.close$date < elc) {
          short.results <- rbind(short.results, exp.close)
        }
      }
    }
  }# End loop through all short trades and find the results

  ord.short.results <- dplyr::group_by(short.results, open.date)
  short.results <- dplyr::filter(ord.short.results,
                                 rank(date, ties.method = "first") == 1)
  short.results <- dplyr::ungroup(short.results)

  # Merge the opening and closing data frames to calculate profit loss
  merge.long.results <- merge(long.results, l.positions,
                              c("trade.num", "expiration"))
  long.results <- dplyr::mutate(merge.long.results,
                                profit.loss = 100 * (mid.price.x - mid.price.y))
  long.results <- dplyr::mutate(long.results, year = year(date.y))
  long.results <- dplyr::select(long.results,
                                long.expiration = expiration,
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
                                exit.reason)
  # Add in commission at a rate of $1.5 per trade excluding closing for .05 or less
  long.results <- dplyr::mutate(long.results, long.profit = long.profit - 3)

  # Merge the opening and closing data frames to calculate profit loss
  merge.short.results <- merge(short.results, s.positions,
                               c("trade.num", "expiration"))
  short.results <- dplyr::mutate(merge.short.results,
                                 profit.loss = 100 * (mid.price.y - mid.price.x))
  short.results <- dplyr::mutate(short.results, year = year(date.y))
  short.results <- dplyr::select(short.results,
                                 short.expiration = expiration,
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
                                 exit.reason)
  # Add in commission at a rate of $1.5 per trade excluding closing for .05 or less
  short.results <- dplyr::mutate(short.results,
                                 short.profit = ifelse(short.close.price <= .05,
                                 short.profit - 1.5, short.profit - 3))

  # Combine short and long results to enable calculation of totals
  for (i in 1:nrow(long.results))  {
    o <- long.results[i, long.open.date]
    c <- long.results[i, long.close.date]
    s <- long.results[i, long.strike]
    d <- long.results[i, long.dte]
    deb <- long.results[i, long.open.price]
    iv <- long.results[i, long.open.ivrank]
    rsi <- long.results[i, long.open.rsi]
    ext <- long.results[i, long.exp_type]
    p <- long.results[i, long.profit]
    t <- long.results[i, trade.num]
    er <- long.results[i, exit.reason]
    df1 <- dplyr::filter(short.results, trade.num == t)
    sum1 <- sum(df1$short.profit)
    profit <- p + sum1
    this.case <- as.data.frame(matrix(nrow = 1,
                                      c(t, o, c, s, d, deb, iv, rsi, ext,
                                        profit, er)),
                               stringsAsFactors = FALSE)
    results <- rbind(results, this.case)
  }

  colnames(results) <- c("trade.num", "open.date", "close.date", "long.strike",
                         "dte", "debit", "open.ivrank", "open.rsi", "exp_type",
                         "profit", "exit.reason")
  results <- dplyr::mutate(results,
                           trade.num = as.integer(trade.num),
                           open.date = as.Date(as.numeric(open.date),
                                               origin = "1970-01-01"),
                           close.date = as.Date(as.numeric(close.date),
                                                origin = "1970-01-01"),
                           call.strike = as.numeric(long.strike),
                           put.strike = "NA",
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
  results.table <- dplyr::select(results,
                                 open.date, close.date, dte, days.held,
                                 open.ivrank, debit, call.strike, exp_type,
                                 exit.reason, profit)

  # Write out closing dates to csv so can be used as the open dates with the custom open option
  close.dates <- dplyr::select(results, close.date, exit.reason)
  write.csv(close.dates, file = "Data/customDates.csv")

  # Calculate totals for display
  assign("num_t", nrow(results), envir = .GlobalEnv)
  assign("tot_profit", sum(results$profit), envir = .GlobalEnv)
  assign("avg_profit", tot_profit/num_t, envir = .GlobalEnv)
  assign("tot_days", sum(results$days.held), envir = .GlobalEnv)
  assign("avg_days", tot_days/num_t, envir = .GlobalEnv)
  assign("avg_prof_day", avg_profit/avg_days, envir = .GlobalEnv)
  assign("maximum_loss",
         ifelse(min(results$profit) >= 0,
                0, min(results$profit)), envir = .GlobalEnv)
  assign("percent_winners",
         scales::percent(length(which(results$profit > 0)) / num_t), .GlobalEnv)
  assign("exit.expiration",
         ifelse(scales::percent(length(which(results$exit.reason ==
                                               "Expiration")) / num_t) == "NaN%",
                0, scales::percent(length(which(results$exit.reason ==
                                                  "Expiration")) / num_t)),
         envir = .GlobalEnv)
  assign("exit.profit.target",
         ifelse(scales::percent(length(which(results$exit.reason ==
                                               "Profit target")) / num_t) == "NaN%",
                0, scales::percent(length(which(results$exit.reason ==
                                                  "Profit target")) / num_t)),
         envir = .GlobalEnv)
  assign("exit.loss.limit",
         ifelse(scales::percent(length(which(results$exit.reason ==
                                               "Loss limit")) / num_t) == "NaN%",
                0, scales::percent(length(which(results$exit.reason ==
                                                  "Loss limit")) / num_t)),
         envir = .GlobalEnv)
  assign("exit.gamma.risk",
         ifelse(scales::percent(length(which(results$exit.reason ==
                                               "Gamma risk")) / num_t) == "NaN%",
                0, scales::percent(length(which(results$exit.reason ==
                                                  "Gamma risk")) / num_t)),
         envir = .GlobalEnv)
  assign("exit.earnings",
         ifelse(scales::percent(length(which(results$exit.reason ==
                                               "Earnings")) / num_t) == "NaN%",
                0, scales::percent(length(which(results$exit.reason ==
                                                  "Earnings")) / num_t)),
         envir = .GlobalEnv)
}) # End creating plot progress bar
# Send results to global environment for further processing in main script
assign("results", results, envir = .GlobalEnv)
assign("results.table", results.table, envir = .GlobalEnv)
}