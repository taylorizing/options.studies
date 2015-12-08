# title: "Short Put Study"
# author: "Jason Taylor"

# Todos:
# - Change the custom open dates from csv to save maybe?

short.put <- function(progress.int, t) {
  shiny::withProgress(message = "Progress Bar", detail = "Opening Trades", {
    p.positions <- data.frame()
    results <- data.frame()

    # Set the first possible opening date based on the stock loaded
    min.date <- min(complete.data$date)

    # Set the last day for expiration dates to keep us from opening trades
    # we can't close
    max.date <- max(complete.data$date)
    trade.data <- dplyr::filter(complete.data, call.put == "P",
                         expiration <= max.date)

    # Filter first day month/week data to dates we have options chains for
    first.day <- dplyr::filter(first.day, date >= min.date)

    for (i in unique(first.day$date))  {
      shiny::incProgress(progress.int)
      t = t + 1
      start.data <- dplyr::filter(trade.data, date == i)
      start.data <- dplyr::filter(start.data, call.put == "P")
      if (nrow(start.data) > 0 && start.data[1, iv.rank] >= low.iv &&
          start.data[1, iv.rank] <= high.iv) {
        short.data <- dplyr::mutate(start.data, dte.diff = abs(o.dte - dte))
        short.data <- dplyr::filter(short.data, dte.diff == min(dte.diff))
        short.data <- dplyr::filter(short.data, delta >= p.delta)
        short.put.trade <- dplyr::filter(short.data, delta == min(delta))
        short.put.trade <- dplyr::mutate(short.put.trade,
                                  margin = max((100 * (.1 * strike)) + (100 * mid.price),
                                  ((.2 * price * 100) - (100* (price - strike)) + (100 * mid.price))))
        short.put.trade <- dplyr::mutate(short.put.trade,
                                  open.roc = 100 * (((100 * mid.price) / margin)))
        short.put.trade <- dplyr::filter(short.put.trade, open.roc >= min.roc)
        if (nrow(short.put.trade) > 0)  {
          short.put.trade <- dplyr::mutate(short.put.trade, trade.num = t)
          p.positions <- rbind(p.positions, short.put.trade, fill = TRUE)
        }
      }
    }
  }) # End opening trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Closing Trades",
                      value = .5, {
    # Loop through all trades and find the results
    for (i in 1:nrow(p.positions))  {
      shiny::incProgress(progress.int)
      p <- p.positions[i, mid.price]
      o <- p.positions[i, date]
      e <- p.positions[i, expiration]
      s <- p.positions[i, strike]
      j <- p.positions[i, trade.num]
      m <- p.positions[i, margin]
      oroc <- p.positions[i, open.roc]
      xc <- p - (p * prof.targ) # Profit target x percent
      xl <- p * loss.lim # Limit loss to input times credit received
      possible.close <- dplyr::filter(trade.data,
                               expiration == e,
                               date >= o,
                               date <= e,
                               strike == s)
      # Find the close if percent profit target is hit
      possible.xc.close <- dplyr::filter(possible.close, mid.price <= xc)
      if (nrow(possible.xc.close) > 0) {
        xc.close <- dplyr::filter(possible.xc.close, date == min(date))
        xc.close <- dplyr::mutate(xc.close,
                           trade.num = j,
                           open.date = o,
                           exit.reason = "Profit target",
                           margin = m,
                           open.roc = oroc)
        results <- rbind(results, xc.close, fill = TRUE)
      }
      # Find the close if the limit loss target is hit
      possible.xl.close <- dplyr::filter(possible.close, mid.price >= xl)
      if (nrow(possible.xl.close) > 0) {
        xl.close <- dplyr::filter(possible.xl.close, date == min(date))
        xl.close <- dplyr::mutate(xl.close,
                           trade.num = j,
                           open.date = o,
                           exit.reason = "Loss limit",
                           margin = m,
                           open.roc = oroc)
        results <- rbind(results, xl.close, fill = TRUE)
      }
      # Close by gamma days input
      if (g > 0) {
        gamma.date <- as.Date(e, origin = "1970-01-01") - g
        ifelse(weekdays(gamma.date, abbreviate = FALSE) == "Sunday",
               gamma.date <- gamma.date - 2, gamma.date)
        ifelse(weekdays(gamma.date, abbreviate = FALSE) == "Saturday",
               gamma.date <- gamma.date - 1, gamma.date)
        g.close <- dplyr::filter(data,
                          date == gamma.date,
                          expiration == e,
                          strike == s)
        g.close <- dplyr::mutate(g.close,
                          trade.num = j,
                          open.date = o,
                          exit.reason = "Gamma risk",
                          margin = m,
                          open.roc = oroc)
        results <- rbind(results, g.close, fill = TRUE)
      }
      # Close prior to earnings
      if (earn.close == "Yes")  {
        e.dates <- dplyr::filter(earn.dates, earn.date > o, earn.date < e)
        e.dates <- dplyr::filter(e.dates, earn.date == min(earn.date))
        if (nrow(e.dates) > 0 ) {
          e.date <- as.Date(e.dates$earn.date)
          e.close <- dplyr::filter(trade.data,
                            date == e.date,
                            expiration == e,
                            strike == s)
          e.close <- dplyr::mutate(e.close,
                            trade.num = j,
                            open.date = o,
                            exit.reason = "Earnings",
                            margin = m,
                            open.roc = oroc)
          results <- rbind(results, e.close, fill = TRUE)
        }
      }
      # Find the close at expiration
      exp.close <- dplyr::filter(trade.data,
                          date == e,
                          expiration == e,
                          strike == s)
      exp.close <- dplyr::mutate(exp.close,
                          trade.num = j,
                          open.date = o,
                          exit.reason = "Expiration",
                          margin = m,
                          open.roc = oroc)
      results <- rbind(results, exp.close, fill = TRUE)
    } # End loop through all trades and find the results

    # Filter down results to the first exit for each trade as multiple
    # targets could be hit
    ord.results <- dplyr::group_by(results, open.date)
    results <- dplyr::filter(ord.results,
                             rank(date, ties.method = "first") == 1)

    # Merge the opening and closing data frames to calculate profit loss
    merge.results <- merge(results, p.positions, c("trade.num", "expiration"))
    results <- dplyr::mutate(merge.results,
                             profit.loss = 100 * (mid.price.y - mid.price.x))
    results <- dplyr::mutate(results, year = year(date.y))
    results <- dplyr::select(results,
                      expiration,
                      trade.num,
                      close.date = date.x,
                      put.strike = strike.x,
                      close.price = mid.price.x,
                      close.rsi = rsi.14.x,
                      close.ivrank = iv.rank.x,
                      open.date = date.y,
                      price = price.y,
                      open.price = mid.price.y,
                      delta = delta.y,
                      dte = dte.y,
                      open.rsi = rsi.14.y,
                      open.ivrank = iv.rank.y,
                      exp_type = exp_type.y,
                      profit = profit.loss,
                      year,
                      exit.reason,
                      margin = margin.y,
                      open.roc = open.roc.y)
    # Add in commission at a rate of $1.5 per trade excluding closing
    # for .05 or less
    results <- dplyr::mutate(results,
                             profit = ifelse(close.price <= .05,
                                             profit - 1.5, profit - 3),
                      exit.roc = (100 * profit) / margin,
                      call.strike = "NA",
                      days.held = as.numeric(close.date) - as.numeric(open.date),
                      has_profit = ifelse(profit > 0, "Yes", "No"))
  }) # End closing trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Creating Plot",
                      value = .95, {
    # select fields we want displayed in the table view
    results.table <- dplyr::select(results,
                                open.date, close.date, expiration, put.strike,
                                dte, days.held, open.ivrank, exp_type,
                                exit.reason, profit, margin, open.roc, exit.roc)

    # Write out closing dates to csv so can be used as the open dates with
    # the custom open option
    close.dates <- dplyr::select(results.table, close.date, exit.reason)
    utils::write.csv(close.dates, file = "Data/customDates.csv")

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
  # Send results to global environment for further processing in main script
  assign("results", results, envir = .GlobalEnv)
  assign("results.table", results.table, envir = .GlobalEnv)
}
