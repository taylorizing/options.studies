# title: "Strangle Study"
# author: "Jason Taylor"

# Todos:
# - Change the custom open dates from csv to save maybe?

strangle <- function(progress.int, t) {
  shiny::withProgress(message = "Progress Bar", detail = "Opening Trades", {
    p.positions <- data.frame()
    c.positions <- data.frame()
    put.results <- data.frame()
    call.results <- data.frame()
    if (exists("results") && is.data.frame(get("results"))) {
      t <- max(results$trade.num)
    } else {
      results <- data.frame()
      t <- 0
    }
    
    # Set the first possible opening date based on the stock loaded
    min.date <- min(complete.data$date)
    
    # Set the last day for expiration dates to keep us from opening trades
    # we can't close
    max.date <- max(complete.data$date)
    trade.data <- dplyr::filter(complete.data, expiration <= max.date)
    
    # Filter first day month/week data to dates we have options chains for
    first.day <- dplyr::filter(first.day, date >= min.date)
    
    for (i in unique(first.day$date))  {
      incProgress(progress.int)
      t <- t + 1
      start.data <- dplyr::filter(trade.data, date == i) # Set open date
      # Check for data set and that iv rank is within range
      if (nrow(start.data) > 0 && start.data[1, iv.rank] >= low.iv &&
          start.data[1, iv.rank] <= high.iv) {
        put.data <- dplyr::filter(start.data, call.put == "P")
        put.data <- dplyr::mutate(put.data, dte.diff = abs(o.dte - dte))
        # Closest to DTE
        put.data <- dplyr::filter(put.data, dte.diff == min(dte.diff))
        put.data <- dplyr::filter(put.data, delta >= p.delta)
        put.data <- dplyr::filter(put.data, delta <= p.delta.lim)
        put.trade <- dplyr::filter(put.data, delta == min(delta))
        
        call.data <- dplyr::filter(start.data, call.put == "C")
        call.data <- dplyr::filter(call.data, dte == put.trade$dte)
        call.data <- dplyr::filter(call.data, delta <= c.delta)
        call.data <- dplyr::filter(call.data, delta >= c.delta.lim)
        call.trade <- dplyr::filter(call.data, delta == max(delta))
        
        if (nrow(call.trade) > 0 && nrow(put.trade) > 0)  {
              call.trade <- dplyr::mutate(call.trade, trade.num = t, type = "call") # Assign trade num
              put.trade <- dplyr::mutate(put.trade, trade.num = t, type = "put") # Assign trade num
              c.positions <- rbind(c.positions, call.trade, fill = TRUE)
              p.positions <- rbind(p.positions, put.trade, fill = TRUE)
        }
      }
    }
    
  }) # End opening trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Closing Trades", value = .5, {
    # Loop through all trades and find the results
    for (i in 1:nrow(c.positions))  {
      incProgress(progress.int)
      ocp <- c.positions[i, mid.price]
      opp <- p.positions[i, mid.price]
      od <- c.positions[i, date]
      e <- c.positions[i, expiration]
      cs <- c.positions[i, strike]
      ps <- p.positions[i, strike]
      j <- c.positions[i, trade.num]
      
      # Close the put at expiration
      put.close <- dplyr::filter(trade.data, date == e, expiration == e,
                                 strike == ps, call.put == "P")
      put.close <- dplyr::mutate(put.close, trade.num = j,
                                 exit.reason = "Expiration",
                                 close.price = mid.price)
      put.results <- rbind(put.results, put.close, fill = TRUE)
      # Close the call at the expiration
      call.close <- dplyr::filter(trade.data, date == e, expiration == e,
                                  strike == cs, call.put == "C")
      call.close <- dplyr::mutate(call.close, trade.num = j,
                                  exit.reason = "Expiration",
                                  close.price = mid.price)
      call.results <- rbind(call.results, call.close, fill = TRUE)
      
      # Close the Strangle if the profit target is met
      profit.target <- (ocp + opp) * prof.targ
      possible.close <- dplyr::filter(trade.data, date >= od, date <= e)
      possible.close <- dplyr::filter(possible.close, expiration == e)
      possible.call.close <- dplyr::filter(possible.close, strike == cs,
                                           call.put == "C")
      possible.put.close <- dplyr::filter(possible.close, strike == ps,
                                          call.put == "P")
      possible.close <- rbind(possible.call.close, possible.put.close,
                              fill = TRUE)
      possible.close <- dplyr::group_by(possible.close, date)
      possible.close <- dplyr::mutate(possible.close, curr.price = sum(mid.price))
      possible.close <- dplyr::filter(possible.close,
                                      curr.price <= (ocp + opp) - profit.target)
      if (nrow(possible.close) > 0) {
        possible.close <- dplyr::filter(possible.close, date == min(date))
        possible.close <- dplyr::mutate(possible.close, trade.num = j,
                                        exit.reason = "Profit target",
                                        close.price = ((ocp + opp) - profit.target) / 2)
        put.close <- dplyr::filter(possible.close, call.put == "P")
        call.close <- dplyr::filter(possible.close, call.put == "C")
        put.results <- rbind(put.results, put.close, fill = TRUE)
        call.results <- rbind(call.results, call.close, fill = TRUE)
      }
      # Close the Strangle if the loss limit is met
      loss.limit <- (ocp + opp) * loss.lim
      possible.close <- dplyr::filter(trade.data, date >= od, date <= e)
      possible.close <- dplyr::filter(possible.close, expiration == e)
      possible.call.close <- dplyr::filter(possible.close, strike == cs,
                                           call.put == "C")
      possible.put.close <- dplyr::filter(possible.close, strike == ps,
                                          call.put == "P")
      possible.close <- rbind(possible.call.close, possible.put.close,
                              fill = TRUE)
      possible.close <- dplyr::group_by(possible.close, date)
      possible.close <- dplyr::mutate(possible.close, curr.price = sum(mid.price))
      possible.close <- dplyr::filter(possible.close, curr.price >= loss.limit)
      if (nrow(possible.close) > 0) {
        possible.close <- dplyr::filter(possible.close, date == min(date))
        possible.close <- dplyr::mutate(possible.close, trade.num = j,
                                        exit.reason = "Loss limit",
                                        close.price = loss.limit / 2)
        put.close <- dplyr::filter(possible.close, call.put == "P")
        call.close <- dplyr::filter(possible.close, call.put == "C")
        put.results <- rbind(put.results, put.close, fill = TRUE)
        call.results <- rbind(call.results, call.close, fill = TRUE)
      }
      # Close by gamma days input
      if (g > 0) {
        gamma.date <- as.Date(e, origin = "1970-01-01") - g
        ifelse(weekdays(gamma.date, abbreviate = FALSE) == "Sunday",
               gamma.date <- gamma.date - 2, gamma.date)
        ifelse(weekdays(gamma.date, abbreviate = FALSE) == "Saturday",
               gamma.date <- gamma.date - 1, gamma.date)
        g.put.close <- dplyr::filter(trade.data, date == gamma.date,
                                     expiration == e, strike == ps)
        g.put.close <- dplyr::filter(g.put.close, call.put == "P")
        g.put.close <- dplyr::mutate(g.put.close, trade.num = j,
                                     exit.reason = "Gamma risk",
                                     close.price = mid.price)
        put.results <- rbind(put.results, g.put.close, fill = TRUE)
        
        g.call.close <- dplyr::filter(trade.data, date == gamma.date,
                                      expiration == e, strike == cs)
        g.call.close <- dplyr::filter(g.call.close, call.put == "C")
        g.call.close <- dplyr::mutate(g.call.close, trade.num = j,
                                      exit.reason = "Gamma risk",
                                      close.price = mid.price)
        call.results <- rbind(call.results, g.call.close, fill = TRUE)
      }
      # Close prior to earnings
      if (earn.close == "Yes")  {
        e.dates <- dplyr::filter(earn.dates, earn.date > od, earn.date < e)
        e.dates <- dplyr::filter(e.dates, earn.date == min(earn.date))
        if (nrow(e.dates) > 0 ) {
          e.date <<- as.Date(e.dates$earn.date)
          e.put.close <- dplyr::filter(trade.data, date == e.date,
                                       expiration == e, strike == ps)
          e.put.close <- dplyr::filter(e.put.close, call.put == "P")
          e.put.close <- dplyr::mutate(e.put.close, trade.num = j,
                                       exit.reason = "Earnings",
                                       close.price = mid.price)
          put.results <- rbind(put.results, e.put.close, fill = TRUE)
          e.call.close <- dplyr::filter(trade.data, date == e.date,
                                        expiration == e, strike == cs)
          e.call.close <- dplyr::filter(e.call.close, call.put == "C")
          e.call.close <- dplyr::mutate(e.call.close, trade.num = j,
                                        exit.reason = "Earnings",
                                        close.price = mid.price)
          call.results <- rbind(call.results, e.call.close, fill = TRUE)
        }
      }
    }
    
    # Filter down results to the first exit for each trade as multiple targets could be hit
    ord.put.results <- dplyr::group_by(put.results, trade.num)
    put.results <- dplyr::filter(ord.put.results,
                                 rank(date, ties.method = "first") == 1)
    put.results <- dplyr::ungroup(put.results)
    
    ord.call.results <- dplyr::group_by(call.results, trade.num)
    call.results <- dplyr::filter(ord.call.results,
                                  rank(date, ties.method = "first") == 1)
    call.results <- dplyr::ungroup(call.results)
    
    # Merge the opening and closing data frames to calculate profit loss
    merge.call.results <- merge(call.results, c.positions, by = "trade.num")
    call.results <- dplyr::mutate(merge.call.results,
                                  profit.loss = 100 * (mid.price.y - close.price))
    call.results <- dplyr::mutate(call.results, year = year(date.y))
    call.results <- dplyr::select(call.results,
                                  trade.num,
                                  symbol = symbol.x,
                                  call.expiration = expiration.x,
                                  call.close.date = date.x,
                                  call.strike = strike.x,
                                  call.close.price = close.price,
                                  call.close.rsi = rsi.14.x,
                                  call.close.ivrank = iv.rank.x,
                                  call.open.date = date.y,
                                  call.price = price.y,
                                  call.open.price = mid.price.y,
                                  call.delta = delta.y,
                                  call.dte = dte.y,
                                  call.open.rsi = rsi.14.y,
                                  call.open.ivrank = iv.rank.y,
                                  call.profit = profit.loss,
                                  call.year = year,
                                  exit.reason)
    # Add in commission at a rate of $1.5 per trade excluding closing for .05 or less
    call.results <- dplyr::mutate(call.results,
                                  call.profit = ifelse(call.close.price <= .05,
                                  call.profit - 1.5, call.profit - 3))
    # Merge the opening and closing data frames to calculate profit loss
    merge.put.results <- merge(put.results, p.positions, by = "trade.num")
    put.results <- dplyr::mutate(merge.put.results,
                                 profit.loss = 100 * (mid.price.y - close.price))
    put.results <- dplyr::mutate(put.results, year = year(date.y))
    put.results <- dplyr::select(put.results,
                                 put.expiration = expiration.x,
                                 trade.num,
                                 symbol = symbol.x,
                                 put.close.date = date.x,
                                 put.strike = strike.x,
                                 put.close.price = close.price,
                                 put.close.rsi = rsi.14.x,
                                 put.close.ivrank = iv.rank.x,
                                 put.open.date = date.y,
                                 put.price = price.y,
                                 put.open.price = mid.price.y,
                                 put.delta = delta.y,
                                 put.dte = dte.y,
                                 put.open.rsi = rsi.14.y,
                                 put.open.ivrank = iv.rank.y,
                                 put.profit = profit.loss,
                                 put.year = year,
                                 exit.reason)
    # Add in commission at a rate of $1.5 per trade excluding closing for .05 or less
    put.results <- dplyr::mutate(put.results,
                                 put.profit = ifelse(put.close.price <= .05,
                                 put.profit - 1.5, put.profit - 3))
    
    # Combine put and call results to enable calculation of totals
    for (i in 1:nrow(put.results))  {
      sym <- put.results[i, symbol]
      t <- put.results[i, trade.num]
      ps <- put.results[i, put.strike]
      od <- put.results[i, put.open.date]
      cd <- put.results[i, put.close.date]
      d <- put.results[i, put.dte]
      pop <- put.results[i, put.open.price]
      iv <- put.results[i, put.open.ivrank]
      rsi <- put.results[i, put.open.rsi]
      pp <- put.results[i, put.profit]
      er <- put.results[i, exit.reason]
      df1 <- dplyr::filter(call.results, trade.num == t)
      cp <- df1$call.profit
      cs <- df1$call.strike
      cop <- df1$call.open.price
      tp <- pp + cp
      year <- 0
      days.held <- 0
      has_profit <- 0
      
      this.case <- as.data.frame(matrix(nrow = 1,
                                        c(sym, t, ps, od, cd, d, pop, iv, rsi,
                                        pp, cp, cs, cop, tp, er, year,
                                        days.held, has_profit)),
                                 stringsAsFactors = FALSE)
      colnames(this.case) <- c("symbol", "trade.num", "put.strike", "open.date",
                               "close.date", "dte", "put.credit", "open.ivrank",
                               "open.rsi", "put.profit", "call.profit",
                               "call.strike", "call.credit", "profit",
                               "exit.reason", "year", "days.held", "has_profit")
      this.case <- dplyr::mutate(this.case,
                               trade.num = as.integer(trade.num),
                               put.strike = as.numeric(put.strike),
                               open.date = as.Date(as.numeric(open.date),
                                                   origin = "1970-01-01"),
                               close.date = as.Date(as.numeric(close.date),
                                                    origin = "1970-01-01"),
                               dte = as.numeric(dte),
                               put.credit = as.numeric(put.credit) * 100,
                               open.ivrank = as.numeric(open.ivrank),
                               open.rsi = as.numeric(open.rsi),
                               put.profit = as.numeric(put.profit),
                               call.profit = as.numeric(call.profit),
                               call.strike = as.numeric(call.strike),
                               call.credit = as.numeric(call.credit) * 100,
                               profit = as.numeric(profit),
                               year = year(open.date),
                               days.held = as.numeric(close.date) - as.numeric(open.date),
                               has_profit = ifelse(profit > 0, "Yes", "No"))
      results <- rbind(results, this.case)
    }
  }) # End closing trades progress bar
  shiny::withProgress(message = "Progress Bar", detail = "Creating Plot", value = .95, {
    colnames(results) <- c("symbol", "trade.num", "put.strike", "open.date",
                           "close.date", "dte", "put.credit", "open.ivrank",
                           "open.rsi", "put.profit", "call.profit",
                           "call.strike", "call.credit", "profit", "exit.reason",
                           "year", "days.held", "has_profit")
    results <- dplyr::mutate(results,
                             trade.num = as.integer(trade.num),
                             put.strike = as.numeric(put.strike),
                             open.date = as.Date(as.numeric(open.date),
                                                 origin = "1970-01-01"),
                             close.date = as.Date(as.numeric(close.date),
                                                  origin = "1970-01-01"),
                             dte = as.numeric(dte),
                             put.credit = as.numeric(put.credit) * 100,
                             open.ivrank = as.numeric(open.ivrank),
                             open.rsi = as.numeric(open.rsi),
                             put.profit = as.numeric(put.profit),
                             call.profit = as.numeric(call.profit),
                             call.strike = as.numeric(call.strike),
                             call.credit = as.numeric(call.credit) * 100,
                             profit = as.numeric(profit),
                             year = year(open.date),
                             days.held = as.numeric(close.date) - as.numeric(open.date),
                             has_profit = ifelse(profit > 0, "Yes", "No"))
    # Send results to global data frame for viewing
    results.table <- select(results,
                            symbol, open.date, close.date, dte, days.held,
                            open.ivrank, put.strike, call.strike, exit.reason,
                            profit)
    
    # Write out closing dates to csv so can be used as the open dates with the custom open option
    close.dates <- select(results, close.date, exit.reason)
    write.csv(close.dates, file = "data/customDates.csv")
    
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
  rm(p.positions)
  rm(c.positions)
  rm(put.results)
  rm(call.results)
}
