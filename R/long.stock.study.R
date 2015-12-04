# LongStock <- function() {
#   source("Study/LongStock/Long_Stock_Setup.R")
#   progress.int <- .001
#   results <- data.frame()
#   st.results.table <- data.frame()
#   p.positions <- data.frame()
#   st.positions <- data.frame()
#   st.results <- data.frame()
#   
#   withProgress(message = "Progress Bar", detail = "Opening Trades", {
#     n <- 0
#     for (i in unique(first.day$open.date))  {
#       incProgress(progress.int)
#       n <- n + 1
#       start.data <- filter(data, date == i)
#       
#       if (nrow(start.data) > 0 && start.data[1, iv.rank] >= low.iv &&
#          start.data[1, iv.rank] <= high.iv) {
#         short.data <- mutate(start.data, dte.diff = abs(o.dte - dte))
#         short.data <- filter(short.data, dte.diff == min(dte.diff)) # Closest to DTE 
#         short.data <- filter(short.data, delta >= p.delta)
#         short.put.trade <- filter(short.data, delta == min(delta))
#         short.put.trade <- filter(short.put.trade, rank(date, ties.method = "first") == 1)
#         short.put.trade <- mutate(short.put.trade, margin = max((100 * (.1 * strike)) + (100 * mid.price), 
#                                                                 ((.2 * price * 100) - (100* (price - strike)) + (100 * mid.price))))
#         short.put.trade <- mutate(short.put.trade, open.roc = 100 * (((100 * mid.price) / margin)))
#         short.put.trade <- filter(short.put.trade, open.roc >= min.roc)
#         long.stock.trade <- short.put.trade
#         long.stock.trade <- mutate(long.stock.trade, margin = (100 * (price /2)))
#         if (nrow(short.put.trade) > 0)  {
#           if (!stock == "AAPL" && !stock == "GOOG") {
#             if (short.put.trade$expiration <= "2015-12-31") {
#               short.put.trade <- mutate(short.put.trade, trade.num = n)
#               long.stock.trade <- mutate(long.stock.trade, trade.num = n)
#               p.positions <- rbind(p.positions, short.put.trade, fill = TRUE)
#               st.positions <- rbind(st.positions, long.stock.trade, fill = TRUE)
#             }  
#           }
#           if (stock == "AAPL") {
#             if (short.put.trade$expiration <= "2014-06-06") {
#               short.put.trade <- mutate(short.put.trade, trade.num = n)
#               p.positions <- rbind(p.positions, short.put.trade, fill = TRUE)
#             }  
#           }
#           if (stock == "GOOG") {
#             if (short.put.trade$expiration <= "2014-03-27") {
#               short.put.trade <- mutate(short.put.trade, trade.num = n)
#               p.positions <- rbind(p.positions, short.put.trade, fill = TRUE)
#             }  
#           }
#         }
#       }
#     }
#   }) # End opening trades progress bar
#   
#   withProgress(message = "Progress Bar", detail = "Closing Trades", value = .5, {
#     # Loop through all trades and find the results
#     for (i in 1:nrow(p.positions))  {
#       incProgress(progress.int)
#       p <- p.positions[i, mid.price]
#       o <<- p.positions[i, date]
#       e <- p.positions[i, expiration]
#       s <- p.positions[i, strike]
#       j <<- p.positions[i, trade.num]
#       m <- p.positions[i, margin]
#       stm <- st.positions[i, margin]
#       oroc <- p.positions[i, open.roc]
#       xc <- p - (p * prof.targ) #proftarg # Profit target x percent
#       xl <- p * loss.lim # Limit loss to input times credit received
#       possible.close <- filter(data, expiration == e, date >= o, date <= e, strike == s)
#       # Find the close if percent profit target is hit 
#       possible.xc.close <- filter(possible.close, mid.price <= xc)
#       if (nrow(possible.xc.close) > 0) {
#         xc.close <- filter(possible.xc.close, date == min(date))
#         xcs.close <- xc.close
#         xcs.close <- mutate(xcs.close, trade.num = j, open.date = o, exit.reason = "Profit target", margin = stm)
#         xc.close <- mutate(xc.close, trade.num = j, open.date = o, exit.reason = "Profit target", margin = m, open.roc = oroc)
#         results <- rbind(results, xc.close, fill = TRUE)
#         st.results <- rbind(st.results, xcs.close, fill = TRUE)
#       }
#       # Find the close if the limit loss target is hit
#       possible.xl.close <- filter(possible.close, mid.price >= xl)
#       if (nrow(possible.xl.close) > 0) {
#         xl.close <- filter(possible.xl.close, date == min(date))
#         xcl.close <- xl.close
#         xcl.close <- mutate(xcl.close, trade.num = j, open.date = o, exit.reason = "Loss limit", margin = stm)
#         xl.close <- mutate(xl.close, trade.num = j, open.date = o, exit.reason = "Loss limit", margin = m, open.roc = oroc)
#         results <- rbind(results, xl.close, fill = TRUE)
#         st.results <- rbind(st.results, xcl.close, fill = TRUE)
#       }
#       # Close by gamma days input
#       if (g > 0) {
#         gamma.date <- as.Date(e, origin = "1970-01-01") - g
#         ifelse(weekdays(gamma.date, abbreviate = FALSE) == "Sunday", gamma.date <- gamma.date - 2, gamma.date)
#         ifelse(weekdays(gamma.date, abbreviate = FALSE) == "Saturday", gamma.date <- gamma.date - 1, gamma.date)
#         g.close <- filter(data, date == gamma.date, expiration == e, strike == s)
#         gs.close <- g.close
#         gs.close <- mutate(gs.close, trade.num = j, open.date = o, exit.reason = "Gamma risk", margin = stm)
#         g.close <- mutate(g.close, trade.num = j, open.date = o, exit.reason = "Gamma risk", margin = m, open.roc = oroc)
#         results <- rbind(results, g.close, fill = TRUE)
#         st.results <- rbind(st.results, gs.close, fill = TRUE)
#       }
#       # Close prior to earnings
#       if (earn.close == "Yes") {
#         e.dates <- filter(earn.dates, earn.date > o, earn.date < e)
#         e.dates <- filter(e.dates, earn.date == min(earn.date))
#         if (nrow(e.dates) > 0 ) {
#           e.date <- as.Date(e.dates$earn.date)
#           e.close <- filter(data, date == e.date, expiration == e, strike == s)
#           es.close <- e.close
#           es.close <- mutate(es.close, trade.num = j, open.date = o, exit.reason = "Earnings", margin = stm)
#           e.close <- mutate(e.close, trade.num = j, open.date = o, exit.reason = "Earnings", margin = m, open.roc = oroc)
#           results <- rbind(results, e.close, fill = TRUE)
#           st.results <- rbind(st.results, es.close, fill = TRUE)
#         }
#       }
#       # Find the close at expiration
#       exp.close <- filter(data, date == e, expiration == e, strike == s)
#       exps.close <- exp.close
#       exps.close <- mutate(exps.close, trade.num = j, open.date = o, exit.reason = "Expiration", margin = stm)
#       exp.close <- mutate(exp.close, trade.num = j, open.date = o, exit.reason = "Expiration", margin = m, open.roc = oroc)
#       results <- rbind(results, exp.close, fill = TRUE)
#       st.results <- rbind(st.results, exps.close, fill = TRUE)
#     } # End loop through all trades and find the results
#     
#     # filter down results to the first exit for each trade as multiple targets could be hit
#     
#     ord.results <- group_by(results, open.date)
#     ord.st.results <- group_by(st.results, open.date)
#     results <- filter(ord.results, rank(date, ties.method = "first") == 1) 
#     st.results <- filter(ord.st.results, rank(date, ties.method = "first") == 1)
#     
#     # Merge the opening and closing data frames to calculate profit loss
#     merge.results <- merge(results, p.positions, c("trade.num", "expiration"))
#     merge.st.results <- merge(st.results, st.positions, "trade.num")
#     
#     results <- mutate(merge.results, profit.loss = 100 * (mid.price.y - mid.price.x))
#     st.results <- mutate(merge.st.results, profit.loss = 100 * (price.x - price.y))
#     
#     results <- mutate(results, year = year(date.y))
#     st.results <- mutate(st.results, year = year(date.y))
#     
#     results <- select(results, expiration, trade.num, close.date = date.x, put.strike = strike.x,
#                       close.price = mid.price.x,
#                       close.rsi = rsi.14.x, close.ivrank = iv.rank.x, open.date = date.y, price = price.y,
#                       open.price = mid.price.y, delta = delta.y, dte = dte.y,
#                       open.rsi = rsi.14.y, open.ivrank = iv.rank.y,
#                       exp_type = exp_type.y, profit = profit.loss, year, exit.reason, margin = margin.y, open.roc = open.roc.y)
#     st.results <- select(st.results,
#                          expiration = expiration.y,
#                          trade.num,
#                          close.date = date.x,
#                          close.price = price.x,
#                          close.rsi = rsi.14.x,
#                          close.ivrank = iv.rank.x,
#                          open.date = date.y,
#                          open.price = price.y,
#                          open.rsi = rsi.14.y,
#                          open.ivrank = iv.rank.y,
#                          profit = profit.loss,
#                          year,
#                          exit.reason,
#                          margin = margin.x)
#     
#     # Add in commission at a rate of $1.5 per trade excluding closing for .05 or less
#     results <- mutate(results, profit = ifelse(close.price <= .05, profit - 1.5, profit - 3),
#                       exit.roc = (100 * profit) / margin,
#                       call.strike = "NA", days.held = as.numeric(close.date) - as.numeric(open.date))
#     st.results <- mutate(st.results, profit = profit - 19.90,
#                          exit.roc = (100 * profit) / margin,
#                          days.held = as.numeric(close.date) - as.numeric(open.date))
#   }) # End closing trades progress bar
#   
#   withProgress(message = "Progress Bar", detail = "Creating Plot", value = .95, {    
#     # Add column which says whether the trade was profitable
#     results$has_profit[results$profit > 0] <- "Yes"
#     results$has_profit[results$profit <= 0] <- "No"
#     
#     # Send results to global data frame for viewing
#     formatted.results <- select(results, open.date, close.date, expiration, put.strike, dte,
#                                 days.held, open.ivrank, exp_type, exit.reason, profit, margin, open.roc, exit.roc)
#     results.table <<- formatted.results
#     
#     formatted.st.results <- select(st.results, open.date, close.date, expiration,
#                                    days.held, exit.reason, profit, margin, exit.roc)
#     st.results.table <<- formatted.st.results
#     
#     results <<- results
#     
#     # Write out closing dates to csv so can be used as the open dates with the custom open option
#     close.dates <- select(formatted.results, close.date, exit.reason)
#     write.csv(close.dates, file = "Data/customDates.csv")
#     
#     # Calculate totals for display
#     num_t <<- nrow(results)
#     tot_profit <<- sum(results$profit)
#     avg_profit <<- tot_profit/num_t
#     tot_days <<- sum(results$days.held)
#     avg_days <<- tot_days/num_t
#     avg_prof_day <<- avg_profit/avg_days
#     maximum_loss <<- min(results$profit)
#     maximum_win <<- max(results$profit)
#     ifelse(maximum_loss >= 0, maximum_loss <<- 0, maximum_loss)
#     percent_winners <<- percent(length(which(results$profit > 0)) / num_t)
#     exit.expiration <<- percent(length(which(results$exit.reason == "Expiration")) / num_t)
#     exit.expiration <<- ifelse(exit.expiration == "NaN%", 0, exit.expiration)
#     exit.profit.target <<- percent(length(which(results$exit.reason == "Profit target")) / num_t)
#     exit.profit.target <<- ifelse(exit.profit.target == "NaN%", 0, exit.profit.target)
#     exit.loss.limit <<- percent(length(which(results$exit.reason == "Loss limit")) / num_t)
#     exit.loss.limit <<- ifelse(exit.loss.limit == "NaN%", 0, exit.loss.limit)
#     exit.gamma.risk <<- percent(length(which(results$exit.reason == "Gamma risk")) / num_t)
#     exit.gamma.risk <<- ifelse(exit.gamma.risk == "NaN%", 0, exit.gamma.risk)
#     exit.earnings <<- percent(length(which(results$exit.reason == "Earnings")) / num_t)
#     exit.earnings <<- ifelse(exit.earnings == "NaN%", 0, exit.earnings)
#     avg.entry.margin <<- sum(results$margin) / num_t
#     avg.exit.roc <<- percent(avg_profit / avg.entry.margin)
#     
#     # Calculate totals for display
#     tot_profit2 <<- sum(st.results$profit)
#     avg_profit2 <<- tot_profit2/num_t
#     avg_prof_day2 <<- avg_profit2/avg_days
#     maximum_loss2 <<- min(st.results$profit)
#     maximum_win2 <<- max(st.results$profit)
#     ifelse(maximum_loss2 >= 0, maximum_loss2 <<- 0, maximum_loss2)
#     percent_winners2 <<- percent(length(which(st.results$profit > 0)) / num_t)
#     avg.entry.margin2 <<- sum(st.results$margin) / num_t
#     avg.exit.roc2 <<- percent(avg_profit2 / avg.entry.margin2)
#   }) # End creating plot progress bar
# }