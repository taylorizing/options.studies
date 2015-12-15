# title: "trade_tooltip function"
# author: "Jason Taylor"

# todos:
# - 

# Function for generating tooltip (hover over) text
trade_tooltip <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.null(x$trade.num)) return(NULL)
  
  # Pick out the trade with this trade.num
  trades <- isolate(trades())
  trade <- trades[trades$trade.num == x$trade.num, ]
  
  paste0("Open: ", trade$open.date, "<br>",
         "Close: ", trade$close.date, "<br>",
         "Call strike: ", trade$call.strike, "<br>",
         "Put strike: ", trade$put.strike, "<br>",
         "DTE: ", trade$dte, "<br>",
         "IVRank: ", trade$open.ivrank, "<br>",
         "rsi: ", round(trade$open.rsi, digits = 0), "<br>",
         "Exit: ", trade$exit.reason, "<br>",
         "Profit: $", format(trade$profit, big.mark = ",", scientific = FALSE)
  )
} # End function for generating tooltip text