# title: "outputHTML function"
# author: "Jason Taylor"

# Todos:
# - check on scoping warnings from code diagnostics to see if they can be cleaned up

# Simple function to format HTML output to pass back to shiny
# Called by the server.R script to output totals

outputHTML <- function(){
  output$this.run <- renderUI({
    str.text <- "You have run the following study: "
    str.study <- paste(" - ", study)
    str.stock <- paste(" - ", stock)
    str.openOption <- paste(" ", openOption)
    h2(paste(str.text, str.openOption, str.stock, str.study))
  })
  output$details <- renderUI({
    str.dte <- paste("(", o.dte, " DTE,")
    str.p.delta <- paste(" ", -100 * p.delta, " Put delta,")
    str.c.delta <- paste(" ", 100 * c.delta, " Call delta,")
    str.low.iv <- paste(" ", low.iv, " Min IV,")
    str.high.iv <- paste(" ", high.iv, " Max IV,")
    str.min.roc <- paste(" ", min.roc, " Min ROC,")
    str.prof.targ <- paste(" ", 100 * prof.targ, "% Prof Target,")
    str.loss.lim <- paste(" ", loss.lim - 1, "X Loss limit)")
    HTML(paste(str.dte, str.p.delta, str.c.delta, str.low.iv, str.high.iv,
               str.min.roc, str.prof.targ, str.loss.lim))
  })
  output$total_profit <- renderUI({
    str1 <- paste0("Total profit of trades: $", printMoney(tot_profit))
    HTML(str1)
  })
  output$avg_prof_trade <- renderUI({
    str2 <- paste0("Average return per trade: $", printMoney(round(avg_profit, digits = 2)))
    HTML(str2)
  })
  output$avg_days <- renderUI({
    str7 <- paste0("Average # days held: ", round(avg_days, digits = 0))
    HTML(str7)
  })
  output$avg_prof_day <- renderUI({
    str8 <- paste0("Average profit/day: $", printMoney(round(avg_prof_day, digits = 2)))
    HTML(str8)
  })
  output$percent_winners <- renderUI({
    str4 <- paste0("Percent winners: ", percent_winners)
    HTML(str4)
  })
  output$exit.profit.target <- renderUI({
    str.profit <- paste0("Profit target: ", exit.profit.target)
    HTML(str.profit)
  })
  output$exit.loss.limit <- renderUI({
    str.loss <- paste0("Loss limit: ", exit.loss.limit)
    HTML(str.loss)
  })
  output$exit.expiration <- renderUI({
    str.expiration <- paste0("Expiration: ", exit.expiration)
    HTML(str.expiration)
  })
  output$exit.gamma.risk <- renderUI({
    str.gamma <- paste0("Gamma risk: ", exit.gamma.risk)
    HTML(str.gamma)
  })
  output$exit.earnings <- renderUI({
    str.earnings <- paste0("Earnings: ", exit.earnings)
    HTML(str.earnings)
  })
  output$max_loss <- renderUI({
    str3 <- paste0("Max loss: $", printMoney(maximum_loss))
    HTML(str3)
  })
}
