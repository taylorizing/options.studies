# title: "Print Money function"
# author: "Jason Taylor"

# todos:
# - 

# Function to format numeric to currency for output
printMoney <- function(x){
  format(x, digits = 10, nsmall = 2, decimal.mark = ".", big.mark = ",")
}