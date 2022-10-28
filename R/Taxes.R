#  TAX PACKAGE
#' This function consists of taxing and budgeting functions. It gives users tax data for the requested state(s),
#' and appends their data frames with the relative tax rates. It also allows the users to determine if a tax will
#' go over their budget.
#' @name Taxes
#' @usage tax_rate(state)
#' @param state a string or vector containing states in the format "AB"
#' @usage append_tax_rates(data, data_frame)
#' @param data a vector containing states in the format "AB"
#' @param data_frame a data frame containing a vector of states
#' @usage budget(state, price, budget)
#' @param price a numeric
#' @param budget a numeric
#' @examples
#'  tax_rate("NC")
#'
#'  append_tax_rates(mydata$states, mydata)
#'
#'  budget("MA", 300, 315)
#'
#'

library(devtools)
library(roxygen2)

# Example Dataset
states <- c("MA", "VA", "NY", "NC", "MD", "OH", "FL", "GA")
prices <- c(10, 14, 13, 8, 25, 7, 35, 19)
row_names <- c("states", "prices")
data <- data.frame(states, prices, row.names = NULL)


# Data frame with States and respective tax rates

tax_data <- data.frame(
  c("ME", "MA", "RI", "CT", "NJ", "NY", "DE", "MD", "DC", "PA", "VA", "NC", "SC", "GA", "FL"),
  c(0.05, 0.0625, 0.07, 0.0635, 0.066, 0.0852, 0, 0.06, 0.06, 0.0634, 0.0575, 0.0698, 0.0744, 0.0735, 0.0701)
)
colnames(tax_data) <- c("States", "Tax Rates")     # Renaming the DF column names


# First function: Gives the tax rate for state requested

tax_rate <- function(state){
  x <- tax_data$`Tax Rates`[match(state, tax_data$States, nomatch = NA)]
  return(x)
}


# Second function: Adding a new column to the data frame with the amount paid in taxes

append_tax_rates <- function(data, data_frame){
  x <- tax_rate(data)
  z <- x * 100          # Converting the decimals to percentages
  `Tax Rate` <- paste(z, "%")
  a <- cbind(`Tax Rate`, data_frame)
  return(a)
}


# Third Function: Enter your WTP for a good, and the function will tell you if the tax is
# going to make you go over budget.

budget <- function(state, price, budget){
  x <- tax_rate(state)
  tax <- (x * price)
  new_price <- price + tax
  #budget1 <- as.numeric(budget)
  if (new_price > budget){
    print("Your tax-adjusted price is over your budget")
  }
  else {
    print("Your tax-adjusted price is under your budget")
  }
}

