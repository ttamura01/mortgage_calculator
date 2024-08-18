setwd("/Users/takayukitamura/Documents/R_Computing/mortgage_calculator")
library(tidyverse)

# M= P(r(1+r)^n/((1+r)^n -1))

mortgage_calculator <- function(principal, annual_rate, years){
  
  monthly_rate <- annual_rate/12/100
  n <- years * 12
  
  monthly_payment <- principal* (monthly_rate * (1 + monthly_rate)^n)/
    ((1 + monthly_rate)^n -1)
  
  return(monthly_payment)
}

# input
principal <- 300000
annual_rate <- 6.4
years <- 30

# output
monthly_payment <- mortgage_calculator(principal, annual_rate, years)
monthly_payment
