library(tidyverse)

# Function to calculate compound loan and return results as data frame
calculate_loan <- function(loan_amount, loan_term, interest_rate, compound_frequency, payment_frequency) {
  n <- loan_term * (12 / ifelse(payment_frequency == "month", 1, 12))
  r <- interest_rate / ifelse(compound_frequency == "monthly", 12, 1)
  PV <- loan_amount
  
  # Calculate monthly payment
  PMT <- PV * (r * (1 + r)^n) / (((1 + r)^n) - 1)
  
  # Initialize variables for results
  terms <- numeric()
  beginning_balance <- numeric()
  interest_payment <- numeric()
  principal_payment <- numeric()
  ending_balance <- numeric()
  
  # Calculate for each term
  for (i in 1:n) {
    terms[i] <- i
    beginning_balance[i] <- PV
    
    # Calculate interest payment
    interest_payment[i] <- PV * r
    
    # Calculate principal payment
    if (payment_frequency == "month") {
      principal_payment[i] <- PMT - interest_payment[i]
    } else {
      principal_payment[i] <- PMT - interest_payment[i]
    }
    
    # Calculate ending balance
    ending_balance[i] <- PV - principal_payment[i]
    
    # Update PV for next iteration
    PV <- ending_balance[i]
  }
  
  # Create data frame
  loan_df <- data.frame(
    term = terms,
    beginning_balance = beginning_balance,
    interest_payment = interest_payment,
    principal_payment = principal_payment,
    ending_balance = ending_balance
  )
  
  return(loan_df)
}

# Example usage
loan_amount <- 300000
loan_term <- 30
interest_rate <- 7.8 / 100
compound_frequency <- "monthly"
payment_frequency <- "month"

results_df <- calculate_loan(loan_amount, loan_term, interest_rate, compound_frequency, payment_frequency)
print(results_df)

