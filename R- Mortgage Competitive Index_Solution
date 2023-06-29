

# Complete the 'calculate_competitive_index' function below.

#' @param df_data Data frame data from 'csv' file
#' @return Data frame with processed data
library(dplyr)
calculate_competitive_index <- function(df_data) {

  # Prosess data  
  # Step 1: Consider only approved applications
  approved_data <- df_data %>%
    filter(Action_taken == 1)

  # Step 2: Calculate Loan_amt_bank (total loan amount sanctioned by a bank each year in a country)
  bank_loan_amount <- approved_data %>%
    group_by(Year, Country, Inst_id) %>%
    summarise(Loan_amt_bank = sum(Loan_amount))

  # Step 3: Calculate Loan_amt_country (total loan amount sanctioned per year by all the banks for each country)
  country_loan_amount <- bank_loan_amount %>%
    group_by(Year, Country) %>%
    summarise(Loan_amt_country = sum(Loan_amt_bank))

  # Step 4: Calculate squared market-share for each bank
  squared_market_share <- bank_loan_amount %>%
    left_join(country_loan_amount, by = c("Year", "Country")) %>%
    mutate(market_share = (Loan_amt_bank / Loan_amt_country)^2)

  # Step 5: Calculate Competitive_index for each country
  competitive_index <- squared_market_share %>%
    group_by(Year, Country) %>%
    summarise(Competitive_index = sum(market_share))

  # Step 6: Sort the data frame
  sorted_data <- competitive_index %>%
    arrange(Year, Country)

  # Step 7: Round the Competitive_index to three digits
  sorted_data$Competitive_index <- round(sorted_data$Competitive_index, 3)
  # Return result
  
  return(sorted_data)

}

# Open connection
fptr <- file(Sys.getenv("OUTPUT_PATH"))
open(fptr, open = "w")

# Read input 'csv' file
df_input <- read.csv("/dev/stdin", stringsAsFactors = FALSE)

# Process result data set
df_output <- calculate_competitive_index(df_input)

# Save results as 'csv' file 
write.csv(df_output, fptr, row.names = FALSE)

# Close connection
close(fptr)


