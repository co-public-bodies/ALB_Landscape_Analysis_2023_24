## Large Number Labels Function
# Martin Ingram | 2023-10-13

# Info --------------------------------------------------------------------

# Purpose: Takes large, machine-readable, numbers and generates 
# publication friendly labels. 

# Example: 1200000 becomes 1.2 M

# Source: Code adapted from initial answer here: 
# https://stackoverflow.com/questions/28159936/format-numbers-with-million-m-and-billion-b-suffixes

# Define Function ---------------------------------------------------------

format_bignum = function(n, input_digits = 2){
    
    library(tidyverse)
    
    case_when(
        n >= 1e12 | n <= -1e12 ~ paste(janitor::round_half_up(n/1e12, 
                                 digits = input_digits), 'Tn'),
        n >= 1e9  | n <= -1e9 ~ paste(janitor::round_half_up(n/1e9,
                                 digits = input_digits), 'Bn'),
        n >= 1e6  | n <= -1e6 ~ paste(janitor::round_half_up(n/1e6,
                                 digits = input_digits), 'M'),
        n >= 1e3  | n <= -1e3 ~ paste(janitor::round_half_up(n/1e3, 
                                 digits = input_digits), 'K'),
        TRUE ~ as.character(n))
}


# Example Use -------------------------------------------------------------

# options(scipen = 999)
# 
# # Some mock data
# test_data <- tibble::tibble(name = c("A", "B", "C", "D"), 
#                             value = c(1000, 
#                                       1200000, 
#                                       8700000000,
#                                       1900000000000))
# 
# # Apply function and create a new column
# test_data <- test_data %>% 
#     mutate(value_label = format_bignum(n = value,
#                                           input_digits = 2))