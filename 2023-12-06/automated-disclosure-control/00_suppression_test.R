library(tidyverse)
library(sdcTable)

source("suppress_table.r")

## Create random data to suppress later --------------------------------------------

values_lower <- c(letters[1:6],"Unknown")
values_upper <- c(LETTERS[1:4],"Prefer not to say", "Unknown")

random_data <- tibble(Lower = sample(values_lower,
                                     size = 400,
                                     replace = TRUE,
                                     prob = c(0.2,
                                              0.24,
                                              0.15,
                                              0.14,
                                              0.06,
                                              0.09,
                                              0.12)),
                      Upper = sample(values_upper,
                                     size = 400,
                                     replace = TRUE,
                                     prob = c(0.2,
                                              0.38,
                                              0.15,
                                              0.09,
                                              0.06,
                                              0.12))) %>%
  count(Lower, Upper) %>%
  pivot_wider(names_from = "Upper", values_from = "n") %>%
  replace(is.na(.), 0)

## View the random input data
random_data

## Suppress the data -------------------------------------------------------

suppressed_data <- random_data %>%
  pivot_longer(-1, names_to = "Upper", values_to = "n") %>%
  
  ## Here's my custom function
  suppress_table(
    is_microdata = FALSE,
    min_unsuppressed = 5,
    # variables_avoiding_prim_supp = c("Prefer not to say",
    #                                  "Unknown"),
    # variables_avoiding_sec_supp = c("Prefer not to say"),
    # variables_to_prefer_supp = c("Unknown"),
    # suppress_zeroes = FALSE,
    sec_supp = "row"
    ) %>%
  
  ## Filter out the total rows if you don't want them
  filter(Lower != "Total",Upper != "Total") %>%
  ## Pivot back into a more readable table
  pivot_wider(names_from = "Upper", values_from = "N")

## View the suppressed output data
suppressed_data