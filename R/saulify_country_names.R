
saulify_country_names <- function(country_names) {
  
  char_names <- as.character(country_names)
  
  dplyr::case_when(char_names == "New Zealand" ~ "NZ",
                   char_names == "United States" ~ "US",
                   char_names == "South Korea" ~ "Korea",
                   char_names == "United Kingdom" ~ "UK",
                   TRUE ~ char_names)
}

