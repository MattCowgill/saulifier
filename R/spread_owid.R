

spread_owid <- function(raw_owid, selected_countries, selected_series) {
  raw_owid %>%
    filter(location %in% selected_countries) %>%
    select(location, date,
           one_of(selected_series)) %>%
    pivot_longer(cols = !one_of("location", "date")) %>%
    mutate(location = factor(location, levels = selected_countries, ordered = TRUE),
           name = factor(name, levels = selected_series, ordered = TRUE)) %>%
    arrange(name, location, date) %>%
    mutate(location = saulify_country_names(location)) %>%
    unite(col = country_series, location, name, sep = ":") %>%
    pivot_wider(names_from = country_series, values_from = value) %>%
    mutate_at(.vars = vars(!matches("per|date")),
              .funs = as.integer)
  
}
