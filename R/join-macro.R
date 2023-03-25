

# Function to join macro ------------------------------------------------

join_macro = function(data) {
  dplyr::left_join(data,
                   readr::read_csv("data/macro-clean.csv"),
                   by = c("mth_code" = "DateJOIN"))
}
