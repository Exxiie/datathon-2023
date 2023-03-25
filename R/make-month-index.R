

# Make Month Index CSV ----------------------------------------------------

d = {
  data.frame(month = c(201801:201812,
                       201901:201912,
                       202001:202012,
                       202101:202112),
             month_index = 1:48)
}

readr::write_csv(d, "data/month-index.csv")
