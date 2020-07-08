## code to prepare `DATASET` dataset goes here

state_tbl <- structure(list(abbrev = c("AL", "AK", "AZ", "AR", "CA", "CO",
                                          "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                                          "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
                                          "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
                                          "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                                          "PR", "VI", "NYC"),
                               state = c("Alabama", "Alaska", "Arizona", "Arkansas",
                                         "California", "Colorado", "Connecticut", "District of Columbia",
                                         "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                                         "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
                                         "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                                         "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                         "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                         "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                                         "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
                                         "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
                                         "Puerto Rico", "Virgin Islands", "New York City")),
                          .Names = c("abbrev", "state"),
                          class = "data.frame", row.names = c(NA, -54L))


usethis::use_data(state_tbl, overwrite = TRUE)
