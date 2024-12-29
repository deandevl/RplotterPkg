library(usethis)
library(data.table)

people <- data.table::data.table(
  col_0 = c(456, 243,765,321),
  col_1 = c("Rick", "John", "Sally", "Jim"),
  col_2 = c("Smith", "Taylor", "Jones", "Reed"),
  col_3 = c(34.5, 63.54, 37.78, 29.3),
  col_4 = c("183 Sunset Blvd. Los Angeles, California",
            "98 Spalding Avenue, Bardstown, Kentucky",
            "173 Columbia Street, Austin, Texas",
            "1003 Frankfort Road, Louisville, Kentucky"
  ),
  col_5 = c("Judy","Tom","Bill","Sharon")
)

usethis::use_data(people, overwrite = TRUE)
