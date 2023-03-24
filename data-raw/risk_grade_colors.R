## code to prepare `risk_grade_colors` dataset goes here
risk_grade_colors <- c(
  "A" = "#33CC33",
  "B" = "#99FF33",
  "C" = "#FFFF00",
  "D" = "#FF9900",
  "E" = "#FF0000")
usethis::use_data(risk_grade_colors, overwrite = TRUE)
