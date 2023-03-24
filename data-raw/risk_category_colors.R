## code to prepare `risk_category_colors` dataset goes here
risk_category_colors <- c(
  "None" = "#00EE00",
  "Slight, Lower" = "#ADFF2F",
  "Slight, Higher" = "#FFFF00",
  "Moderate, Lower" = "#FFD700",
  "Moderate, Higher" = "#FFA500",
  "Severe, Lower" = "#EE7600",
  "Severe, Upper" = "#FF4500",
  "Extreme" = "#FF0000"
)
usethis::use_data(risk_category_colors, overwrite = TRUE)
