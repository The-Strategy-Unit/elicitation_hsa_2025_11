library(targets)
library(tarchetypes)

tar_source("targets")
source("R/ZZZ.R")

list(
  # generate user mappings ----
  tar_target(
    user_mapping_raw_excel,
    "recruitment.xlsx",
    format = "file"
  ),
  tar_target(
    user_mapping_file,
    generate_user_mappings(user_mapping_raw_excel),
    format = "file"
  )
)
