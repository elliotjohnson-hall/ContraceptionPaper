library(tidyverse)
library(here)
HealthBoardCode <-
  c(
    "AaA",
    "Bor",
    "DaG",
    "Fif",
    "Fif",
    "FoV",
    "Gra",
    "GGC",
    "GGC",
    "Hig",
    "Lan",
    "Lan",
    "Lot",
    "Ork",
    "She",
    "Tay",
    "Tay",
    "WIs"
  )
HBName <-
  c(
    "NHS Ayrshire and Arran", "NHS Borders", "NHS Dumfries and Galloway", "NHS Fife", "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow and Clyde", "NHS Greater Glasgow and Clyde",
    "NHS Highland",
    "NHS Lanarkshire",
    "NHS Lanarkshire",
    "NHS Lothian",
    "NHS Orkney",
    "NHS Shetland",
    "NHS Tayside",
    "NHS Tayside",
    "NHS Western Isles"
  )
HB <-
  c(
    "S08000015",
    "S08000016",
    "S08000017",
    "S08000029",
    "S08000018",
    "S08000019",
    "S08000020",
    "S08000031",
    "S08000021",
    "S08000022",
    "S08000023",
    "S08000032",
    "S08000024",
    "S08000025",
    "S08000026",
    "S08000030",
    "S08000027",
    "S08000028"
  )
HBCodes <- tibble(HealthBoardCode, HBName, HB)

write_csv(HBCodes, here("Data", "HBCodes.csv"))
