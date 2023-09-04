library(tidyverse)
library(vroom)
library(here)

CompleteDataset <- CompleteDataset %>%
  mutate(HealthBoard = case_when(is.na(HBT) ~ HBT2014,!is.na(HBT) ~ HBT)) %>%
  select(-c(`_id`, `_full_text`, HBT2014, HBT))

HealthBoards <- vroom("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv") %>%
  mutate(HealthBoard = HB) %>%
  select(HealthBoard, HBName)

GPPractices <- vroom("https://www.opendata.nhs.scot/dataset/f23655c3-6e23-4103-a511-a80d998adb90/resource/1a15cb34-fcf9-4d3f-ad63-1ba3e675fbe2/download/practice_contactdetails_oct2022-open-data.csv") %>%
  mutate(GPPractice = as.character(PracticeCode), HealthBoard = HB)

CompleteDataset <- left_join(CompleteDataset, HealthBoards)
CompleteDataset <- left_join(CompleteDataset, GPPractices)

remove(list = c("GPPractices", "HealthBoards", "DataList", "DataFrameList", "ResourceIDs", "BaseURL", "ChoiceToUpdate", "NumberOfRecordsFound", "NumberOfRecordsRetrieved", "Record", "SQL", "ResourceID", "TempResult"))

if (dir.exists(here("Data")) == TRUE) {
  unlink(here("Data"), recursive = TRUE)
  dir.create(here("Data"))
  write_rds(CompleteDataset, file = here("Data", "Data.RDS"))
} else {
  dir.create(here("Data"))
  write_rds(CompleteDataset, file = here("Data", "Data.RDS"))
}
