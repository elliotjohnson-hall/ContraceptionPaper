library(jsonlite)
library(glue)
library(ckanr)
library(tidyverse)
library(here)

if (file.exists(here("Data", "Data.RDS"))) {
  CompleteDataset <- read_rds(here("Data", "Data.RDS"))
} else {
  if (exists("CompleteDataset") == FALSE) {
    CompleteDataset <- NULL
  } else {
    CompleteDataset <- CompleteDataset
  }
}

BaseURL <-  "https://www.opendata.nhs.scot/"

ResourceIDs <-
  package_show(id = "84393984-14e9-4b0d-a797-b288db64d088",
               url = BaseURL,
               as = "table")$resources %>% select(id)

NumberOfRecordsFound <- nrow(ResourceIDs)
NumberOfRecordsRetrieved <-
  length(unique(CompleteDataset$RecordNumber))

if (NumberOfRecordsFound > NumberOfRecordsRetrieved) {
  ChoiceToUpdate <-
    menu(
      title = glue(
        "{NumberOfRecordsFound - NumberOfRecordsRetrieved} new records found. Would you like to update the data?"
      ),
      choices = c("Yes", "No")
    )

  if (ChoiceToUpdate == 1L) {
    message("Updating data. This may take some time.")
    for (Record in NumberOfRecordsRetrieved + 1:NumberOfRecordsFound) {
      message(glue("Retrieving record {Record} of {NumberOfRecordsFound}."))
      ResourceID <- ResourceIDs[Record,]
      SQL <-
        glue(
          'SELECT * from "{ResourceID}" WHERE ',
          '"BNFItemCode"',
          " LIKE '07030%' OR ",
          '"BNFItemCode"',
          " LIKE '21040%'"
        )
      TempResult <- ds_search_sql(SQL, url = BaseURL, as = "json")
      DataList <- fromJSON(TempResult)
      assign(glue("DF{Record}"), DataList$result$records)
      assign(glue("DF{Record}"), get(glue("DF{Record}")) %>% mutate(RecordNumber = glue("{Record}")))
      message(
        glue(
          "Success! {round((Record / (NumberOfRecordsFound - NumberOfRecordsRetrieved) * 100), 2)}% complete."
        )
      )
    }
    DataFrameList <- mget(ls(pattern = "DF"))
    if (is.null(CompleteDataset) == FALSE) {
      CompleteDataset <- bind_rows(list(CompleteDataset, DataFrameList))
      remove(list = ls(pattern = "DF"))
    } else {
      CompleteDataset <- bind_rows(DataFrameList)
      remove(list = ls(pattern = "DF"))
    }
source(here("Scripts", "Wrangle.R"))
  }
  else {
    message("You got it Chief, doing nada.")
  }
}
