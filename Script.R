URLs <- c("https://www.opendata.nhs.scot/dataset/84393984-14e9-4b0d-a797-b288db64d088/resource/02197246-5d98-4ba9-b25d-218ac9cd91e6/download/pitc201904.csv",
"https://www.opendata.nhs.scot/dataset/84393984-14e9-4b0d-a797-b288db64d088/resource/7479536b-0b95-43d6-9152-31bbd522e6b4/download/pitc201905.csv",
"https://www.opendata.nhs.scot/dataset/84393984-14e9-4b0d-a797-b288db64d088/resource/6ea2f299-76bc-49cd-ab43-9228b601da5f/download/pitc201906.csv",
"https://www.opendata.nhs.scot/dataset/84393984-14e9-4b0d-a797-b288db64d088/resource/9cdc0526-21ab-43de-b832-bc032cd31b24/download/pitc202004.csv",
"https://www.opendata.nhs.scot/dataset/84393984-14e9-4b0d-a797-b288db64d088/resource/f2ffa7b3-3f93-470a-8125-880afb9aafe0/download/pitc202005.csv",
"https://www.opendata.nhs.scot/dataset/84393984-14e9-4b0d-a797-b288db64d088/resource/64131d22-ab47-43ff-9674-ebe8ed7edbd7/download/pitc202006.csv")
URLCodes <- c(201904, 201905, 201906, 202004, 202005, 202006)

library(tidyverse)
library(vroom)
library(glue)

for (URL in 1:length(URLs)) {
  assign(glue("DF", URLCodes[URL]), vroom(URLs[URL]))
  URL = URL + 1
}

DF2019 <- bind_rows(DF201904, DF201905, DF201906)
DF2020 <- bind_rows(DF202004, DF202005, DF202006)

DF2019 <- DF2019 %>%
  select(c(BNFItemCode, BNFItemDescription, NumberOfPaidItems, PaidQuantity, PaidDateMonth))

DF2020 <- DF2020 %>%
  select(c(BNFItemCode, BNFItemDescription, NumberOfPaidItems, PaidQuantity, PaidDateMonth))

CombinedData <- bind_rows(DF2020, DF2019)

rm(list = ls(pattern = "DF"))

BNFCodes <- c(070301, 070302, 210400, 070305)

CombinedData <- CombinedData %>%
  filter(str_detect(BNFItemCode, "070301|070302|210400|070305"))

library(ggplot2)

CombinedData <- CombinedData %>%
  mutate(Type = as.factor(case_when(
    str_detect(BNFItemCode, "0703022M") ~ "Injection",
    str_detect(BNFItemCode, "0703023") ~ "IUS",
    str_detect(BNFItemCode, "21040") ~ "IUD",
    str_detect(BNFItemDescription, "NEXPLANON") ~ "Implant",
    str_detect(BNFItemCode, "0703050") ~"Emergency Contraception",
    str_detect(BNFItemCode, "0703010") ~ "COCP",
    str_detect(BNFItemCode, "0703021") ~ "POP",
    str_detect(BNFItemDescription, "RING") ~ "Ring")),
    Year = as.factor(case_when(
      str_detect(PaidDateMonth, "2019") ~ "2019",
      str_detect(PaidDateMonth, "2020") ~ "2020")))

CombinedData %>%
  filter() %>%
  ggplot(aes(x = Type, fill = Year)) +
  geom_bar(position = "dodge")

NoOralData <- CombinedData %>%
  filter(!(Type == "POP" | Type == "COCP"))

library(ElliotFunctions)

NoOralData %>%
  filter() %>%
  ggplot(aes(x = Type, fill = Year)) +
  geom_bar(position = "dodge") +
  ThemeElliot

