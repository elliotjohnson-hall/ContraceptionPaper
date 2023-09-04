library(lubridate)
library(tidyverse)

TestData <- CompletePrescriptionDataset[1:10000,]
TestData <- TestData %>%
  mutate(PaidDateMonth = lubridate::parse_date_time(PaidDateMonth, "ymd")) %>% 
  mutate(Month = month(PaidDateMonth))
TestData <- Wrangle(TestData)

str(TestData)


TestData <- TestData %>%
  group_by(PaidDateMonth) %>% 
  summarise(HealthBoardCode, sum(PaidQuantity))


TestDataSummary <- TestData %>%
  mutate(group_by(Month, Year)) 

  summarise(SumPaidQuantity = sum(PaidQuantity))

  summarise(across(sum(PaidQuantity)))
  
TestData2015 <- TestData %>% 
  filter(HealthBoardCode == "AaA") %>% 
  filter(Month == 10) %>% 
  filter(Year == 2015) %>% 
  filter(str_detect(BNFItemDescription, "DOSULEPIN HCL_CAP"))


Take DF then calculate totals of PaidQuantity based on the Month and Year


LARCTest <- LARC %>% 
  filter(Year == 2018) %>% 
  filter(Month == 4) %>% 
  filter(HealthBoardCode == "Lot") %>% 
  summarise(sum(PaidQuantity))


HealthBoardCodes <- list(unique(HBCodes$HealthBoardCode))
Years <- list(unique(LARC$Year))
Months <- list(unique(LARC$Month))
Dates <- list(unique(LARC$PaidDateMonth))



map


LARCTest2 <- LARC %>% 
  group_by(HealthBoardCode)

LARCTest2 <- LARC %>% 
  select(PaidQuantity, HealthBoardCode, PaidDateMonth) %>% 
  map_dfr(sum)
LARCTest2 <- LARC %>% 
  map(Dates, sum(LARC$PaidQuantity))
  




sum(LARC[which(LARC$team == 'A' | df$team =='C'), 3])


###########3
LARCTest3 <- Tablet %>%
  group_by(PaidDateMonth, HealthBoardCode) %>% 
  summarise(SumPaidQuantity = sum(PaidQuantity),
            .groups = 'drop') %>%
  ggplot(aes(x = PaidDateMonth, y = SumPaidQuantity)) +
  geom_rect(
    data = LARCTest3,
    mapping = aes(
      xmin = dmy("23/03/2020"),
      xmax = dmy("19/07/2020"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "yellow",
    alpha = 0.1
  ) +
  geom_rect(
    data = LARCTest3,
    mapping = aes(
      xmin = dmy("26/12/2020"),
      xmax = dmy("16/04/2021"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "yellow",
    alpha = 0.1
  ) +
  geom_rect(
    data = LARCTest3,
    mapping = aes(
      xmin = dmy("26/12/2021"),
      xmax = dmy("21/03/2022"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "yellow",
    alpha = 0.1
  ) +
geom_point() +
  scale_x_date() +
  facet_wrap( ~ HealthBoardCode, ncol = 2, scales = "free") +
  theme_minimal() +
  geom_line(colour = "blue") +
  scale_fill_brewer(palette="Spectral")



p + 
  # geom_smooth(#method = "lm", 
  #   colour = "blue", se = FALSE)


Lockdowns

interval(start = dmy("23/03/2020"), end = dmy("19/07/2020"))
interval(start = dmy("26/12/2020"), end = dmy("16/04/2021"))
interval(start = dmy("26/12/2021"), end = dmy("21/03/2022"))

