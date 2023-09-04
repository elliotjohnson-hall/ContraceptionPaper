# Install and load required libraries
if (!require(pacman))
  install.packages("pacman")
pacman::p_load(
  "jsonlite",
  "tidyverse",
  "glue",
  "ckanr",
  "ggplot2",
  "here",
  "lubridate",
  "patchwork",
  "gt"
)

# Connect to NHS Scotland Open Data API and import data on contraception prescriptions dispensed
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
      ResourceID <- ResourceIDs[Record, ]
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
      assign(glue("DF{Record}"),
             get(glue("DF{Record}")) %>% mutate(RecordNumber = glue("{Record}")))
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
    source(here("Wrangle.R"))
  }
  else {
    message("Exited successfully.")
  }
}

# Now the data on contraception is available in the local workspace, time to wrangle it

# Months between dates
# Oct 2015 to Apr 2020
Period1 <- 54

# Apr 2020 to Apr 2022
Period2 <- 24

# May 2022 to end of dataset
Period3 <- (NumberOfRecordsFound - 1) - (Period1 + Period2)

CompleteDataset <- CompleteDataset %>%
  mutate(
    PaidDateMonth = ym(PaidDateMonth),
    PaidQuantity = as.integer(PaidQuantity),
    HealthBoard = as.factor(HealthBoard),
    # 1 = Pre-Covid; 2 = During Covid; 3 = Post-Covid
    Period = as.factor(
      case_when(
        PaidDateMonth < dmy("01/04/2020") ~ "1",
        between(PaidDateMonth, dmy("01/04/2020"), dmy("01/04/2022")) ~ "2",
        PaidDateMonth > dmy("01/04/2022") ~ "3"
      )
    ),
    TotalMonths = case_when(Period == 1 ~ 31119114,
                            Period == 2 ~ 12500003,
                            Period == 3 ~ 5520534),
    Months = case_when(Period == 1 ~ Period1,
                       Period == 2 ~ Period2,
                       Period == 3 ~ Period3),
    Type = case_when(
      str_detect(BNFItemCode, "0703021") ~ "POP",
      str_detect(BNFItemCode, "0703010") &
        str_detect(BNFItemDescription, "TAB") ~ "COCP",
      str_detect(BNFItemCode, "0703022M|0703022N") ~ "Injection",
      str_detect(BNFItemCode, "0703023") ~ "IUS",
      str_detect(BNFItemCode, "21040") ~ "IUD",
      str_detect(BNFItemCode, "0703022P") ~ "Implant",
      str_detect(BNFItemCode, "0703050") ~ "EC",
      str_detect(BNFItemCode, "0703010E0BG") ~ "Patch",
      str_detect(BNFItemCode, "0703011") ~ "Ring",
      str_detect(BNFItemCode, "0703030") ~ "Jelly"
    ),
    ProtectionLength = case_when(
      str_detect(BNFItemCode, "0703021") ~ 1,
      str_detect(BNFItemCode, "0703010") &
        str_detect(BNFItemDescription, "TAB") ~ 1,
      str_detect(BNFItemCode, "0703022M|0703022N") ~ 3,
      str_detect(BNFItemCode, "0703023") ~ 60,
      str_detect(BNFItemCode, "21040") ~ 60,
      str_detect(BNFItemCode, "0703022P") ~ 36,
      str_detect(BNFItemCode, "0703010E0BG") ~ 21,
      str_detect(BNFItemCode, "0703011") ~ 21
    ),
    PackSize = case_when(
      str_detect(BNFItemCode, "0703021") ~ 21,
      str_detect(BNFItemCode, "0703010") &
        str_detect(BNFItemDescription, "TAB") ~ 21,
      str_detect(BNFItemCode, "0703022M|0703022N") ~ 1,
      str_detect(BNFItemCode, "0703023") ~ 1,
      str_detect(BNFItemCode, "21040") ~ 1,
      str_detect(BNFItemCode, "0703022P") ~ 1,
      str_detect(BNFItemCode, "0703010E0BG") ~ 3,
      str_detect(BNFItemCode, "0703011") ~ 1
    ),
    MonthsContraception = (PaidQuantity / PackSize) * ProtectionLength
  )

# Extracting contraceptive types from data

# LARC
Injection <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703022M|0703022N"))

IUS <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703023"))

IUD <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "21040"))

Implant <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703022P"))

LARCList <- list(IUD, IUS, Implant, Injection)
LARC <- list_rbind(LARCList)
rm(LARCList)

# Emergency contraception
EmergencyContraception <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703050")) %>%
  mutate(LorU = case_when(
    str_detect(BNFItemCode, "0703050A") ~ "L",
    str_detect(BNFItemCode, "0703050B") ~ "U"
  ))

Levo <- EmergencyContraception %>%
  filter(str_detect(BNFItemCode, "0703050A"))

Ulip <- EmergencyContraception %>%
  filter(str_detect(BNFItemCode, "0703050B"))

# Oral contraception
OralContraception <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703010F")) %>%
  rbind(., CompleteDataset %>% filter(str_detect(BNFItemCode, "0703021")))

# Oral contraception seperated into combined and progesterone-only
COCP <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703010")) %>%
  filter(str_detect(BNFItemDescription, "TAB"))

POP <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703021"))

# Patch
Patch <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703010E0BG"))

# Ring
Ring <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703011"))

# Jelly
Jelly <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703030"))

# Test if number of observations in the dataset match the sum of the number of observations in each subgroup

nrows <-
  nrow(Injection) + nrow(COCP) + nrow(EmergencyContraception) + nrow(Implant) + nrow(IUD) + nrow(IUS) + nrow(Patch) + nrow(POP) + nrow(Ring) + nrow(Jelly)

nrows == nrow(CompleteDataset)

# Plotting
theme_ <- function() {
  theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      axis.title = element_text(face = "bold"),
      axis.line = element_line(
        colour = "black",
        linewidth = 0.5,
        linetype = "solid",
        lineend = "butt"
      ),
      axis.ticks = element_line(colour = "black"),
      axis.ticks.length = unit(1, "mm"),
      legend.title = element_text(face = "bold")
    )
}

annotate_lockdowns <- function() {
  annotate(
    geom = "rect",
    xmin = c(dmy("23/03/2020"), dmy("26/12/2020"), dmy("26/12/2021")),
    xmax = c(dmy("19/07/2020"), dmy("16/04/2021"), dmy("21/03/2022")),
    ymin = c(-Inf,-Inf,-Inf),
    ymax = c(Inf, Inf, Inf),
    fill = "yellow",
    alpha = 0.25
  )
}

annotate_means <- function(dataset) {
  mean_pre_vector <- dataset %>%
    filter(period == 1) %>%
    group_by(PaidDateMonth) %>%
    summarise(sum(PaidQuantity))

  mean_during_vector <- dataset %>%
    filter(period == 2) %>%
    group_by(PaidDateMonth) %>%
    summarise(sum(PaidQuantity))

  mean_post_vector <- dataset %>%
    filter(period == 3) %>%
    group_by(PaidDateMonth) %>%
    summarise(sum(PaidQuantity))

  mean_pre_vector <- c(mean_pre_vector$`sum(PaidQuantity)`)
  mean_during_vector <- c(mean_during_vector$`sum(PaidQuantity)`)
  mean_post_vector <- c(mean_post_vector$`sum(PaidQuantity)`)

  mean_pre <- mean(mean_pre_vector)
  mean_during <- mean(mean_during_vector)
  mean_post <- mean(mean_post_vector)

  sd_pre <- sd(mean_pre_vector)
  sd_during <- sd(mean_during_vector)
  sd_post <- sd(mean_post_vector)

  annotate(
    geom = "segment",
    x = c(
      dmy("01/10/2015"),
      dmy("23/03/2020"),
      dmy("22/03/2022"),
      dmy("01/10/2015"),
      dmy("23/03/2020"),
      dmy("22/03/2022"),
      dmy("01/10/2015"),
      dmy("23/03/2020"),
      dmy("22/03/2022")
    ),
    xend = c(
      dmy("22/03/2020"),
      dmy("21/03/2022"),
      dmy("01/01/2023"),
      dmy("22/03/2020"),
      dmy("21/03/2022"),
      dmy("01/01/2023"),
      dmy("22/03/2020"),
      dmy("21/03/2022"),
      dmy("01/01/2023")
    ),
    y = c(
      mean_pre,
      mean_during,
      mean_post,
      (mean_pre + sd_pre),
      (mean_during + sd_during),
      (mean_post + sd_post),
      (mean_pre - sd_pre),
      (mean_during - sd_during),
      (mean_post - sd_post)
    ),
    yend = c(
      mean_pre,
      mean_during,
      mean_post,
      (mean_pre + sd_pre),
      (mean_during + sd_during),
      (mean_post + sd_post),
      (mean_pre - sd_pre),
      (mean_during - sd_during),
      (mean_post - sd_post)
    ),
    linetype = c(1, 1, 1,
                 2, 2, 2,
                 2, 2, 2)
  )
}

Dot_Plot <- function(dataset) {
  dataset %>%
    group_by(PaidDateMonth) %>%
    ggplot(aes(x = PaidDateMonth, y = PaidQuantity)) +
    stat_summary(fun = sum, geom = "point", aes(shape = period)) +
    annotate_lockdowns() +
    annotate_means(dataset) +
    theme_() +
    labs(x = "Quantity dispensed",
         y = "Year",
         shape = "Time period", ) +
    scale_shape_discrete(labels = c("Pre-Covid", "During Covid", "Post-Covid"))
}

Box_Plot <- function(dataset) {
  dataset %>%
    group_by(PaidDateMonth, Period) %>%
    summarise(sum = sum(MonthsContraception)) %>%
    ggplot(aes(y = sum, x = Period, fill = Period)) +
    geom_boxplot() +
    theme_() +
    labs(y = "Months of contraception dispensed per month",
         x = "Period") +
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(from = 100000, to = 200000, by = 20000)) +
    scale_fill_manual(values = c("grey80", "white", "grey60")) +
    guides(fill = "none") +
    scale_x_discrete(labels = c("Pre-Covid", "During Covid", "Post-Covid"))
}


# p1 <- Box_Plot(COCP) +
#   ggtitle("Combined oral contraception")
# p2 <- Box_Plot(POP) +
#   ggtitle("Progesterone only contraception")
# p1 + p2

OralContraception <- OralContraception %>%
  mutate(Type = case_when(
    str_detect(BNFItemCode, "0703021") ~ "POP",
    str_detect(BNFItemCode, "0703010") ~ "COC"
  ))


PLOT_COCPvsPOP <- OralContraception %>%
  group_by(PaidDateMonth, Period, Type) %>%
  summarise(sum = sum(MonthsContraception)) %>%
  ggplot(aes(y = sum, x = Period, fill = Type)) +
  geom_boxplot() +
  theme_() +
  labs(y = "Months of contraception dispensed per month",
       x = "Period") +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(from = 100000, to = 200000, by = 20000)) +
  scale_x_discrete(labels = c("Pre-Covid", "During Covid", "Post-Covid")) +
  scale_fill_manual(
    values = c("white", "grey"),
    labels = c(
      "Combined oral contraceptives",
      "Progesterone-only contraceptives"
    )
  )

# CompleteDataset %>%
#   group_by(PaidDateMonth, Period, Type) %>%
#   filter(!str_detect(Type, "EC|Jelly")) %>%
#   ggplot(aes(y = MonthsContraception, x = Period, fill = Type)) +
#   geom_col(position = "dodge") +
#   theme_() +
#   labs(y = "Months of contraception provided",
#        x = "Period") +
#   scale_fill_brewer(palette = "Accent")

# EmergencyContraception %>%
#   group_by(Period) %>%
#   summarise(PQ = (sum(PaidQuantity)/Months),
#             LorU = LorU) %>%
# ggplot(aes(x = Period, y = PQ, fill = as.factor(LorU))) +
#   stat_summary(fun = sum, geom = "col", position = "stack") +
#   theme_()

PeriodTotals <- CompleteDataset %>%
  filter(!str_detect(Type, "EC|Jelly")) %>%
  group_by(Type, Period) %>%
  summarise(Total = sum(MonthsContraception))

MonthsPerPeriod <- PeriodTotals %>%
  group_by(Period) %>%
  summarise(Duration = sum(Total))

PeriodTotals <- PeriodTotals %>%
  mutate(
    TotalMonths = case_when(
      Period == 1 ~ MonthsPerPeriod$Duration[1],
      Period == 2 ~ MonthsPerPeriod$Duration[2],
      Period == 3 ~ MonthsPerPeriod$Duration[3]
    ),
    Proportion = Total / TotalMonths
  )


# PLOT_PartsOfWhole <- PeriodTotals %>%
#     ggplot(aes(x = Period, y = Proportion, fill = Type)) +
#     stat_summary(fun = sum, geom = "col", position = "stack", colour = "black") +
#     theme_() +
#     labs(y = "Percentage of all months contraception provided",
#          x = "Period") +
#   scale_fill_brewer(palette = "Greys", guide = guide_legend(nrow = 1)) +
#   scale_y_continuous(labels = scales::label_percent()) +
#   scale_x_discrete(labels = c("Pre-Covid", "During Covid", "Post-Covid"))

PLOT_ContraceptionTypesPerPeriodBars <- CompleteDataset %>%
  ggplot(aes(
    y = MonthsContraception / TotalMonths,
    x = Period,
    fill = Type
  )) +
  stat_summary(
    fun = sum,
    geom = "col",
    colour = "black",
    position = "dodge"
  ) +
  scale_fill_manual(
    values =
      c(
        "grey90",
        "grey80",
        "grey70",
        "grey60",
        "grey50",
        "grey40",
        "grey30",
        "grey20"
      ),
    guide = guide_legend(nrow = 1)
  ) +
  theme_() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c("Pre-Covid", "During Covid", "Post-Covid"))


PLOT_ChangeLines <- PeriodTotals %>%
  ggplot(aes(
    y = Proportion,
    x = Period,
    group = Type,
    linetype = Type,
    shape = Type
  )) +
  geom_line(linewidth = 0.75) +
  geom_point(fill = "black", size = 2.5) +
  theme_() +
  scale_y_continuous(labels = scales::label_percent(),
                     breaks = seq(from = 0, to = 0.3, by = 0.05)) +
  scale_x_discrete(labels = c("Pre-Covid", "During Covid", "Post-Covid")) +
  scale_linetype_manual(values = c(1, 2, 5, 3, 1, 2, 3, 5),
                        guide = guide_legend(
                          nrow = 1,
                          override.aes = list(size = 2, linewidth = 0.5)
                        )) +
  scale_shape_manual(values = c(1, 2, 6, 5, 21, 24, 25, 22),
                     guide = guide_legend(nrow = 1)) +
  expand_limits(y = 0) +
  labs(y = "Percentage of contraception prescribed") +
  theme(legend.key.size = unit(1, "cm"))

TotalMonths <- CompleteDataset %>%
  filter(!str_detect(Type, "EC|Jelly")) %>%
  group_by(Period) %>%
  summarise(sum = sum(MonthsContraception))

TotalMonths <- TotalMonths %>%
  mutate(NumberMonths = case_when(Period == 1 ~ Period1,
                                  Period == 2 ~ Period2,
                                  Period == 3 ~ Period3))

TotalMonths %>%
  summarise(sum / NumberMonths)


TotalMonths2 <- CompleteDataset %>%
  filter(!str_detect(Type, "EC|Jelly")) %>%
  group_by(PaidDateMonth) %>%
  summarise(sum = sum(MonthsContraception)) %>%
  mutate(Period = as.factor(
    case_when(
      PaidDateMonth < dmy("22/03/2020") ~ "1",
      between(PaidDateMonth, dmy("23/03/2020"), dmy("21/03/2022")) ~ "2",
      PaidDateMonth > dmy("22/03/2022") ~ "3"
    )
  ))

TotalMonths2 %>%
  filter(Period == 1) %>%
  summarise(mean(sum), sd(sum))

TotalMonths2Outliers <- TotalMonths2 %>%
  filter(sum < 475000)

PLOT_ContraceptiveCoverageBox <- TotalMonths2 %>%
  filter(sum > 475000) %>%
  ggplot(aes(x = Period, y = sum)) +
  geom_boxplot(aes(fill = Period), outlier.shape = NA) +
  geom_jitter(
    shape = 21,
    size = 2,
    width = 0.25,
    alpha = 0.5
  ) +
  geom_jitter(
    shape = 21,
    size = 2,
    width = 0.25,
    fill = "black",
    data = TotalMonths2Outliers
  ) +
  theme_() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("grey80", "white", "grey60")) +
  guides(fill = "none") +
  scale_x_discrete(labels = c("Pre-Covid", "During Covid", "Post-Covid")) +
  labs(y = "Months of contraceptive coverage")



TotalMonthsContraceptionByPeriod <- CompleteDataset %>%
  group_by(Period) %>%
  filter(!str_detect(Type, "EC|Jelly")) %>%
  summarise(Sum = sum(MonthsContraception)) %>%
  mutate(NumberMonths = case_when(Period == 1 ~ Period1,
                                  Period == 2 ~ Period2,
                                  Period == 3 ~ Period3)) %>%
  summarise(MonthsPerMonth = Sum / NumberMonths) %>%
  mutate(Period = c(1, 2, 3))

# TotalMonthsContraceptionByPeriod %>%
#   ggplot(aes(x = as.factor(Period), y = MonthsPerMonth)) +
#   geom_col(fill = "black") +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_discrete(labels = c("Pre-Covid", "During Covid", "Post-Covid")) +
#   labs(y = "Months of contraceptive coverage dispensed per month") +
#   theme_()

Figure1 <-
  PLOT_ContraceptiveCoverageBox + PLOT_ChangeLines + patchwork::plot_annotation(tag_levels = "A") + plot_layout(widths = c(1, 2), ncol = 2) &
  theme(plot.tag = element_text(face = "bold"))

Figure1

OralContraception %>%
  filter(Type == "COC") %>%
  ggplot(aes(x = Period, y = PaidQuantity, group = BNFItemDescription)) +
  stat_summary(fun = sum, geom = "line")

Summary <- OralContraception %>%
  group_by(Period, BNFItemDescription) %>%
  summarise(MonthsContraception = mean(((PaidQuantity / PackSize) * ProtectionLength
  ) / Months)) %>%
  group_by(Period) %>%
  mutate(Total = sum(MonthsContraception)) %>%
  ungroup() %>%
  mutate(Proportion = MonthsContraception / Total) %>%
  select(c(BNFItemDescription, Period, Proportion))

TypeOC <- OralContraception %>%
  select(BNFItemDescription, Type) %>%
  unique()

SummaryOC <- left_join(Summary, TypeOC, by = "BNFItemDescription")

SummaryOC %>%
  ggplot(aes(x = Period, y = Proportion, fill = BNFItemDescription)) +
  geom_col(position = "stack", colour = "black")

SummaryOC %>%
  group_by(Type, Period) %>%
  summarise(Proportion = sum(Proportion)) %>%
  ggplot(aes(x = Period, y = Proportion, fill = Type)) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = c("grey75", "grey25"),
    labels = c(
      "Combined oral contraceptives",
      "Progesterone-only contraceptives"
    )
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_() +
  scale_x_discrete(labels = c("Pre-Covid", "During Covid", "Post-Covid"))


SummaryOC %>%
  mutate(Proportion = as.numeric(Proportion),
         Period = as.numeric(Period)) %>%
  pivot_wider(names_from = Period, values_from = Proportion) %>%
  group_by(Type) %>%
  mutate(BNFItemDescription = str_remove_all(BNFItemDescription, "_TAB")) %>%
  gt(rowname_col = "BNFItemDescription", groupname_col = "Type") %>%
  summary_rows(
    groups = everything(),
    columns = c(`1`, `2`, `3`),
    fns = list(total = "sum"),
    fmt = ~ fmt_percent(.)
  ) %>%
  grand_summary_rows(
    columns = c(`1`, `2`, `3`),
    fns = list(total = "sum"),
    fmt = ~ fmt_percent(.)
  ) %>%
  fmt_percent(columns = c(`1`, `2`, `3`))


PeriodTotals %>%
  select(c(Type, Period, Proportion)) %>%
  pivot_wider(names_from = Period, values_from = Proportion) %>%
  gt() %>%
  grand_summary_rows(
    columns = c(`1`, `2`, `3`),
    fns = list(total = "sum"),
    fmt = ~ fmt_percent(.)
  ) %>%
  fmt_percent(columns = c(`1`, `2`, `3`))

# HB-wise analysis

CompleteDataset <- CompleteDataset %>%
  mutate(HB = case_when(HealthBoard == "S08000015" ~ "AaA",
                        HealthBoard == "S08000016" ~ "Bor",
                        HealthBoard == "S08000017" ~ "DaG",
                        HealthBoard == "S08000029" ~ "Fif",
                        HealthBoard == "S08000018" ~ "Fif",
                        HealthBoard == "S08000019" ~ "FoV",
                        HealthBoard == "S08000020" ~ "Gra",
                        HealthBoard == "S08000031" ~ "GGC",
                        HealthBoard == "S08000021" ~ "GGC",
                        HealthBoard == "S08000022" ~ "Hig",
                        HealthBoard == "S08000023" ~ "Lan",
                        HealthBoard == "S08000032" ~ "Lan",
                        HealthBoard == "S08000024" ~ "Lot",
                        HealthBoard == "S08000025" ~ "Ork",
                        HealthBoard == "S08000026" ~ "She",
                        HealthBoard == "S08000030" ~ "Tay",
                        HealthBoard == "S08000027" ~ "Tay",
                        HealthBoard == "S08000028" ~ "WIs"))

HBsData <- CompleteDataset %>%
  filter(!str_detect(Type, "EC|Jelly")) %>%
  group_by(HB) %>%
  select(c(Type, MonthsContraception, Period, HB, Months)) %>%
  mutate(MonthsPerMonth = MonthsContraception/Months) %>%
  group_by(Type, Period, HB) %>%
  summarise(Total = sum(MonthsPerMonth)) %>%
  pivot_wider(names_from = Period, values_from = Total) %>%
  rowwise() %>%
  mutate(`2` = `2`/`1`,
         `3` = `3`/`1`,
         `1` = `1`/`1`) %>%
  pivot_longer(cols = c(`1`, `2`, `3`))

HBsData %>%
  filter(HB == "GGC",
         name != 1) %>%
  ggplot(aes(x = name, y = value, fill = Type)) +
  geom_col(colour = "black", position = position_dodge())
  facet_wrap(facets = ~HBsData$Type)

# Totals

  TotalData <- CompleteDataset %>%
    filter(!str_detect(Type, "EC|Jelly")) %>%
    select(c(Type, MonthsContraception, Period, Months)) %>%
    mutate(MonthsPerMonth = MonthsContraception/Months) %>%
    group_by(Type, Period) %>%
    summarise(Total = sum(MonthsPerMonth)) %>%
    pivot_wider(names_from = Period, values_from = Total) %>%
    rowwise() %>%
    mutate(`2` = `2`/`1`,
           `3` = `3`/`1`,
           `1` = `1`/`1`) %>%
    pivot_longer(cols = c(`1`, `2`, `3`))

  TotalData %>%
    filter(name != 1) %>%
    ggplot(aes(x = name, y = value, fill = Type)) +
    geom_col(colour = "black", position = position_dodge()) +
    scale_fill_manual(
      values =
        c(
          "grey90",
          "grey80",
          "grey70",
          "grey60",
          "grey50",
          "grey40",
          "grey30",
          "grey20"
        ),
      guide = guide_legend(nrow = 1)) +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_discrete(labels = c("During Covid", "Post-Covid")) +
    labs(x = "Period", y = "Percentage of Pre-Covid Prescriptions") +
    theme_()



# Modelling take 2

ModelData <- PeriodTotals %>%
  select(-c(Total, TotalMonths)) %>%
  pivot_wider(names_from = Period, values_from = Proportion)




# Modelling
#
# ModelData <- PeriodTotals %>%
#   select(c(Type, Period, Proportion)) %>%
#   mutate(Period = case_when(Period == 1 ~ "A",
#                             Period == 2 ~ "B",
#                             Period == 3 ~ "C")) %>%
#   filter(Type == "COCP")
#
# Model <- lm(data = ModelData, Proportion ~ Period)
#
# summary(Model)
#
# Anova(type = 2, Model)
#
#
#
# Test <- CompleteDataset %>%
#   filter(!str_detect(Type, "EC|Jelly")) %>%
#   filter(Type == "COCP")
#
# Model2 <- lm(data = Test, MonthsContraception ~ Period)
#
# summary(Model2)
# Anova(type = 2, Model2)
# DunnettTest(x = Test$MonthsContraception, g = Test$Period, data = Test)
#
#
# MixedModelData <- CompleteDataset %>%
#   select(c(Period, Type, MonthsContraception, GPPractice, HealthBoard, PaidDateMonth)) %>%
#   filter(!str_detect(Type, "EC|Jelly"))
#
# MixedModel <- lme(MonthsContraception ~ Type + Period, random = ~1|GPPractice, method = "REML", data = MixedModelData)
#
# Model3 <- lm(MonthsContraception ~ Type * Period, data = MixedModelData)
# summary(Model3)
# Anova(type = 2, Model3)
# TukeyHSD()
#
# emmeans()
#
# summary(MixedModel)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# y ~ x + (x|HB/GP)
#
# # Months of contraception of each type is explained by period with random effects of GP and HB
# # Months Contraception Type ~ Period + (Period|HealthBoard/GPPractice)
#
# #MonthsContraception ~ Period + (Period|HealthBoard/GPPractice)
#
# lmer(MonthsContraception ~ Period + (Period|HealthBoard/GPPractice), data = COCP)
