#############################################################################
###README###
#############################################################################

#Author information#
# Elliot Johnson-Hall
# Orcid: 0009-0003-5105-034X
# elliot@elliotjh.com

#############################################################################
###SETUP###
#############################################################################

#Install and load required libraries
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
  "gt",
  "vroom",
  "ckanr",
  "ggpattern"
)

#ggplot2 Theme
theme_base <- function() {
  theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      plot.tag = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 16
      ),
      axis.line = element_line(
        colour = "black",
        linewidth = 0.5,
        linetype = "solid",
        lineend = "butt"
      ),
      axis.ticks = element_line(colour = "black"),
      axis.ticks.length = unit(1, "mm")
    )
}
#############################################################################
###LOAD DATASET###
#############################################################################

#Load Data.RDS
if (file.exists(here("Data", "Data.RDS"))) {
  CompleteDataset <- read_rds(here("Data", "Data.RDS"))
} else {
  if (exists("CompleteDataset") == FALSE) {
    CompleteDataset <- NULL
  } else {
    CompleteDataset <- CompleteDataset
  }
}

###TO ACCESS NHS SCOTLAND OPEN DATA API UNCOMMENT THE FOLLOWING LINES###
###THIS WILL TAKE A LONG TIME TO LOAD!###

#Connect to NHS Scotland Open Data API and import data on contraception prescriptions dispensed
# BaseURL <-  "https://www.opendata.nhs.scot/"
#
# ResourceIDs <-
#   package_show(id = "84393984-14e9-4b0d-a797-b288db64d088",
#                url = BaseURL,
#                as = "table")$resources %>% select(id)
#
# NumberOfRecordsFound <- nrow(ResourceIDs)
# NumberOfRecordsRetrieved <-
#   length(unique(CompleteDataset$RecordNumber))
#
# if (NumberOfRecordsFound > NumberOfRecordsRetrieved) {
#   ChoiceToUpdate <-
#     menu(
#       title = glue(
#         "{NumberOfRecordsFound - NumberOfRecordsRetrieved} new records found. Would you like to update the data?"
#       ),
#       choices = c("Yes", "No")
#     )
#
#   if (ChoiceToUpdate == 1L) {
#     message("Updating data. This may take some time.")
#     for (Record in 1:NumberOfRecordsFound) {
#       message(glue("Retrieving record {Record} of {NumberOfRecordsFound}."))
#       ResourceID <- ResourceIDs[Record,]
#       SQL <-
#         glue(
#           'SELECT * from "{ResourceID}" WHERE ',
#           '"BNFItemCode"',
#           " LIKE '07030%' OR ",
#           '"BNFItemCode"',
#           " LIKE '21040%'"
#         )
#       TempResult <- ds_search_sql(SQL, url = BaseURL, as = "json")
#       DataList <- fromJSON(TempResult)
#       assign(glue("DF{Record}"), DataList$result$records)
#       assign(glue("DF{Record}"),
#              get(glue("DF{Record}")) %>% mutate(RecordNumber = glue("{Record}")))
#       message(glue(
#         "Success! {round(
#           (Record / NumberOfRecordsFound * 100), 2)
#           }% complete."
#       ))
#     }
#     DataFrameList <- mget(ls(pattern = "DF"))
#     if (is.null(CompleteDataset) == FALSE) {
#       DFTemp <- bind_rows(DataFrameList)
#       CompleteDataset <- left_join(CompleteDataset, DFTemp)
#       remove(list = ls(pattern = "DF"))
#     } else {
#       CompleteDataset <- bind_rows(DataFrameList)
#       remove(list = ls(pattern = "DF"))
#     }
#   }
#   else {
#     message("Exited successfully.")
#   }
# }
#
# #Write Dataset TO DISK Function
# WriteOut <- function(Dataframe) {
#   if (dir.exists(here("Data")) == TRUE) {
#     unlink(here("Data"), recursive = TRUE)
#     dir.create(here("Data"))
#     write_rds(CompleteDataset, file = here("Data", "Data.RDS"))
#   } else {
#     dir.create(here("Data"))
#     write_rds(CompleteDataset, file = here("Data", "Data.RDS"))
#   }
# }
#
# #Write out Data.RDS
# CompleteDataset <- CompleteDataset %>%
#   select(c(NumberOfPaidItems:PaidDateMonth, RecordNumber))
# WriteOut(CompleteDataset)

#############################################################################
###TABLE 1 - BNF ITEM CODE EXAMPLES###
#############################################################################

#Table 1 - BNF Item Codes
Table1Data <-
  tibble(
    "Truncated BNF Item Code" = c(
      "0703021*",
      "0703010*",
      "0703022M*",
      "0703022N*",
      "0703023*",
      "21040*",
      "0703022P*",
      "0703050*",
      "0703010E0BG*",
      "0703011*"
    ),
    "Category" = c(
      "POP",
      "COCP",
      "Injection",
      "Injection",
      "IUS",
      "IUD",
      "Implant",
      "EC",
      "Patch",
      "Ring"
    ),
    "Example BNF Item Description" = c(
      "Desogestrel  Tablet 75mcg",
      "Rigevidon  Tablet",
      "Depo-Provera Injection 150mg/ml 1ml Pre-filled Syringes",
      "Noristerat Injection 200mg/ml 1ml Ampoules",
      "Mirena Intra-uterine System",
      "T-Safe 380A QL Intra-uterine Contraceptive Device",
      "Nexplanon Implant 68mg",
      "Upostelle  Tablet 1500mcg",
      "Evra Transdermal Patch",
      "NuvaRing 0.12mg/0.015mg per day Vaginal Delivery System"
    )
  )
Table1 <- gt(Table1Data) %>%
  tab_header(
    title = md(
      "**Table 1** Truncated BNF item codes used during data extraction and example medicines in these categories."
    )
  )
Table1

#############################################################################
###DATA WRANGLING###
#############################################################################
# Periods
# Months between dates
# Jan 2016 to Apr 2020
Period1 <- 51
# Apr 2020 to Apr 2022
Period2 <- 24
# May 2022 to Jan 2023
Period3 <- 8
TotalMonths  <- sum(Period1 + Period2 + Period3)

#Wrangle CompleteDataset
CompleteDataset <- CompleteDataset %>%
  filter(between(ym(PaidDateMonth), ym("201601"), ym("202301"))) %>%
  mutate(
    PaidDateMonth = ym(PaidDateMonth),
    PaidQuantity = as.integer(PaidQuantity),
    # 1 = Pre-COVID-19; 2 = During COVID-19; 3 = Post-COVID-19
    Period = as.factor(
      case_when(
        PaidDateMonth < dmy("01/04/2020") ~ "1",
        between(PaidDateMonth, dmy("01/04/2020"), dmy("30/04/2022")) ~ "2",
        PaidDateMonth > dmy("30/04/2022") ~ "3"
      )
    ),
    TotalMonths = case_when(Period == 1 ~ 31119114,
                            Period == 2 ~ 12500003,
                            Period == 3 ~ 5520534),
    Months = case_when(Period == 1 ~ Period1,
                       Period == 2 ~ Period2,
                       Period == 3 ~ Period3),
    Type = as.factor(
      case_when(
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
      )
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
    MonthsContraception = (PaidQuantity / PackSize) * ProtectionLength,
    Group = case_when(
      str_detect(Type, "POP|COCP") ~ "Oral",
      str_detect(Type, "IUS|IUD|Injection|Implant") ~ "LARC",
      str_detect(Type, "EC") ~ "EC",
      str_detect(Type, "POP|COCP|IUS|IUD|Injection|Implant|EC", negate = T) ~ "Other"
    ),
    ECType = case_when(
      str_detect(BNFItemCode, "0703050B") ~ "ULI",
      str_detect(BNFItemCode, "0703050A") ~ "LEV"
    ),
    Lockdown = as.factor(
      case_when(
        between(PaidDateMonth, dmy("24/03/2020"), dmy("29/05/2020")) ~ "Y",
        between(PaidDateMonth, dmy("05/01/2021"), dmy("26/04/2021")) ~ "Y",
        .default = "N"
      )
    ),
    Period2 = factor(
      case_when(
        PaidDateMonth < dmy("01/04/2020") & Lockdown == "N" ~ "1",
        between(PaidDateMonth, dmy("01/04/2020"), dmy("30/04/2022")) &
          Lockdown == "N" ~ "2",
        between(PaidDateMonth, dmy("01/04/2020"), dmy("30/04/2022")) &
          Lockdown == "Y" ~ "4",
        PaidDateMonth > dmy("30/04/2022") & Lockdown == "N" ~ "3"
      )
    )
  )
CompleteDataset$MonthsContraception <-
  CompleteDataset$MonthsContraception %>% replace_na(0)
CompleteDataset <- CompleteDataset %>%
  filter(!Type == "Jelly")
CompleteDataset$Period2 <-
  fct_relevel(CompleteDataset$Period2, "1", "4", "2", "3")

#############################################################################
###FIGURES###
#############################################################################

#Figure1
Figure1Data <- CompleteDataset %>%
  filter(!Type == "EC") %>%
  group_by(Period2, Type) %>%
  reframe(Sum = sum(MonthsContraception) / Months) %>%
  unique()
# slice_sample(prop = 0.001) %>%
Figure1 <- Figure1Data %>%
  ggplot(aes(
    x = fct_reorder(Type,Sum),
    y = Sum,
    fill = Period2,
    pattern = Period2,
    pattern_angle = Period2
  )) +
  geom_bar_pattern(
    width = 0.5,
    position = position_dodge(width = 0.8),
    stat = "identity",
    colour = "black",
    pattern_fill = "black",
    pattern_spacing = 0.01,
    pattern_alpha = 0.5) +
  theme_base() +
  scale_fill_manual(
    #values = c("grey80", "grey60", "grey40", "grey20"), #MONO
    values = c("#d9f0a3",
               "#addd8e",
               "#78c679",
               "#005a32"),
    labels = c("Pre-COVID-19", "Lockdown", "Restrictions", "Post-COVID-19"),
    name = "Period"
  ) +
  labs(x = "Type", y = "Months of contraception \n dispensed per month") +
  scale_y_continuous(labels = scales::comma) +
  scale_pattern_manual(values = c("stripe", "circle", "stripe", "none"), guide = "none") +
  scale_pattern_angle_manual(values = c(45, 0, -45, 0), guide = "none") +
  guides(fill = guide_legend(override.aes = list(pattern = c("stripe", "circle", "stripe", "none")))) +
  coord_flip() +
  theme(panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
Figure1

#Figure2
PLOT_COCPvsPOP <- CompleteDataset %>%
  filter(Type %in% c("COCP", "POP")) %>%
  group_by(PaidDateMonth, Type, Period2) %>%
  reframe(Sum = sum(MonthsContraception)) %>%
  mutate(Period2 = fct_relevel(Period2, "3", "2", "4", "1")) %>%
  ggplot(aes(y = Sum, x = Period2, fill = Type, pattern = Type)) +
  geom_violin_pattern(colour = "black",
                      pattern_fill = "black",
                      pattern_angle = 45,
                      pattern_density = 0.05,
                      pattern_spacing = 0.025,
                      pattern_alpha = 0.5
  ) +
  geom_boxplot(width=0.1, position = position_dodge(width = 0.9), show.legend = F, aes(colour = Type), outlier.colour = "black") +
  # geom_boxplot(outlier.shape = NA) +
  # geom_point(
  #   position = position_jitterdodge(jitter.height = 0, jitter.width = 0.5),
  #   aes(group = Type, shape = Type),
  #   colour = "black"
  # ) +
  theme_base() +
  labs(y = "Months of contraception dispensed per month",
       x = "Period") +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(from = 100000, to = 200000, by = 20000)) +
  scale_shape_manual(
    values = c(21, 24),
    name = "Type",
    labels = c(
      "Combined oral contraceptives",
      "Progesterone-only oral contraceptives"
    )
  ) +
  scale_x_discrete(
    labels = rev(c("Pre-COVID-19", "Lockdown", "Restrictions", "Post-COVID-19"))
  ) +
  scale_fill_manual(
    name = "Type",
    #values = c("white", "grey"), #MONO
    values = c("#ffd700", "#0000ff"), #COLOUR
    labels = c(
      "Combined oral contraceptives",
      "Progesterone-only oral contraceptives"
    )) +
  scale_colour_manual(values = c("black", "white")) + #ON FOR COLOUR
  scale_pattern_manual(values = c("stripe", "none"), guide = "none") +
  guides(fill = guide_legend(override.aes = list(pattern = c("stripe", "none")))) +
  coord_flip() +
  theme(panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

Figure2 <- PLOT_COCPvsPOP
Figure2

#Figure3
LARC_LinePlot <- CompleteDataset %>%
  filter(!Type %in% c("COCP", "POP", "Patch", "Ring", "Jelly", "EC")) %>%
  select(c(PaidDateMonth, Period, Type, MonthsContraception)) %>%
  group_by(PaidDateMonth, Type) %>%
  reframe(Sum = sum(MonthsContraception)) %>%
  ggplot(aes(y = Sum, x = PaidDateMonth, linetype = Type, colour = Type)) +
  annotate("rect",
           xmin = c(
             dmy("01/01/2016"),
             dmy("24/03/2020"),
             dmy("30/05/2020"),
             dmy("05/01/2021"),
             dmy("27/04/2021"),
             dmy("01/05/2022")),
           xmax = c(
             dmy("23/03/2020"),
             dmy("30/05/2020"),
             dmy("05/01/2021"),
             dmy("27/04/2021"),
             dmy("30/04/2022"),
             dmy("01/01/2023")),
           ymin = rep(-4000, times = 6),
           ymax = rep(0, times = 6),
           alpha = 0.25,
           fill = c("#33BBEE", "#CC3311", "#EE7733",
                    "#CC3311", "#EE7733", "#33BBEE")
  ) +
  annotate("text",
           y = rep(-2000, times = 6),
           x = c(dmy("10/02/2018"),
                 dmy("26/04/2020"),
                 dmy("17/09/2020"),
                 dmy("01/03/2021"),
                 dmy("12/10/2021"),
                 dmy("16/08/2022")),
           label = c("Pre-COVID-19",
                     "L",
                     "R",
                     "L",
                     "R",
                     "Post-COVID-19")) +
  geom_line(linewidth = 1) +
  theme_base() +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(from = 0, to = 800000, by = 10000)) +
  labs(y = "Months of contraception dispensed per month", x = "Year") +
  scale_linetype_manual(values = c(1, 2, 3, 5)) +
  scale_colour_manual(values = c("black", "#e59c00", "#56b2dd", "#009e73")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(panel.grid.minor.x = element_blank()) +
  coord_cartesian(xlim = c(dmy("01/01/2016"), dmy("01/01/2023")),
                  ylim = c(0, 80000),
                  clip = 'off')

# LARC_LinePlot <- LARC_LinePlot + annotate(
#   "rect",
#   xmin = c(dmy("24/03/2020"), dmy("05/01/2021")),
#   xmax = c(dmy("29/05/2020"), dmy("26/04/2021")),
#   ymin = c(-4000,-4000),
#   ymax = c(80000, 80000),
#   alpha = .2
# ) +
# annotate("rect",
#   xmin = c(dmy("01/01/2016"), dmy("24/03/2020"),
#            dmy("30/05/2020"), dmy("05/01/2021"),
#            dmy("27/04/2021"), dmy("01/05/2022")),
#   xmax = c(dmy("23/03/2020"), dmy("29/05/2020"),
#            dmy("26/04/2021"), dmy("30/04/2022"),
#            dmy("04/01/2021"), dmy("01/01/2023")),
#   ymin = rep(-4000, times = 6),
#   ymax = rep(c(0, 80000, 0), times = 2),
#   alpha = 0.1,
#   fill = c("#33BBEE", "#CC3311", "#EE7733", "#CC3311", "#EE7733", "#33BBEE")
#   )

# CompleteDataset %>%
#   filter(!Type %in% c("COCP", "POP", "Patch", "Ring", "Jelly", "EC")) %>%
#   select(c(PaidDateMonth,Period2, Type, MonthsContraception)) %>%
#   group_by(PaidDateMonth, Period2, Type) %>%
#   reframe(Sum = sum(MonthsContraception)) %>%
#   ggplot(aes(y = Sum, x = Period2)) +
#   geom_violin_pattern(colour = "black",
#                       aes(fill = Type),
#                       pattern_fill = "black",
#                       pattern_angle = 45,
#                       pattern_density = 0.05,
#                       pattern_spacing = 0.025,
#                       pattern_alpha = 0.5
#   ) +
#   geom_boxplot(width=0.08, position = position_dodge(width = 0.9), show.legend = F, aes(fill = Type), outlier.colour = "black") +
#   theme_base() +
#   scale_y_continuous(labels = scales::comma,
#                      breaks = seq(from = 0, to = 800000, by = 10000)) +
#   labs(y = "Months of contraception dispensed per month", x = "Year") +
#   facet_wrap(~Type)

Figure3 <- LARC_LinePlot
Figure3


#Figure4
Figure4 <- CompleteDataset %>%
  filter(Type == "EC") %>%
  group_by(Period2, ECType) %>%
  reframe(Sum = sum(PaidQuantity) / Months) %>%
  unique() %>%
  group_by(Period2) %>%
  pivot_wider(names_from = ECType, values_from = Sum) %>%
  mutate(TOT = sum(LEV + ULI)) %>%
  pivot_longer(cols = LEV:TOT,
               names_to = "Type",
               values_to = "Sum") %>%
  mutate(Type = factor(Type, levels = c("TOT", "LEV", "ULI"))) %>%
  mutate(Period2 = fct_relevel(Period2, "3", "2", "4", "1")) %>%
  ggplot(aes(
    x = Period2,
    y = Sum,
    fill = Type,
    group = Type,
    pattern = Type
  )) +
  geom_col_pattern(
    position = position_dodge(width = 0.9),
    width = 0.8,
    colour = "black",
    pattern_fill = "black",
    pattern_spacing = 0.01,
    pattern_alpha = 0.5
  ) +
  labs(x = "Period", y = "EC Prescriptions Paid Quantity \n Dispensed per Month", fill = "") +
  theme_base() +
  scale_fill_manual(
    #values = c("grey80", "grey60", "grey40"), #MONO
    values = c("purple", "orange", "black"),
    labels = c("Levonorgestrel", "Ulipristal acetate", "Total"),
    breaks = c("LEV", "ULI", "TOT")
  ) +
  scale_pattern_manual(values = c("none", "circle", "stripe"), guide = "none") +
  scale_pattern_angle_manual(values = c(45, 0, -45, 0), guide = "none") +
  guides(fill = guide_legend(override.aes = list(pattern = c("none", "circle", "stripe")))) +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 10000, by = 1000)) +
  scale_x_discrete(labels = rev(c("Pre-COVID-19", "Lockdown", "Restrictions", "Post-COVID-19"))) +
  coord_flip() +
  theme(panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

Figure4

#############################################################################
###SUPPLEMENTARY INFORMATION###
#############################################################################

CompareTimesTypes <- function(type) {
  Lockdown <- CompleteDataset %>%
    filter(Lockdown == "Y",
           Type == glue("{type}")) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()
  PostLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "3",
           Type == glue("{type}")) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()
  PreLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "1",
           Type == glue("{type}")) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()
  PeriLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "2",
           Type == glue("{type}")) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()

  return(unlist(
    c(
      Lockdown / PreLockdown,
      PeriLockdown / PreLockdown,
      PostLockdown / PreLockdown
    )
  ))
}


CompareTimesECTypes <- function(ECtype) {
  Lockdown <- CompleteDataset %>%
    filter(Lockdown == "Y",
           ECType == glue("{ECtype}")) %>%
    reframe(Sum = sum(PaidQuantity) / Months) %>%
    unique()
  PostLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "3",
           ECType == glue("{ECtype}")) %>%
    reframe(Sum = sum(PaidQuantity) / Months) %>%
    unique()
  PreLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "1",
           ECType == glue("{ECtype}")) %>%
    reframe(Sum = sum(PaidQuantity) / Months) %>%
    unique()
  PeriLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "2",
           ECType == glue("{ECtype}")) %>%
    reframe(Sum = sum(PaidQuantity) / Months) %>%
    unique()

  return(unlist(
    c(
      Lockdown / PreLockdown,
      PeriLockdown / PreLockdown,
      PostLockdown / PreLockdown
    )
  ))
}



CompareTimesEC <- function(group) {
  Lockdown <- CompleteDataset %>%
    filter(Lockdown == "Y",
           Group == glue("{group}")) %>%
    reframe(Sum = sum(PaidQuantity) / Months) %>%
    unique()
  PostLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "3",
           Group == glue("{group}")) %>%
    reframe(Sum = sum(PaidQuantity) / Months) %>%
    unique()
  PreLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "1",
           Group == glue("{group}")) %>%
    reframe(Sum = sum(PaidQuantity) / Months) %>%
    unique()
  PeriLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "2",
           Group == glue("{group}")) %>%
    reframe(Sum = sum(PaidQuantity) / Months) %>%
    unique()

  return(unlist(
    c(
      Lockdown / PreLockdown,
      PeriLockdown / PreLockdown,
      PostLockdown / PreLockdown
    )
  ))
}


#Table3 Data
CompareTimes <- function(group) {
  Lockdown <- CompleteDataset %>%
    filter(Lockdown == "Y",
           Group == glue("{group}")) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()
  PostLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "3",
           Group == glue("{group}")) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()
  PreLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "1",
           Group == glue("{group}")) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()
  PeriLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "2",
           Group == glue("{group}")) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()

  return(unlist(
    c(
      Lockdown / PreLockdown,
      PeriLockdown / PreLockdown,
      PostLockdown / PreLockdown
    )
  ))
}


#Table3
Table3 <-
  tibble(
    "Timeframes" = c("Lockdown", "Peri-Lockdown", "Post-Lockdown"),
    "All LARC" = CompareTimes("LARC"),
    "IUS" = CompareTimesTypes("IUS"),
    "IUD" = CompareTimesTypes("IUD"),
    "Implant" = CompareTimesTypes("Implant"),
    "Injection" = CompareTimesTypes("Injection"),
    "All Oral" = CompareTimes("Oral"),
    "COCP" = CompareTimesTypes("COCP"),
    "POP" = CompareTimesTypes("POP"),
    "All Other" = CompareTimes("Other"),
    "Patch" = CompareTimesTypes("Patch"),
    "Ring" = CompareTimesTypes("Ring"),
    "All EC" = CompareTimesEC("EC"),
    "Uli" = CompareTimesECTypes("ULI"),
    "Levo" = CompareTimesECTypes("LEV")
  ) %>%
  gt() %>%
  fmt_percent(decimals = 2) %>%
  tab_spanner(label = "LARCs", columns = c("All LARC":"Injection")) %>%
  tab_spanner(label = "Oral", columns = c("All Oral":"POP")) %>%
  tab_spanner(label = "Other", columns = c("All Other":"Ring")) %>%
  tab_spanner(label = "EC", columns = c("All EC":"Levo")) %>%
  tab_style(locations = cells_body(columns = c("All LARC", "All Other", "All Oral", "All EC")),
            style = list(cell_text(weight = "bold"), cell_fill(color = "grey80")))
Table3


## Oral contraception

CompleteDataset %>%
  select(c("Group", "MonthsContraception")) %>%
  group_by(Group) %>%
  filter(Group == "Oral") %>%
  summarise(mean = sum(MonthsContraception) / TotalMonths)

CompleteDataset %>%
  select(c("Group", "PaidQuantity")) %>%
  group_by(Group) %>%
  filter(Group == "Oral") %>%
  summarise(mean = sum(PaidQuantity) / TotalMonths)


#Months contraception dispensed by Type of contraception
CompleteDataset %>%
  select(c("Group", "MonthsContraception")) %>%
  group_by(Group) %>%
  reframe(Sum = sum(MonthsContraception) / TotalMonths) %>%
  ggplot(aes(
    x = fct_reorder(Group,-Sum),
    y = Sum,
    fill = Group
  )) +
  geom_bar(
    position = position_dodge(width = 0.9),
    stat = "identity",
    width = 0.7,
    colour = "black"
  ) +
  theme_base() +
  scale_fill_manual(values = c("grey80", "grey60", "grey40", "grey20")) +
  scale_y_continuous(labels = scales::comma)




#Mean contraception dispensed per period
CompleteDataset %>%
  filter(Type %in% c("COCP", "POP")) %>%
  select(c(PaidDateMonth, Period, Type, MonthsContraception)) %>%
  group_by(PaidDateMonth, Type) %>%
  reframe(Sum = sum(MonthsContraception)) %>%
  mutate(Period = as.factor(
    case_when(
      PaidDateMonth < dmy("01/04/2020") ~ "1",
      between(PaidDateMonth, dmy("01/04/2020"), dmy("30/04/2022")) ~ "2",
      PaidDateMonth > dmy("30/04/2022") ~ "3"
    )
  )) %>%
  group_by(Period, Type) %>%
  summarise(mean(Sum))


CompareType <- "Injection"

During <- CompleteDataset %>%
  filter(Type == CompareType, Lockdown == "Y") %>%
  summarise(mean = mean(MonthsContraception))

Pre <- CompleteDataset %>%
  filter(Type == CompareType, Lockdown == "N", Period == "1") %>%
  summarise(mean = mean(MonthsContraception))

During / Pre



BNFDesc <- "DEPO"

During <- CompleteDataset %>%
  filter(str_detect(BNFItemDescription, BNFDesc), Lockdown == "Y") %>%
  summarise(sum = sum(MonthsContraception) / Months)

Pre <- CompleteDataset %>%
  filter(str_detect(BNFItemDescription, BNFDesc),
         Lockdown == "N",
         Period == "1") %>%
  summarise(sum = sum(MonthsContraception) / Months)

During / Pre


CompareTimes <- function(BNFDesc) {
  Lockdown <- CompleteDataset %>%
    filter(Lockdown == "Y",
           str_detect(BNFItemDescription, glue("{BNFDesc}"))) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()
  PostLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "3",
           str_detect(BNFItemDescription, glue("{BNFDesc}"))) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()
  PreLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "1",
           str_detect(BNFItemDescription, glue("{BNFDesc}"))) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()
  PeriLockdown <- CompleteDataset %>%
    filter(Lockdown == "N",
           Period == "2",
           str_detect(BNFItemDescription, glue("{BNFDesc}"))) %>%
    reframe(Sum = sum(MonthsContraception) / Months) %>%
    unique()

  return(unlist(
    c(
      Lockdown / PreLockdown,
      PeriLockdown / PreLockdown,
      PostLockdown / PreLockdown
    )
  ))
}

CompareTimes("SAYANA")

CompareTimes("DEPO")


g <- CompleteDataset %>%
  filter(Period2 == "3",
         str_detect(BNFItemDescription, "SAYANA")) %>%
  reframe(Sum = sum(MonthsContraception) / Months) %>%
  unique() %>%
  deframe()


tibble(c(a, b, c, d, 0, b/a, c/a, d/a),
       c(e, f, g, h, 0, f/e, g/e, h/e))










#############################################################################
###ENDS###
#############################################################################
