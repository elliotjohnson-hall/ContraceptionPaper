

CompleteDataset <- CompleteDataset %>%
  mutate(PaidDateMonth = ym(PaidDateMonth),
         PaidQuantity = as.integer(PaidQuantity),
         HealthBoard = as.factor(HealthBoard),
         # 1 = Pre-Covid; 2 = During Covid; 3 = Post-Covid
         period = as.factor(case_when(
           PaidDateMonth < dmy("22/03/2020") ~ "1",
           between(PaidDateMonth, dmy("23/03/2020"), dmy("21/03/2022")) ~ "2",
           PaidDateMonth > dmy("22/03/2022") ~ "3")))

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
  filter(str_detect(BNFItemCode, "0703050"))

# Oral contraception
OralContraception <- CompleteDataset %>%
  filter(str_detect(BNFItemCode, "0703010F")) %>%
  rbind(., CompleteDataset %>% filter(str_detect(BNFItemCode, "0703021")))

# Oral contraception seperated into combined and progesterone-only
COC <- CompleteDataset %>%
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

# Plotting
theme_elliot <- function() {
  theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      axis.title = element_text(face = "bold"),
      axis.line = element_line(
        colour = "black",
        linewidth = 0.5,
        linetype = "solid",
        lineend = "butt"),
      axis.ticks = element_line(colour = "black"),
      axis.ticks.length = unit(1, "mm"))
}



means <- function(dataset) {
  mean_pre_vector <- dataset %>%
    filter(PaidDateMonth < dmy("01/03/2020")) %>%
    group_by(PaidDateMonth) %>%
    summarise(sum(PaidQuantity))

  mean_during_vector <- dataset %>%
    filter(between(PaidDateMonth, dmy("01/03/2020"), dmy("01/04/2022"))) %>%
    group_by(PaidDateMonth) %>%
    summarise(sum(PaidQuantity))

  mean_post_vector <- dataset %>%
    filter(PaidDateMonth > dmy("01/04/2022")) %>%
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
}


Injection2 %>%
  group_by(PaidDateMonth) %>%
  ggplot(aes(x = PaidDateMonth, y = PaidQuantity)) +
  stat_summary(fun = sum, geom = "point", aes(shape = period)) +
  annotate_lockdowns() +
  theme_elliot() +
  annotate(geom = "segment",
         x = c(dmy("01/10/2015"), dmy("23/03/2020"), dmy("22/03/2022")),
         xend = c(dmy("22/03/2020"), dmy("21/03/2022"), dmy("01/01/2023")),
         y = c(mean_pre, mean_during, mean_post),
         yend = c(mean_pre, mean_during, mean_post)) +
  annotate(geom = "segment",
           x = c(dmy("01/10/2015"), dmy("23/03/2020"), dmy("22/03/2022")),
           xend = c(dmy("22/03/2020"), dmy("21/03/2022"), dmy("01/01/2023")),
           y = c((mean_pre + sd_pre), (mean_during + sd_during), (mean_post + sd_post)),
           yend = c((mean_pre + sd_pre), (mean_during + sd_during), (mean_post + sd_post)),
           linetype = "dashed") +
annotate(geom = "segment",
         x = c(dmy("01/10/2015"), dmy("23/03/2020"), dmy("22/03/2022")),
         xend = c(dmy("22/03/2020"), dmy("21/03/2022"), dmy("01/01/2023")),
         y = c((mean_pre - sd_pre), (mean_during - sd_during), (mean_post - sd_post)),
         yend = c((mean_pre - sd_pre), (mean_during - sd_during), (mean_post - sd_post)),
         linetype = "dashed")


Injection2 <- Injection %>%
  mutate(period = as.factor(case_when(PaidDateMonth < dmy("22/03/2020") ~ "pre",
                            between(PaidDateMonth, dmy("23/03/2020"), dmy("21/03/2022")) ~ "during", PaidDateMonth > dmy("22/03/2022") ~ "post")))

means(Injection)

t.test(mean_pre_vector, mean_during_vector)
t.test(mean_post_vector, mean_during_vector)
t.test(mean_pre_vector, mean_post_vector)

lm(PaidQuantity ~ period, data = Injection2)
a <- anova(lm(PaidQuantity ~ period, data = Injection2))
summary(a)
library(DescTools)

DunnettTest(Injection2$PaidQuantity, Injection2$period, control = "pre")

nrows <- nrow(Injection) + nrow(COC) + nrow(EmergencyContraception) + nrow(Implant) + nrow(IUD) + nrow(IUS) + nrow(Patch) + nrow(POP) + nrow(Ring) + nrow(Jelly)

# Test if number of observations in the dataset match the sum of the number of observations in each subgroup

nrows == nrow(CompleteDataset)
