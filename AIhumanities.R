## read scoring .csv files

pcs_hm <- read.csv("/Volumes/SAMSUNG SSD 1TB/Biological Engineering/paper cup_score.csv")
wcs_hm <- read.csv("/Volumes/SAMSUNG SSD 1TB/Biological Engineering/wooden chopsticks_score.csv")
hgs_hm <- read.csv("/Volumes/SAMSUNG SSD 1TB/Biological Engineering/hanger_score.csv")

pcs_4 <- read.csv("/Volumes/SAMSUNG SSD 1TB/Biological Engineering/pcs_4.csv")
wcs_4 <- read.csv("/Volumes/SAMSUNG SSD 1TB/Biological Engineering/wcs_4.csv")
hgs_4 <- read.csv("/Volumes/SAMSUNG SSD 1TB/Biological Engineering/hgs_4.csv")

pcs_3.5 <- read.csv("/Volumes/SAMSUNG SSD 1TB/Biological Engineering/pcs_3.5.csv")
wcs_3.5 <- read.csv("/Volumes/SAMSUNG SSD 1TB/Biological Engineering/wcs_3.5.csv")
hgs_3.5 <- read.csv("/Volumes/SAMSUNG SSD 1TB/Biological Engineering/hgs_3.5.csv")


## 각 사물 별 score 통합

pcs <- data.frame(pcs_hm$score, pcs_4$score, pcs_3.5$score)
wcs <- data.frame(wcs_hm$score, wcs_4$score, wcs_3.5$score)
hgs <- data.frame(hgs_hm$score, hgs_4$score, hgs_3.5$score)


## NA value cleaning

cleaned_pcs <- pcs %>%
  filter_all(all_vars(!is.na(.) & . != 0))

cleaned_wcs <- wcs %>%
  filter_all(all_vars(!is.na(.) & . != 0))

cleaned_hgs <- hgs %>%
  filter_all(all_vars(!is.na(.) & . != 0))


## 각 통합score long format으로 변환

long_pcs <- pivot_longer(pcs,
                         cols = c(pcs_hm.score, pcs_4.score, pcs_3.5.score),
                         names_to = "Group", 
                         values_to = "Score")

long_wcs <- pivot_longer(wcs, 
                         cols = c(wcs_hm.score, wcs_4.score, wcs_3.5.score),
                         names_to = "Group", 
                         values_to = "Score")

long_hgs <- pivot_longer(hgs, 
                         cols = c(hgs_hm.score, hgs_4.score, hgs_3.5.score),
                         names_to = "Group", 
                         values_to = "Score")

## boxplot 그리기

ggplot(cleaned_long_pcs, aes(x = Group, y = Score)) +
  geom_boxplot(aes(fill = Group)) +
  scale_fill_manual(values = c("pcs_hm.score" = "blue", 
                               "pcs_4.score" = "#006400", 
                               "pcs_3.5.score" = "red")) + 
  geom_jitter(width = 0.35, size = 1.2) +
  labs(title = "Comparison of paper cup Scores",
       y = "Score") + 
  theme_light()

ggplot(cleaned_long_wcs, aes(x = Group, y = Score)) +
  geom_boxplot(aes(fill = Group)) +
  scale_fill_manual(values = c("wcs_hm.score" = "blue", 
                               "wcs_4.score" = "#006400", 
                               "wcs_3.5.score" = "red")) + 
  geom_jitter(width = 0.35, size = 1.2) +
  labs(title = "Comparison of wooden chopsticks Scores",
       y = "Score") + 
  theme_light()

ggplot(cleaned_long_hgs, aes(x = Group, y = Score)) +
  geom_boxplot(aes(fill = Group)) +
  scale_fill_manual(values = c("hgs_hm.score" = "blue", 
                               "hgs_4.score" = "#006400", 
                               "hgs_3.5.score" = "red")) + 
  geom_jitter(width = 0.35, size = 1.2) +
  labs(title = "Comparison of hanger Scores",
       y = "Score") + 
  theme_light()


## ANOVA

anova_pcs <- aov(Score ~ Group, data = cleaned_long_pcs)
anova_wcs <- aov(Score ~ Group, data = cleaned_long_wcs)
anova_hgs <- aov(Score ~ Group, data = cleaned_long_hgs)


## summary

summary(cleaned_pcs)
summary(cleaned_wcs)
summary(cleaned_hgs)

summary(anova_pcs)
summary(anova_wcs)
summary(anova_hgs)
sd(cleaned_long_hgs$Score)
