library(tidyverse)    # Data wrangling 
library(ggplot2)      # Data visualization
library(performance)  # Model assessment
library(skimr)        # Exploratory analysis
library(sjPlot)       # Plot and tables for linear models
library(kableExtra)   # Formatting html table

obesity <- read.csv("DAProject1.csv")
obesity$Obese <- as.factor(obesity$Obese)
obesity$Year <- as.factor(obesity$Year)

ggplot(obesity, aes(x = Year, fill = Obese)) + 
  geom_bar(colour = "black", position = "fill") + 
  scale_fill_manual(values = c("Yes" = "lightblue", "No" = "salmon")) + 
  labs(y = "Proportion", 
       title = "Obesity Proportion by Year")

obesity %>%
  group_by(Year) %>%
  summarise(
    Obese = sum(Obese == "Yes"),
    Total = n(),
    Prevalence = Obese / Total
  ) %>%
  kable(digits = 3, caption = "Obesity Prevalence by Year")
