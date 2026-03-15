library(tidyverse)
library(broom)
library(knitr)
library(kableExtra)
library(car)
library(ggplot2)

# 1. Read and prepare data
dat <- read.csv("/Users/daniel/Desktop/DAProject1.csv")

dat <- dat %>%
  mutate(
    Obese = factor(Obese, levels = c("No", "Yes")),
    Year = factor(Year),
    Sex = factor(Sex),
    Education = factor(Education),
    Veg = factor(Veg),
    Fruit = factor(Fruit)
  )

str(dat)

# 2. Descriptive prevalence by year
prev_year <- dat %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    obese_n = sum(Obese == "Yes"),
    prevalence = mean(Obese == "Yes")
  ) %>%
  mutate(prevalence_pct = 100 * prevalence)

kable(prev_year, digits = 3, caption = "Observed obesity prevalence by survey year.") %>%
  kable_styling(full_width = FALSE)

# Optional plot for report
ggplot(prev_year, aes(x = Year, y = prevalence_pct, group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Observed obesity prevalence by year",
    x = "Survey year",
    y = "Obesity prevalence (%)"
  ) +
  theme_minimal()

# 3. Formal analysis: Logistic regression with Year as a categorical predictor
mod_year_cat <- glm(Obese ~ Year, data = dat, family = binomial)

summary(mod_year_cat)

anova(mod_year_cat, test = "Chisq")

# Odds ratios and 95% CIs
or_year_cat <- tidy(mod_year_cat, exponentiate = TRUE, conf.int = TRUE)

kable(or_year_cat, digits = 3,
      caption = "Logistic regression for obesity with year as a categorical predictor (ORs relative to 2008).") %>%
  kable_styling(full_width = FALSE)

# Likelihood ratio test
mod_null <- glm(Obese ~ 1, data = dat, family = binomial)
anova(mod_null, mod_year_cat, test = "Chisq")

# 4. Formal analysis 2:
#    Test for linear trend over time
dat <- dat %>%
  mutate(Year_num = as.numeric(as.character(Year)))

mod_year_trend <- glm(Obese ~ Year_num, data = dat, family = binomial)

summary(mod_year_trend)

or_year_trend <- tidy(mod_year_trend, exponentiate = TRUE, conf.int = TRUE)

kable(or_year_trend, digits = 3,
      caption = "Logistic regression testing linear yearly trend in obesity prevalence.") %>%
  kable_styling(full_width = FALSE)

mod_year_adj <- glm(Obese ~ Year + Age + Sex + Education + Veg + Fruit,
                    data = dat, family = binomial)

summary(mod_year_adj)

# Overall test for Year in adjusted model
Anova(mod_year_adj, type = "II", test.statistic = "LR")

or_year_adj <- tidy(mod_year_adj, exponentiate = TRUE, conf.int = TRUE)

kable(or_year_adj, digits = 3,
      caption = "Adjusted logistic regression model for obesity.") %>%
  kable_styling(full_width = FALSE)

# 6. Predicted probabilities by year from unadjusted categorical model
newdat <- data.frame(Year = levels(dat$Year))
newdat$pred <- predict(mod_year_cat, newdata = newdat, type = "response")

kable(newdat, digits = 3,
      caption = "Predicted probability of obesity by year from the unadjusted model.") %>%
  kable_styling(full_width = FALSE)

ggplot(newdat, aes(x = Year, y = 100 * pred, group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Model-based estimated obesity prevalence by year",
    x = "Survey year",
    y = "Predicted obesity prevalence (%)"
  ) +
  theme_minimal()

