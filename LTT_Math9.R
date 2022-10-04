## LTT Math Age 9

## LOAD PACKAGES

library(tidyverse)
library(readxl) 

## Read Excel file
dat <- read_excel("LTT_Math9__1978_2022.xlsx")

## Clean dataset
#as tibble
dat <- as_tibble(dat)

# make Format a factor
dat <- dat %>%
  mutate(Format = factor(Format, levels=c("NotOriginal", "OriginalFormat"), labels=c("Not Original Format", "Original Format")))

# Add columns calculating difference in math scores since 1978
# Long-term Trend national average scores in 1978:
# Average Math LTT in 1978 Age 9: 219
# SD of math9 scores in 1978: 36

dat <- dat %>%
  mutate(Diff78_9M = as.integer(Math9 - 219))

# Add column calculating difference in SD since 1978
dat <- dat %>%
  mutate(Diff78sd_9M = Diff78_9M/SD9)

## Create plot 
# Create plot for 9-year-olds similar to page 94 in Measuring Up
LTTmath9.plot <- ggplot(dat) +
  geom_line(aes(Year, Diff78sd_9M, group = Format)) +
  theme_classic() +
  labs(x = "Year",
       y = "Difference in Standard Deviation") +
  labs(title = "LTT National Average Math Score 9-year-olds", 
       subtitle = "Difference in average scores (in SD) since 1978") +
  scale_x_continuous(limits = c(1978, 2022))
LTTmath9.plot         

## Create plot by gender
dat <- dat %>%
  mutate(Diff78_math_female = FemaleAvg - 219) %>%
  mutate(Diff78_math_male = MaleAvg - 219) %>%
  mutate(Diff78sd_math_female = Diff78_math_female/FemaleSD) %>%
  mutate(Diff78sd_math_male = Diff78_math_male/MaleSD)

LTTmath9_gender.plot <- ggplot(dat) +
  geom_line(aes(Year, Diff78sd_math_female, group = Format, color = "Girls")) +
  geom_line(aes(Year, Diff78sd_math_male, group = Format, color = "Boys")) +
  theme_classic() +
  labs(x = "Year",
       y = "Difference in Standard Deviation") +
  labs(title = "LTT National Average Math Score 9-year-olds", 
       subtitle = "Difference in average scores (in SD) since 1978") +
  scale_x_continuous(limits = c(1978, 2022))
LTTmath9_gender.plot   

## Create plot by race
dat <- dat %>%
  mutate(Diff78_math_white = WhiteAvg - 219) %>%
  mutate(Diff78_math_black = BlackAvg - 219) %>%
  mutate(Diff78_math_hispanic = HispanicAvg - 219) %>%
  mutate(Diff78_math_asian = AsianAvg - 219) %>%
  mutate(Diff78sd_math_white = Diff78_math_white/WhiteSD) %>%
  mutate(Diff78sd_math_black = Diff78_math_black/BlackSD) %>%
  mutate(Diff78sd_math_hispanic = Diff78_math_hispanic/HispanicSD) %>%
  mutate(Diff78sd_math_asian = Diff78_math_asian/AsianSD) 

LTTmath9_race.plot <- ggplot(dat) +
  geom_line(aes(Year, Diff78sd_math_white, group = Format, color = "White")) +
  geom_line(aes(Year, Diff78sd_math_black, group = Format, color = "Black")) +
  geom_line(aes(Year, Diff78sd_math_hispanic, group = Format, color = "Hispanic")) +
  geom_line(aes(Year, Diff78sd_math_asian, group = Format, color = "Asian")) +
  theme_classic() +
  labs(x = "Year",
       y = "Difference in Standard Deviation") +
  labs(title = "LTT National Average Math Score 9-year-olds", 
       subtitle = "Difference in average scores (in SD) since 1978") +
  scale_x_continuous(limits = c(1978, 2022))
LTTmath9_race.plot    


## Normal Curves 2022 LTT Math
ggplot(data = data.frame(x = c(100, 360)), aes(x)) +
  stat_function(fun = dnorm, n = 590, args = list(mean = 244, sd = 37), color = "Pink") + 
  stat_function(fun = dnorm, n = 140, args = list(mean = 212, sd = 42), color = "Black") +
  stat_function(fun = dnorm, n = 190, args = list(mean = 223, sd = 40), color = "Brown") + 
  stat_function(fun = dnorm, n = 60, args = list(mean = 257, sd = 39), color = "Dark Green") +
  scale_y_continuous(breaks = NULL) +
  ylab("") +
  ggtitle("LTT estimated math scores for 9-year-olds in 2022")
