install.packages("gitcreds")
library(gitcreds)
gitcreds_set()


install.packages(c("tidyr", "dplyr", "lubridate", "ggplot2"))
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("scales")


prefix <- "Project_Code/Data/"
data <- read.csv(paste0(prefix, list.files(prefix)))
head(data)


## most collisions occur around 8 am and 6 pm (rush hours)
sam_col <- data |> slice_sample(n = 1e4)
ts <- strptime(sam_col$collision_time, format = "%H:%M:%S")
hrs <- as.numeric(format(round_date(ts, unit = "hour"), format = "%H"))

ggplot(sam, aes(x = hrs)) +
  geom_bar() +
  labs(title = "Time Frequencies of Car Collisions",
       x = "Hour in the Day",
       y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 23, 2))


## no obvious correlations between party ages and people injured/killed
sam_age <- data |>
  drop_na(party_age, number_injured, number_killed) |>
  filter(party_age > 0) |>
  slice_sample(n = 1e4)

ggplot(sam_age, aes(x = party_age, y = number_injured + number_killed)) +
  geom_point() +
  scale_y_continuous(breaks = breaks_pretty())


## Dark - No Street Light = max casualty
sam_lgh <- data |>
  group_by(lighting) |>
  summarize(hurt = mean(number_injured + number_killed, na.rm = T)) |>
  slice_sample(n = 1e4)

ggplot(sam_lgh, aes(lighting, hurt)) +
  geom_col()