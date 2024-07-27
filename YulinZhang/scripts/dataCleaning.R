install.packages(c("sf", "tidyr", "dplyr", "lubridate", "ggplot2", "caTools"))
library("sf")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("scales")
library("caTools")


## Get the dataset
prefix <- "Project_Code/Data/"
data <- read.csv(paste0(prefix, list.files(prefix)))
ts <- strptime(data$collision_time, format = "%H:%M:%S")
data$hrs <- as.numeric(format(round_date(ts, unit = "hour"), format = "%H"))
head(data)

## Graph the number of collisions every hour in the day
## Observation: Most collisions occur around 8 am and 6 pm (rush hours)
sam_col <- data |> slice_sample(n = 1e4)

ggplot(sam_col, aes(x = hrs)) +
  geom_density() +
  labs(title = "Time Frequencies of Car Collisions",
       x = "Hour in the Day",
       y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 23, 2))


## Graph party ages vs. casualty
## Observation: Slightly negative correlations
## Not enough data? Outliers?
sam_age <- data |>
  drop_na(party_age, number_injured, number_killed) |>
  filter(party_age > 0) |>
  group_by(party_age) |>
  summarize(mean_cas = mean(number_injured + number_killed)) |>
  slice_sample(n = 1e4)

ggplot(sam_age, aes(x = party_age, y = mean_cas)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(breaks = breaks_pretty()) +
  labs(title = "Average Casualty In a Car Accident of Different Age Groups",
       x = "Party Age",
       y = "Average Casualty")


## Graph lighting vs. casualty
## Observation: Dark - No Street Light = max casualty
sam_lgh <- data |>
  drop_na(lighting, number_injured, number_killed) |>
  filter(lighting != "Not Stated") |>
  group_by(lighting) |>
  summarize(hurt = mean(number_injured + number_killed)) |>
  slice_sample(n = 1e4)

ggplot(sam_lgh, aes(lighting, hurt)) +
  geom_col() +
  labs(title = "Average Casualty per Lighting Condition",
       x = "Lighting",
       y = "Average Casualty")


## Graph locations on a map
## Observation: Weird road conditions are clustered
sam_map <- data |>
  drop_na(tb_latitude, tb_longitude) |>
  filter(!road_cond_1 %in% c("Not Stated", "Other", "No Unusual Condition"))

map <- st_as_sf(sam_map, coords = c("tb_longitude", "tb_latitude"))
map <- st_set_crs(map, 4326)
severity <- factor(sam_map$collision_severity,
                   level = c("Injury (Complaint of Pain)",
                             "Injury (Other Visible)",
                             "Injury (Severe)", "Fatal"))
ggplot(map) +
  geom_sf(aes(color = road_cond_1, size = severity))


## Graph hour in the day vs. causality
sam_hrs <- data |>
  drop_na(number_injured, number_killed) |>
  group_by(hrs) |>
  summarize(hurt = mean(number_injured + number_killed)) |>
  slice_sample(n = 1e4)

ggplot(sam_hrs, aes(x = hrs, y = hurt)) +
  geom_point()


## RQ1: Predict car accident causality given party age and lighting (hour of the day?)
## How to remove discrete outliers?
## ML algorithm that use number + category to predict number?
## application: know party age = estimate causality = know what help to send

## RQ2: Predict road condition given location (longitude + latitude)
## KNN?

## RQ3: Estimate collision_severity with vehicle_autonomous, lighting, and stwd_vehicle_type
## What ML model to use? How to graph?

## graphs for different lighting conditions
popu <- data |>
  drop_na(party_age, number_injured, number_killed) |>
  filter(party_age > 0) |>
  mutate(total_cas = number_injured + number_killed)


act_cas_vs_age_canvas <- ggplot()
pred_cas_vs_age_canvas <- ggplot()

run_lr = function(sample, lighting) {
  sam_age_cas <- sample |>
    # subset(q1-9e9*(q3-q1) <= total_cas & total_cas <= q3+9e9*(q3-q1)) |>
    group_by(party_age) |>
    summarize(mean_cas = mean(number_injured + number_killed))
  
  print(ggplot(sam_age_cas, aes(x = party_age, y = mean_cas)) +
    geom_point() +
    geom_smooth() +
    labs(title = paste("Average Casualty vs. Age with", lighting),
         x = "Party Age",
         y = "Avg. Casualty"))
  
  split <- sample.split(sam_age_cas$mean_cas, SplitRatio = 0.8)
  train_data <- subset(sam_age_cas, split == T)
  test_data <- subset(sam_age_cas, split == F)
    
  lr_model <- lm(mean_cas ~ poly(party_age, 2), data = train_data)
  pred <- predict(lr_model, newdata = test_data)

  print(ggplot(sam_age_cas, aes(x = party_age, y = mean_cas)) +
          geom_point() +
          geom_smooth(lm =) +
          labs(title = paste("Average Casualty vs. Age with", lighting),
               x = "Party Age",
               y = "Avg. Casualty"))
  
  # ggplot(test_data, aes(x = mean_cas, y = pred)) +
  #   geom_point() +
  #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  #   labs(title = paste("Predicted vs. Actual Average Casualty with", lighting),
  #        x = "Actual Avg. Casualty",
  #        y = "Predicted Avg. Casualty")
}

for (lgh in unique(popu$lighting)) {
  sample <- subset(popu, lighting == lgh)
  run_lr(sample, lgh)
}

## https://stackoverflow.com/questions/65137690/plotting-polynomial-regression-curves-in-r
## too few data points!!
table(popu$lighting)

## mean square error, R score
