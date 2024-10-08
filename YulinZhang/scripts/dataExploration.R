libs <- c("MASS", "sf", "tidyr", "dplyr", "lubridate", "ggplot2",
          "caTools", "scales", "ggpubr", "gridExtra", "cowplot")
for (lib in libs) {
  if (!require(lib, character.only = TRUE)) install.packages(lib, character.only = TRUE)
  library(lib, character.only = TRUE)
}
library(stringr)

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

## Investigation of RQ1
group_by_age <- function(data) {
  return(data |>
           group_by(party_age) |>
           summarize(mean_cas = mean(data$number_killed)))
}

graph_data <- function(data, subtitle) {
  return(
    ggplot(data, aes(x = party_age, y = mean_cas)) +
    geom_point() +
    labs(subtitle = subtitle,
         x = "Party Age",
         y = "Average Casualty"))
}

popu <- data |>
  drop_na(party_age, number_injured, number_killed) |>
  ## Only focus on driver's data to eliminate irrelevant data points
  filter(party_age > 0, party_type == "Driver")

## Extremely few data points <10-years-old and >90-yearsold
ggplot(popu, aes(x = party_age)) +
  geom_histogram(binwidth = 10, boundary = 0, aes(fill = after_stat(count))) +
  geom_text(
    stat = "bin",
    aes(label = percent(after_stat(count) / sum(after_stat(count)), accuracy = 0.01)),
    vjust = -0.5,
    breaks = seq(0, 120, 10)
  ) +
  scale_fill_gradient(low = "purple", high = "orange") +
  scale_x_continuous(breaks = seq(0, 120, 10)) +
  labs(title = "Distribution of Population by Ages")


h_lim <- xlim(range(popu$party_age))
v_lim <- ylim(1, 2)

before <- graph_data(group_by_age(popu), "With Outliers")
popu <- filter(group_by_age(popu), party_age > 15, party_age < 75)
after <- graph_data(group_by_age(popu), "Without Outliers")
gphs <- plot_grid(before + h_lim + v_lim,
                  after + h_lim + v_lim,
                  labels = "AUTO",
                  ncol = 2)
title <- ggdraw() + draw_label("Average Casualty vs. Party Age",
                               fontface = 'bold', size = 18)
plot_grid(title, gphs, ncol = 1, rel_heights = c(0.1, 1))


## Split the data into dark (merge them) and light condition
table(popu$lighting)
## Too few dark data points, have to merge
dark_data <- group_by_age(filter(popu, str_detect(lighting, "Dark|Dusk")))
light_data <- group_by_age(filter(popu, lighting == "Daylight"))

run_rlr = function(data, lgh) {
  ## Split the data into training and testing datasets (80% : 20%)
  split <- sample.split(data$mean_cas, SplitRatio = 0.8)
  train_data <- subset(data, split == T)
  test_data <- subset(data, split == F)
  
  ## Train and test the robust linear regression model
  ## RLM can diminish the impacts of the outliers by
  ## assigning them lower weights than non-outliers
  rlr_model <- rlm(mean_cas ~ party_age, data = train_data)
  pred <- predict(rlr_model, newdata = test_data)
  
  ## Print the performance of model
  mse <- mean((test_data$mean_cas - pred) ^ 2)
  mae <- mean(abs(test_data$mean_cas - pred))
  
  ## Low R-square due to outliers
  Y <- test_data$mean_cas
  r_sq <- 1 - sum((Y - pred)^2) / sum((Y - mean(Y))^2)
  
  cat("Mean Squared Error:", mse,
      "\nMean Absolute Error:", mae,
      "\nR-Squared:", r_sq)
  cat("\nOn average, predictions are off from the actual casualties by",
      mae,
      "people.")
  
  return(ggplot(data, aes(x = party_age, y = mean_cas)) +
           geom_point() +
           geom_smooth(method = MASS::rlm) +
           labs(subtitle = lgh,
                x = "Party Age",
                y = "Avg. Casualty"))
}


## [Task] Observe the diff. between avg. casualty by lighting
dark_gph <- run_rlr(dark_data, "Dark/dusk")
light_gph <- run_rlr(light_data, "Daylight")

h_lim <- xlim(range(popu$party_age))
v_lim <- ylim(range(paste(
                          
                          )))))
gphs <- plot_grid(dark_gph + h_lim + v_lim,
                  light_gph + h_lim + v_lim,
                  labels = "AUTO",
                  ncol = 2)
title <- ggdraw() + draw_label("Predicted Average Casualty vs. Party Age",
                               fontface = 'bold', size = 18)
plot_grid(title, gphs, ncol = 1, rel_heights = c(0.1, 1))

## compare robust regression model performance

