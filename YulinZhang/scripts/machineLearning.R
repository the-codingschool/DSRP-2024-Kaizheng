libs <- c("MASS", "sf", "tidyr", "dplyr", "lubridate", "ggplot2",
          "caTools", "scales", "ggpubr", "gridExtra", "cowplot", "ggpmisc")
for (lib in libs) {
  if (!require(lib, character.only = TRUE))
    install.packages(lib, character.only = TRUE)
  library(lib, character.only = TRUE)
}
library(stringr)

## Helper functions
group_by_age <- function(data) {
  data <<- data |>
    group_by(party_age) |>
    summarize(mean_cas = mean(total_cas))
}

graph_data <- function(data, subtitle) {
  return(
    ggplot(data, aes(x = party_age, y = mean_cas)) +
      geom_point() +
      labs(subtitle = subtitle,
           x = "Party Age",
           y = "Average Casualty"))
}

## Clean data
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
    breaks = seq(0, 110, 10)
  ) +
  scale_fill_gradient(low = "purple", high = "orange") +
  scale_x_continuous(breaks = seq(0, 110, 10)) +
  labs(title = "Distribution of Population by Ages")


## Remove outliers
h_lim <- xlim(range(popu$party_age))
v_lim <- ylim(1, 2)

before <- graph_data(group_by_age(popu), "With Outliers")
popu <- filter(popu, party_age > 15, party_age < 75)
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
dark_data <- popu |>
  filter(str_detect(lighting, "Dark|Dusk")) |>
  mutate(total_cas = number_injured + number_killed)

light_data <- popu |>
  filter(lighting == "Daylight") |>
  mutate(total_cas = number_injured + number_killed)

nrow(dark_data)
nrow(light_data)

run_rlr = function(data, lgh) {
  ## Split the data into training and testing datasets (80% : 20%)
  split <- sample.split(data$total_cas, SplitRatio = 0.7)
  train_data <- group_by_age(subset(data, split == T))
  test_data <- group_by_age(subset(data, split == F))
  print(nrow(train_data))
  print(nrow(test_data))
  
  ## Train and test the robust linear regression model
  ## RLM can diminish the impacts of the outliers by
  ## assigning them lower weights than non-outliers
  rlr_model <- rlm(mean_cas ~ party_age, data = train_data)
  pred <- predict(rlr_model, newdata = test_data)
  
  ## Print the performance of model
  Y <- test_data$mean_cas
  mse <- mean((Y - pred)^2)
  mae <- mean(abs(Y - pred))
  
  ## Low R-square due to outliers
  r_sq <- 1 - sum((Y - pred)^2) / sum((Y - mean(Y))^2)
  
  cat("Mean Squared Error:", signif(mse, 3),
      "\nMean Absolute Error:", signif(mae, 3),
      "\nR-Squared:", signif(r_sq, 3))
  cat("\nOn average, predictions are off from the actual casualties by",
      signif(mae, 3),
      "people.")

  return(ggplot(data, aes(x = party_age, y = mean_cas)) +
           geom_point() +
           geom_smooth(method = MASS::rlm) +
           stat_poly_eq(method = MASS::rlm,
                        use_label("eq"),
                        label.x = "right") +
           labs(subtitle = lgh,
                x = "Party Age",
                y = "Average Casualty"))
}


## [Task] Observe the diff. between avg. casualty by lighting
dark_gph <- run_rlr(dark_data, "Dark/Dusk")
light_gph <- run_rlr(light_data, "Daylight")

h_lim <- xlim(range(popu$party_age))
v_lim <- ylim(range(c(dark_data$mean_cas, light_data$mean_cas)))

gphs <- plot_grid(dark_gph + h_lim + v_lim,
                  light_gph + h_lim + v_lim,
                  labels = "AUTO",
                  ncol = 2)
title <- ggdraw() +
  draw_label("Predicted Average Casualty vs. Party Age", 
             fontface = 'bold', size = 18)

plot_grid(title, gphs, ncol = 1, rel_heights = c(0.1, 1))

## compare robust regression model performance

