library(tidyverse)
library(broom)
library(knitr)
library(patchwork)
library(plotly)

# Section 2: Simple linear regression ####

## Generate dummy medical data ####

set.seed(42) # For repoducibility

data <- tibble(
  age = runif(100, 20, 80), # Ages 20 to 80
  # SBP increases by 0.8 per year approx, plus noise
  sbp = 100 + 0.8 * age + rnorm(100, mean = 0, sd = 10) 
)

ggplot(data, aes(x = age, y = sbp)) +
  geom_point(alpha = 1.0) +
  xlab("Age (years)") +
  ylab("SBP (mmHg)") +
  theme_minimal()

## Bad model (manual guess) ####
# Let's guess y = 0.5x + 110 (Too flat)

data <- data %>%
  mutate(
    pred_bad = 110 + 0.5 * age,
    resid_bad = sbp - pred_bad,
    sq_error_bad = resid_bad^2
  )

ggplot(data, aes(x = age, y = sbp)) +
  geom_point(alpha = 0.6) +
  # Add trendline
  geom_abline(slope = 0.5, intercept = 110, color = "red", size = 1) +
  # Add residuals
  geom_segment(aes(xend = age, yend = pred_bad), color = "red", alpha = 0.4) +
  xlab("Age (years)") +
  ylab("SBP (mmHg)") +
  theme_minimal()

## Good Model (Actual Regression) ####
model <- lm(sbp ~ age, data = data)
data_aug <- augment(model, data)

data <- data %>%
  mutate(
    pred_good = predict(model),
    resid_good = resid(model),
    sq_error_good = resid_good^2
  )

ggplot(data_aug, aes(x = age, y = sbp)) +
  geom_point(alpha = 0.6) +
  # Use the fitted values from the model
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_segment(aes(xend = age, yend = .fitted), color = "blue", alpha = 0.4) +
  xlab("Age (years)") +
  ylab("SBP (mmHg)") +
  theme_minimal()

tibble(
  Model = c("Bad Guess (Manual)", "Good Guess (Regression)"),
  Total_Squared_Error = c(sum(data$sq_error_bad), sum(data$sq_error_good)),
  Mean_Squared_Error = c(mean(data$sq_error_bad), mean(data$sq_error_good))
)

## Plot the loss surface ####

# Define a "Grid Search" of possible lines
grid <- expand_grid(
  intercept = seq(50, 150, length.out = 50), # Test intercepts from 50 to 150
  slope = seq(0, 1.5, length.out = 50)       # Test slopes from 0 to 1.5
)

# Calculate Error (MSE) for every single combination
calculate_mse <- function(i, s) {
  pred <- i + s * data$age
  mean((data$sbp - pred)^2)
}

# Apply the function to the grid
grid_with_error <- grid %>%
  mutate(mse = map2_dbl(intercept, slope, calculate_mse))

# Find the actual OLS minimum (for plotting the red X)
true_model <- lm(sbp ~ age, data = data)
true_coefs <- tidy(true_model)
true_intercept <- true_coefs$estimate[1]
true_slope <- true_coefs$estimate[2]

# Plot the contour Plot
ggplot(grid_with_error, aes(x = intercept, y = slope, fill = mse)) +
  # Smooth pixelated heatmap
  geom_raster(interpolate = TRUE) + 
  # Add contour lines on top for clarity
  geom_contour(aes(z = mse), color = "white", alpha = 0.3, bins = 20) +
  # Mark the OLS solution
  geom_point(aes(x = true_intercept, y = true_slope), color = "red", size = 4, shape = 4) +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(
    x = "Intercept (Beta 0)",
    y = "Slope (Beta 1)",
    fill = "MSE"
  ) +
  theme_minimal()

## Interpreting the model ####
results <- tidy(model, conf.int = TRUE) %>%
  select(term, estimate, p.value, conf.low, conf.high) 
kable(results, digits = 2, caption = "Linear Regression Results: Age vs SBP")

model_quality <- glance(model) %>%
  select(r.squared, p.value)
kable(model_quality, digits = 3)

## Limitations of LR ####

tidy_anscombe <- anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)") %>%
  mutate(set = paste("Dataset", set))

stats <- tidy_anscombe %>%
  group_by(set) %>%
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x = sd(x),
    correlation = cor(x, y),
    regression_slope = lm(y ~ x)$coefficients[2],
    r_squared = summary(lm(y ~ x))$r.squared
  )

kable(stats, digits = 2, caption = "The Statistical Output (Identical Results)")

ggplot(tidy_anscombe, aes(x = x, y = y)) +
  geom_point(size = 3, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1) +
  facet_wrap(~set) + 
  labs(x = "Variable X", y = "Variable Y") +
  theme_minimal()

# Section 3: Multiple linear regression ####

## Create the Data ####
set.seed(123)
n <- 200
age <- runif(n, 20, 70)

# Coffee: Young people drink more.
# 6 cups/day at age 20, dropping to 1 cup/day at age 70 (plus noise)
coffee <- 8 - 0.1 * age + rnorm(n, mean = 0, sd = 1)
coffee <- pmax(0, coffee) # Ensure no negative cups

# SBP: Increases with Age (strong) AND Increases with Coffee (weak)
# True mechanism: Base 100 + 0.5*Age + 2.0*Coffee
sbp <- 100 + 0.5 * age + 2.0 * coffee + rnorm(n, mean = 0, sd = 5)

data_confound <- tibble(age, coffee, sbp)

ggplot(data_confound, aes(x = coffee, y = sbp)) +
  geom_point() +
  labs(x = "Cups of Coffee", y = "SBP (mmHg)") +
  theme_minimal()

## Naive Model: Does coffee affect BP? ####
model_naive <- lm(sbp ~ coffee, data = data_confound)

tidy(model_naive, conf.int = TRUE) %>% 
  select(term, estimate, p.value, conf.low, conf.high) %>%
  kable(digits = 2, caption = "Naive Model: SBP ~ Coffee")

ggplot(data_confound, aes(x = coffee, y = sbp)) +
  geom_point(alpha = 0.5, color = "grey50") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(x = "Cups of Coffee", y = "SBP (mmHg)") +
  theme_minimal()

## Adjusted Model: Does coffee affect BP, controlling for Age? ####
model_adjusted <- lm(sbp ~ coffee + age, data = data_confound)

tidy(model_adjusted, conf.int = TRUE) %>% 
  select(term, estimate, p.value, conf.low, conf.high) %>%
  kable(digits = 2, caption = "Adjusted Model: SBP ~ Coffee + Age")

ggiraphExtra::ggPredict(model_adjusted) +
  labs(x = "Cups of Coffee", y = "SBP (mmHg)") +
  theme_minimal()

## Visualise in 3D ####

# Create grid for the plane
x_seq <- seq(min(data_confound$coffee), max(data_confound$coffee), length.out = 25)
y_seq <- seq(min(data_confound$age), max(data_confound$age), length.out = 25)
z_grid <- expand.grid(coffee = x_seq, age = y_seq)
z_grid$sbp <- predict(model_adjusted, newdata = z_grid)
z_matrix <- matrix(z_grid$sbp, nrow = 25, ncol = 25)

# Calculate Fitted Values for the actual points (to draw the lines)
lines_data <- data_confound %>%
  mutate(
    id = row_number(), 
    fitted_sbp = predict(model_adjusted)
  ) %>%
  select(id, coffee, age, sbp, fitted_sbp) %>%
  pivot_longer(cols = c(sbp, fitted_sbp), names_to = "type", values_to = "sbp_value")

# Plot
p <- plot_ly() %>%
  
  # Regression Plane
  add_surface(
    x = x_seq, y = y_seq, z = z_matrix,
    opacity = 0.5,
    colorscale = list(c(0, 1), c("blue", "red")),
    showscale = FALSE,
    name = "Regression Plane"
  ) %>%
  
  # Data Points
  add_markers(
    data = data_confound,
    x = ~coffee, y = ~age, z = ~sbp,
    marker = list(size = 3, color = "black"),
    name = "Patients",
    showlegend = FALSE
  ) %>%
  
  # Residual Lines
  add_paths(
    data = lines_data,
    x = ~coffee, y = ~age, z = ~sbp_value,
    split = ~id, 
    line = list(color = "red", width = 1),
    showlegend = FALSE,
    hoverinfo = "none"
  ) %>%
  
  layout(
    scene = list(
      xaxis = list(title = 'Coffee'),
      yaxis = list(title = 'Age'),
      zaxis = list(title = 'SBP')
    )
  )

# Save to use in talk
htmlwidgets::saveWidget(p, "regression_cube.html")

# Section 4: Logistic regression ####

## Generate binary data (dose vs survival) ####
set.seed(69)
n_log <- 100
dose <- runif(n_log, 0, 10)

# The Probability of survival increases with dose
prob <- 1 / (1 + exp(-(1.5 * dose - 7))) 
outcome <- rbinom(n_log, 1, prob) # Generate 0s (Dead) and 1s (Alive)
data_logistic <- tibble(dose, prob, outcome)

ggplot(data_logistic, aes(x = dose, y = outcome)) +
  # Data points
  geom_point(alpha = 0.6, size = 3) +
  # LR line
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  annotate("text", x = 2, y = -0.2, label = "Linear (Impossible predictions)", color = "red") +
  # LR curve
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              color = "blue", size = 1.2) +
  annotate("text", x = 8, y = 0.5, label = "Logistic (S-Curve)", color = "blue", fontface = "bold") +
  labs(x = "Drug Dose (mg)", y = "Outcome (0 = Dead, 1 = Alive)"
  ) +
  theme_minimal()

## Run the model ####
model_log <- glm(outcome ~ dose, data = data_logistic, family = "binomial")

# exponentiate = TRUE turns the confusing 'Log-Odds' into 'Odds Ratios'
tidy(model_log, exponentiate = TRUE, conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  kable(digits = 2, caption = "Logistic Regression Results (Odds Ratios)")

## Forest plot ####

results <- tidy(model_log, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") # We usually ignore the intercept in plots

ggplot(results, aes(x = estimate, y = term)) +
  # Line of No Effect (Vertical line at x = 1)
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  # Confidence Interval (Error Bar)
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "blue") +
  # Point Estimate
  geom_point(size = 5, color = "blue") +
  # Annotations
  annotate("text", x = 1.5, y = 1.1, label = "OR > 1", color = "darkgreen", size = 3) +
  annotate("text", x = 0.5, y = 1.1, label = "OR < 1", color = "darkred", size = 3) +
  # Log scale is standard for OR plots to make them symmetrical
  scale_x_log10() + 
  labs(x = "Odds Ratio (Log Scale)", y = "Variable"
  ) +
  theme_minimal()

## Turning odds into risk ####

data_conversion <- tibble(
  risk = seq(0.01, 0.99, by = 0.01)
) %>%
  mutate(
    # Odds = Risk / (1 - Risk)
    odds = risk / (1 - risk),
    # Label for coloring regions
    zone = case_when(
      risk < 0.1 ~ "Safe Zone (Odds ≈ Risk)",
      TRUE ~ "Danger Zone (Odds >> Risk)"
    )
  )

ggplot(data_conversion, aes(x = risk, y = odds)) +
  geom_line(aes(color = zone), size = 1.5) +
  # Reference Line (If Odds equaled Risk)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  annotate("text", x = 0.6, y = 0.5, label = "Line of Identity (Risk = Odds)", color = "gray50") +
  # Highlight the "Safe Zone"
  geom_rect(xmin = 0, xmax = 0.1, ymin = 0, ymax = 0.11, 
            fill = "green", alpha = 0.1) +
  annotate("text", x = 0.2, y = 9, label = "At high risk,\nOdds exaggerate the number", color = "red") +
  scale_color_manual(values = c("Danger Zone (Odds >> Risk)" = "red", "Safe Zone (Odds ≈ Risk)" = "darkgreen")) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 10)) + # Cap y-axis because Odds go to infinity
  labs(x = "Risk (Probability)", y = "Odds") +
  theme_minimal() +
  theme(legend.position = "none")

## Probability of belonging to a class ####

data_pred <- data_logistic %>%
  mutate(
    pred_prob = predict(model_log, type = "response"),
    actual_outcome = factor(outcome, labels = c("Died", "Survived"))
  )

ggplot(data_pred, aes(x = dose, y = pred_prob)) +
  geom_line(color = "black", size = 1) +
  geom_point(aes(color = actual_outcome), size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  annotate("text", x = min(data_pred$dose), y = 0.52, label = "50% Probability Cutoff", color = "gray50", hjust = 0) +
  scale_color_manual(values = c("Died" = "darkred", "Survived" = "darkgreen")) +
  labs(
    x = "Dose of Drug",
    y = "Predicted Probability of Survival",
    color = "Actual Outcome"
  ) +
  theme_minimal()
