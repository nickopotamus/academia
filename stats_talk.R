library(tidyverse) # For data manipulation
library(broom)     # For cleaning up model output
library(rsample)   # For tidy bootstrapping
library(patchwork) # For combining plots

# 1 - THE "DATA MODEL" (Our One Study) ####
set.seed(123)

# These are the "True" parameters we will *pretend* exist in the Process Model
TRUE_MEAN_DRIVER  = 42.0
TRUE_MEAN_CYCLIST = 46.5 # So the "True Difference" is +4.5
TRUE_SD_DRIVER    = 7.0
TRUE_SD_CYCLIST   = 7.0

# Now, we take our *one sample* (our Data Model)
N_PER_GROUP = 30

# Generate our one sample
sample_drivers <- tibble(
  group = "Driver",
  vo2max = rnorm(N_PER_GROUP, mean = TRUE_MEAN_DRIVER, sd = TRUE_SD_DRIVER)
)

sample_cyclists <- tibble(
  group = "Cyclist",
  vo2max = rnorm(N_PER_GROUP, mean = TRUE_MEAN_CYCLIST, sd = TRUE_SD_CYCLIST)
)

our_study_data <- bind_rows(sample_drivers, sample_cyclists) %>%
  mutate(group = as_factor(group))

summary(our_study_data)

# Calculate our *observed* difference
(obs_means <- our_study_data %>%
    group_by(group) %>%
    summarise(mean_vo2 = mean(vo2max)))

(obs_diff <- obs_means$mean_vo2[2] - obs_means$mean_vo2[1])

# Run the *actual* t-test (the "Statistical Model")
our_t_test <- t.test(vo2max ~ group, data = our_study_data, var.equal = TRUE)
tidy(our_t_test)

## PLOT 1A: The "Process" vs. The "Data" ####

# Define a shared x-axis for direct comparison
XLIMS = c(20, 70)

# 1. The Process Model ("Truth")
(process_plot <- ggplot(data.frame(x = XLIMS), aes(x = x)) +
    stat_function(
      fun = dnorm,
      args = list(mean = TRUE_MEAN_DRIVER, sd = TRUE_SD_DRIVER),
      aes(color = "Driver"),
      linetype = "dashed",
      linewidth = 1.2
    ) +
    stat_function(
      fun = dnorm,
      args = list(mean = TRUE_MEAN_CYCLIST, sd = TRUE_SD_CYCLIST),
      aes(color = "Cyclist"),
      linetype = "dashed",
      linewidth = 1.2
    ) +
    geom_vline(xintercept = TRUE_MEAN_DRIVER, linetype = "dashed", color = "blue", alpha = 0.6) +
    geom_vline(xintercept = TRUE_MEAN_CYCLIST, linetype = "dashed", color = "red", alpha = 0.6) +
    #annotate("text", x = TRUE_MEAN_DRIVER, y = 0.055, label = "True Driver\nMean = 42.0", vjust = 0, color = "blue", size = 3.5) +
    #annotate("text", x = TRUE_MEAN_CYCLIST, y = 0.055, label = "True Cyclist\nMean = 46.5", vjust = 0, color = "red", size = 3.5) +
    scale_color_manual(values = c("Driver" = "blue", "Cyclist" = "red"), name = "Group") +
    coord_cartesian(xlim = XLIMS) +
    labs(
      #title = "1. Process Model ('The Truth')",
      #subtitle = "The true, smooth distributions (unseen)",
      x = "VO2max (ml/kg/min)",
      y = "Density"
    ) +
    theme_minimal()
)

# 2. The Data Model ("Sample")
(data_plot <- ggplot(our_study_data, aes(x = vo2max, fill = group)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1.5, alpha = 0.6, position = "identity") +
    #geom_rug(aes(color = group), length = unit(0.05, "npc"), linewidth = 1) +
    scale_fill_manual(values = c("Driver" = "blue", "Cyclist" = "red"), name = "Group") +
    scale_color_manual(values = c("Driver" = "blue", "Cyclist" = "red"), guide = "none") + # Hide legend for rug
    coord_cartesian(xlim = XLIMS) +
    labs(
      #title = "2. Data Model ('Our Sample')",
      #subtitle = "The messy n=30 histogram (all we see)",
      x = "VO2max (ml/kg/min)",
      y = "Density"
    ) +
    theme_minimal()
)

# 3. Combine the plots
(process_vs_data_plot <- process_plot + data_plot + 
    plot_layout(guides = 'collect') & # 'collect' makes a single, shared legend
    theme(legend.position = 'bottom')
)

## PLOT 1B: Illustrating Sampling Variation ####

run_one_study_and_plot <- function(study_id) {
  
  one_study_sample <- bind_rows(
    tibble(
      group = "Driver",
      vo2max = rnorm(N_PER_GROUP, mean = TRUE_MEAN_DRIVER, sd = TRUE_SD_DRIVER)
    ),
    tibble(
      group = "Cyclist",
      vo2max = rnorm(N_PER_GROUP, mean = TRUE_MEAN_CYCLIST, sd = TRUE_SD_CYCLIST)
    )
  )
  
  test_result <- t.test(vo2max ~ group, data = one_study_sample, var.equal = TRUE)
  obs_diff <- test_result$estimate[2] - test_result$estimate[1]
  p_val <- test_result$p.value
  
  (p <- ggplot(one_study_sample, aes(x = vo2max, fill = group)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = 1.5, alpha = 0.6, position = "identity") +
      scale_fill_manual(values = c("Driver" = "blue", "Cyclist" = "red"), guide = "none") +
      coord_cartesian(xlim = XLIMS, ylim = c(0, 0.08)) + # Fix y-axis
      theme_minimal() +
      theme(
        axis.text.y = element_blank(), # Hide y-axis text
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 8)
      ) +
      labs(
        title = paste("Study #", study_id),
        subtitle = paste("Diff = ", round(obs_diff, 1), " | p = ", round(p_val, 3)),
        x = "", y = ""
      )
  )
  
  return(p)
}

set.seed(999)

list_of_plots <- map(1:30, run_one_study_and_plot)

(sampling_variation_grid <- wrap_plots(list_of_plots, ncol = 10, nrow = 3) +
    plot_annotation(
      #title = "The 30 'Data Models' We *Could* Have Gotten",
      #subtitle = "Each plot is one simulated study (n=30 vs n=30) drawn from the *same* 'Truth' (True Diff = 4.5)",
      #caption = "Notice how the sample histogram, mean difference, and p-value change *every single time*."
    ) &
    theme(legend.position = "none")
)

## PLOT 1C: The T-Distribution ####

# Get stats from our first t-test 
T_STAT <- our_t_test$statistic
DF <- our_t_test$parameter

# Create a dataframe of x-values for the plot
t_dist_data <- tibble(
  x = seq(-4, 4, length.out = 200),
  y = dt(x, df = DF)
)

# Create a "tail" dataframe for shading
t_tail_data <- t_dist_data %>%
  filter(x >= T_STAT | x <= -T_STAT)

# Create the plot
(t_distribution_plot <- ggplot(t_dist_data, aes(x = x, y = y)) +
    geom_line(color = "black", linewidth = 1) +
    geom_ribbon(data = t_tail_data, aes(ymin = 0, ymax = y), fill = "red", alpha = 0.6) +
    geom_vline(xintercept = c(T_STAT, -T_STAT), linetype = "dashed", color = "red") +
    geom_label(aes(x = T_STAT, y = 0.2), label = paste("Our t-stat = ", round(T_STAT, 2)), color = "red") +
    geom_label(aes(x = -T_STAT, y = 0.2), label = paste("-t-stat = ", round(-T_STAT, 2)), color = "red") +
    labs(
      #title = "The Theoretical T-Distribution (df=58)",
      #subtitle = "The 'maths' behind the p-value. Area of red tails = p-value (0.0326)",
      x = "t-statistic ('Signal-to-Noise' Ratio)",
      y = "Density"
    ) +
    theme_minimal()
)

# 2 - SIMULATING THE P-VALUE (The "Null World") ####

N_SIMULATIONS = 10000
POOLED_SD = 7.0 # From our original TRUE_SD

# Create a tibble to store results
null_simulations <- rerun(N_SIMULATIONS, {
  
  # Create a "null" sample
  sim_drivers  = rnorm(N_PER_GROUP, mean = 40, sd = POOLED_SD)
  sim_cyclists = rnorm(N_PER_GROUP, mean = 40, sd = POOLED_SD) # mean is THE SAME
  
  # Calculate the mean difference
  mean(sim_cyclists) - mean(sim_drivers)
}) %>% 
  set_names(1:N_SIMULATIONS) %>% 
  unlist() %>% 
  enframe(name = "sim_id", value = "mean_diff")

## PLOT 2A: The Null Distribution ####
(null_plot <- null_simulations %>%
    ggplot(aes(x = mean_diff)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "dodgerblue", alpha = 0.7, color = "white") +
    geom_density(color = "dodgerblue", linewidth = 1.2) +
    labs(
      title = "Sampling Distribution under the Null Hypothesis (H₀)",
      subtitle = "10,000 simulated studies where True Difference = 0",
      x = "Simulated Mean Difference (Cyclist - Driver)",
      y = "Density"
    ) +
    theme_minimal())

# Find the proportion of sims more extreme than our |observed diff|
(p_value_from_sim <- null_simulations %>%
    summarise(
      p_value = mean(abs(mean_diff) >= obs_diff)
    ))

# Same plot, but showing the p-value
(null_plot_with_p <- null_plot +
    geom_vline(xintercept = c(obs_diff, -obs_diff), color = "red", linetype = "dashed", linewidth = 1) +
    geom_label(aes(x = obs_diff, y = 0.1), label = paste("Our Diff = ", round(obs_diff, 1)), color = "red") +
    labs(title = "Where does our sample fit in the 'Null World'?",
         subtitle = "p-value = Area in the 'tails' beyond our result"))


# 2A - P-VALUE & POWER SIMULATIONS ####

# Our guesses/parameters for power analysis:
H0_MEAN_DIFF = 0
H1_MEAN_DIFF = TRUE_MEAN_CYCLIST-TRUE_MEAN_DRIVER
H1_SD = 7.0         

# Sample sizes to compare
N_PER_GROUP_LOW = 30
N_PER_GROUP_HIGH = 100

# Function to run one simulation and return *just* the p-value
run_power_sim <- function(n, true_diff, sd) {
  sim_drivers  = rnorm(n, mean = 40, sd = sd)
  sim_cyclists = rnorm(n, mean = 40 + true_diff, sd = sd)
  # Run a t-test and return *just* the p-value
  t.test(sim_cyclists, sim_drivers, var.equal = TRUE)$p.value
}

## PLOT 2B: P-Value Distributions (H0 vs H1) ####

# 1. Simulate 10,000 p-values from the "Null World" (H0 is True)
set.seed(2024)
p_values_h0 <- rerun(N_SIMULATIONS, run_power_sim(
  n = N_PER_GROUP_LOW,      # n=30
  true_diff = H0_MEAN_DIFF, # diff=0
  sd = H1_SD                # sd=7
)) %>% unlist() %>% tibble(p_value = .)

# 2. Simulate 10,000 p-values from the "Alternative World" (H1 is True)
set.seed(2025)
p_values_h1 <- rerun(N_SIMULATIONS, run_power_sim(
  n = N_PER_GROUP_LOW,      # n=30
  true_diff = H1_MEAN_DIFF, # diff=3
  sd = H1_SD                # sd=7
)) %>% unlist() %>% tibble(p_value = .)

# 3. Create the plots
(plot_p_dist_h0 <- p_values_h0 %>%
    ggplot(aes(x = p_value)) +
    geom_histogram(binwidth = 0.05, boundary = 0, fill = "dodgerblue", alpha = 0.8) +
    geom_vline(xintercept = 0.05, color = "red", linetype = "dashed") +
    labs(
      title = "P-Value Distribution (H0 is True)",
      subtitle = "In a 'Null World', p-values are uniformly distributed.",
      x = "p-value", y = "Count"
    ) +
    theme_minimal())

(plot_p_dist_h1 <- p_values_h1 %>%
    ggplot(aes(x = p_value)) +
    geom_histogram(binwidth = 0.05, boundary = 0, fill = "red", alpha = 0.8) +
    geom_vline(xintercept = 0.05, color = "black", linetype = "dashed") +
    labs(
      title = "P-Value Distribution (H1 is True)",
      subtitle = "In an 'Alternative World', p-values stack up near zero.",
      x = "p-value", y = "Count"
    ) +
    theme_minimal())

(p_value_distributions_plot <- plot_p_dist_h0 + plot_p_dist_h1)

# Power calc
table(p_values_h1$p_value < 0.05) # Number <0.05
(power_low_n <- mean(p_values_h1$p_value < 0.05))


# PART 3 - SIMULATING THE CI (The "Bootstrap World") ####
set.seed(456) 

# Run 10,000 bootstrap resamples
bootstraps <- bootstraps(our_study_data, times = 10000)

# Now, create a function to calculate our statistic (mean difference)
calc_mean_diff <- function(split) {
  data <- analysis(split)
  
  mean_cyclist = mean(data$vo2max[data$group == "Cyclist"])
  mean_driver  = mean(data$vo2max[data$group == "Driver"])
  
  tibble(term = "mean_diff", estimate = mean_cyclist - mean_driver)
}

# Map this function over all 10,000 bootstrap samples
bootstrap_diffs <- bootstraps %>%
  mutate(stats = map(splits, calc_mean_diff)) %>%
  unnest(stats)

## PLOT 3A: Bootstrap Distribution ####
(bootstrap_plot <- bootstrap_diffs %>%
    ggplot(aes(x = estimate)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "purple", alpha = 0.7, color = "white") +
    geom_density(color = "purple", linewidth = 1.2) +
    geom_vline(xintercept = obs_diff, color = "red", linetype = "dashed", linewidth = 1) +
    geom_label(aes(x = obs_diff, y = 0.1), label = paste("Obs. Diff = ", round(obs_diff, 1)), color = "red") +
    labs(
      title = "Bootstrap Distribution of Mean Differences",
      subtitle = "10,000 're-studies' simulated from *our sample*",
      x = "Bootstrap Mean Difference (Cyclist - Driver)",
      y = "Density"
    ) +
    theme_minimal())

# Get the 95% CI from the percentiles
(ci_from_sim <- bootstrap_diffs %>%
    summarise(
      lower_ci = quantile(estimate, 0.025),
      upper_ci = quantile(estimate, 0.975)
    ))

# Same plot, showing the CI
(bootstrap_plot_with_ci <- bootstrap_plot +
    geom_vline(xintercept = c(ci_from_sim$lower_ci, ci_from_sim$upper_ci), color = "black", linetype = "dotted", linewidth = 1) +
    annotate("rect", xmin = ci_from_sim$lower_ci, xmax = ci_from_sim$upper_ci, ymin = 0, ymax = Inf, fill = "purple", alpha = 0.1) +
    labs(subtitle = "The 95% CI is the middle 95% of the bootstrap distribution")
)


# PART 4 - SIMULATING POWER (H0 vs H1) ####
set.seed(789)

## PLOT 4A: Power simulation (n=30) ####

# Simulate 10,000 studies under H0
h0_dist_low_n <- rerun(N_SIMULATIONS, {
  sim_drivers  = rnorm(N_PER_GROUP_LOW, mean = 40, sd = H1_SD)
  sim_cyclists = rnorm(N_PER_GROUP_LOW, mean = 40 + H0_MEAN_DIFF, sd = H1_SD)
  mean(sim_cyclists) - mean(sim_drivers)
}) %>% unlist() %>% # FIX: unlist
  enframe(name = "sim", value = "mean_diff") %>% 
  mutate(hyp = "H0 (Diff=0)")

# Simulate 10,000 studies under H1
h1_dist_low_n <- rerun(N_SIMULATIONS, {
  sim_drivers  = rnorm(N_PER_GROUP_LOW, mean = 40, sd = H1_SD)
  sim_cyclists = rnorm(N_PER_GROUP_LOW, mean = 40 + H1_MEAN_DIFF, sd = H1_SD)
  mean(sim_cyclists) - mean(sim_drivers)
}) %>% unlist() %>% # FIX: unlist
  enframe(name = "sim", value = "mean_diff") %>% 
  mutate(hyp = "H1 (Diff=4.5)")

# Combine and plot
(power_plot_low_n <- bind_rows(h0_dist_low_n, h1_dist_low_n) %>%
    ggplot(aes(x = mean_diff, fill = hyp)) +
    geom_density(alpha = 0.6) +
    scale_fill_manual(values = c("dodgerblue", "red")) +
    geom_vline(xintercept = H1_MEAN_DIFF, linetype = "dashed", color = "red") +
    geom_vline(xintercept = H0_MEAN_DIFF, linetype = "dashed", color = "blue") +
    labs(title = "Power Simulation (n=30 per group)",
         subtitle = "Huge overlap = Low Power (high chance of Type II Error)",
         x = "Simulated Mean Difference") +
    theme_minimal())

# Calculate Power:We already did this!
(power_low_n <- mean(p_values_h1$p_value < 0.05)) 

## PLOT 4B: Power simulation (n=100) ####
set.seed(101)

# Simulate 10,000 studies under H0
h0_dist_high_n <- rerun(N_SIMULATIONS, {
  sim_drivers  = rnorm(N_PER_GROUP_HIGH, mean = 40, sd = H1_SD)
  sim_cyclists = rnorm(N_PER_GROUP_HIGH, mean = 40 + H0_MEAN_DIFF, sd = H1_SD)
  mean(sim_cyclists) - mean(sim_drivers)
}) %>% unlist() %>% # FIX: unlist
  enframe(name = "sim", value = "mean_diff") %>% 
  mutate(hyp = "H0 (Diff=0)")

# Simulate 10,000 studies under H1
h1_dist_high_n <- rerun(N_SIMULATIONS, {
  sim_drivers  = rnorm(N_PER_GROUP_HIGH, mean = 40, sd = H1_SD)
  sim_cyclists = rnorm(N_PER_GROUP_HIGH, mean = 40 + H1_MEAN_DIFF, sd = H1_SD)
  mean(sim_cyclists) - mean(sim_drivers)
}) %>% unlist() %>% # FIX: unlist
  enframe(name = "sim", value = "mean_diff") %>% 
  mutate(hyp = "H1 (Diff=4.5)")

# Combine and plot
(power_plot_high_n <- bind_rows(h0_dist_high_n, h1_dist_high_n) %>%
    ggplot(aes(x = mean_diff, fill = hyp)) +
    geom_density(alpha = 0.6) +
    scale_fill_manual(values = c("dodgerblue", "red")) +
    geom_vline(xintercept = H1_MEAN_DIFF, linetype = "dashed", color = "red") +
    geom_vline(xintercept = H0_MEAN_DIFF, linetype = "dashed", color = "blue") +
    labs(title = "Power Simulation (n=100 per group)",
         subtitle = "Distributions are 'skinnier' = High Power",
         x = "Simulated Mean Difference") +
    theme_minimal())

# Calculate Power: What % of H1 sims had p < 0.05?
sim_p_values_h1_high_n <- rerun(5000, run_power_sim(N_PER_GROUP_HIGH, H1_MEAN_DIFF, H1_SD)) %>%
  unlist() 
(power_high_n <- mean(sim_p_values_h1_high_n < 0.05)) 

# Can put plots side by side
power_plot_low_n + power_plot_high_n

# The "real" power calculation 
power.t.test(
  n = NULL, # Solve for n
  delta = 3.0,  # H1 effect
  sd = 7.0,     # SD
  sig.level = 0.05,
  power = 0.80, # 80% desired power
  type = "two.sample"
)
