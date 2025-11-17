# Script to calculate and visual medians for signal types per acoustic events
# Yvonne Barkley
# October 27, 2025



# find median number of signals per event in the prediction data set

sig_count <- read.csv(file.path('output', 'CountsSApreds_20251010.csv'))


sig_summ <- sig_count %>% summarise(mean_ec = mean(NClicks),
                        mean_dw = mean(NWhistle),
                        mean_cep = mean(NCep),
                        mean_ts = mean(TS),
                        med_ec = median(NClicks),
                        med_dw = median(NWhistle),
                        med_cep = median(NCep),
                        med_ts = median(TS) )



# 2. CALCULATE MEDIANS PER COLUMN
# Calculate the median for each column and store them in a tidy dataframe
# This is necessary so we can easily add the vertical lines later, 
# especially after reshaping the data.
median_data <- sig_count %>%
  # Reshape the data to long format first to easily group and calculate medians
  pivot_longer(cols = c('NCep', 'NClicks', 'NWhistle', 'TS'), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    IQR = IQR(Value, na.rm = TRUE), #spread of central 50%, distance btwn Q1 and Q3
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  
    )

# 3. RESHAPE DATA FROM WIDE TO LONG FORMAT
# ggplot often works best with 'long' data, where each variable and its value 
# are in separate columns.
df_long <- sig_count %>%
  pivot_longer(
    cols = c('NCep', 'NClicks', 'NWhistle', 'TS'), # Apply to all columns
    names_to = "Variable", # Column names become this new column's values
    values_to = "Value"    # The actual data points go here
  )
# Join Q3 limits to the long data and filter
df_visual <- df_long %>%
  left_join(median_data %>% select(Variable, Q3), by = "Variable") %>%
  filter(Value <= Q3)


# 4. GENERATE THE PLOT
ggplot(df_visual, aes(x = Value)) +
  
  # A. Create the Histograms
  geom_histogram(
    bins = 20, # Adjust bin count for better visualization
    fill = "skyblue", 
    color = "black", 
    alpha = 0.7
  ) +
  
  # B. Draw the Median Line
  # Use geom_vline and join the pre-calculated median data using the 'Variable' column
  geom_vline(
    data = median_data, 
    aes(xintercept = Median),
    linetype = "dashed", 
    color = "red", 
    linewidth = .8
  ) +
  
  # C. Facet the plot by the 'Variable' column
  # This creates a separate plot for each of the three original columns (A, B, C)
  facet_wrap(~ Variable, scales = "free_x") + 
  
  # D. Apply titles and theme
  labs(
    title = "Distribution and Median of Signals Types:",
    subtitle = "Prediction Test Data Set",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)



