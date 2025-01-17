# Data Prep #############
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
### select stations ##########
# Convert dates to Date format
dt2$StartDate <- as.Date(dt2$StartDate, format = "%m/%d/%Y")
dt2$EndDate <- as.Date(dt2$EndDate, format = "%m/%d/%Y")
dt2$EndDate[is.na(dt2$EndDate)] <- Sys.Date()  # Replace NA with current date for active stations

# Calculate activation duration
dt2$ActivationDuration <- as.numeric(difftime(dt2$EndDate, dt2$StartDate, units = "days"))

# Filter active stations
active_stations <- dt2[dt2$Status == "Active", ]

# Manually link regions to station codes (from `stations` dataset)
regions <- data.frame(
  Region = c("Carquinez", "Central Delta", "Confluence", "North Delta", 
             "San Pablo Bay", "South Delta", "Suisun and Grizzly Bays", "Suisun Marsh"),
  StationCodes = c("NZ002,NZ004", "D16,D19,D26,D28A", "D4,D10,D12,D22", 
                   "C3A,NZ068", "D41,D41A,NZ325", "C9,C10A,MD10A,P8", 
                   "D6,D7,D8", "NZ032,NZS42")
)

# Split station codes for matching
regions <- regions %>%
  tidyr::separate_rows(StationCodes, sep = ",")

# Merge with active station data
merged_data <- merge(regions, active_stations, by.x = "StationCodes", by.y = "Station")

# Select strictly one station per region (resolve ties with StartDate)
selected_stations <- merged_data %>%
  group_by(Region) %>%
  arrange(desc(ActivationDuration), StartDate) %>% # Sort by duration and then earliest start date
  slice(1) %>%                                    # Pick the first station in each group
  ungroup()

# View selected stations
print(selected_stations)


### merge



# Filter dt1 to include only stations listed in selected_stations and merge station info
dt1_filtered <- dt1 %>%
  inner_join(selected_stations, by = c("Station" = "StationCodes"))



# Define variables of interest
variables_of_interest <- c(
  "DissOrthophos", "TotPhos", "DissSilica", "TDS", 
  "TSS", "VSS", "TKN", "WaterDepth", "Secchi", 
  "LightExtinction", "SpCndSurface", "DOSurface", 
  "WaterTempSurface", "TurbiditySurface", "pHSurface"
)

# Prepare data for analysis
regional_trends <- dt1_filtered %>%
  mutate(Year = year(Date)) %>%
  group_by(Region, Year) %>%
  summarise(across(
    all_of(variables_of_interest), 
    list(
      mean = ~mean(., na.rm = TRUE),
      median = ~median(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      n = ~sum(!is.na(.))
    )
  ), .groups = 'drop')

# Save regional trends summary
write.csv(regional_trends, "regional_trends_summary.csv", row.names = FALSE)

# Function to create time series plot for a parameter
create_time_series_plot <- function(data, parameter) {
  # Prepare data for plotting
  plot_data <- data %>%
    select(Region, Year, starts_with(parameter)) %>%
    rename(mean = paste0(parameter, "_mean"),
           sd = paste0(parameter, "_sd"))
  
  # Create plot
  ggplot(plot_data, aes(x = Year, y = mean, color = Region, group = Region)) +
    geom_line() +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = Region), alpha = 0.2) +
    theme_minimal() +
    labs(
      title = paste("Regional Trends for", parameter),
      y = "Mean Value",
      x = "Year"
    ) +
    theme(legend.position = "bottom")
}

# Generate time series plots for each parameter
plot_list <- lapply(variables_of_interest, function(param) {
  plot <- create_time_series_plot(regional_trends, param)
  ggsave(paste0(param, "_regional_trends.png"), plot, width = 10, height = 6)
  return(plot)
})

# Create a comprehensive regional comparison boxplot
regional_comparison <- dt1_filtered %>%
  pivot_longer(
    cols = all_of(variables_of_interest),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  ggplot(aes(x = Region, y = Value, fill = Region)) +
  geom_boxplot() +
  facet_wrap(~ Parameter, scales = "free_y", ncol = 3) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  labs(
    title = "Regional Comparison of Water Quality Parameters",
    y = "Value",
    x = "Region"
  )

# Save regional comparison plot
ggsave("regional_comparison_boxplot.png", regional_comparison, width = 16, height = 12)

# Print out some basic insights
print("Regional Trends Analysis Complete")
print("Files generated:")
print("1. regional_trends_summary.csv - Detailed summary statistics")
print(paste("2. *_regional_trends.png - Time series plots for each parameter"))
print("3. regional_comparison_boxplot.png - Boxplots comparing parameters across regions")
