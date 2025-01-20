# first run `get_data.R` #############
library(fdapace)
variables_of_interest = c("TotAmmonia","DissAmmonia", 
                          "TotChloride", "DissChloride", 
                          "Chla",
                          "DissNitrateNitrite", 
                          "DissOrthophos", "TotPhos", 
                          "SpCndSurface", 
                          "WaterTempSurface", "WaterTempBottom")

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

# Deal with Left-Censored/Truncated Data #############
for (var in variables_of_interest) {
  # Check if the corresponding sign column exists
  sign_col <- paste0(var, "_Sign")
  if (sign_col %in% colnames(dt1)) {
    # Halve the values where the sign column has '<'
    dt1[[var]] <- ifelse(dt1[[sign_col]] == "<", dt1[[var]] / 2, dt1[[var]])
  }
}


# Prepare data for analysis ###############
regional_trends <- dt1_filtered %>%
  mutate(
    Year = year(Date),
    Season = case_when(
      month(Date) %in% 1:3 ~ "Q1",
      month(Date) %in% 4:6 ~ "Q2",
      month(Date) %in% 7:9 ~ "Q3",
      month(Date) %in% 10:12 ~ "Q4"
    )
  ) %>%
  group_by(Region, Year, Season) %>%
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

###FPCA
# Function to create fpca for a parameter ############
create_fpca_obj <- function(data, parameter) {
  # Prepare data for creating fpca objects
  sub_data <- data %>%
    select(Region, Year, Season, starts_with(parameter)) %>%
    rename(mean = paste0(parameter, "_mean"))
  
  # Get the earliest and latest time (Year and Season)
  time_range <- sub_data %>%
    summarise(
      start_year = min(Year),
      start_season = first(Season[Year == min(Year)]),
      end_year = max(Year),
      end_season = last(Season[Year == max(Year)])
    )
  
  # Create a time grid of length M (seasonal-level)
  all_seasons <- factor(c("Q1", "Q2", "Q3", "Q4"), ordered = TRUE)
  full_time_grid <- expand.grid(
    Year = seq(time_range$start_year, time_range$end_year),
    Season = all_seasons
  ) %>%
    arrange(Year, Season) %>%
    filter(
      !(Year == time_range$start_year & Season < time_range$start_season) &
        !(Year == time_range$end_year & Season > time_range$end_season)
    ) %>%
    mutate(TimeIndex = row_number()) # Index to keep track of the grid's length (M)
  
  M <- nrow(full_time_grid) # Length of the time grid
  
  # Create a 0-1 matrix of size N x M
  # N is the number of regions
  regions <- unique(sub_data$Region)
  N <- length(regions)
  
  # Initialize the 0-1 matrix
  presence_matrix <- matrix(0, nrow = N, ncol = M, dimnames = list(regions, 1:M))
  
  # Populate the matrix based on the presence of non-NA values for "mean"
  for (region in regions) {
    for (i in 1:M) {
      grid_year <- full_time_grid$Year[i]
      grid_season <- full_time_grid$Season[i]
      
      # Check if the region has a non-NA value for "mean" for the given year and season
      match_row <- sub_data %>%
        filter(Region == region, Year == grid_year, Season == grid_season)
      flag = ifelse(nrow(match_row)==0, FALSE, !is.na(match_row$mean))
      
      # Update presence matrix
      presence_matrix[region, i] <- as.integer(flag)
    }
  }
  
  id = c()
  time = c()
  y = c()
  
  for(i in 1:N){
    region = regions[i]
    valid_time_indices <- which(presence_matrix[i, ] == 1)
    id <- append(id, rep(i, length(valid_time_indices)))
    time <- append(time, full_time_grid$TimeIndex[valid_time_indices])
    valid_means <- sub_data %>%
      filter(Region == region) %>%
      filter(paste(Year, Season) %in% paste(
        full_time_grid$Year[valid_time_indices], 
        full_time_grid$Season[valid_time_indices]
      )) %>%
      pull(mean)
    
    y <- append(y, valid_means)
  }
  return(list(id = id, time = time, y = y))
}

# test: perform PACE ##########
fpca_obj <- create_fpca_obj(regional_trends, variables_of_interest[2])
L3 <- MakeFPCAInputs(IDs = fpca_obj$id, tVec = fpca_obj$time, fpca_obj$y)
optns <- list(plot = TRUE, dataType = 'Sparse', usergrid = TRUE, methodXi = 'CE')
fit <- FPCA(L3$Ly, L3$Lt, optns = optns)

require('ks')
CreatePathPlot(fit, subset = c(1:4), main = 'K = 2', pch = 4); grid()
CreateOutliersPlot(fit, optns = list(K = 2, variant = 'KDE'))
CreateModeOfVarPlot(fit, k = 1, rainbowPlot = FALSE)
CreateFuncBoxPlot(fit, xlab = 'Time', ylab = 'Chemical Desity', 
                  optns = list(K =2, variant='pointwise'))

# perform PACE on all variables in `variables_of_interest` ############
