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

###FPCA ############
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
  return(list(id = id, time = time, y = y, full_time_grid=full_time_grid))
}


# function to customize x ticks for the path plots and the funcbox plots in FPCA ###########
custom_xticks <- function(full_time_grid, original_x_ticks){
  seasons <- c("Q1", "Q2", "Q3", "Q4")
  flag <- 0 # if adding TimeIndex = 0 in the full_time_grid then turns to 1
  if (tail(original_x_ticks,1)[[1]] > max(full_time_grid$TimeIndex)){
    # Determine the maximum TimeIndex and corresponding Year and Season
    max_time_index <- max(full_time_grid$TimeIndex)
    last_year <- tail(full_time_grid$Year, 1)
    last_season <- tail(full_time_grid$Season, 1)
    
    # Generate new entries for full_time_grid
    new_entries <- data.frame(Year = integer(0), Season = character(0), TimeIndex = numeric(0))
    
    # Start from the next season
    
    season_idx <- match(last_season, seasons)
    year <- last_year
    
    for (time_idx in (max_time_index + 1):tail(original_x_ticks, 1)[[1]]) {
      # Determine the next season
      season_idx <- season_idx + 1
      if (season_idx > 4) {
        season_idx <- 1
        year <- year + 1
      }
      
      # Add the new entry
      new_entries <- rbind(new_entries, data.frame(
        Year = year,
        Season = seasons[season_idx],
        TimeIndex = time_idx
      ))
    }
    
    # Combine the old and new full_time_grid
    full_time_grid <- rbind(full_time_grid, new_entries)
  }
  
  # Determine the minimum TimeIndex and corresponding Year and Season
  if (original_x_ticks[1] == 0) {
    flag <- 1
    first_season = full_time_grid$Season[1]
    first_year = full_time_grid$Year[1]
    season_idx <- match(first_season, seasons)
    year = first_year
    
    season_idx <- season_idx - 1
    if (season_idx == 0) {
      season_idx <- 4
      year <- year - 1
    }
    
    new_entries <- data.frame(Year = year, Season = seasons[season_idx],
                              TimeIndex = 0)
    
    full_time_grid <- rbind(new_entries, full_time_grid)
  }
  
  if (flag == 1){formatted_ticks <- paste(full_time_grid$Year[original_x_ticks + 1], 
                                          full_time_grid$Season[original_x_ticks + 1])}
  else{
    formatted_ticks <- paste(full_time_grid$Year[original_x_ticks], 
                             full_time_grid$Season[original_x_ticks])
  }
  return(formatted_ticks)
}


# perform PACE on all variables in `variables_of_interest` ############
# produce plots into file: 8stations_fpca ###########
output_folder <- "C:/Users/20878/260Project-Water Quality/dataset/WaterQualityEDA/8stations_fpca"
for (var in variables_of_interest){
  fpca_obj <- create_fpca_obj(regional_trends, var)
  L3 <- MakeFPCAInputs(IDs = fpca_obj$id, tVec = fpca_obj$time, fpca_obj$y)
  optns <- list(dataType = 'Sparse', usergrid = TRUE, methodXi = 'CE')
  fit <- FPCA(L3$Ly, L3$Lt, optns = optns)
  full_time_grid = fpca_obj$full_time_grid
  
  
  # 1_fpca_plot
  pic_nm = paste0("1_fpca_plot_", var, ".png")
  full_path = file.path(output_folder, pic_nm)
  png(full_path, width = 800, height = 600)
  plot(fit, main=var)
  dev.off()
  
  # 2_path_plot
  pic_nm = paste0("2_path_plot_", var, ".png")
  full_path = file.path(output_folder, pic_nm)
  png(full_path, width = 800, height = 600)
  CreatePathPlot(fit, main = paste0(var, " changes with time: K=",length(fit$lambda)), 
                 pch = 4, xlab="Date", ylab=var, xaxt = "n")
  # Customize x-ticks for the `2_path_plot`
  original_x_ticks <- axTicks(1)
  formatted_ticks = custom_xticks(full_time_grid, original_x_ticks)
  axis(1, at = original_x_ticks, labels = formatted_ticks) 
  # customize legends
  id_map = unique(regional_trends$Region)
  unique_ids <- unique(fpca_obj$id)
  legend("topright", legend = id_map[unique_ids], 
         col = 1:length(unique_ids), lty = 1:length(unique_ids))
  grid()
  dev.off()
  
  # 3_funcbox_plot
  pic_nm = paste0("3_funcbox_plot_", var, ".png")
  full_path = file.path(output_folder, pic_nm)
  png(full_path, width = 800, height = 600)
  CreateFuncBoxPlot(fit, xlab = 'Date', ylab = var, 
                    main = paste0(var, ": functional boxplot (K=",length(fit$lambda), ")"), 
                    optns = list(K =length(fit$lambda), variant='pointwise'),
                    xaxt = "n"
                    )
  # Customize x-ticks for the `3_funcbox_plot` 
  original_x_ticks <- axTicks(1)
  formatted_ticks = custom_xticks(full_time_grid, original_x_ticks)
  axis(1, at = original_x_ticks, labels = formatted_ticks) 
  grid()
  dev.off()
  
  }


# test: perform PACE ##########
var = variables_of_interest[5]
fpca_obj <- create_fpca_obj(regional_trends, var)
L3 <- MakeFPCAInputs(IDs = fpca_obj$id, tVec = fpca_obj$time, fpca_obj$y)
optns <- list(dataType = 'Sparse', usergrid = TRUE, methodXi = 'CE')
fit <- FPCA(L3$Ly, L3$Lt, optns = optns)
png("custom_fpca_plot.png", width = 800, height = 600)
plot(fit, main=var)
dev.off()
# test: save customized path plot .png ##########
id_map = unique(regional_trends$Region)

# Define the output file
png("custom_path_plot.png", width = 800, height = 600)

# Create the Path Plot
CreatePathPlot(fit, main = paste0(var, " changes with time: K=",length(fit$lambda)), 
                pch = 4, xlab="Date", ylab=var, xaxt = "n")
original_x_ticks <- axTicks(1)
grid()
 
# Customize x-ticks
full_time_grid = fpca_obj$full_time_grid
formatted_ticks = custom_xticks(full_time_grid, original_x_ticks)
axis(1, at = original_x_ticks, labels = formatted_ticks)

# Add legend
unique_ids <- unique(fpca_obj$id)
legend("topright", legend = id_map[unique_ids], col = 1:length(unique_ids), lty = 1:length(unique_ids), 
        title = "Path IDs")

dev.off()

# if (tail(original_x_ticks,1)[[1]] > max(full_time_grid$TimeIndex)){
#   # Determine the maximum TimeIndex and corresponding Year and Season
#   max_time_index <- max(full_time_grid$TimeIndex)
#   last_year <- tail(full_time_grid$Year, 1)
#   last_season <- tail(full_time_grid$Season, 1)
#   
#   # Generate new entries for full_time_grid
#   new_entries <- data.frame(Year = integer(0), Season = character(0), TimeIndex = numeric(0))
#   
#   # Start from the next season
#   seasons <- c("Q1", "Q2", "Q3", "Q4")
#   season_idx <- match(last_season, seasons)
#   year <- last_year
#   
#   for (time_idx in (max_time_index + 1):tail(original_x_ticks, 1)[[1]]) {
#     # Determine the next season
#     season_idx <- season_idx + 1
#     if (season_idx > 4) {
#       season_idx <- 1
#       year <- year + 1
#     }
#     
#     # Add the new entry
#     new_entries <- rbind(new_entries, data.frame(
#       Year = year,
#       Season = seasons[season_idx],
#       TimeIndex = time_idx
#     ))
#   }
#   
#   # Combine the old and new full_time_grid
#   full_time_grid <- rbind(full_time_grid, new_entries)
# }
# 
# formatted_ticks <- paste(full_time_grid$Year[original_x_ticks], 
#                          full_time_grid$Season[original_x_ticks])
# axis(1, at = original_x_ticks, labels = formatted_ticks)
# 
# 
# # Add legend
# unique_ids <- unique(fpca_obj$id)
# legend("topright", legend = id_map[unique_ids], col = 1:length(unique_ids), lty = 1:length(unique_ids), 
#        title = "Path IDs")
# 
# # Close the device to save the file
# dev.off()
# 
# 
# CreateOutliersPlot(fit, optns = list(K = 2, variant = 'KDE'))
# CreateModeOfVarPlot(fit, k = 1, rainbowPlot = FALSE)
# CreateFuncBoxPlot(fit, xlab = 'Time', ylab = 'Chemical Desity', 
#                   optns = list(K =2, variant='pointwise'))


