knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE, 
                      warning = FALSE)
library(c3dr)
library(tidyverse)
library(signal)
library(reticulate)
py_install("spm1d")
spm1d <- import("spm1d")

# import C3D file (change to your directory if wanting to run)
c3d1 <- c3d_read("/Users/thomasyoung/Documents/R/c3d_files/S1C1T1.c3d")
c3d2 <- c3d_read("/Users/thomasyoung/Documents/R/c3d_files/S1C1T2.c3d")
c3d3 <- c3d_read("/Users/thomasyoung/Documents/R/c3d_files/S1C1T3.c3d")
c3d4 <- c3d_read("/Users/thomasyoung/Documents/R/c3d_files/S1C1T4.c3d")
c3d5 <- c3d_read("/Users/thomasyoung/Documents/R/c3d_files/S1C1T5.c3d")
c3d6 <- c3d_read("/Users/thomasyoung/Documents/R/c3d_files/S1C2T1.c3d")
c3d7 <- c3d_read("/Users/thomasyoung/Documents/R/c3d_files/S1C2T2.c3d")
c3d8 <- c3d_read("/Users/thomasyoung/Documents/R/c3d_files/S1C2T3.c3d")
c3d9 <- c3d_read("/Users/thomasyoung/Documents/R/c3d_files/S1C2T4.c3d")
c3d10 <- c3d_read("/Users/thomasyoung/Documents/R/c3d_files/S1C2T5.c3d")

# plot the vertical force vector of the second force platform
plot(
  c3d1$forceplatform[[1]]$forces[, 3],
  xlab = "Recording Frame",
  ylab = "Vertical Force (N)"
)

# Grab Fz from each trial
vertical_grf_data <- list()
c3d_objects <- list(c3d1, c3d2, c3d3, c3d4, c3d5, c3d6, c3d7, c3d8, c3d9, c3d10)

# Loop through each c3d object and extract the vertical ground reaction force
for (i in 1:length(c3d_objects)) {
  c3d_file <- c3d_objects[[i]]
  file_name <- paste0("c3d", i) # Create a name for the list element
  
  if (!is.null(c3d_file$forceplatform) && length(c3d_file$forceplatform) >= 1) {
    vertical_grf_data[[file_name]] <- c3d_file$forceplatform[[1]]$forces[, 3]
  } else {
    vertical_grf_data[[file_name]] <- NULL
    cat("Warning: Force platform data not found or first force platform not available in", file_name, "\n")
  }
}

# Plot commented out, but can use to check 
# for (file_name in names(vertical_grf_data)) {
#   grf_data <- vertical_grf_data[[file_name]]
# 
#   # Check if the data is not NULL (in case a file didn't have the expected data)
#   if (!is.null(grf_data)) {
#     # Create a plot for the current file's GRF data
#     plot(grf_data,
#          type = "l",
#          xlab = "Frames",
#          ylab = "Vertical Force (N)",
#          main = paste("Vertical GRF for", file_name)) 
#   } else {
#     cat("No data to plot for", file_name, "\n")
#   }
# }

# Standardize
body_weight_N <- 779.58
standardized_vertical_grf_data <- list()

# Loop through vertical_grf_data 
for (file_name in names(vertical_grf_data)) {
  grf_data <- vertical_grf_data[[file_name]]
  
  if (!is.null(grf_data)) {
    # Standardize the data by dividing by body weight
    standardized_vertical_grf_data[[file_name]] <- grf_data / body_weight_N
  } else {
    standardized_vertical_grf_data[[file_name]] <- NULL
    cat("No data to standardize for", file_name, "\n")
  }
}

# Plot commented out, but can use to check
# for (file_name in names(standardized_vertical_grf_data)) {
#   grf_data <- standardized_vertical_grf_data[[file_name]]
# 
#   # Check if the data is not NULL
#   if (!is.null(grf_data)) {
#     # Create a plot for the current file's standardized GRF data
#     plot(grf_data,
#          type = "l", # Use lines to connect the data points
#          xlab = "Data point index", # You might want to change this label
#          ylab = "Standardized Vertical Force (Body Weight)", # Updated y-axis label
#          main = paste("Standardized Vertical GRF for", file_name)) # Set the title based on the file name
#   } else {
#     cat("No standardized data to plot for", file_name, "\n")
#   }
# }
target_frames <- 101
normalized_grf_data <- list()

# Loop through the standardized_vertical_grf_data list and normalize each curve
for (file_name in names(standardized_vertical_grf_data)) {
  grf_data <- standardized_vertical_grf_data[[file_name]]
  
  if (!is.null(grf_data) && length(grf_data) > 1) {
    original_frames <- length(grf_data)
    original_time <- seq(0, 1, length.out = original_frames)
    target_time <- seq(0, 1, length.out = target_frames)
    
    normalized_curve <- signal::interp1(original_time, grf_data, target_time, method = "linear")
    
    normalized_grf_data[[file_name]] <- normalized_curve
  } else {
    normalized_grf_data[[file_name]] <- NULL
    cat("Warning: Data not suitable for normalization in", file_name, "\n")
  }
}

max_grf <- max(unlist(normalized_grf_data), na.rm = TRUE)
min_grf <- min(unlist(normalized_grf_data), na.rm = TRUE)
colors <- rainbow(length(normalized_grf_data))

# Plot
first_file <- names(normalized_grf_data)[1]
if (!is.null(normalized_grf_data[[first_file]])) {
  plot(normalized_grf_data[[first_file]],
       type = "l",
       ylim = c(min_grf, max_grf), 
       xlab = "Gait Cycle (%)",
       ylab = "Standardized Vertical Force (Body Weight)",
       main = "Normalized Vertical GRF Curves",
       col = colors[1]) 
}

# Add the remaining normalized curves to the plot
if (length(normalized_grf_data) > 1) {
  for (i in 2:length(normalized_grf_data)) {
    file_name <- names(normalized_grf_data)[i]
    if (!is.null(normalized_grf_data[[file_name]])) {
      lines(normalized_grf_data[[file_name]],
            col = colors[i]) 
    }
  }
}

legend("topright",
       legend = names(normalized_grf_data),
       col = colors,
       lty = 1, 
       cex = 0.8) 

normalized_matrix <- do.call(cbind, normalized_grf_data)
walking_trials <- c("c3d1", "c3d2", "c3d3", "c3d4", "c3d5")
running_trials <- c("c3d6", "c3d7", "c3d8", "c3d9", "c3d10") 

# Extract normalized data for walking and running trials using the explicit names
walking_data <- normalized_matrix[, walking_trials, drop = FALSE] 
running_data <- normalized_matrix[, running_trials, drop = FALSE]

# Calculate the mean and standard deviation for walking
mean_curve_walking <- if (ncol(walking_data) > 0) rowMeans(walking_data, na.rm = TRUE) else numeric(0)
sd_curve_walking <- if (ncol(walking_data) > 0) apply(walking_data, 1, sd, na.rm = TRUE) else numeric(0)
upper_bound_walking <- mean_curve_walking + sd_curve_walking
lower_bound_walking <- mean_curve_walking - sd_curve_walking

# Calculate the mean and standard deviation for running
mean_curve_running <- if (ncol(running_data) > 0) rowMeans(running_data, na.rm = TRUE) else numeric(0)
sd_curve_running <- if (ncol(running_data) > 0) apply(running_data, 1, sd, na.rm = TRUE) else numeric(0)
upper_bound_running <- mean_curve_running + sd_curve_running
lower_bound_running <- mean_curve_running - sd_curve_running

# Find overall min/max across both conditions for consistent y-axis limits
all_data_combined <- c(mean_curve_walking, mean_curve_running, lower_bound_walking, upper_bound_walking, lower_bound_running, upper_bound_running)
min_overall_grf <- min(all_data_combined, na.rm = TRUE)
max_overall_grf <- max(all_data_combined, na.rm = TRUE)

if (length(mean_curve_walking) > 0) {
  plot(mean_curve_walking,
       type = "l",
       ylim = c(min_overall_grf, max_overall_grf), 
       xlab = "Gait Cycle (%)",
       ylab = "Standardized Vertical Force (Body Weight)",
       main = "Ensemble Vertical GRF Curves: Walking vs Running", 
       col = "blue", 
       lwd = 2) 
  
  # Add the standard deviation shaded area for walking
  if (length(upper_bound_walking) > 0 && length(lower_bound_walking) > 0) {
    polygon(c(1:length(mean_curve_walking), rev(1:length(mean_curve_walking))),
            c(upper_bound_walking, rev(lower_bound_walking)),
            col = rgb(0, 0, 1, 0.2),
            border = NA) 
  }
} else if (length(mean_curve_running) > 0) {
  plot(mean_curve_running,
       type = "l",
       ylim = c(min_overall_grf, max_overall_grf), 
       xlab = "Gait Cycle (%)",
       ylab = "Standardized Vertical Force (Body Weight)",
       main = "Ensemble Vertical GRF Curves: Walking vs Running", 
       col = "red", # Red line for running mean
       lwd = 2) # Thicker line for mean curves
} else {
  plot(1, type="n", main="No data to plot", xlab="", ylab="", xlim=c(0,101), ylim=c(0,1))
}

if (length(mean_curve_running) > 0 && length(mean_curve_walking) > 0) { 
  lines(mean_curve_running, col = "red", lwd = 2) 
  
  # Add the standard deviation shaded area for running
  if (length(upper_bound_running) > 0 && length(lower_bound_running) > 0) {
    polygon(c(1:length(mean_curve_running), rev(1:length(mean_curve_running))),
            c(upper_bound_running, rev(lower_bound_running)),
            col = rgb(1, 0, 0, 0.2), 
            border = NA) 
  }
}

legend("topright",
       legend = c("Mean (Walking)", "Mean (Running)"),
       col = c("blue", "red"),
       lty = c(1, 1),
       lwd = c(2, 2),
       fill = c(NA, NA),
       border = c(NA, NA))

combined_data <- cbind(walking_data, running_data)
conditions <- factor(c(rep("Walking", ncol(walking_data)), rep("Running", ncol(running_data))))
spm_data <- t(combined_data)

walking_data_r <- spm_data[conditions == "Walking", , drop = FALSE]
running_data_r <- spm_data[conditions == "Running", , drop = FALSE]

if (nrow(walking_data_r) > 0 && nrow(running_data_r) > 0) {
  
  # Perform the independent two-sample t-test using ttest2
  spm_result_py <- spm1d$stats$ttest2(walking_data_r, running_data_r)
  print(spm_result_py)
  spmi <- spm_result_py$inference(alpha = 0.05)
  print(spmi)
  
} else {
  message("One or both normalized Fz condition arrays are empty. Cannot perform SPM t-test.")
  spm_result_py <- NULL
  spmi <- NULL
}

spmi$plot()
reticulate::py_run_string("import matplotlib.pyplot as plt; plt.show()")

# Get the exact time frames of the significant threshold crossings
if (!is.null(spmi) && !is.null(spmi$clusters)) {
  significant_clusters <- spmi$clusters
  
  if (length(significant_clusters) > 0) {
    message("Significant clusters found:")
    for (i in 1:length(significant_clusters)) {
      cluster <- significant_clusters[[i]]
      
      start_frame <- cluster$endpoints[[1]] + 1 
      end_frame <- cluster$endpoints[[2]] + 1   
      p_value <- cluster$p                     
      
      cat(paste0("  Cluster ", i, ": Frames ", start_frame, " to ", end_frame, " (p = ", p_value, ")\n"))
    }
  } else {
    message("No significant clusters found at the specified alpha level.")
  }
} else if (!is.null(spmi) && !is.null(spmi$intervals) && length(spmi$intervals) > 0) {
  message("Significant intervals found (using 'intervals' attribute):")
  for (i in 1:length(significant_intervals)) {
    interval <- significant_intervals[[i]]
    
    start_frame <- interval[[1]] + 1 # Add 1 for R indexing
    end_frame <- interval[[2]] + 1   # Add 1 for R indexing
    
    cat(paste0("  Interval ", i, ": Frames ", start_frame, " to ", end_frame, "\n"))
  }
  
} else {
  message("Could not access significant cluster or interval information from the spmi object.")
  message("Please check the spm1d documentation for how significant regions are stored in the inference result.")
}
