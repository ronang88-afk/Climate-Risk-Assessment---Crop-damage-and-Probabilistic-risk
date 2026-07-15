### CUMULATIVE SUM : 

## Cleaned version of my GPD model (Without all the notes): 

# Data scenario 
#Scenario_NAME <- "Historical Data (1971-2024)" 
#Scenario_NAME <- "RCP 2.6 (2025-2080)" 
#Scenario_NAME <- "RCP 4.5 (2025-2080)" 
Scenario_NAME <- "RCP 8.5 (2025-2080)" 

# File name 
#Data_FILE <- "Combined_Historical_Data_1971-2024.csv"
#Data_FILE <- "Combined_RCP2.6_2025-2080.csv"
#Data_FILE <- "Combined_RCP4.5_2025-2080.csv"
Data_FILE <- "Combined_RCP8.5_2025-2080.csv"
#Combined_Historical_Data_1971-2024.csv
#Combined_RCP2.6_2025-2080.csv
#Combined_RCP4.5_2025-2080.csv
#Combined_RCP8.5_2025-2080.csv

precip_threshold <- 50
#setwd("F:\1 - Thesis\1 - Results")

# Packages: 
library(POT)
library(extRemes)

# Data Preparation: 
print(paste("Analysis for the scenario:", Scenario_NAME, "---"))

data <- read.csv(Data_FILE)

# Convert date column from chr to num 
#data$Date <- as.Date(data$Date)  # wrong 
data$Date <- as.Date(data$Date) #, format="%d/%m/%Y")

# Convert date column from chr to num 
#data$Date <- as.Date(data$Date)  # wrong 

# check for NAs
if (any(is.na(data$Date))) {
  warning("NA's added when converting the date. Check OG format")
}

# remove the rows with NAs 
data <- data[!is.na(data$Date), ]

# Calculate the data timespan 
start_date <- min(data$Date, na.rm = TRUE)
end_date <- max(data$Date, na.rm = TRUE)

# Calc total years 
total_years <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.5 ########### change to match time duration 
print(paste("Duration of the data", round(total_years, 2), "years"))

# make a numeric variable for the modelling, in days since 1970-01-01
data$TimeNumeric <-as.numeric(data$Date)

## Declustering process, ensuring the exceedences are independant to meet model requirements 
print("Declustering") 

# defining the time window. For Crop damage analysis a 3 day precip event maxima should be the best fit. For residential analysis it could change.. 
time_condition_days <- 3

# Preliminary threshold selection , set at 90th percentile of WET days ATM, can be changed. 
prelim_precip_thresh <- quantile(data$Precipitation_mm_day[data$Precipitation_mm_day > 1e-6], 0.90, na.rm = TRUE)  # >1e-6] could be removed to reduce bias but WET day modeling 
print(paste("Preliminary Precipitation Threshold:", round(prelim_precip_thresh, 2), "mm"))
# number might vary a bit but mathematically correct 


# Prep the DF for the clust function 
# Time numeric date and obs labelled pr data is nessesary 
precip_data_for_clust <- data.frame(
  time = data$TimeNumeric, 
  obs = data$Precipitation_mm_day
)

###
# DECLUSTERING : 
###
#In this section i have kept the old code which declusters the data taking the largest value from each cluster selected as the exceedance being modelled
# this option came before improvements with runoff coefficients in the MC sim which makes the waterlogging dynamics more realistic
# if too complicated or gives odd results -> use old verion 

# output is a matrix of the max values in each cluster 
#precip_cluster_matrix <- clust(precip_data_for_clust, 
#                               u = prelim_precip_thresh, 
#                               time.cond = time_condition_days, 
#                               clust.max = TRUE, 
#                               plot = FALSE)  # can make a better plot later 
# convert matrix to df to help handling 
#precip_clusters <-as.data.frame(precip_cluster_matrix)
#print(paste("Number of Clusters Found: ", nrow(precip_clusters)))

###
### Declustering with integrated cunulative sum: 
#by setting the clust.max to false i get a matrix where Col1 = the clust ID and Col2 = raw index of the extreme event in the df 
cluster_info <- clust(precip_data_for_clust, 
                      u = prelim_precip_thresh, 
                      time.cond = time_condition_days,
                      clust.max = FALSE,
                      plot = FALSE)

# Setting the clust.max as false has given me a list of matrices 
# where each element is a seperate cluster (matrix)
# each has a row with time and obs (pr) 
cumulative_precip <- sapply(cluster_info, function(cluster) {
  sum(cluster["obs", ]) # summing all the pr values in the cluster
})

# I also am going to take the time of the peak event for each cluster 
# this i can use for the ts 
# make sure to take the whole list using , ] ... 
peak_times <- sapply(cluster_info, function(cluster) {
  max_idx <- which.max(cluster["obs", ]) 
  cluster["time", max_idx]
})

# create a new df 
precip_clusters <- data.frame(
  time = peak_times,
  obs = cumulative_precip
)

# Stationarity Analysis: 
print("Stationarity Analysis")

# Checking for changes in extremes over time : Event magnitude vs time 
plot(precip_clusters$time, precip_clusters$obs,
     main = paste(Scenario_NAME, ": Trend in Extreme Precipitation"),
     xlab = "Time (Numeric Days since 1970-01-01)",
     ylab = "Cumulative Precipitation (mm)",
     pch = 19, 
     col = "#0072B2", 
     cex = 0.8)
grid()
abline(lm(obs ~ time, data = precip_clusters), col = "red", lwd = 2)
legend("topleft", legend = "Linear Trend", col = "red", lwd = 2, bty = "n")



##### Selecting the THRESHOLD: 
print("Plots for Selecting the threshold")

# The maxima being modelled: 
precip_maxima <- precip_clusters$obs   # now its cumulative and not maxima 

# ERxceedances plot vs threshold (u)
# Using this to highlight what thresholds retain enough data for my model to remain stable 
min_thresh <- floor(prelim_precip_thresh)
max_thresh <- ceiling(max(precip_maxima))
threshold_candidates <- seq(from = min_thresh, to = max_thresh, by = 1)
exceedance_counts <- sapply(threshold_candidates, function(thr) sum(precip_maxima > thr))
# PLot hist: 
plot(threshold_candidates, exceedance_counts, type = 'b', pch = 19,
     main = paste(Scenario_NAME, ": Exceedance Count vs. Threshold"),
     xlab = "Potential GPD Threshold (mm)", ylab = "Number of Exceedances")
grid()
abline(v=100, col='red') 

## MRL plot: 
# Looking for when the plot becomes linear 
mrlplot(precip_maxima, main = paste(Scenario_NAME, ": Mean Residual Life Plot"))
abline(v=55, col='red') 

# Threshold Stability Plots 
par(mfrow = c(1, 1))
tcplot(precip_maxima, which = 1)
title(main = "Scale Parameter Plot:", Scenario_NAME)
abline(v=47, col='red') 

tcplot(precip_maxima, which = 2)
title(main = "Shape Parameter Plot", Scenario_NAME)
abline(v=68, col='red') 
#par(mfrow = c(1, 1))

# Choosing the final Threshold: 
#1. Where is the MRL plot linear? 
#2. Where are the shape and scale parameters in the tc plots stable? (without wide fluctuations)
#2. are there enough exceedances? (aiming for 50-100 for a good fit)

##
#precip_threshold <- 47  ####################
##
print(paste("Final Threshold Selected:", precip_threshold, "mm"))
sum(precip_maxima > precip_threshold) 

####
#Fitting the GPD model: 
# (Stationary vs Non stationary)

# Model 1 = Stationary 
# This assumes that the risk is constant over time 
gpd_stationary <- fevd(x = precip_clusters$obs, 
                       threshold = precip_threshold, 
                       type = "GP", 
                       units = "mm")

# Model 2 = Non stationary 
# This one allows the Parameters of the GPD to vary with the time. 
# The scale parameter in particular is modeled as a function of time... where scale(t) = scale_0 + scale_1 * t 
# because the obs is in days since 1970 and the non-stationary model is sensitive to bcos small scale is multiplied by large t in σ(t) = σ0 + σ1 . t 
# Nan was produced as it multiplied it into very small σ, so necessary to change it into change in scale per 1 sd of time 
precip_clusters$time_scaled <- scale(precip_clusters$time)

gpd_nonstationary <- fevd(
  x = precip_clusters$obs,
  data = precip_clusters,
  threshold = precip_threshold,
  scale.fun = ~ time_scaled, 
  type = "GP", 
  units = "mm"
)

# Comparing the models: 
# Likelihood ratio test: A small p value (<0.05) will mean that the non stationary model is a better fit and should be used 
# this test compares the goodness of fit of both models, by maximizing the parameter space on one end and imposing a constraint on another end based on the ratio of likelihood 
lr_test <- lr.test(gpd_stationary, gpd_nonstationary)
print("Model comparison results:")
print(lr_test)

# Automated model choice based on test results: 
if(lr_test$p.value<0.05){
  print("Non stationary model is the better fit. Chosen for analysis")
  final_gpd_fit <- gpd_nonstationary
  is_stationary <- FALSE
} else{
  print("Stationary model is sufficient. Chosen for analysis")
  final_gpd_fit <- gpd_stationary
  is_stationary <- TRUE
}
# diagnostic plots: 
plot(final_gpd_fit)
print(final_gpd_fit)

# Return Levels: 
print("estimating return levels")
return_periods <- c(10, 20, 50, 100, 250, 500, 1000)    # can be used for plotting the RPs with confidence intevals in results 
#return_periods <- c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
#return_periods <- c(2, 5, 10, 25, 50, 100, 250, 500, 1000)
# The return levels for a non stationary model will change over time, and so are calculated for the "end" of the projected period
# this represents future risk 
# on the other hand for a stationary model they stay consistent
if (is_stationary) {
  return_levels <- ci(final_gpd_fit, return.period = return_periods)
} else {
  # Calculate return levels for the end of the time series.
  end_time_scaled <- max(precip_clusters$time_scaled)
  return_levels <- ci(final_gpd_fit,
                      return.period = return_periods,
                      t_cov_val = data.frame(time = end_time_scaled))
}

# What the ci function does differently is it gives a matrix with an estimate, as well as an upper and lower bound at the 95th 
# Using the main estimate going forward: 
return_level_est <- return_levels[, "Estimate"]
names(return_level_est) <- paste0(return_periods, "-year")
# ration for cumulative pr / the maxima #
cluster_maxima <- sapply(cluster_info, function(cluster) {
  max(cluster["obs", ])
})
ratio2 <- round(mean(cumulative_precip/cluster_maxima), 2)

# end 
print(paste("--- Estimated Return Levels for", Scenario_NAME, "---"))
print(round(return_level_est, 2))

######
# Crop Risk Quantification: 
print("calculating crop risk")



