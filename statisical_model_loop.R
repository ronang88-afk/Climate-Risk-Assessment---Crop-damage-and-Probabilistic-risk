### Final QGIS H*E*V = risk run through 

### CUMULATIVE SUM : 

# Packages: 
library(POT)
library(extRemes)
library(data.table)

output_file <- "scenario_outputs_2.txt"
sink(output_file, append = TRUE, split = TRUE) # Split = true means it appears both in the console and on txt

# Data scenario 
Scenario_1 <- "Historical Data (1971-2024)" 
Scenario_2 <- "RCP 2.6 (2025-2080)" 
Scenario_3 <- "RCP 4.5 (2025-2080)" 
Scenario_4 <- "RCP 8.5 (2025-2080)" 

# File name 
Data_1 <- "Combined_Historical_Data_1971-2024.csv"
Data_2 <- "Combined_RCP2.6_2025-2080.csv"
Data_3 <- "Combined_RCP4.5_2025-2080.csv"
Data_4 <- "Combined_RCP8.5_2025-2080.csv"
#Combined_Historical_Data_1971-2024.csv
#Combined_RCP2.6_2025-2080.csv
#Combined_RCP4.5_2025-2080.csv
#Combined_RCP8.5_2025-2080.csv
Data_1 <- read.csv("Combined_Historical_Data_1971-2024.csv")
Data_2 <- read.csv("Combined_RCP2.6_2025-2080.csv")
Data_3 <- read.csv("Combined_RCP4.5_2025-2080.csv")
Data_4 <- read.csv("Combined_RCP8.5_2025-2080.csv")


scenarios_dt <- data.table(
  Scenario = c("Historical Data (1971-2024)",
               "RCP 2.6 (2025-2080)",
               "RCP 4.5 (2025-2080)",
               "RCP 8.5 (2025-2080)"),
  File_Name = c("Combined_Historical_Data_1971-2024.csv",
                "Combined_RCP2.6_2025-2080.csv",
                "Combined_RCP4.5_2025-2080.csv",
                "Combined_RCP8.5_2025-2080.csv"),
  Data = list(Data_1, Data_2, Data_3, Data_4),
  Period = c("1971-2024", "2025-2080", "2025-2080", "2025-2080"),
  anchored_threshold = c(47, 47, 47, 47),
  diagnostic_optimal_threshold = c(47, 50, 46, 63)
)

scenarios_dt$Data <- lapply(scenarios_dt$Data, function(df) {
  df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
  return(df)
})
scenarios_dt
str(scenarios_dt$Data)

#setwd("F:\1 - Thesis\1 - Results")

#plan
#1. create a list with each scenario 
# 2. create a list with each threshold for each scenario 
# try combine the two -> or make two iterations -> each iterating through each list 
GPD_results_list <- list()
current_RP_list <- list()
return_periods <- c(10, 20, 50, 100, 250, 500, 1000) 

### Threshold lists -> select one per run i.e either historic vs diagnostic  

for ( i in 1:nrow(scenarios_dt)) {  # iterate through the 4 rows in dt 
  current_data <- scenarios_dt$Data[[i]] # get the df for the current scenario 
  current_scenario <- scenarios_dt$Scenario[i] # egt scenario name for logs 
  precip_threshold <- scenarios_dt$anchored_threshold[i]  # get GPD u for current scenario 
  #current_thresh <- historic_baseline_threshold
  # Convert date column from chr to num 
  #data$Date <- as.Date(data$Date)  # wrong 
  print(paste("Analysis for the scenario:", current_scenario, "---"))
  # check for NAs
  if (any(is.na(current_data$Date))) {
    warning("NA's added when converting the date. Check OG format")
  }
  # remove the rows with NAs 
  current_data <- current_data[!is.na(current_data$Date), ]
  
  # Calculate the data timespan 
  start_date <- min(current_data$Date, na.rm = TRUE)
  end_date <- max(current_data$Date, na.rm = TRUE)
  
  # Calc total years 
  total_years <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25 ########### do i change to match time duration?  
  print(paste("Duration of the data", round(total_years, 2), "years"))
  
  # make a numeric variable for the modelling, in days since 1970-01-01
  current_data$TimeNumeric <-as.numeric(current_data$Date)
  
  ## Declustering process, ensuring the exceedences are independant to meet model requirements 
  print("Declustering")
  
  # defining the time window. For Crop damage analysis a 3 day precip event maxima should be the best fit. For residential analysis it could change.. 
  time_condition_days <- 3
  
  # Preliminary threshold selection , set at 90th percentile of WET days ATM, can be changed. 
  prelim_precip_thresh <- quantile(current_data$Precipitation_mm_day[current_data$Precipitation_mm_day > 1e-6], 0.90, na.rm = TRUE)  # >1e-6] could be removed to reduce bias but WET day modeling 
  print(paste("Preliminary Precipitation Threshold:", round(prelim_precip_thresh, 2), "mm"))
  # number might vary a bit but mathematically correct 
  
  
  # Prep the DF for the clust function 
  # Time numeric date and obs labelled pr data is nessesary 
  precip_data_for_clust <- data.frame(
    time = current_data$TimeNumeric, 
    obs = current_data$Precipitation_mm_day
  )
  
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
  
  print("Plots for Selecting the threshold")
  
  # The maxima being modelled: 
  precip_maxima <- precip_clusters$obs   # now its cumulative and not maxima 
  
  # ERxceedances plot vs threshold (u)
  # Using this to highlight what thresholds retain enough data for my model to remain stable 
  min_thresh <- floor(prelim_precip_thresh)
  max_thresh <- ceiling(max(precip_maxima))
  threshold_candidates <- seq(from = min_thresh, to = max_thresh, by = 1)
  exceedance_counts <- sapply(threshold_candidates, function(thr) sum(precip_maxima > thr))
  
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
  #plot(final_gpd_fit)
  print(final_gpd_fit)
  
  # Return Levels: 
  print("estimating return levels")
  return_periods <- c(10, 20, 50, 100, 250, 500, 1000)    # can be used for plotting the RPs with confidence intevals in results 
  #return_periods <- c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
  #return_periods <- c(2, 5, 10, 2  5, 50, 100, 250, 500, 1000)
  # The return levels for a non stationary model will change over time, and so are calculated for the "end" of the projected period
  # this represents future risk 
  # on the other hand for a stationary model they stay consistent
  if (is_stationary) {
    return_levels <- ci(final_gpd_fit, return.period = return_periods)
  } else {
    # Calculate return levels for the end of the time series.
    end_time_scaled <- max(precip_clusters$time_scaled)
    end_time_value <- max(precip_clusters$time)
    return_levels <- ci(final_gpd_fit,
                        return.period = return_periods,
                        t_cov_val = data.frame(time = end_time_scaled))
  }
  
  # What the ci function does differently is it gives a matrix with an estimate, as well as an upper and lower bound at the 95th 
  # Using the main estimate going forward: 
  return_level_est <- return_levels[, "Estimate"]
  names(return_level_est) <- paste0(return_periods, "-year")
  
  print(paste("--- Estimated Return Levels for", current_scenario, "---"))
  print(round(return_level_est, 2))
  
  # results storage
  GPD_results_list[[i]] <- list(scenario = current_scenario, 
                                threshold = precip_threshold, 
                                Return_levels = return_levels,
                                Fit = final_gpd_fit)
  
  # change of plan -> make a list like for the GPD -> try exctract directly from here into something i can reiterate into 
  current_RP_list[[i]] <- list(scenario = current_scenario,
                               Return_levels = return_levels
  )
  
  ## add storage for the RL data -> store in a data.table and then add it to the list 
  # elements include -> scenario, rp, rl , lower bound CI, upper b CI, thresh and stationary or not 
  # for model type use ifelse with similar operation than in xl 
  current_scenario_results <- data.table(
    scenario = current_scenario, 
    Return_period = return_periods,
    Return_level = return_level_est,
    Lower_bound_CI = return_levels[, "95% lower CI"],
    Upper_bound_CI = return_levels[, "95% upper CI"],
    Threshold = precip_threshold, 
    Model_type = ifelse(is_stationary, "Stationary", "Non-Stationary")
  )
  # save to GPD results 
  GPD_results_list[[i]]$results_table <- current_scenario_results
}

# stop feeding into the txt file 
sink()
#GPD_results_list





