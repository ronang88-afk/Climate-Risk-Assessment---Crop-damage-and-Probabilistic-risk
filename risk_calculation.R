library(dplyr)
library(ggplot2)
library(scales)


price <- read.csv("crop_prices.csv")
yield <- read.csv("YPH_statsline.csv")
dd_func <- read.csv("jrc dd functions.csv")


Fhist_100y <- read.csv("FHist_100year.csv")
FRCP2.6 <- read.csv("FRCP2.6_100yearRL.csv")
FRCP4.5 <- read.csv("FRCP4.5_100yearRL.csv")
FRCP8.5 <- read.csv("FRCP8.5_100yearRL.csv")
FRCP8.5_1k <- read.csv("FRCP8.5_1000yrRL.csv")

df2 <- read.csv("price and yield.csv")


calculate_jrc_damage <- function(depth_vector, dd_df) {
  approx(x = dd_df$depth, y = dd_df$damage.., xout = depth_vector, rule = 2)$y
}


process_scenario <- function(df, scenario_name) {
  df$price_per_tonne <- df2$price_per_tonne
  df$yield_t_per_m2 <- df2$yield_t_per_m2
  
  df <- df %>%
    mutate(mean_filt = ifelse(mean > 0.211, 0.211, 
                              ifelse(mean < 0, 0, mean)))
  
  df$JRC_Damage <- calculate_jrc_damage(df$mean_filt, dd_func)
  
  df <- df %>%
    mutate(
      v = JRC_Damage,
      e = count_pix * 4,
      E = e * price_per_tonne * yield_t_per_m2,
      V = JRC_Damage,
      final_risk = 0.01 * (as.numeric(E) * as.numeric(V)),
      final_risk_per_plot = 0.01 * (count_pix * 4 * price_per_tonne * yield_t_per_m2) * JRC_Damage,
      Scenario = scenario_name
    )
  
  return(df)
}

scenarios <- list(
  "Historical" = Fhist_100y,
  "RCP 2.6" = FRCP2.6,
  "RCP 4.5" = FRCP4.5,
  "RCP 8.5" = FRCP8.5,
  "RCP 8.5 1000yr" = FRCP8.5_1k
)

all_results <- list()
summary_list <- list()

for (name in names(scenarios)) {
  cat("Processing:", name, "\n")
  
  df_processed <- process_scenario(scenarios[[name]], name)
  
  all_results[[name]] <- df_processed
  
  summary_list[[name]] <- df_processed %>%
    group_by(gewas) %>%
    summarise(
      Scenario = name,
      Total_Risk_Euro = sum(final_risk, na.rm = TRUE),
      Total_Risk_per_Plot_Euro = sum(final_risk_per_plot, na.rm = TRUE),
      Mean_Flood_Depth = mean(mean_filt, na.rm = TRUE),
      Mean_JRC_Damage = mean(JRC_Damage, na.rm = TRUE),
      Number_of_Plots = n(),
      Total_Area_ha = sum(count_pix * 4 / 10000, na.rm = TRUE)
    )
}

final_results <- do.call(rbind, all_results)
final_summary <- do.call(rbind, summary_list)


summary_table_clean <- final_summary %>%
  mutate(crop_en = case_when(
    #  grepl  - potato name fucking me over - should have cleaned at the start 
    # hard-corrected yph & price added form the start
    grepl("Aardappel", gewas, ignore.case = TRUE) ~ "Potatoes",
    
    gewas == "Bieten, suiker-" | grepl("Bieten.*suiker", gewas) ~ "Sugar Beets",
    
    grepl("Tarwe", gewas, ignore.case = TRUE) ~ "Wheat",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(crop_en)) %>%
  group_by(Scenario, crop_en) %>%
  summarise(
    Total_Risk_Euro = sum(Total_Risk_Euro, na.rm = TRUE),
    Total_Risk_per_Plot_Euro = sum(Total_Risk_per_Plot_Euro, na.rm = TRUE),
    Mean_Flood_Depth = mean(Mean_Flood_Depth, na.rm = TRUE),
    Number_of_Plots = sum(Number_of_Plots, na.rm = TRUE),
    Total_Area_ha = sum(Total_Area_ha, na.rm = TRUE),
    .groups = "drop"
  )

print("Final Risk Summary:")
print(summary_table_clean)


stats_for_text <- summary_table_clean %>%
  group_by(crop_en) %>%
  mutate(
    Hist_Val = first(Total_Risk_Euro[Scenario == "Historical"]),
    Pct_Increase = (Total_Risk_Euro - Hist_Val) / Hist_Val * 100
  ) %>%
  select(-Hist_Val)

print("Percentage Increase vs Historical:")
print(stats_for_text)