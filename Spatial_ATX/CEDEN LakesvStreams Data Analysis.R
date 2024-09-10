
# CEDEN Data Analysis

## Looking at the differences of ATX concentratins in Lakes vs Streams

# Load Packages ----

library(tidyverse)
library(here)
library(dplyr)
library(leaflet)
library(lubridate)
library(ggplot2)

# Load data set ----

data <- read_csv("~/PhD_Code/Spatial_ATX/Raw_Data/CEDEN_allyears_ATX.csv")

# Start by filtering out the lab samples 
## They do not have stations, lat/longs

data <- data %>% 
  filter(data$StationName !="Laboratory QA Samples")

# Look at proportion of NAs in each analyte type (Total, dissolved, particulate) ---

proportion_ATXtotal <- data %>% 
  filter(data$Analyte == "Anatoxin-A, Total") %>% 
  group_by(Result_missing = is.na(Result)) %>% 
  summarize(proportion = n()/nrow(.))

proportion_ATXparticulate <- data %>% 
  filter(data$Analyte == "Anatoxin-A, Particulate") %>% 
  group_by(Result_missing = is.na(Result)) %>% 
  summarize(proportion = n()/nrow(.))

proportion_ATXdissolved <- data %>% 
  filter(data$Analyte == "Anatoxin-A, Dissolved") %>% 
  group_by(Result_missing = is.na(Result)) %>%
  summarize(proportion = n()/nrow(.))

# Create a bar plot for each ----

## For total
ggplot(proportion_ATXtotal, aes(x = Result_missing, y = proportion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Result Missing", y = "Proportion") +
  ggtitle("Proportion of Missing Results for Anatoxin-A, Total")

## For particulate
ggplot(proportion_ATXparticulate, aes(x = Result_missing, y = proportion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Result Missing", y = "Proportion") +
  ggtitle("Proportion of Missing Results for Anatoxin-A, Particulate")

## For dissolved
ggplot(proportion_ATXdissolved, aes(x = Result_missing, y = proportion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Result Missing", y = "Proportion") +
  ggtitle("Proportion of Missing Results for Anatoxin-A, Dissolved")


# Filter out rows with NA values in SampleDate
data <- data %>% 
  filter(!is.na(SampleDate))

# Based on results from the NA proportions, ATX will tell us more
## Filter by ATX and filter out anything that is not a river, stream, or creek ----

ATX_total_streams <- data %>% 
  filter(data$Analyte == "Anatoxin-A, Total") %>% 
  filter(grepl("river|creek|stream", StationName, ignore.case = TRUE))


# Filtering out anything that is not a lake

ATX_total_lakes <- data %>% 
  filter(data$Analyte == "Anatoxin-A, Total") %>% 
  filter(!grepl("river|creek|stream", StationName, ignore.case = TRUE))

ATX_proportion <- data %>% 
  mutate(LocationType = case_when(
    grepl("stream|creek|river", StationName, ignore.case = TRUE) ~ "Stream",
    grepl("lake|pond|beach", StationName, ignore.case = TRUE) ~ "Lake",
    TRUE ~ "Other"
  ))

# Extracting year

ATX_proportion$Year <- year(mdy(ATX_proportion$SampleDate))

proportion_results <- ATX_proportion %>% 
  group_by(LocationType, Year) %>% 
  summarize(proportion = n() / nrow(data))

print(proportion_results)

# Graphing proportions based on location type and year ----
proportion_results %>%
  mutate(LocationType = factor(LocationType, 
                               levels = c("Other", "Lake", "Stream"))) %>% 
ggplot(aes(x = LocationType, y = proportion, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Location Type", y = "Proportion of Results", 
       title = "ATX Streams vs Lakes by Year", fill = "") +
  theme_minimal() +
  scale_fill_viridis_d(option = "H") +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))  # Center plot title horizontally

# Save plot to fit in conference poster
ggsave("ggplot.png", plot = last_plot(), width = 10, height = 8, units = "in", dpi = 300)












