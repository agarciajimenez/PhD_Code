########### Most recent script (05/16/2024) ##############

# CEDEN Data Analysis for SFS

# Load Packages ----

library(tidyverse)
library(here)
library(dplyr)
library(leaflet)
library(lubridate)
library(readr)
# Load data set ----
data <- read_csv("~/PhD_Code/Spatial_ATX/Raw_Data/CEDEN_allyears_ATX.csv")

# Start by filtering out the lab samples 
## They do not have stations, lat/longs
data <- data %>% 
  filter(data$StationName !="Laboratory QA Samples")

# Using some of Helen's code to split ecosystems ----
extract_words <- function(input_texts, words_to_extract) {
  selected_words <- character(length(input_texts)) # Create an empty character vector to store selected words for each input text
  
  for (i in seq_along(input_texts)) {
    cleaned_text <- str_replace_all(input_texts[i], "[[:punct:]]", "") # Remove punctuation from each input text
    selected_words[i] <- paste(words_to_extract[words_to_extract %in% strsplit(cleaned_text, " ")[[1]]], collapse = " ") # Extract and concatenate selected words
  }
  
  return(selected_words)
}

# Define the words we want to extract
words_to_extract <- c("Dam", "Pond", "Lake", "Lake,", "River","River,", 
                      "Slough", "Lagoon", "Creek", "Creek,", "Mid-slough", 
                      "Bay", "Reservoir", "Estuary", "Channel", "Canal")  


# Extract words from the "StationName" column
selected_words <- extract_words(data$StationName, words_to_extract)

# Add the selected words as a new column to the dataset, if needed
data$SelectedWords <- selected_words

## Filtering out marine ecosystems ----

# Define marine ecosystem keywords ----
marine_keywords <- c("ocean", "sea", "marine", "coastal", "estuary",
                     "channel", "canal", "bay")

benthic_data <-  data %>%
  filter(!grepl(paste(marine_keywords, collapse = "|"), SelectedWords, 
                ignore.case = TRUE)) %>% 
  filter(is.na(Result) | Result >= 0)

# For this analysis, we are only looking at ATX-total ----
## Anatoxin-Particulate: only two samples had results
## Anatoxin-dissolved results were negative
## This portion of the code does not include any lake or stream that had non-detects

anatoxin_total_data <- benthic_data %>%
  filter(Analyte == "Anatoxin-A, Total", Result >= 0)

# Extracting location type and year

anatoxin_data <- anatoxin_total_data %>% 
  mutate(LocationType = case_when(
  grepl("stream|creek|river", SelectedWords, ignore.case = TRUE) ~ "Stream",
  grepl("lake|pond|beach", SelectedWords, ignore.case = TRUE) ~ "Lake")) %>% 
  mutate(SampleDate = as.Date(SampleDate, format = "%m/%d/%Y")) %>% 
  mutate(Year = lubridate::year(SampleDate))

# Calculating the mean results per location type and year ----

mean_results <- anatoxin_data %>% 
  filter(!is.na(LocationType)) %>% 
  group_by(LocationType, Year) %>% 
  summarise(mean_Result = mean(Result, na.rm = TRUE)) %>% 
  ungroup()

# Plotting results with a log-scale ----

ggplot(mean_results, aes(x= Year, y=mean_Result, color = LocationType)) +
  geom_line(linewidth = 1.3) +
  labs(x = "Year", y = "ATX Concentration (ug/L)",
       title = " Mean Anatoxin-A Concentrations by Year") +
  scale_y_log10() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18))

# Save plot to fit in conference poster

ggsave("meanATXresults.png", 
       plot = last_plot(), 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

# Plotting collection methods ----

# Group by CollectionMethod, LocationType, and count occurrences
collection_method <- anatoxin_data %>%
  filter(!is.na(LocationType)) %>% 
  group_by(CollectionMethodName, LocationType) %>%
  summarise(count = n()) %>%
  ungroup()

# Plotting
ggplot(collection_method, aes(x = CollectionMethodName, y = count, fill = LocationType)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Collection Method", y = "Count", 
       title = "Collection Methods") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18))

# Save plot

ggsave("collectionmethod.png", 
       plot = last_plot(), 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)





