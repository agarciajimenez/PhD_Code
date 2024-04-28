########### Most recent script (02/04/24) ##############

# CEDEN Data Analysis for SFS

# Load Packages ----

library(tidyverse)
library(here)
library(dplyr)
library(leaflet)
library(lubridate)
library(readr)
# Load data set ----
data <- read_csv("CEDEN_allyears_ATX.csv")

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

# Define marine ecosystem keywords
marine_keywords <- c("ocean", "sea", "marine", "coastal", "estuary",
                     "channel", "canal", "bay")

benthic_data <-  data %>%
  filter(!grepl(paste(marine_keywords, collapse = "|"), SelectedWords, 
                ignore.case = TRUE)) %>% 
  filter(is.na(Result) | Result >= 0)


# Look at proportion of NDs in each ecosystem ---
## ND is non-detect

# Proportion of detection in Streams ----
streams_proportion <- benthic_data %>% 
  filter(benthic_data$Analyte == "Anatoxin-A, Total") %>% 
  filter(grepl("river|creek|stream", SelectedWords, ignore.case = TRUE)) %>%
  filter(ResultQualCode %in% c("ND", "=")) %>% 
  group_by(ResultQualCode) %>% 
  summarize(proportion = n()/nrow(.))

# Plotted results
ggplot(streams_proportion, aes(x = ResultQualCode, y = proportion, fill = ResultQualCode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Detection", y = "Proportion  of ATX Detected", 
       title = "Proportion of Total Anatoxin-A Detected in Streams",
       fill = "") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Detected", "Not Detected")) +
  theme_minimal()


# Proportion of detection in Lakes ----
lakes_proportion <- benthic_data %>% 
  filter(benthic_data$Analyte == "Anatoxin-A, Total") %>% 
  filter(!grepl("river|creek|stream", StationName, ignore.case = TRUE)) %>% 
  filter(ResultQualCode %in% c("ND", "=")) %>% 
  group_by(ResultQualCode) %>% 
  summarize(proportion = n()/nrow(.))

# Plotting Lakes results
ggplot(lakes_proportion, aes(x = ResultQualCode, y = proportion, fill = ResultQualCode)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Detection", y = "Proportion  of ATX Detected", 
       title = "Proportion of Total Anatoxin-A Detected in Lakes",
       fill = "") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Detected", "Not Detected")) +
  theme_minimal()


# Select columns to "shorten" data variables ----
selected_columns <- c("StationName", "Analyte", "SampleDate", "MethodName",
                      "Result", "ResultQualCode", "CollectionMethodName",
                      "Latitude", "Longitude", "SelectedWords")

# Use select() to choose specific columns
selected_data <- benthic_data %>%
  select(all_of(selected_columns))

# Calculate counts of each category under the "MethodName" column for "Anatoxin-A, Total" analyte ----
method_counts_anatoxin <- selected_data %>%
  filter(Analyte == "Anatoxin-A, Total") %>%
  group_by(MethodName) %>%
  summarize(count = n())

# Calculate total count of observations for "Anatoxin-A, Total" analyte
total_count_method <- nrow(selected_data[selected_data$Analyte == "Anatoxin-A, Total", ])

# Calculate percentage of each category
method_counts_anatoxin <- method_counts_anatoxin %>%
  mutate(percentage = (count / total_count_method) * 100)

# View the counts and percentage for "Anatoxin-A, Total" analyte
print(method_counts_anatoxin)


# Finding total lakes and streams  ----
counts <- selected_data %>% 
  mutate(LocationType = case_when(
    grepl("stream|creek|river", SelectedWords, ignore.case = TRUE) ~ "Stream",
    grepl("lake|pond|beach", SelectedWords, ignore.case = TRUE) ~ "Lake"))

# Filter anything that does not fall within stream or lake
counts_total <- counts %>% 
  filter(counts$LocationType != "NA")

# Count occurences of lakes and streams in location type
lake_count <- sum(grepl("lake", counts_total$LocationType, ignore.case = TRUE))
stream_count <- sum(grepl("stream", counts_total$LocationType, ignore.case = TRUE))

# Print the counts
cat("Total number of lakes:", lake_count, "\n")
cat("Total number of streams:", stream_count, "\n")


###### Look at detection in streams vs lakes ##### ----
## Start by filtering ND values from data ----


detections <- selected_data %>% 
  filter(Analyte == "Anatoxin-A, Total" & ResultQualCode != "ND") %>%
  mutate(LocationType = case_when(
    grepl("stream|creek|river", SelectedWords, ignore.case = TRUE) ~ "Stream",
    grepl("lake|pond|beach", SelectedWords, ignore.case = TRUE) ~ "Lake"
  ))
head(detections)

ATX_counts <- detections %>% 
  mutate(LocationType = case_when(
    grepl("stream|creek|river", SelectedWords, ignore.case = TRUE) ~ "Stream",
    grepl("lake|pond|beach", SelectedWords, ignore.case = TRUE) ~ "Lake",
    TRUE ~ "Other"
  )) %>% 
  filter(LocationType != "Other")


### Calculating results above thresholds ----
stream_only <- ATX_counts %>% 
  filter(LocationType == "Stream")

recreation_threshold <- stream_only %>% 
  filter(Result > 90)

percent_recreation_threshold <- (nrow(recreation_threshold) / nrow(stream_only)) * 100
cat("Percent of streams with Result >90:", round(percent_recreation_threshold, 2), "%\n")

dog_threshold <- stream_only %>% 
  filter(Result > 100)
percent_dog_threshold <- (nrow(dog_threshold) / nrow(stream_only)) * 100
cat("Percent of streams with Result >100:", round(percent_dog_threshold, 2), "%\n")

# Calculate range and median for lakes ----
lakes_result_range <- range(ATX_counts$Result[ATX_counts$LocationType == "Lake"], na.rm = TRUE)
lakes_result_median <- median(ATX_counts$Result[ATX_counts$LocationType == "Lake"], na.rm = TRUE)
cat("Range of Anatoxin-A result in Lakes:", lakes_result_range[1], "-", lakes_result_range[2], "\n")
cat("Median of Anatoxin-A result in Lakes:", lakes_result_median, "\n")

# Calculate range and median for streams ----
streams_result_range <- range(ATX_counts$Result[ATX_counts$LocationType == "Stream"], na.rm = TRUE)
streams_result_median <- median(ATX_counts$Result[ATX_counts$LocationType == "Stream"], na.rm = TRUE)
cat("Range of Anatoxin-A result in Streams:", streams_result_range[1], "-", streams_result_range[2], "\n")
cat("Median of Anatoxin-A result in Streams:", streams_result_median, "\n")

# Calculating max results for streams to see what collection method was used
max_index <- which.max(stream_only$Result)
max_result <- stream_only[max_index, ]
print(max_result)

# Calculating max result for lakes
max_lake <- which.max(lake_only$Result)
max_result_lake <- lake_only[max_lake, ]
print(max_result_lake)

# Calculating percent detections 
total_lakes <- sum(counts_total$LocationType == "Lake")
total_streams <- sum(counts_total$LocationType == "Stream")

# Total detections in lakes and streams
detections_lakes <- sum(ATX_counts$LocationType == "Lake")
detections_streams <- sum(ATX_counts$LocationType == "Stream")

# Percentage of detections in lakes and streams
percentage_lakes <- (detections_lakes / total_lakes) * 100
percentage_streams <- (detections_streams / total_streams) * 100

# Print the results
cat("Percentage of detections in lakes:", round(percentage_lakes, 2), "%\n")
cat("Percentage of detections in streams:", round(percentage_streams, 2), "%\n")

####### Plotting detected counts ATX based on location ###### ----
ggplot(ATX_counts, aes(x = LocationType, fill = LocationType)) +
  geom_bar(fill = c("Lakes" = "skyblue", "Streams" = "orange")) +
  scale_fill_manual(values = c("Lakes" = "skyblue", "Streams" = "orange"),
                    labels = c("Lakes", "Streams")) +
  labs(x = "Location Type", y = "Count Detected", 
       title = "Count of Anatoxin-A Detection by Location Type") +
  theme_minimal()

# Making the counts into proportions ----
ATX_proportion <- ATX_counts %>%
  group_by(LocationType) %>%
  summarize(Proportion = n() / nrow(ATX_counts))

# Plotting detected ATX based on location
ggplot(ATX_proportion, aes(x = LocationType, y = Proportion, fill = LocationType)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Lake" = "skyblue", "Stream" = "orange"), labels = c("Lakes", "Streams")) +
  labs(x = "Location Type", y = "Proportion Detected", 
       title = "Proportion of Anatoxin-A Detection by Location Type") +
  theme_minimal()

####### Splitting into collection methods ######## ----
detections_by_method <- detections %>%
  group_by(CollectionMethodName) %>%
  summarize(detections = n())

# Plotting detections by CollectionMethodName
ggplot(detections_by_method, aes(x = CollectionMethodName, y = detections, fill = CollectionMethodName)) +
  geom_bar(stat = "identity") +
  labs(x = "Collection Method", y = "Number of Detections", 
       title = "Number of Anatoxin-A Detections by Collection Method") +
  theme_minimal()

# Splitting into collection methods, "Stream", and "Lake" categories and calculating detection percentage
detections_by_method_location <- detections %>%
  group_by(CollectionMethodName, LocationType) %>%
  summarize(detections = n())

# Calculate total number of detections per location type
total_detections <- detections_by_method_location %>%
  group_by(LocationType) %>%
  summarize(total_detections = sum(detections))

# Calculate detection percentage for each collection method and location type
detections_by_method_location <- detections_by_method_location %>%
  left_join(total_detections, by = "LocationType") %>%
  mutate(detection_percentage = detections / total_detections * 100)

# Plotting detection percentage by CollectionMethodName and LocationType
ggplot(detections_by_method_location, aes(x = CollectionMethodName, y = detection_percentage, fill = CollectionMethodName)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~LocationType) +  # Separate plots for Stream and Lake
  labs(x = "Collection Method", y = "Detection Percentage", 
       title = "Anatoxin-A Detection Percentage by Collection Method and Location Type") +
  theme_minimal()

# Calculating percentage of collection method in streams----
stream_detections_by_method <- stream_only %>%
  group_by(CollectionMethodName) %>%
  summarize(detections = n())

total_detections_streams <- nrow(stream_only)

stream_detections_by_method <- stream_detections_by_method %>%
  mutate(percentage = (detections / total_detections_streams) * 100)

print(stream_detections_by_method)

# Calculate percentage of collection method in lakes ----
lake_only <- ATX_counts %>% 
  filter(LocationType == "Lake")

lake_by_collection_method <- lake_only %>% 
  group_by(CollectionMethodName) %>% 
  summarize(detections = n())

total_detections_lakes <- nrow(lake_only)

lake_only_by_method <- lake_by_collection_method %>% 
  mutate(percentage = (detections / total_detections_lakes)* 100)

print(lake_only_by_method)
