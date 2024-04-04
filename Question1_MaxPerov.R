# Load necessary libraries
install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")

library(ggplot2)
library(dplyr)
library(readr)

# Read the dataset
data <- read_csv("C:/Users/max/Desktop/Police_Arrests.csv")

# Filter for drug-related arrests and non-missing 'Drug Type' values
drug_data <- data %>%
  filter(`Drug Related` == "Yes" & !is.na(`Drug Type`))

# Add an Age Group column
drug_data <- drug_data %>%
  mutate(AgeGroup = case_when(
    `Arrestee Age At Arrest Time` >= 18 & `Arrestee Age At Arrest Time` <= 21 ~ "18-21",
    `Arrestee Age At Arrest Time` >= 22 ~ ">=22",
    TRUE ~ as.character(NA)
  ))

# Find the most common drug for each age group
most_common_drug <- drug_data %>%
  group_by(AgeGroup) %>%
  count(`Drug Type`, sort = TRUE) %>%
  slice(1) %>%
  ungroup()

# Count the occurrences of each drug within each age group
drug_counts <- drug_data %>%
  group_by(AgeGroup, `Drug Type`) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(AgeGroup, desc(Count))

# Create a bar plot with ggplot2
ggplot(drug_counts, aes(x = reorder(`Drug Type`, Count), y = Count, fill = AgeGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency of Drug-Related Arrests by Age Group",
       x = "Drug Type", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~AgeGroup, scales = "free_x")

