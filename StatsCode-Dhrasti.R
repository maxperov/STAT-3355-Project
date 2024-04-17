library(readr)
library(dplyr)
crimeDallas <- read_csv("C:/Users/dhras/Downloads/dallasCrimes/Police_Arrests.csv")
crimeChicagoFull <- read_csv("C:/Users/dhras/Downloads/Crimes_-_2001_to_Present_20240307.csv")

crimeChicago <- crimeChicagoFull %>% filter(Year >= 2014 & Year <= 2022)
incidents <- read_csv("C:/Users/dhras/Downloads/Police_Incidents_20240404.csv")

names(incidents)[2] <- "Year"
incidents <- incidents %>% filter(Year  >= 2014 & Year <= 2022)

# Save the filtered data to a new CSV file
write.csv(crimeChicago, file = "crimeChicago.csv", row.names = FALSE)
names(incidents)[1] <- "IncidentNum"
names(incidents)[6] <- "IncidentType"

merged_data <- merge(crimeDallas, incidents, by = "IncidentNum", all.x = TRUE)
crimes <- merged_data[c("IncidentNum", "Arrest Year", "Arrest Date",
                                    "Arrest Time", "Arrest Address", "Arrest Zipcode",
                                    "Latitude", "Longitude", "Arrest City", "Arrest State",
                                    "Arrest Day of The Week", "Arrest Location", "Arrest Weapon",
                                    "Arrestee Age At Arrest Time", "Arrestee Race", "Arrestee Sex",
                                    "Drug Related", "Drug Type", "IncidentType")]

names(crimeChicago)[6] <- "PrimaryType"
names(crimes)[14] <- "Age"
names(crimes)[4] <- "time"
names(crimes)[9] <- "City"
names(crimes)[17] <- "DrugRelated"

crimes <- mutate(crimes, age_group = cut(Age, breaks = c(18, 25, 35, 45, 55, 65, Inf),
                                                 labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "65+")))

crimes$drug_use <- case_when(
  crimes$DrugRelated == "Yes" ~ 1,
  crimes$DrugRelated == "No" ~ 0,
  crimes$DrugRelated == "Uknown" ~ 2,
  TRUE ~ NA_integer_
)

crimes$timedata <- cut(strptime(crimes$time, format = "%H:%M:%S"), 
                               breaks = c(as.POSIXct("01:00:00", format = "%H:%M:%S"),
                                          as.POSIXct("06:00:00", format = "%H:%M:%S"),
                                          as.POSIXct("12:00:00", format = "%H:%M:%S"),
                                          as.POSIXct("17:00:00", format = "%H:%M:%S"),
                                          as.POSIXct("21:00:00", format = "%H:%M:%S"),
                                          as.POSIXct("23:59:59", format = "%H:%M:%S")),
                               labels = c("Midnight", "Morning", "Afternoon", "Evening", "Night"))

# Group by age group and time period, then calculate frequency of drug-related crimes
drug_use_summary <- crimes %>%
  group_by(age_group, timedata) %>%
  summarise(Drug_Use_Count = sum(drug_use == 1, na.rm = TRUE))

drug_use_count <- aggregate(drug_use ~ age_group + timedata, data = crimes, FUN = function(x) sum(x == 1))

ggplot(drug_use_count, aes(x = timedata, y = drug_use, fill = age_group)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Drug Use in Dallas by Age Group and Time Period",
       x = "Time Period",
       y = "Drug Use Count",
       fill = "Age Group") +
  scale_x_discrete(labels = c("Morning" = "Morning\n[6 AM - 12 PM)", 
                              "Afternoon" = "Afternoon\n[12 PM - 5 PM)", 
                              "Evening" = "Evening\n[5 PM - 9 PM)", 
                              "Night" = "Night\n[9 PM - 1 AM)",
                              "Midnight" = "Midnight\n[1 AM - 6 AM)")) +
  theme_minimal() +
  theme((legend.position = "right"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 9))

library(ggplot2)
ggplot(crimes, aes(x = age_group, y = drug_use)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Drug Use by Age Group",
       x = "Age Group", y = "Drug Use") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#ALL THE CRIMES NOT JUST DRUG RELATED
crimes$timedata <- cut(strptime(crimes$time, format = "%H:%M:%S"), 
                       breaks = c(as.POSIXct("01:00:00", format = "%H:%M:%S"),
                                  as.POSIXct("06:00:00", format = "%H:%M:%S"),
                                  as.POSIXct("12:00:00", format = "%H:%M:%S"),
                                  as.POSIXct("17:00:00", format = "%H:%M:%S"),
                                  as.POSIXct("21:00:00", format = "%H:%M:%S"),
                                  as.POSIXct("23:59:59", format = "%H:%M:%S")),
                       labels = c("Midnight", "Morning", "Afternoon", "Evening", "Night"))

crimes_by_time <- table(crimes$timedata)




# Plotting the graph
library(ggplot2)

# Create a bar plot
ggplot(data = data.frame(TimeOfDay = names(crimes_by_time), Count = as.numeric(crimes_by_time)),
       aes(x = TimeOfDay, y = Count, fill = TimeOfDay)) +
  geom_bar(stat = "identity") +
  labs(x = "Time of Day", y = "Number of Crimes", title = "Number of Crimes by Time of Day") +
  theme_minimal()



#----------------------->CHICAGO

crimeChicago$drug_related <- ifelse(crimeChicago$PrimaryType %in% c("NARCOTICS", "OTHER NARCOTIC VIOLATIONS"), 1, 0)

table(crimeChicago$drug_related)







#CRIMES BASED ON INCIDENT TYPE

unique_values <- unique(crimes$IncidentType)
  

# Define a function to categorize crimes
categorize_crime <- function(crime) {

  if (grepl("ASSAULT", crime)) {
    return("Assault")
  } else if (grepl("TRAF", crime)) {
    return("Traffic Violation")
  } else if (grepl("THEFT", crime) || grepl("ROBBERY", crime) || grepl("BURGLARY", crime)) {
    return("Theft")
  } else if (grepl("DRUG", crime) || grepl("POSS", crime) || grepl("CONT", crime)){
    return("Drug Related")
  } else if (grepl("FRAUD", crime)) {
    return("Fraud")
  } else if (grepl("SEXUAL", crime) || grepl("SEX", crime)) {
    return("Sexual Offense")
  } else if (grepl("DWI", crime)) {
    return("Driving Intoxicated")
  } else if (grepl("PUBLIC", crime)) {
    return("Public Offense")
  } else if (grepl("MOTOR", crime) || grepl("BMV", crime)){
    return("Unauthorized Vehicle Use")
  } else if (grepl("TRESPASS", crime)){
    return("Criminal Trespass")
  } else if (grepl("WEAPON", crime)){
    return("Unlawful Carry of Weapons")
  } else {
    return("Other")
  }
}

# Apply the categorize_crime function to each crime
crimes$crime_categories <- sapply(crimes$IncidentType, categorize_crime)

column_summary <- table(crimes$crime_categories)

# Convert the summary table to a data frame
column_summary_df <- as.data.frame(column_summary)

# Rename the columns for better readability
colnames(column_summary_df) <- c("Value", "Frequency")

# Sort the data frame by frequency in descending order
column_summary_df <- column_summary_df[order(-column_summary_df$Frequency),]

# Plot the bar graph
ggplot(column_summary_df, aes(x = reorder(Value, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Values in Column",
       x = "Column Values",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ----->FOR CHICAGO
crimeChicago$crime_categories <- sapply(crimeChicago$PrimaryType, categorize_crime)

column_summary <- table(crimeChicago$crime_categories)

# Convert the summary table to a data frame
column_summary_df <- as.data.frame(column_summary)

# Rename the columns for better readability
colnames(column_summary_df) <- c("Value", "Frequency")

# Sort the data frame by frequency in descending order
column_summary_df <- column_summary_df[order(-column_summary_df$Frequency),]

# Plot the bar graph
ggplot(column_summary_df, aes(x = reorder(Value, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Values in Column",
       x = "Column Values",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









#ARREST BY YEAR
arrests_by_year <- table(crimes$`Arrest Year`)

# Convert the result to a dataframe
arrests_by_year <- as.data.frame(arrests_by_year)

# Rename the columns for clarity
names(arrests_by_year) <- c("Year", "Arrest_Count")

ggplot(arrests_by_year, aes(x = Year, y = Arrest_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Arrest_Count), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Year", y = "Number of Arrests", 
       title = "Number of Arrests by Year")

#---------------------->CHICAGO
crimeChicago <- crimeChicago[complete.cases(crimeChicago), ]
crimeChicago$Date <- as.POSIXct(crimeChicago$Date, format = "%m/%d/%Y %I:%M:%S %p")

# Extract the year from the date column
crimeChicago$Year <- format(crimeChicago$Date, "%Y")

arrests_per_year <- crimeChicago %>% group_by(Year) %>% summarise(Arrests = n())

# Plot the arrests made per year
options(scipen = 999)
ggplot(arrests_per_year, aes(x = Year, y = Arrests)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Arrests Made per Year",
       x = "Year",
       y = "Number of Arrests") +
  theme_minimal()



arrests_by_year <- table(incidents$Year)

# Convert the result to a dataframe
arrests_by_year <- as.data.frame(arrests_by_year)

# Rename the columns for clarity
names(arrests_by_year) <- c("Year", "Arrest_Count")

ggplot(arrests_by_year, aes(x = Year, y = Arrest_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Arrest_Count), vjust = -0.5, color = "black", size = 3) +
  labs(x = "Year", y = "Number of Arrests", 
       title = "Number of Arrests by Year")







#ARRESTS PER LOCATION FOR DRUG
drug_data <- crimes[crimes$drug_use == 1, ]

# Create a frequency table for each location with drug-related incidents
location_frequency <- table(drug_data$`Arrest Location`)

# Convert the frequency table to a data frame
location_frequency_df <- as.data.frame(location_frequency)
names(location_frequency_df) <- c("Location", "Frequency")

# Sort the data frame by frequency in descending order
location_frequency_df <- location_frequency_df[order(-location_frequency_df$Frequency), ]

ggplot(location_frequency_df, aes(x = reorder(Location, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Location", y = "Frequency", 
       title = "Frequency of Drug-Related Incidents by Location") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(location_frequency_df, aes(x = "", y = Frequency, fill = Location)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("Distribution of Drug-Related Incidents by Location") +
  theme_void() +
  theme(legend.position = "right")





ggplot(crimes) +
  geom_point(aes(x = Longitude, y = Latitude)) +
  borders("state", colour = "gray40", fill = "gray90") +
  labs(title = "Map with Longitude and Latitude Points") +
  coord_map(xlim = c(min(crimes$Longitude), max(crimes$Longitude)), 
            ylim = c(min(crimes$Latitude), max(crimes$Latitude)))



library(leaflet)

crimes$Longitude <- as.numeric(crimes$Longitude)

# Assuming your dataset is named 'data' and contains columns 'Latitude' and 'Longitude'
# Replace 'data' with the name of your dataset

# Create a leaflet map
map <- leaflet(crimes) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles as the basemap
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude)  # Add circle markers for each point



library(sf)
library(ggplot2)

# Load example data
nc <- st_read(system.file("shape/nc.shp", package = "sf"))

# Plot the map
ggplot() +
  geom_sf(data = nc)

ggplot() +
  geom_point(data = crimes, aes(x = Longitude, y = Latitude), color = "red", size = 3) +
  borders("state", colour = "gray50", fill = "transparent") +
  coord_quickmap() +
  labs(title = "Points on Map", x = "Longitude", y = "Latitude")
