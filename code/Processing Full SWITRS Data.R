# ------------
# Cleaning SWITRS dataset for analysis
# Source and well-documented assumptions: https://alexgude.com/blog/switrs-sqlite-hosted-dataset/
# ------------

# Load necessary libraries
library("RSQLite")
library(dplyr)
library(ggplot2)
library(lubridate)



# Connect to SQLite database and retrieve table names
cleandata <- dbConnect(RSQLite::SQLite(), "switrs.sqlite")
tables <- dbListTables(cleandata)

# Retrieve and store tables as dataframes
lDataFrames <- lapply(tables, function(table) {
  dbGetQuery(conn = cleandata, statement = paste("SELECT * FROM '", table, "'", sep = ""))
})

# Save tables as separate dataframes
df_case_year <- lDataFrames[[1]] # Case ID and year
df_collisions <- lDataFrames[[2]] # Case ID and collisions
df_parties <- lDataFrames[[3]] # Case ID and parties
df_victims <- lDataFrames[[4]] # Case ID and victims

# Combine datasets - only need case ID, year, and collision data
cleanswitrs <- merge(df_case_year, df_collisions, by = "case_id")
# 9,424,334 observations w/ 76 variables


# Remove extraneous datasets to spare memory
rm(cleandata, df_case_year, df_collisions, df_parties, df_victims, lDataFrames)

# Subset relevant columns
cleanswitrssubset <- cleanswitrs %>%
  select(collision_date, latitude, longitude, type_of_collision, collision_severity,
         caltrans_district, caltrans_county, primary_road, secondary_road, intersection,
         db_year, case_id)

# Convert collision_date to Date format and filter years of interest
cleanswitrssubset <- cleanswitrssubset %>%
  mutate(collision_date = as.Date(collision_date)) %>%
  filter(collision_date >= as.Date("2004-01-01") & collision_date <= as.Date("2013-12-31"))
# Now 4,550,457 observations and 12 variables


# Filter data to correct roads
filter_roads <- function(data, primary, secondary) {
  filter(data, primary_road == primary | secondary_road == secondary)
}

millbraeav <- filter_roads(cleanswitrssubset, "MILLBRAE AV", "MILLBRAE AV")
# 1022 observations

rollinsrd <- filter_roads(cleanswitrssubset, "ROLLINS RD", "ROLLINS RD")
# 423 observations

elcam <- filter_roads(cleanswitrssubset, "EL CAMINO REAL", "EL CAMINO REAL")
# 12,431 observations

RT101 <- filter_roads(cleanswitrssubset, "RT 101", "RT 101")
# 162,445 observations

# Create intersection-specific datasets
create_intersection_data <- function(primary_data, secondary_data, primary, secondary) {
  data <- filter(primary_data, primary_road == secondary | secondary_road == primary)
  data$accident <- 1
  return(data)
}

rollinsnmillbrae <- create_intersection_data(rollinsrd, millbraeav, "ROLLINS RD", "MILLBRAE AV")
elcamnmillbrae <- create_intersection_data(millbraeav, elcam, "EL CAMINO REAL", "MILLBRAE AV")
RT101nmillbrae <- create_intersection_data(millbraeav, RT101, "RT 101", "MILLBRAE AV")


##########
# Basic plots of data
##########

# Basic plots of data
plot_accidents <- function(data, title) {
  data <- data %>%
    mutate(year = format(collision_date, "%Y")) %>%
    group_by(month = floor_date(collision_date, "month")) %>%
    summarize(accidents = n(), .groups = 'drop')
  
  ggplot(data, aes(x = month, y = accidents)) +
    geom_line() +
    geom_vline(xintercept = as.Date("2006-09-01"), linetype = "dotted") +
    labs(title = title, x = "Month", y = "Number of Accidents") +
    theme_minimal()
}

# Plot accidents for El Camino and Millbrae
plot_accidents(elcamnmillbrae, "Accidents at El Camino Real and Millbrae Avenue")

# Plot accidents for all of Millbrae Avenue
millbraeav <- millbraeav %>%
  mutate(accident = 1, year = format(collision_date, "%Y"), yearmo = format(collision_date, "%m/%y"))

millbraeag <- millbraeav %>%
  group_by(month = floor_date(collision_date, "month")) %>%
  summarize(accidents = n(), .groups = 'drop')

ggplot(millbraeag, aes(x = month, y = accidents)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2006-09-01"), linetype = "dotted") +
  labs(title = "Accidents on Millbrae Avenue", x = "Month", y = "Number of Accidents") +
  theme_minimal()
