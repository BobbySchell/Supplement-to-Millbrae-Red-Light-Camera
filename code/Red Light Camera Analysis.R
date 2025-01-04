# ------------
# Redlight Camera Analysis
# ------------

# Load necessary libraries
library(readxl)
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(zoo)

# Clear workspace
rm(list = ls())

# -------------
# Import citation data
# Source: http://www.highwayrobbery.net/redlightcamsdocsMillbraeMain.html
# -------------

citedata <- read_excel("citedata.xlsx", sheet = "Cleaned Cites Data")
totdata <- read_excel("citedata.xlsx", sheet = "Cleaned Events Data")

# Melt citedata into long format for plotting
citedatalong <- citedata %>%
  melt(id.vars = 'Date', variable.name = 'camera')

# Create dummy variable for total citations
citedatalong <- citedatalong %>%
  mutate(Total = ifelse(camera == "Total", 1, 0),
         Date = as.Date(Date))

# Restrict to same time period as collisions dataset
citedatalongfilter <- citedatalong %>%
  filter(Date <= as.Date("2013-12-31"))

# Create smoothed dataset - 3 month rolling average
citedatalongfilter <- citedatalongfilter %>%
  mutate(rollingcites = rollmean(value, k = 3, fill = NA))

# Plotting number of citations
plot <- ggplot(citedatalongfilter, aes(x = Date, y = rollingcites, color = as.factor(Total), group = camera)) +
  geom_line(linewidth = 0.5)

plot2 <- plot + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size = 11, color = "black"),
        plot.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black", size = 0.25),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks.x = element_blank())

plot3 <- plot2 +
  scale_color_manual(name = "", values = c("#AEB6Bf", "#339966"), labels = c("Citations by Camera", "Total Citations")) +
  ggtitle("\n Monthly Citations across Millbrae Redlight Cameras \n September 2006 - December 2013") +
  ylab(expression(bold("Citations"))) +
  xlab(expression(bold("Month"))) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "12 months", date_labels = "%b '%y", expand = c(0, 0.5))

plot4 <- annotate_figure(plot3,
                         bottom = text_grob("Source:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

plot5 <- annotate_figure(plot4,
                         bottom = text_grob("highwayrobbery.net", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))

citesplot <- plot5
print(citesplot)

tiff("FigCite.tiff", units = "in", width = 7, height = 7, res = 600)
print(citesplot)
dev.off()


#################################
# Creating SWITRS results
# Source: https://alexgude.com/blog/switrs-sqlite-hosted-dataset/
# 2004 to 2013
#################################

# Function to filter primary and secondary roads
filter_roads <- function(data, primary_roads, secondary_roads) {
  data %>%
    filter(!primary_road %in% primary_roads) %>%
    filter(!secondary_road %in% secondary_roads) %>%
    distinct()
}

# Lists of primary and secondary roads to remove
primary_roads <- c("RT 280", "OLD BAYSHORE HWY", "ASHTON AV", "ARLINGTON AV", "MACARTHUR BL", 
                   "FERNLEY DR", "BROADWAY", "MAGNOLIA AV", "WHISTLER RD", "MINORCA WY", 
                   "PALM AV", "VALLEJO DR", "WHISTLER AV", "SOUTH MAGNOLIA AV", "REDWOOD DR", 
                   "STONY POINT RD", "LANGNER AV", "LABATH AV", "RT 260", "NATIVIDAD RD", 
                   "HARGRAVE AV", "S ASHTON AV")

secondary_roads <- c("MILLBRAE CIRC", "DOWDELL AV", "HEIDI LN", "VALENCIA DR", "TERRA LINDA", 
                     "PRIMROSE AV", "MAGNOLIA AV", "VALLEJO DR", "HAZEL", "VILLA LN", "WILLOW RD", 
                     "MAGNOLIA", "EL PASEO", "BAYSHORE HWY", "WHISTLER AV", "CORONA AV", 
                     "BROADWAY", "ARLINGTON RD", "SOUTH ASHTON AV", "WILLOW DR", "LABATH AV", 
                     "SPRING VALLEY LN", "SKYLINE BL", "PALM AV", "LEWIS AV", "S BROADWAY", 
                     "LAUREL AV", "POPLAR", "STONY POINT RD", "WILLOW AV", "S MAGNOLIA AV", 
                     "ELDER AV", "POPLAR AV", "HARGRAVE AV", "LOREE LN", "W MILLBRAE CIRCLE", 
                     "TAYLOR AV", "LANGNER AV", "FERNLEY DR", "HAZEL AV", "ASHTON AV", "TERRA LINDA CT")

secondary_roads2 <- c("MILLBRAE AV 1188", "MILLBRAE AV 1184", "MILLBRAE AV 400", "401 MILLBRAE AV")

# Apply filters to millbraeav dataset
millbraeavsubset <- filter_roads(millbraeav, primary_roads, secondary_roads) %>%
  filter(!secondary_road %in% secondary_roads2)

# Create year and month/year variables
millbraeavsubset <- millbraeavsubset %>%
  mutate(collision_date = as.Date(collision_date),
         year = format(collision_date, "%Y"),
         yearmo = format(collision_date, "%m/%y"))

# Aggregate by month/year
newdata <- millbraeavsubset %>%
  group_by(month = floor_date(collision_date, 'month')) %>%
  summarize(accidents = sum(accident))

# Function to create plot
create_plot <- function(data, y_var, title, y_label, vline_dates, vline_labels) {
  ggplot(data, aes(x = month, y = !!sym(y_var))) +
    geom_line() +
    geom_vline(xintercept = as.Date(vline_dates), linetype = "dotted") +
    annotate("text", x = as.Date(vline_dates[1]), y = max(data[[y_var]], na.rm = TRUE) - 5, 
             label = vline_labels, color = "black") +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      text = element_text(size = 11, color = "black"),
      plot.title = element_text(size = 12, face = "bold"),
      plot.caption = element_text(hjust = 0),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.line = element_line(color = "black", size = 0.25),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    scale_x_date(date_breaks = "12 months", date_labels = "%b '%y") +
    ggtitle(title) +
    ylab(expression(bold(y_label))) +
    xlab(expression(bold("Month")))
}

# Create initial plot
plot1 <- create_plot(newdata, "accidents", 
                     "\n Total Accidents across Milbrae Redlight Cameras \n January 2004 - December 2013",
                     "Accidents", 
                     c("2006-09-30", "2009-11-01"), 
                     c("Year redlight cameras installed"))

# Adding notes to the plot
plot2 <- annotate_figure(plot1,
                         bottom = text_grob("Source:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

plot3 <- annotate_figure(plot2,
                         bottom = text_grob("SWITRS", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))

print(plot3)

# Smoothing results over 3 months
newdata <- newdata %>%
  mutate(rollingaccidents = rollmean(accidents, k = 3, fill = NA))

# Create smoothed plot
plot4 <- create_plot(newdata, "rollingaccidents", 
                     "\n Monthly Accidents on Millbrae Avenue from El Camino Real to Rt 101 \n January 2004 - December 2013",
                     "Accidents", 
                     c("2006-10-01", "2009-12-01"), 
                     c("Year redlight cameras installed"))

# Adding notes to the smoothed plot
plot5 <- annotate_figure(plot4,
                         bottom = text_grob("Note:", vjust = 0.25, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

plot6 <- annotate_figure(plot5,
                         bottom = text_grob("Dotted lines denote dates redlight cameras installed.", vjust = 0.25, color = "black", size = 10, just = "left", x = 0.05))

plot7 <- annotate_figure(plot6,
                         bottom = text_grob("Source:", vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

plot8 <- annotate_figure(plot7,
                         bottom = text_grob("SWITRS", vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05))

print(plot8)

# Save the final plot as a tiff file
tiff("Figcol.tiff", units = "in", width = 6, height = 6, res = 600)
print(plot8)
dev.off()


# Function to process data for each intersection
process_data <- function(df, intersection_name) {
  df <- df %>%
    mutate(collision_date = as.Date(collision_date),
           year = format(collision_date, "%Y"),
           yearmo = format(collision_date, "%m/%Y"),
           accident = 1,
           camera = intersection_name)
  return(df)
}

# Process data for each intersection
rollinsnmillbrae <- process_data(rollinsnmillbrae, "Rollins Rd")
elcamnmillbrae <- process_data(elcamnmillbrae, "El Camino Real")
RT101nmillbrae <- process_data(RT101nmillbrae, "Rt 101")

# Combine data
totaccidents <- bind_rows(rollinsnmillbrae, elcamnmillbrae, RT101nmillbrae) %>%
  mutate(camera = "All Intersections")

# Combine individual and total data
totaccidentsfinal <- bind_rows(totaccidents, rollinsnmillbrae, elcamnmillbrae, RT101nmillbrae) %>%
  mutate(TotDummy = ifelse(camera == "All Intersections", 1, 0))

# Aggregate by month/year
totaccidentsfinal1 <- totaccidentsfinal %>%
  group_by(TotDummy, camera, month = floor_date(collision_date, 'month')) %>%
  summarize(accidents = sum(accident), .groups = 'drop')

# Function to create plots
# Function to create plot
create_plot <- function(data, y_var, title, y_label, vline_dates, vline_labels) {
  ggplot(data, aes(x = month, y = !!sym(y_var), color = as.factor(TotDummy), group = camera)) +
    geom_line(linewidth = 0.5) +
    geom_vline(xintercept = as.Date(vline_dates), linetype = "dotted") +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      text = element_text(size = 11, color = "black"),
      plot.title = element_text(size = 12, face = "bold"),
      plot.caption = element_text(hjust = 0),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.line = element_line(color = "black", size = 0.25),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    scale_x_date(date_breaks = "12 months", date_labels = "%b '%y") +
    ggtitle(title) +
    ylab(expression(bold(y_label))) +
    xlab(expression(bold("Year"))) +
    scale_color_manual(name = "", values = c("#AEB6Bf", "#339966"), labels = c("Accidents by Camera", "Total Accidents")) +
    guides() +
    annotate("text", x = as.Date(vline_dates[1]), y = max(data[[y_var]], na.rm = TRUE) - 5, label = vline_labels, color = "black")
}

# Create initial plot
plot1 <- create_plot(totaccidentsfinal1, "accidents", 
                     "\n Total Accidents across Milbrae Redlight Cameras \n January 2004 - December 2013",
                     "Accidents", 
                     c("2006-01-01", "2009-01-01"), 
                     c("Year redlight cameras installed"))

# Adding notes to the plot
plot2 <- annotate_figure(plot1,
                         bottom = text_grob("Source:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

plot3 <- annotate_figure(plot2,
                         bottom = text_grob("CalTrans Traffic Accident Surveillance and Analysis System", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))

print(plot3)

# Smoothing results over 3 months
totaccidentsfinal1 <- totaccidentsfinal1 %>%
  mutate(rollingaccidents = rollmean(accidents, k = 3, fill = NA))

# Create smoothed plot
plot4 <- create_plot(totaccidentsfinal1, "rollingaccidents", 
                     "\n Total Accidents across Milbrae Redlight Cameras \n January 2004 - December 2013",
                     "Accidents", 
                     c("2006-01-01", "2009-01-01"), 
                     c("Year redlight cameras installed"))

# Adding notes to the smoothed plot
plot5 <- annotate_figure(plot4,
                         bottom = text_grob("Note:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

plot6 <- annotate_figure(plot5,
                         bottom = text_grob("Dotted lines denote dates redlight cameras installed", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))

plot7 <- annotate_figure(plot6,
                         bottom = text_grob("Source:", vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

plot8 <- annotate_figure(plot7,
                         bottom = text_grob("SWITRS", vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05))

print(plot8)
