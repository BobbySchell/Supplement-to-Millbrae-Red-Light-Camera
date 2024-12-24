# ------------
# Redlight Camera Analysis
# ------------

library(readxl)
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(zoo)

# clear workspace
rm(list=ls())


setwd("C:/Users/robsc/OneDrive/Desktop/Old Desktop/Job App Docs/Traffic Camera Analysis/")


###########################
# Creating "AG style" plot
# Citations from 2006 to 2019
###########################


# Import Excel for number of cites
citedata <- read_excel("C:/Users/robsc/OneDrive/Desktop/Old Desktop/Job App Docs/Traffic Camera Analysis/citedata.xlsx", sheet = "Cleaned Cites Data")


# Import Excel for number of events
totdata <- read_excel("C:/Users/robsc/OneDrive/Desktop/Old Desktop/Job App Docs/Traffic Camera Analysis/citedata.xlsx", sheet = "Cleaned Events Data")


# Putting citedata into long format for plotting
citedatalong <- melt(citedata , id.vars = 'Date', variable.name = 'camera')

# Create dummy to plot total vs cameras
citedatalong$Total <- ifelse(citedatalong$camera == "Total", 1, 0)

# Make sure date is registered as date
citedatalong$Date <- as.Date(citedatalong$Date)

# Restrict to same time period as collisions dataset
citedatalongfilter <- filter(citedatalong, citedatalong$Date <= "2013-12-31")


# Plotting number of citations

# Importing font for figures
#font_import()
#loadfonts(device = "win")
#fonts()

# Creating smoothed dataset
citedatalongfilter$rollingcites <- rollmean(citedatalongfilter$value, k = 3, fill=NA)


# Creating initial plot
# NOW rolling 3 months (rolling cites) - to get in monthly form use value
plot <- ggplot(data = citedatalongfilter, aes(x = Date, y = rollingcites, color = as.factor(Total), group = camera)) + geom_line(linewidth = 0.5)
#plot



# THIS is really the main point
plot2 <- plot + 
  theme(legend.position = "bottom",
         legend.direction = "horizontal",
         text = element_text(size = 11, color = "black"), # removed family = "Times New Roman"
         plot.title = element_text(size = 12, face = "bold"), # removed family = "Times New Roman",
         plot.caption = element_text( hjust = 0), # family = "Times New Roman"
         axis.text.x = element_text(color = "black"),
         axis.text.y = element_text(color = "black"),
         axis.line = element_line(color = "black", size = 0.25),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         axis.ticks.x = element_blank())
#plot2


plot3 <- plot2 +
  scale_color_manual(name = "", values = c("#AEB6Bf","#339966"), labels = c("Citations by Camera","Total Citations"))+
  guides() +
  ggtitle("\n Monthly Citations across Millbrae Redlight Cameras \n September 2006 - December 2013")+
  ylab(expression(bold("Citations")))+
  xlab(expression(bold("Month")))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_date(date_breaks = "12 months", date_labels = "%b '%y", expand = c(0,0.5))#limits = c(min_date, max_date), expand = c(0,0))
#plot3



# Adding notes to this - worked previously but now ruins figure
plot4 <- annotate_figure(plot3,
                         bottom = text_grob(
                           "Source:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

# Removed family = "Times New Roman"
#postscript("plot.eps", family="Times New Roman")
#dev.off()

#plot4

# Then actually add note content
plot5 <- annotate_figure(plot4,
                         bottom = text_grob(
                           paste0("highwayrobbery.net"),
                           vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))


citesplot <- plot5

citesplot

tiff("FigCite.tiff", units = "in", width = 7, height = 7, res = 600)

citesplot

dev.off()

# Export as high res image of proper size
#tiff("AGCites.tiff", units = "in", width = 8, height = 5, res = 600)

#plot5

#########
# Recreating this figure with ONLY the main line for total across cameras
#########

citedatalongtot <- filter(citedatalongfilter, camera=="Total")

# Creating initial plot
plot <- ggplot(data = citedatalongtot, aes(x = Date, y = rollingcites)) + geom_line(linewidth = 0.5)
#plot



# THIS is really the main point
plot2 <- plot + 
  theme( legend.position = "bottom",
         legend.direction = "horizontal",
         text = element_text(size = 11, color = "black"), # removed family = "Times New Roman"
         plot.title = element_text(size = 12, face = "bold"), # removed family = "Times New Roman",
         plot.caption = element_text( hjust = 0), # family = "Times New Roman"
         axis.text.x = element_text(color = "black"),
         axis.text.y = element_text(color = "black"),
         axis.line = element_line(color = "black", size = 0.25),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         axis.ticks.x = element_blank())
#plot2


plot3 <- plot2 +
  guides() +
  ggtitle("\n Total Citations across Millbrae Redlight Cameras \n September 2006 - January 2014")+
  ylab(expression(bold("Citations")))+
  xlab(expression(bold("Month")))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_date(date_breaks = "12 months", date_labels = "%b '%y", expand = c(0,0.5))#limits = c(min_date, max_date), expand = c(0,0))
#plot3



# Adding notes to this - worked previously but now ruins figure
plot4 <- annotate_figure(plot3,
                         bottom = text_grob(
                           "Source:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

# Removed family = "Times New Roman"
#postscript("plot.eps", family="Times New Roman")
#dev.off()

#plot4

# Then actually add note content
plot5 <- annotate_figure(plot4,
                         bottom = text_grob(
                           paste0("highwayrobbery.net"),
                           vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))


simplecitesplot <- plot5


######
# Creating revenue chart
# Assuming fine of $490 per citation
######

revenuedata <- citedatalongtot
revenuedata$revenue <- citedatalongtot$rollingcites*490

# Creating initial plot
plot <- ggplot(data = revenuedata, aes(x = Date, y = revenue)) + geom_area(linewidth = 0.5)
#plot



# THIS is really the main point
plot2 <- plot + 
  theme( legend.position = "bottom",
         legend.direction = "horizontal",
         text = element_text(size = 11, color = "black"), # removed family = "Times New Roman"
         plot.title = element_text(size = 12, face = "bold"), # removed family = "Times New Roman",
         plot.caption = element_text( hjust = 0), # family = "Times New Roman"
         axis.text.x = element_text(color = "black"),
         axis.text.y = element_text(color = "black"),
         axis.line = element_line(color = "black", size = 0.25),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         axis.ticks.x = element_blank()) + scale_y_continuous(labels = scales::dollar, expand=c(0,0))
#plot2


plot3 <- plot2 +
  guides() +
  ggtitle("\n Government Revenue across Millbrae Redlight Cameras \n September 2006 - January 2014")+
  ylab(expression(bold("Monthly Revenue")))+
  xlab(expression(bold("Month")))+
  scale_x_date(date_breaks = "12 months", date_labels = "%b '%y", expand = c(0,0.5))#limits = c(min_date, max_date), expand = c(0,0))
#plot3



# Adding notes to this - worked previously but now ruins figure
plot4 <- annotate_figure(plot3,
                         bottom = text_grob(
                           "Source:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

# Removed family = "Times New Roman"
#postscript("plot.eps", family="Times New Roman")
#dev.off()

#plot4

# Then actually add note content
plot5 <- annotate_figure(plot4,
                         bottom = text_grob(
                           paste0("highwayrobbery.net"),
                           vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))


revplot <- plot5



#################################
# Creating SWITRS results
# Source: https://alexgude.com/blog/switrs-sqlite-hosted-dataset/
# 2004 to 2013
#################################

# 1. Focus first on isolating correct parts of Millbrae Av - all cameras intersect here
# Between El Camino Real and Rt 101 is the focus

# Removing primary roads from different areas
primrds <- c("RT 280", "OLD BAYSHORE HWY", "ASHTON AV", "ARLINGTON AV", "MACARTHUR BL", "FERNLEY DR", "BROADWAY", "MAGNOLIA AV", "WHISTLER RD", "MINORCA WY", "PALM AV", "VALLEJO DR",
             "WHISTLER AV", "SOUTH MAGNOLIA AV", "REDWOOD DR", "STONY POINT RD", "LANGNER AV", "LABATH AV", "RT 260", "NATIVIDAD RD", "HARGRAVE AV", "S ASHTON AV")

millbraeavsubset <- filter(millbraeav, !(millbraeav$primary_road %in% primrds))
# From 1022 to 939 rows

# Removing secondary roads from different areas
secrds <- c("MILLBRAE CIRC", "DOWDELL AV", "HEIDI LN", "VALENCIA DR", "TERRA LINDA", "PRIMROSE AV", "MAGNOLIA AV", "VALLEJO DR", "HAZEL", "VILLA LN", "WILLOW RD", "MAGNOLIA",
            "EL PASEO", "BAYSHORE HWY", "WHISTLER AV", "CORONA AV", "BROADWAY", "ARLINGTON RD", "SOUTH ASHTON AV", "WILLOW DR", "LABATH AV", "SPRING VALLEY LN", "SKYLINE BL",
            "PALM AV", "LEWIS AV", "S BROADWAY", "LAUREL AV", "POPLAR", "STONY POINT RD", "WILLOW AV", "S MAGNOLIA AV", "ELDER AV", "POPLAR AV", "HARGRAVE AV", "LOREE LN",
            "W MILLBRAE CIRCLE", "TAYLOR AV", "LANGNER AV", "FERNLEY DR", "HAZEL AV", "ASHTON AV", "TERRA LINDA CT")

millbraeavsubset <- filter(millbraeavsubset, !(millbraeavsubset$secondary_road %in% secrds))
# From 939 to 820 rows


# Only keep in data if unique rows
millbraeavsubsetunique <- millbraeavsubset %>% distinct()
# No change - still 820 rows


# Further screening of secondary roads
secrds2 <- c("MILLBRAE AV 1188", "MILLBRAE AV 1184", "MILLBRAE AV 400", "401 MILLBRAE AV")

millbraeavsubset <- filter(millbraeavsubset, !(millbraeavsubset$secondary_road %in% secrds2))
# From 820 to 816 rows


# Creating year and month/year variables
# Convert to date format
millbraeavsubset$collision_date <- as.Date(millbraeavsubset$collision_date)

millbraeavsubset$year <- format(as.Date(millbraeavsubset$collision_date, format="%d/%m/%Y"),"%Y")
millbraeavsubset$yearmo <- format(as.Date(millbraeavsubset$collision_date, format="%d/%m/%Y"),"%m/%y")

# Aggregate by mo/year
newdata <- millbraeavsubset %>%
  group_by(month = lubridate::floor_date(collision_date, 'month')) %>%
  summarize(accidents = sum(accident))


plot <- ggplot(data = newdata, aes(x = month, y = accidents)) + geom_line() + geom_vline(xintercept=as.Date("2006-09-30"), linetype="dotted")
# Accidents fell to low right BEFORE cameras were installed and then began to rise once more




# THIS is really the main point
plot2 <- plot + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size = 11, color = "black"), # Removed family = "Times New Roman",
        plot.title = element_text(size = 12, face = "bold"), # Removed family = "Times New Roman",
        plot.caption = element_text(hjust = 0), # Removed family = "Times New Roman", 
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black", size = 0.25),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks.x = element_blank()) + geom_text(x=as.Date("2008-01-01"), y=15, label="Year redlight cameras installed", color = "black") + scale_x_date(date_breaks = "12 months", date_labels = "%b '%y")


plot3 <- plot2 +
  scale_color_manual(name = "", values = c("#AEB6Bf","#339966"), labels = c("Accidents by Camera","Total Accidents"))+
  guides() +
  ggtitle("\n Total Accidents across Milbrae Redlight Cameras \n January 2004 - December 2013")+
  ylab(expression(bold("Accidents")))+
  xlab(expression(bold("Year")))+
  scale_y_continuous(expand = expansion(add = c(0, 5))) + geom_vline(xintercept=as.Date("2006-09-30"), linetype="dotted") + geom_vline(xintercept=as.Date("2009-11-01"), linetype="dotted")
#plot3



# Adding notes to this
plot4 <- annotate_figure(plot3,
                         bottom = text_grob(
                           "Source:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

# Removed family = "Times New Roman"
#postscript("plot.eps", family="Times")
#dev.off()

#plot4

# Then actually add note content
plot5 <- annotate_figure(plot4,
                         bottom = text_grob(
                           paste0("SWITRS"),
                           vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))

plot5
# Created overall accidents in relevant section of Millbrae Ave - but difficult to read...



# Smoothing results over 3 months

# Creating smoothed dataset
newdata$rollingaccidents <- rollmean(newdata$accidents, k = 3, fill=NA)


plot <- ggplot(data = newdata, aes(x = month, y = rollingaccidents)) + geom_line() + geom_vline(xintercept=as.Date("2006-10-01"), linetype="dotted")
# Accidents fell to low right BEFORE cameras were installed and then began to rise once more




# THIS is really the main point
plot2 <- plot + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size = 11, color = "black"), # Removed family = "Times New Roman",
        plot.title = element_text(size = 12, face = "bold"), # Removed family = "Times New Roman",
        plot.caption = element_text(hjust = 0), # Removed family = "Times New Roman", 
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black", size = 0.25),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks.x = element_blank()) + scale_x_date(date_breaks = "12 months", date_labels = "%b '%y")

# Removed: geom_text(x=as.Date("2008-01-01"), y=15, label="Year redlight cameras installed", color = "black")

plot3 <- plot2 +
  scale_color_manual(name = "", values = c("#AEB6Bf","#339966"), labels = c("Accidents by Camera","Total Accidents"))+
  guides() +
  ggtitle("\n Monthly Accidents on Millbrae Avenue from El Camino Real to Rt 101 \n January 2004 - December 2013")+
  ylab(expression(bold("Accidents")))+
  xlab(expression(bold("Months")))+
  scale_y_continuous(expand = expansion(add = c(0, 5))) + geom_vline(xintercept=as.Date("2006-10-01"), linetype="dotted") + geom_vline(xintercept=as.Date("2009-12-01"), linetype="dotted")
#plot3



# Adding notes to this
plot4 <- annotate_figure(plot3,
                         bottom = text_grob(
                           "Note:", vjust = 0.25, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

# Removed family = "Times New Roman"
#postscript("plot.eps", family="Times")
#dev.off()

#plot4

# Then actually add note content
plot5 <- annotate_figure(plot4,
                         bottom = text_grob(
                           paste0("Dotted lines denote dates redlight cameras installed."),
                           vjust = 0.25, color = "black", size = 10, just = "left", x = 0.05))

plot5
# Created overall accidents in relevant section of Millbrae Ave - but difficult to read...

plot6 <- annotate_figure(plot5,
                         bottom = text_grob(
                           "Source:", vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))


plot6

plot7 <- annotate_figure(plot6,
                         bottom = text_grob(
                           paste0("SWITRS"),
                           vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05))

plot7

simplecollisions <- plot7

simplecollisions

tiff("Figcol.tiff", units = "in", width = 6, height = 6, res = 600)

simplecollisions

########
# Repeating the process BEYOND 2014 - using all the data available - IN DIFFERENT R FILE
########





# 2. ALSO plot using intersections - such few accidents that it almost looks unfair... Millbrae & Rollins, Millbrae & El Camino, Millbrae & 101 (SB)

#######
# Focus on when primary and secondary align... Then try out restricting to intersection
#######





###########
# Creating figure that has green overall and then grayed out lines from each of the different specific intersections
###########



# Combining data
rollinsnmillbrae$collision_date <- as.Date(rollinsnmillbrae$collision_date)
elcamnmillbrae$collision_date <- as.Date(elcamnmillbrae$collision_date)
RT101nmillbrae$collision_date <- as.Date(RT101nmillbrae$collision_date)

rollinsnmillbrae$year <- format(as.Date(rollinsnmillbrae$collision_date, format="%d/%m/%Y"),"%Y")
rollinsnmillbrae$yearmo <- format(as.Date(rollinsnmillbrae$collision_date, format="%d/%m/%Y"),"%m/%y")
rollinsnmillbrae$accident <- 1

elcamnmillbrae$year <- format(as.Date(elcamnmillbrae$collision_date, format="%d/%m/%Y"),"%Y")
elcamnmillbrae$yearmo <- format(as.Date(elcamnmillbrae$collision_date, format="%d/%m/%Y"),"%m/%y")
elcamnmillbrae$accident <- 1

RT101nmillbrae$year <- format(as.Date(RT101nmillbrae$collision_date, format="%d/%m/%Y"),"%Y")
RT101nmillbrae$yearmo <- format(as.Date(RT101nmillbrae$collision_date, format="%d/%m/%Y"),"%m/%y")
RT101nmillbrae$accident <- 1

totaccidents <- rbind(rollinsnmillbrae, elcamnmillbrae, RT101nmillbrae)

totaccidents$camera <- "All Intersections"

rollinsnmillbrae$camera <- "Rollins Rd"
elcamnmillbrae$camera <- "El Camino Real"
RT101nmillbrae$camera <- "Rt 101"


# Combining these w/ other cameras
totaccidentsfinal <- rbind(totaccidents, rollinsnmillbrae, elcamnmillbrae, RT101nmillbrae)


# Adding dummy for total vs ind
totaccidentsfinal$TotDummy <- ifelse(totaccidentsfinal$camera == "All Intersections", 1,0)



# Aggregate by mo/year
totaccidentsfinal1 <- totaccidentsfinal %>%
  group_by(TotDummy, camera, month = lubridate::floor_date(collision_date, 'month')) %>%
  summarize(accidents = sum(accident))



# Plotting number of accidents

# Importing font for figures
#font_import()
#loadfonts(device = "win")
#fonts()


# Creating initial plot
plot <- ggplot(data = totaccidentsfinal1, aes(x = month, y = accidents, color = as.factor(TotDummy), group = camera)) + geom_line(linewidth = 0.5)
#plot



# THIS is really the main point
plot2 <- plot + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size = 11, color = "black"), # Removed family = "Times New Roman",
        plot.title = element_text(size = 12, face = "bold"), # Removed family = "Times New Roman",
        plot.caption = element_text(hjust = 0), # Removed family = "Times New Roman", 
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black", size = 0.25),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks.x = element_blank()) + geom_text(x=as.Date("2011-01-01"), y=17, label="Year redlight cameras installed", color = "black") + scale_x_date(date_breaks = "12 months", date_labels = "%b '%y")
#plot2


plot3 <- plot2 +
  scale_color_manual(name = "", values = c("#AEB6Bf","#339966"), labels = c("Accidents by Camera","Total Accidents"))+
  guides() +
  ggtitle("\n Total Accidents across Milbrae Redlight Cameras \n January 2004 - December 2013")+
  ylab(expression(bold("Accidents")))+
  xlab(expression(bold("Year")))+
  scale_y_continuous(expand = expansion(add = c(0, 5))) + geom_vline(xintercept=as.Date("2006-01-01"), linetype="dotted") + geom_vline(xintercept=as.Date("2009-01-01"), linetype="dotted")
#plot3



# Adding notes to this
plot4 <- annotate_figure(plot3,
                         bottom = text_grob(
                           "Source:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

# Removed family = "Times New Roman"
#postscript("plot.eps", family="Times")
#dev.off()

#plot4

# Then actually add note content
plot5 <- annotate_figure(plot4,
                         bottom = text_grob(
                           paste0("CalTrans Traffic Accident Surveillance and Analysis System"),
                           vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))

plot5




# Smoothing results over 3 months

# Creating smoothed dataset
totaccidentsfinal1$rollingaccidents <- rollmean(totaccidentsfinal1$accidents, k = 3, fill=NA)




# Creating initial plot
plot <- ggplot(data = totaccidentsfinal1, aes(x = month, y = rollingaccidents, color = as.factor(TotDummy), group = camera)) + geom_line(linewidth = 0.5)
#plot



# THIS is really the main point
plot2 <- plot + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size = 11, color = "black"), # Removed family = "Times New Roman",
        plot.title = element_text(size = 12, face = "bold"), # Removed family = "Times New Roman",
        plot.caption = element_text(hjust = 0), # Removed family = "Times New Roman", 
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black", size = 0.25),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks.x = element_blank()) + geom_text(x=as.Date("2011-01-01"), y=17, label="Year redlight cameras installed", color = "black") + scale_x_date(date_breaks = "12 months", date_labels = "%b '%y")
#plot2


plot3 <- plot2 +
  scale_color_manual(name = "", values = c("#AEB6Bf","#339966"), labels = c("Accidents by Intersection","Total Accidents"))+
  guides() +
  ggtitle("\n Total Accidents across Milbrae Redlight Cameras \n January 2004 - December 2013")+
  ylab(expression(bold("Accidents")))+
  xlab(expression(bold("Year")))+
  scale_y_continuous(expand = expansion(add = c(0, 5))) + geom_vline(xintercept=as.Date("2006-01-01"), linetype="dotted") + geom_vline(xintercept=as.Date("2009-01-01"), linetype="dotted")
#plot3



# Adding notes to this
plot4 <- annotate_figure(plot3,
                         bottom = text_grob(
                           "Note:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

# Removed family = "Times New Roman"
#postscript("plot.eps", family="Times")
#dev.off()

#plot4

# Then actually add note content
plot5 <- annotate_figure(plot4,
                         bottom = text_grob(
                           paste0("Dotted lines denote dates redlight cameras installed"),
                           vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))

plot5

plot6 <- annotate_figure(plot5,
                         bottom = text_grob(
                           "Source:", vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))


plot6

plot7 <- annotate_figure(plot6,
                         bottom = text_grob(
                           paste0("SWITRS"),
                           vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05))

plot7

allcollisions <- plot7


##################
# What if we remove Rt 101 accidents???
##################


no101accidents <- rbind(rollinsnmillbrae, elcamnmillbrae)

no101accidents$camera <- "All Intersections"



# Combining these w/ other cameras
no101accidentsfinal <- rbind(no101accidents, rollinsnmillbrae, elcamnmillbrae)


# Adding dummy for total vs ind
no101accidentsfinal$TotDummy <- ifelse(no101accidentsfinal$camera == "All Intersections", 1,0)



# Aggregate by mo/year
no101accidentsfinal1 <- no101accidentsfinal %>%
  group_by(TotDummy, camera, month = lubridate::floor_date(collision_date, 'month')) %>%
  summarize(accidents = sum(accident))


# Only keeping totals for this graph now
no101accidentsfinal1 <- filter(no101accidentsfinal1, no101accidentsfinal1$TotDummy == 1)

# Plotting number of accidents

# Creating initial plot
plot <- ggplot(data = no101accidentsfinal1, aes(x = month, y = accidents)) + geom_line(linewidth = 0.5)
#plot



# THIS is really the main point
plot2 <- plot + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size = 11, color = "black"), # Removed family = "Times New Roman",
        plot.title = element_text(size = 12, face = "bold"), # Removed family = "Times New Roman",
        plot.caption = element_text(hjust = 0), # Removed family = "Times New Roman", 
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black", size = 0.25),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks.x = element_blank()) + geom_text(x=as.Date("2011-01-01"), y=17, label="Year redlight cameras installed", color = "black") + scale_x_date(date_breaks = "12 months", date_labels = "%b '%y")
#plot2


plot3 <- plot2 +
  guides() +
  ggtitle("\n Monthly Accidents at Millbrae Ave/Rollins Rd and Millbrae Ave/El Camino Real \n January 2004 - December 2013")+
  ylab(expression(bold("Accidents")))+
  xlab(expression(bold("Months")))+
  scale_y_continuous(expand = expansion(add = c(0, 5))) + geom_vline(xintercept=as.Date("2006-10-01"), linetype="dotted") + geom_vline(xintercept=as.Date("2009-12-01"), linetype="dotted")
#plot3



# Adding notes to this
plot4 <- annotate_figure(plot3,
                         bottom = text_grob(
                           "Note:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

# Removed family = "Times New Roman"
#postscript("plot.eps", family="Times")
#dev.off()

#plot4

# Then actually add note content
plot5 <- annotate_figure(plot4,
                         bottom = text_grob(
                           paste0("Dotted lines denote dates redlight cameras installed"),
                           vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))

plot5
# So basically ZERO accidents at Rollins Rd & Millbrae or El Camino & Millbrae

plot6 <- annotate_figure(plot5,
                         bottom = text_grob(
                           "Source:", vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))


plot6

plot7 <- annotate_figure(plot6,
                         bottom = text_grob(
                           paste0("SWITRS"),
                           vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05))

plot7

no101 <- plot7





################
# WHAT IF WE FOCUS ON SPECIFIC INTERSECTIONS *WITH INTERSECTION DUMMY SPECIFIED*
###############


# Combining data
rollinsnmillbrae$collision_date <- as.Date(rollinsnmillbrae$collision_date)
elcamnmillbrae$collision_date <- as.Date(elcamnmillbrae$collision_date)
RT101nmillbrae$collision_date <- as.Date(RT101nmillbrae$collision_date)

rollinsnmillbrae$year <- format(as.Date(rollinsnmillbrae$collision_date, format="%d/%m/%Y"),"%Y")
rollinsnmillbrae$yearmo <- format(as.Date(rollinsnmillbrae$collision_date, format="%d/%m/%Y"),"%m/%y")
rollinsnmillbrae$accident <- 1

elcamnmillbrae$year <- format(as.Date(elcamnmillbrae$collision_date, format="%d/%m/%Y"),"%Y")
elcamnmillbrae$yearmo <- format(as.Date(elcamnmillbrae$collision_date, format="%d/%m/%Y"),"%m/%y")
elcamnmillbrae$accident <- 1

RT101nmillbrae$year <- format(as.Date(RT101nmillbrae$collision_date, format="%d/%m/%Y"),"%Y")
RT101nmillbrae$yearmo <- format(as.Date(RT101nmillbrae$collision_date, format="%d/%m/%Y"),"%m/%y")
RT101nmillbrae$accident <- 1

totaccidents <- rbind(rollinsnmillbrae, elcamnmillbrae, RT101nmillbrae)

totaccidents$camera <- "All Intersections"

rollinsnmillbrae$camera <- "Rollins Rd"
elcamnmillbrae$camera <- "El Camino Real"
RT101nmillbrae$camera <- "Rt 101"


# Combining these w/ other cameras
totaccidentsfinal <- rbind(totaccidents, rollinsnmillbrae, elcamnmillbrae, RT101nmillbrae)


# Adding dummy for total vs ind
totaccidentsfinal$TotDummy <- ifelse(totaccidentsfinal$camera == "All Intersections", 1,0)

totaccidentsfinal <- filter(totaccidentsfinal, totaccidentsfinal$intersection == 1)

# Aggregate by mo/year
totaccidentsfinal1 <- totaccidentsfinal %>%
  group_by(TotDummy, camera, month = lubridate::floor_date(collision_date, 'month')) %>%
  summarize(accidents = sum(accident))



# Plotting number of accidents

# Importing font for figures
#font_import()
#loadfonts(device = "win")
#fonts()


# Creating initial plot
plot <- ggplot(data = totaccidentsfinal1, aes(x = month, y = accidents, color = as.factor(TotDummy), group = camera)) + geom_line(linewidth = 0.5)
#plot



# THIS is really the main point
plot2 <- plot + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size = 11, color = "black"), # Removed family = "Times New Roman",
        plot.title = element_text(size = 12, face = "bold"), # Removed family = "Times New Roman",
        plot.caption = element_text(hjust = 0), # Removed family = "Times New Roman", 
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black", size = 0.25),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks.x = element_blank()) + geom_text(x=as.Date("2011-01-01"), y=17, label="Year redlight cameras installed", color = "black") + scale_x_date(date_breaks = "12 months", date_labels = "%b '%y")
#plot2


plot3 <- plot2 +
  scale_color_manual(name = "", values = c("#AEB6Bf","#339966"), labels = c("Accidents by Camera","Total Accidents"))+
  guides() +
  ggtitle("\n Total Accidents across Milbrae Redlight Cameras \n 2004 - 2013")+
  ylab(expression(bold("Accidents")))+
  xlab(expression(bold("Year")))+
  scale_y_continuous(expand = expansion(add = c(0, 5))) + geom_vline(xintercept=as.Date("2006-01-01"), linetype="dotted") + geom_vline(xintercept=as.Date("2009-01-01"), linetype="dotted")
#plot3



# Adding notes to this
plot4 <- annotate_figure(plot3,
                         bottom = text_grob(
                           "Source:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

# Removed family = "Times New Roman"
#postscript("plot.eps", family="Times")
#dev.off()

#plot4

# Then actually add note content
plot5 <- annotate_figure(plot4,
                         bottom = text_grob(
                           paste0("CalTrans Traffic Accident Surveillance and Analysis System"),
                           vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))

plot5




# Smoothing results over 3 months

# Creating smoothed dataset
totaccidentsfinal1$rollingaccidents <- rollmean(totaccidentsfinal1$accidents, k = 3, fill=NA)




# Creating initial plot
plot <- ggplot(data = totaccidentsfinal1, aes(x = month, y = rollingaccidents, color = as.factor(TotDummy), group = camera)) + geom_line(linewidth = 0.5)
#plot



# THIS is really the main point
plot2 <- plot + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(size = 11, color = "black"), # Removed family = "Times New Roman",
        plot.title = element_text(size = 12, face = "bold"), # Removed family = "Times New Roman",
        plot.caption = element_text(hjust = 0), # Removed family = "Times New Roman", 
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line = element_line(color = "black", size = 0.25),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.ticks.x = element_blank()) + geom_text(x=as.Date("2011-01-01"), y=17, label="Year redlight cameras installed", color = "black") + scale_x_date(date_breaks = "12 months", date_labels = "%b '%y")
#plot2


plot3 <- plot2 +
  scale_color_manual(name = "", values = c("#AEB6Bf","#339966"), labels = c("Accidents by Intersection","Total Accidents"))+
  guides() +
  ggtitle("\n Total Accidents across Milbrae Redlight Cameras \n 2004 - 2014")+
  ylab(expression(bold("Accidents")))+
  xlab(expression(bold("Year")))+
  scale_y_continuous(expand = expansion(add = c(0, 5))) + geom_vline(xintercept=as.Date("2006-01-01"), linetype="dotted") + geom_vline(xintercept=as.Date("2009-01-01"), linetype="dotted")
#plot3



# Adding notes to this
plot4 <- annotate_figure(plot3,
                         bottom = text_grob(
                           "Note:", vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))

# Removed family = "Times New Roman"
#postscript("plot.eps", family="Times")
#dev.off()

#plot4

# Then actually add note content
plot5 <- annotate_figure(plot4,
                         bottom = text_grob(
                           paste0("Dotted lines denote dates redlight cameras installed"),
                           vjust = 0.5, color = "black", size = 10, just = "left", x = 0.05))

plot5

plot6 <- annotate_figure(plot5,
                         bottom = text_grob(
                           "Source:", vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05, face = "bold"))


plot6

plot7 <- annotate_figure(plot6,
                         bottom = text_grob(
                           paste0("SWITRS"),
                           vjust = 0.6, color = "black", size = 10, just = "left", x = 0.05))

plot7

allcollisions <- plot7





# 3. Can repeat process for Rollins, El Cam, 101 as sanity checks...


