# Publication quality plotting in R
# Hannah M. Carroll, PhD (she/her)
# 2/17/2021

########################################################################################
# Part 1: ggplot2 and friends

install.packages("ggplot2")
library(ggplot2)

# For this exercise, we will use the built in dataset 'faithful', which contains:
# 1: Eruption time in minutes
# 2: Waiting time to the next eruption in minutes

data(faithful) # Load a default dataset

# Base ggplot2 with no customization
ggplot(data = faithful, aes(x = eruptions, y = waiting)) +
  geom_point()

# ggplot2 with custom axis labels
ggplot(data = faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)")

# add color (color name cheetsheets: https://www.r-graph-gallery.com/ggplot2-color.html)
ggplot(data = faithful, aes(x = eruptions, y = waiting)) +
  geom_point(color = "cornflowerblue") +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)")

# color by value
ggplot(data = faithful, aes(x = eruptions, y = waiting, color = waiting)) +
  geom_point() +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)")

# customize legend
ggplot(data = faithful, aes(x = eruptions, y = waiting, color = waiting)) +
  geom_point() +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)",
       color = "Minutes")

# move the legend
ggplot(data = faithful, aes(x = eruptions, y = waiting, color = waiting)) +
  geom_point() +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)",
       color = "Minutes") +
  theme(legend.position = "bottom")

# Customize plotting area
ggplot(data = faithful, aes(x = eruptions, y = waiting, color = waiting)) +
  geom_point() +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)",
       color = "Minutes") +
  theme_classic() +
  theme(legend.position = "bottom") # Note the change in order!!

# change the base font size
ggplot(data = faithful, aes(x = eruptions, y = waiting, color = waiting)) +
  geom_point() +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)",
       color = "Minutes") +
  theme_classic(base_size = 16) +
  theme(legend.position = "bottom")

# change the base font
ggplot(data = faithful, aes(x = eruptions, y = waiting, color = waiting)) +
  geom_point() +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)",
       color = "Minutes") +
  theme_classic(base_size = 16, base_family="serif") +
  theme(legend.position = "bottom")

# change the base line size
ggplot(data = faithful, aes(x = eruptions, y = waiting, color = waiting)) +
  geom_point() +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)",
       color = "Minutes") +
  theme_classic(base_size = 16, base_family="serif", base_line_size = 3) + # Not saying you should, just saying you can
  theme(legend.position = "bottom") 

##################################################################################################
# We will get rid of those options and go back to a more simple plot to finish our customization #
##################################################################################################

# Make the points a solid color
ggplot(data = faithful, aes(x = eruptions, y = waiting)) +
  geom_point(color = "blue4") + # The legend goes away by default so we can remove the legend.position argument
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)") +
  theme_classic()

# Add a trendline
ggplot(data = faithful, aes(x = eruptions, y = waiting)) +
  geom_point(color = "blue4") +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)") +
  theme_classic() +
  geom_smooth(method = "lm") # each additional geom will inherit from the ggplot call unless you tell it otherwise

# Change trendline color and add model fit info
ggplot(data = faithful, aes(x = eruptions, y = waiting)) +
  geom_point(color = "blue4") +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)") +
  theme_classic() +
  geom_smooth(method = "lm", color = "black") +
  annotate(geom="text", x=(min(faithful$eruptions + 0.5)), y=90, 
           label=("Model fit info"),
             color="red")

# Extract model fit info from a model
round(summary(lm(waiting ~ eruptions, data = faithful))$adj.r.squared, 2) # Wrapped in the round command to round to 2 digits for us

ggplot(data = faithful, aes(x = eruptions, y = waiting)) +
  geom_point(color = "black") +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)") +
  theme_classic() +
  geom_smooth(method = "lm", color = "blue3") +
  annotate(geom="text", x=(min(faithful$eruptions + 0.5)), y=90, 
           label=(expression(paste(R^2*" = 0.81"))),
           color="black")

# There are external packages that make adding model coefficients much easier!

install.packages("ggpmisc")
library(ggpmisc) # Note: ggpmisc will conflict with annotate. To use annotate with ggpmisc loaded, add ggplot2:: immediately before annotate()

formula <- y ~ x # We need to tell ggpmisc what order the variables are in

ggplot(data = faithful, aes(x = eruptions, y = waiting)) +
  geom_point(color = "black") +
  labs(x = "Length of eruption (mins)", y = "Wait time between eruptions (min)") +
  theme_classic() +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm", color = "blue3") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
             label.x.npc = "left", label.y.npc = "top",
             formula = formula, parse = TRUE, size = 3)

# Exporting figures
# Default using Export button is 72 dpi. This is basically unusable. 300 is necessary for presentations. Journals usually want 600 - 1200 dpi.

ggsave("eruptions.tiff", dpi = 300) # Most basic options
ggsave("eruptions2.tiff", width = 6, height = 6, units = "in", compression = "lzw", dpi = 300) # Customize size, use lossless compression

# Moving on from a basic scatterplot

# Using the built-in PlantGrowth dataset

data("PlantGrowth")

# Variables are dry plant weight (g) and treatment group

# Most basic boxplot
ggplot(data = PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot()

# Add axis labels a new way
ggplot(data = PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot() +
  xlab("Treatment group") + ylab("Dry weight (g)")

# Customize axis labels
ggplot(data = PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot() +
  xlab("Treatment group") + ylab("Dry weight (g)") +
  scale_x_discrete(labels = c("Control", "Treatment 1", "Treatment 2")) # Use scale_x_continuous for continous variables

# Marking differences between groups:
TukeyHSD(aov(weight ~ group, data = PlantGrowth)) # Pairwise differences between groups in an ANOVA

ggplot(data = PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot() +
  xlab("Treatment group") + ylab("Dry weight (g)") +
  scale_x_discrete(labels = c("Control", "Treatment 1", "Treatment 2")) +
  annotate(geom = "text", x = "ctrl", y = 6.5, label = "ab") + # Note that we are using the name in the dataset itself, not the new x label we set
  annotate(geom = "text", x = "trt1", y = 6.5, label = "a") +
  annotate(geom = "text", x = "trt2", y = 6.5, label = "b") 

# Now get it publication ready

ggplot(data = PlantGrowth, aes(x = group, y = weight, fill = group)) + # Fill instead of color
  geom_boxplot() +
  xlab("Treatment group") + ylab("Dry weight (g)") +
  scale_x_discrete(labels = c("Control", "Treatment 1", "Treatment 2")) +
  annotate(geom = "text", x = "ctrl", y = 6.5, label = "ab") +
  annotate(geom = "text", x = "trt1", y = 6.5, label = "a") +
  annotate(geom = "text", x = "trt2", y = 6.5, label = "b") +
  theme_classic()

# We don't want awful default colors. We want them to be colorblind-safe and appealing to our audience.

install.packages("viridis")
library("viridis")

ggplot(data = PlantGrowth, aes(x = group, y = weight, fill = group)) + # Fill instead of color
  geom_boxplot() +
  xlab("Treatment group") + ylab("Dry weight (g)") +
  scale_x_discrete(labels = c("Control", "Treatment 1", "Treatment 2")) +
  annotate(geom = "text", x = "ctrl", y = 6.5, label = "ab") +
  annotate(geom = "text", x = "trt1", y = 6.5, label = "a") +
  annotate(geom = "text", x = "trt2", y = 6.5, label = "b") +
  theme_classic() +
  scale_fill_viridis_d() # There are discrete and continuous options for both color and fill

# The legend really isn't needed
ggplot(data = PlantGrowth, aes(x = group, y = weight, fill = group)) + # Fill instead of color
  geom_boxplot() +
  xlab("Treatment group") + ylab("Dry weight (g)") +
  scale_x_discrete(labels = c("Control", "Treatment 1", "Treatment 2")) +
  annotate(geom = "text", x = "ctrl", y = 6.5, label = "ab") +
  annotate(geom = "text", x = "trt1", y = 6.5, label = "a") +
  annotate(geom = "text", x = "trt2", y = 6.5, label = "b") +
  theme_classic() +
  scale_fill_viridis_d() +
  theme(legend.position = "none") # Goes AFTER theme_classic()

# Change the Viridis color scheme and fill
ggplot(data = PlantGrowth, aes(x = group, y = weight, fill = group)) + # Fill instead of color
  geom_boxplot() +
  xlab("Treatment group") + ylab("Dry weight (g)") +
  scale_x_discrete(labels = c("Control", "Treatment 1", "Treatment 2")) +
  annotate(geom = "text", x = "ctrl", y = 6.5, label = "ab") +
  annotate(geom = "text", x = "trt1", y = 6.5, label = "a") +
  annotate(geom = "text", x = "trt2", y = 6.5, label = "b") +
  theme_classic() +
  scale_fill_viridis_d(option = "E", alpha = 0.7) + # Two things: setting a default option AND using alpha to make the fill semi-transparent
  theme(legend.position = "none")

# Pause here and change the viridis options!

# Same data now used in density plots
ggplot(data = PlantGrowth, aes(x = weight, fill = group)) + # Now weight is the x value. Y is automatically assigned to be the density.
  geom_density() +
  xlab("Dry weight (g)") + ylab("Density") +
  theme_classic() +
  scale_fill_viridis_d(option = "E", alpha = 0.5)

# Customize legend
ggplot(data = PlantGrowth, aes(x = weight, fill = group)) + # Now weight is the x value. Y is automatically assigned to be the density.
  geom_density() +
  xlab("Dry weight (g)") + ylab("Density") +
  theme_classic() +
  scale_fill_viridis_d(option = "E", alpha = 0.5, name = "Group", labels = c("Control", "Treatment 1", "Treatment 2"))

# Move the legend and change its direction
ggplot(data = PlantGrowth, aes(x = weight, fill = group)) + # Now weight is the x value. Y is automatically assigned to be the density.
  geom_density() +
  xlab("Dry weight (g)") + ylab("Density") +
  theme_classic() +
  scale_fill_viridis_d(option = "E", alpha = 0.5, name = "Group", labels = c("Control", "Treatment 1", "Treatment 2")) +
  theme(legend.position = "bottom", legend.direction = "horizontal")

# New dataset examining the hair and eye color of statistics students
data(HairEyeColor)

HairEyeColor <- as.data.frame(HairEyeColor) # Needs to be a data frame or coercible to a data frame for ggplot2

ggplot(data = HairEyeColor, aes(x = Sex, y = Freq, fill = Hair)) +
  geom_bar(position = "fill", stat = "identity")  # Stat identity is key here

# Customize this plot
ggplot(data = HairEyeColor, aes(x = Sex, y = Freq, fill = Hair)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Gender identity", y = "Frequency") +
  scale_fill_brewer(palette = "YlGnBu", name = "Hair color") + # Use the colorbrewer2 palettes (NOT ALL ARE COLORBLIND SAFE)
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray80")) # Add y axis lines to the plot in a light gray

# Note: The yellow looks okay on small screens but DOES NOT work on projectors or in print.

# See all of the options at https://colorbrewer2.org/

# Prefer a horizontal plot?
ggplot(data = HairEyeColor, aes(x = Sex, y = Freq, fill = Hair)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Gender identity", y = "Frequency") +
  scale_fill_brewer(palette = "YlGnBu", name = "Hair color") +
  theme_classic() +
  theme(panel.grid.major.x = element_line(color = "gray80")) + # This changes to x instead of y
  coord_flip() # Use coord_flip!

# Complex datasets

data(mpg)

ggplot(data = mpg, aes(x = manufacturer, y = cty)) +
  geom_boxplot() +
  labs(x = "Manufacturer", y = "City mileage (mpg)") +
  theme_bw()

# The axis labels need to be readable
ggplot(data = mpg, aes(x = manufacturer, y = cty)) +
  geom_boxplot() +
  labs(x = "Manufacturer", y = "City mileage (mpg)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# The x axis labels will look better capitalized. Here's how to do that without changing your unerlying data:

# Function provided in the toupper documentation. This capitalizes the first letter of each word in a string
# and makes the rest lower case
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Here we recode the levels of manufacturer
levels(mpg$manufacturer) <- capwords(levels(as.factor(mpg$manufacturer)))

ggplot(data = mpg, aes(x = manufacturer, y = cty)) +
  geom_boxplot() +
  labs(x = "Manufacturer", y = "City mileage (mpg)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels = levels(mpg$manufacturer))

# OR we can skip the recoding step and use capwords right in ggplot:
ggplot(data = mpg, aes(x = manufacturer, y = cty)) +
  geom_boxplot() +
  labs(x = "Manufacturer", y = "City mileage (mpg)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels = capwords(levels(as.factor(mpg$manufacturer))))

# Make this more information rich
ggplot(data = mpg, aes(x = manufacturer, y = cty, fill = as.factor(cyl))) + # as.factor within the call doesn't change the underlying data
  geom_boxplot() +
  labs(x = "Manufacturer", y = "City mileage (mpg)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels = capwords(levels(as.factor(mpg$manufacturer)))) +
  scale_fill_viridis_d(option = "C", name = "Number of cylinders")

# Make this more usable
ggplot(data = mpg, aes(x = manufacturer, y = cty, fill = as.factor(cyl))) + # as.factor within the call doesn't change the underlying data
  geom_boxplot() +
  labs(x = "Manufacturer", y = "City mileage (mpg)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels = capwords(levels(as.factor(mpg$manufacturer)))) +
  scale_fill_viridis_d(option = "C", name = "Number of cylinders") +
  facet_grid(~as.factor(cyl)) + 
  theme(legend.position = "none")

# The panel with 5 cylinders is a waste of room
ggplot(data = mpg[mpg$cyl != 5,], aes(x = manufacturer, y = cty, fill = as.factor(cyl))) + # exclude 5 cylinder without changing the data
  geom_boxplot() +
  labs(x = "Manufacturer", y = "City mileage (mpg)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_discrete(labels = capwords(levels(as.factor(mpg$manufacturer)))) +
  scale_fill_viridis_d(option = "C", name = "Number of cylinders") +
  facet_grid(~as.factor(cyl)) + 
  theme(legend.position = "none")

# ggplot2 recap and more resources
#  Modify components of a theme: https://ggplot2.tidyverse.org/reference/theme.html
#  ggplot2 tutorial: http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
#  Running list of color scheme packages with examples: https://rpubs.com/MRufino/colorr

####################################################################################################################
# A brief guide to cowplot

install.packages("cowplot")
library(cowplot)

# Generate three sets of random data
dat1 <- rnorm(n = 500, mean = 1, sd = 1.2)
dat2 <- rnorm(n = 500, mean = 2, sd = 1)
dat3 <- rnorm(n = 500, mean = 3, sd = 0.8)

# Turn them into a data frame
exdata <- data.frame(x = dat1, y = dat2, z = dat3)

p1 <- ggplot(data = exdata, aes(x = x)) + 
  geom_histogram(bins = 50) +
  theme_classic()
p1

p2 <- ggplot(data = exdata, aes(x = y)) + 
  geom_histogram(bins = 50) +
  theme_classic()
p2

p3 <- ggplot(data = exdata, aes(x = z)) + 
  geom_histogram(bins = 50) +
  theme_classic()
p3

p4 <- ggplot(data = exdata, aes(x = x, y = y)) + 
  geom_point() +
  theme_classic()
p4

# Cowplot will arrange them into a nice grid:
plot_grid(p1, p2, p3, p4)

# Give each plot a label automatically
plot_grid(p1, p2, p3, p4, labels = "AUTO")

# Or customize them
plot_grid(p1, p2, p3, p4, labels = c("The first plot", "Another plot", "Yet another plot", "A different plot"))

# Cowplot offers stamps:
stamp_good(p1)
stamp_bad(p1)
stamp_ugly(p1)

# The real power is arranging plots with a common legend. Here are two that we made before:

p5 <- ggplot(data = PlantGrowth, aes(x = weight, fill = group)) + # Now weight is the x value. Y is automatically assigned to be the density.
  geom_density() +
  xlab("Dry weight (g)") + ylab("Density") +
  theme_classic() +
  scale_fill_viridis_d(option = "E", alpha = 0.5, name = "Group", labels = c("Control", "Treatment 1", "Treatment 2"))
p5

p6 <- ggplot(data = PlantGrowth, aes(x = group, y = weight, fill = group)) + # Fill instead of color
  geom_boxplot() +
  xlab("Treatment group") + ylab("Dry weight (g)") +
  scale_x_discrete(labels = c("Control", "Treatment 1", "Treatment 2")) +
  annotate(geom = "text", x = "ctrl", y = 6.5, label = "ab") +
  annotate(geom = "text", x = "trt1", y = 6.5, label = "a") +
  annotate(geom = "text", x = "trt2", y = 6.5, label = "b") +
  theme_classic() +
  scale_fill_viridis_d(option = "E", alpha = 0.7) + # Two things: setting a default option AND using alpha to make the fill semi-transparent
  theme(legend.position = "none")
p6

# Cowplot extracts just the legend from a gpplot
mylegend <- get_legend(p5)

# Now we turn off the legend on p5:
plot_grid(p5 + theme(legend.position = "none"), p6, mylegend)

# It can be arranged better:
plot_grid(p5 + theme(legend.position = "none"), p6, mylegend,
          nrow = 1, rel_widths = c(1, 1, 0.5)) # Put everything on one row and make the legend half as wide as the two plots

### ggsave works with cowplot but you will likely have to manually set sizes ###
ggsave("testcow.tiff", dpi = 600, width = 9, units = "in", compression = "lzw")

# Making 3D ggplots with gg3D

# This package is currently only available from github. You need devtools to install it.
install.packages("devtools") # Get devtools if you don't have it already
devtools::install_github("AckerDWM/gg3D") # to install it
library(gg3D)

# Make the initial plot
data(iris)

ggplot(iris, aes(x=Petal.Width, y=Sepal.Width, z=Petal.Length, color=Species)) + 
  theme_void() +
  axes_3D() +
  stat_3D()

# Another great package is plot3D (does not depend on ggplot2)
# Here's a guide: http://www.sthda.com/english/wiki/impressive-package-for-3d-and-4d-graph-r-software-and-data-visualization

###########################
##  Non-ggplot2 graphing ##
###########################

# Heatmaps in plotrix

install.packages("plotrix")
library(plotrix)

# Read in example data from Houston et al. (in review)
exdata2 <- read.csv("exdat2.csv")

# Plotrix takes a numeric matrix only, and it must have unique row names

# Use the first column, X, as row names
rownames(exdata2) <- exdata2$X

# Then drop the X column now that row names are set
exdata2 <- exdata2[,-1]

# Set up color scheme
pal <- viridis(6) # Get a 6-color default viridis palette

cellcolors <- unlist(exdata2) # Make a list of every cell so we can color code them

# Set up conditions for color coding. Assign each cell a color based on its rowname.
# I do it using ifelse statements for ease. THERE ARE MANY WAYS TO DO THIS.
cellcolors <- ifelse(exdata2 == 0, pal[1], 
                     ifelse(exdata2 != 0 & rownames(exdata2) %in% c("Su60", "Su61", "Su62", "Su63", "Su64"),
                            pal[2], 
                            ifelse(exdata2 != 0 & rownames(exdata2) %in% c("Su31", "Su37", "Su65"), 
                                   pal[3], 
                                   ifelse(exdata2 != 0 & rownames(exdata2) %in% c("Su15", "Su18", "Su37", "Su50"),
                                          pal[4],
                                          ifelse(exdata2 != 0 & rownames(exdata2) %in% c("Su10", "Su12", "Su18", "Su20"),
                                                 pal[5], 
                                                 pal[6]))))) # Anything not specifically listed above gets the sixth color

# Basic plot
color2D.matplot(exdata2, cellcolors = cellcolors, xlab = "", ylab = "Individual",
                axes = FALSE, cex.lab=0.8)

# Add in axis labels
color2D.matplot(exdata2, cellcolors = cellcolors, xlab = "", ylab = "Individual",
                axes = FALSE, cex.lab=0.8)
axis(side=1,at=0.5:ncol(finaldata),labels=colnames(finaldata), las=2, cex.axis=0.7) # las=2 sets axis labels to be perpendicular to axis
axis(side=2, at=0.5:nrow(finaldata),labels = sort(rownames(finaldata), decreasing = TRUE), las=2, cex.axis=0.7)

# This doesn't work with ggsave. Use the default device like this:
tiff(filename = "plotrixexample.tiff", width = 12, height = 9, units = "in", res = 300, compression = "lzw")
color2D.matplot(exdata2, cellcolors = cellcolors, xlab = "", ylab = "Individual",
                axes = FALSE, cex.lab=0.8)
axis(side=1,at=0.5:ncol(finaldata),labels=colnames(finaldata), las=2, cex.axis=0.7)
axis(side=2, at=0.5:nrow(finaldata),labels = sort(rownames(finaldata), decreasing = TRUE), las=2, cex.axis=0.7)
dev.off() # Stops the capture

######################################################################################################
# A very basic intro to Plotly
install.packages("plotly")
library(plotly)
# library(viridis)

data(iris)

# The most basic scatterplot:
plotly1 <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
plotly1

plotly2 <- plot_ly(iris, 
          x = ~Sepal.Length, 
          y = ~Sepal.Width, 
          type = 'scatter', 
          mode = 'markers', 
          color = ~Species,
          colors = viridis_pal(option = "D")(length(iris$Species)))

plotly2

plotly3 <- plotly2 %>% layout(title = '<b> Iris example dataset </b>',
                                  legend=list(title=list(text='Species')),
                                  yaxis = list(title = 'Sepal width (mm)'), 
                                  xaxis = list(title = 'Sepal length (mm)'))

plotly3

# Plotly's real power is in 3D visualizations

plotly4 <- plot_ly(iris, x=~Sepal.Length, y=~Sepal.Width, z=~Petal.Length,
        type = "scatter3d", 
        mode = "markers", 
        color = ~Species,
        colors = ~viridis_pal(option = "D")(length(iris$Species)))

plotly4

# Make it look nicer
plotly5 <- plotly4 %>% layout(scene = list(xaxis = list(title = 'Sepal length (mm)'),
                                yaxis = list(title = 'Sepal length (mm)'),
                                zaxis = list(title = 'Petal length (mm)')))

plotly5

# Every plotly option available here:
# https://plotly-r.com/index.html

# Export your plotly as a standalone widget
install.packages("htmlwidgets")
library(htmlwidgets)

saveWidget(plotly5, "3dplotlyexample.html", selfcontained = T, libdir = "lib") # selfcontained makes it a standalone html document
