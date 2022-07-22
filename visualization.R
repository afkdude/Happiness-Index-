
###############################[loading packages]###############################

library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(caTools)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(data.table)
library(tidyr)
library(corrgram)       
library(corrplot)
library(formattable)
library(cowplot)
library(ggpubr)
library(plot3D)

###############################[Visualization]##################################

########## Correlation between variables

# Finding the correlation between numerical columns


Happiness <- read.csv("new_dataset.csv")

Num.cols <- sapply(Happiness, is.numeric)
Cor.data <- cor(Happiness[, Num.cols])

corrplot(Cor.data, method = 'color')  

# Create a correlation plot 
newdatacor = cor(Happiness[c(4:11)])
corrplot(newdatacor, method = "number")


Happiness.Continent <- Happiness %>%
  select(-3) %>%
  group_by(Continent) %>%
  summarise_at(vars(-Country), funs(mean(., na.rm=TRUE)))

# Or we can use aggregate
# aggregate(Happiness[, 4:11], list(Happiness$Continent), mean)

# Melting the "Happiness.Continent" dataset
#Happiness.Continent.melt <- melt(Happiness.Continent)


# Faceting
ggplot(Happiness.Continent.melt, aes(y=value, x=Continent, color=Continent, fill=Continent)) + 
  geom_bar( stat="identity") +    
  facet_wrap(~variable) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of happiness variables for different continents", 
       y = "Average value") 


corrgram(Happiness %>% select(-3) %>% filter(Continent == "Africa"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for Africa")

corrgram(Happiness %>% select(-3) %>% filter(Continent == "Asia"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for Asia")

corrgram(Happiness %>% select(-3) %>% filter(Continent == "Europe"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for Europe")

corrgram(Happiness %>% select(-3) %>% filter(Continent == "North America"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for North America")

corrgram(Happiness %>% select(-3) %>% filter(Continent == "South America"), order=TRUE,
         upper.panel=panel.cor, main="Happiness Matrix for South America")


####### Happiness score for each continent

gg1 <- ggplot(Happiness,
              aes(x=Continent,
                  y=Happiness.Score,
                  color=Continent))+
  geom_point() + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8)))

gg2 <- ggplot(Happiness , aes(x = Continent, y = Happiness.Score)) +
  geom_boxplot(aes(fill=Continent)) + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8)))

gg3 <- ggplot(Happiness,aes(x=Continent,y=Happiness.Score))+
  geom_violin(aes(fill=Continent),alpha=0.7)+ theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8)))

# Compute descriptive statistics by groups
stable <- desc_statby(Happiness, measure.var = "Happiness.Score",
                      grps = "Continent")
stable <- stable[, c("Continent","mean","median")]
names(stable) <- c("Continent", "Mean of happiness score","Median of happiness score")
# Summary table plot
stable.p <- ggtexttable(stable,rows = NULL, 
                        theme = ttheme("classic"))


ggarrange(gg1, gg2, ncol = 1, nrow = 2)


#Scatter plot with regression line

ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Life.Expectancy, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")


ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Economy, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")


ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Freedom, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")


ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Trust, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Generosity, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Family, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")


ggplot(subset(Happiness, Happiness$Continent != "Australia"), aes(x = Dystopia.Residual, y = Happiness.Score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")


#::::::::::::::::::::::::::::Generosity::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Generosity", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Generosity, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Generosity); xmax <- max(Happiness$Generosity)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)


#::::::::::::::::::::::::::::Family::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Family", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Family, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Family); xmax <- max(Happiness$Family)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)


#::::::::::::::::::::::::::::Life.Expectancy::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Life.Expectancy", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Life.Expectancy, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Life.Expectancy); xmax <- max(Happiness$Life.Expectancy)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)

#::::::::::::::::::::::::::::Freedom::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Freedom", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Freedom, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Freedom); xmax <- max(Happiness$Freedom)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)

#::::::::::::::::::::::::::::Economy::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Economy", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Economy, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Economy); xmax <- max(Happiness$Economy)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)


#::::::::::::::::::::::::::::Trust::::::::::::::::::::::::::::::

sp <- ggscatter(Happiness, x = "Trust", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)

# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Trust, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()

# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()

# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)

# Place box plots inside the scatter plot
xmin <- min(Happiness$Trust); xmax <- max(Happiness$Trust)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax

# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)


#::::::::::::::::::::::::::::Dystopia.Residual::::::::::::::::::::::::::::::
sp <- ggscatter(Happiness, x = "Dystopia.Residual", y = "Happiness.Score",
                color = "Continent", palette = "jco",
                size = 3, alpha = 0.6)
# Create box plots of x/y variables
# Box plot of the x variable
xbp <- ggboxplot(Happiness$Dystopia.Residual, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(Happiness$Happiness.Score, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(Happiness$Dystopia.Residual); xmax <- max(Happiness$Dystopia.Residual)
ymin <- min(Happiness$Happiness.Score); ymax <- max(Happiness$Happiness.Score)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
sp + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
  annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)
