################################################################################
# The examples and explanations in this script are taken from the following book.

# Schwabish, J. (2021). Better data visualizations: A guide for scholars, 
# researchers, and wonks. Columbia University Press.

# Data to reproduce the plots were compiled by Cengiz Zopluoglu
# using the sources printed on the book.

# R code to reproduce the plots is written by 
# Cengiz Zopluoglu, University of Oregon

# 2/16/2024

require(ggplot2)
require(ggtext)
require(tidyverse)
require(countrycode)
require(ggforce)
library(here)



################################################################################
#
#                            Relationships
#
################################################################################


LE  <- na.omit(read.csv(here('Data/life expectancy.csv')))

LE_long <- LE %>% 
           pivot_longer(cols      = c(X1960:X2021),
                        names_to  = 'Year',
                        values_to = 'LE') 

FR  <- na.omit(read.csv(here('Data/fertility rate.csv')))
FR_long <- FR %>% 
           pivot_longer(cols      = c(X1960:X2021),
                        names_to  = 'Year',
                        values_to = 'FR') 

POP <- na.omit(read.csv(here('Data/population.csv')))
POP_long <- POP %>% 
            pivot_longer(cols      = c(X1960:X2021),
                         names_to  = 'Year',
                         values_to = 'POP') 

country_code <- countrycode::codelist[,c('continent','iso.name.en','ioc')]
colnames(country_code) <- c('Continent','Country.Name','Country.Code')

merged_data <- POP_long %>%
               left_join(FR_long, by = c("Country.Name", "Country.Code", "Year")) %>%
               left_join(LE_long, by = c("Country.Name", "Country.Code", "Year")) %>%
               left_join(country_code,by=c('Country.Name','Country.Code'))

merged_data <- na.omit(merged_data)

merged_data$Year <- as.numeric(gsub('X','',merged_data$Year))

# Final data has FR, LE, and Population for 91 countries from five continents
# during the period of 1960-2021


################################################################################
# 
#                            Scatterplot
# 

# The scatterplot is the most common visualization to illustrate correlations 
# (or lack thereof) between two variables-one variable is plotted along a 
# horizontal axis, and the other along a vertical axis. 

# The specific observations are plotted in the created space. Unlike a bar chart, 
# the scatterplot axes do not necessarily need to start at zero, especially if 
# zero is not a possible value for the data series.

# A scatterplot can help the reader see whether two variables are associated with 
# one another. If the two variables move in the same direction-to the right along 
# the horizontal axis and up along the vertical axis-they are said to be 
# positively correlated. In other words, when both variables get bigger or 
# smaller simultaneously, they are positively correlated. 

# If they move in opposite directions, they are said to be negatively correlated. 
# And if there is no apparent relationship, then they are not correlated.

# Below, we look at the relationship between fertility rate and life expectancy
# for all countries in 1960.

sub_1960 <- subset(merged_data,Year==1960)

p <- ggplot(sub_1960,
            aes(x=FR,y=LE))+
     geom_point(alpha=0.25,color='#be4c15',size=3)

p
  # Transparency (alpha=0.5) helps seeing overlapping data points 

  # Add other custom theme elements

p + 
  labs(title = '**Negative relationship between the fertility rate and life expectancy**',
       x     = 'Fertility Rate',
       y     = 'Life Expectancy',
       subtitle = '',
       caption = "Source: World Bank")+
  theme(plot.title         = element_markdown(margin = margin(b=20),hjust=0),
        panel.background   = element_rect(fill='white',colour='white'),
        axis.title.y       = element_markdown(size   = 10, 
                                              vjust  = 1.075,
                                              angle  = 360, 
                                              margin = margin(r = -80)),
        panel.grid.major.y = element_blank(),
        axis.line          = element_blank(),
        axis.ticks         = element_blank(),
        axis.title.x       = element_text(margin = margin(t=10),size=10),
        plot.caption       = element_text(hjust = 0.01, 
                                          size  = 9)
  )+
  scale_y_continuous(breaks = seq(10,80,10),
                     labels = seq(10,80,10),
                     expand = c(0,0),
                     limits = c(10,86))+
  scale_x_continuous(breaks = 1:9,
                     labels = 1:9,
                     expand = c(0,0),
                     limits = c(1,9))

# In the scatterplot above, you get a visual sense that the two metrics 
# are negatively correlated-that life expectancy is higher for regions with 
# lower fertility rate.

#####################################
# Adding the line of best fit

# One way to make the correlation even clearer is to add what statisticians 
# call a line of best fit to the scatterplot. These are also called 
# "regression lines" or "trendlines," and they show the general direction 
# of the relationship. 

# The statistical calculations to create lines of best fit are beyond the 
# scope of this book, but the point is that you can make it even clearer 
# to the reader in what direction (and to what magnitude) the two variables 
# are correlated by calculating and including this line.

p <- ggplot(sub_1960,
            aes(x=FR,y=LE))+
     geom_point(alpha=0.25,color='#be4c15',size=3)+
     geom_smooth(method = 'lm',se = FALSE)

p

  # geom_smooth() understands the following aesthetics 
  # very similar to geom_line
  #   alpha
  #   colour
  #   fill
  #   group
  #   linetype
  #   linewidth
  
  # method = "lm" or "loess" 
  #   lm, forces a linear relationship through regression
  #   loess, the functional shape of the line is data-driven through smoothing
  # se     = TRUE or FALSE
  
  p <- ggplot(sub_1960,
              aes(x=FR,y=LE))+
    geom_point(alpha=0.25,color='#be4c15',size=3)+
    geom_smooth(method = 'lm',se = FALSE,colour='gray',linetype='dashed',linewidth=0.5)
  
  p

  p <- ggplot(sub_1960,
              aes(x=FR,y=LE))+
    geom_point(alpha=0.25,color='#be4c15',size=3)+
    geom_smooth(method = 'loess',se = TRUE)
  
  p
  
  
  p + 
  labs(title = '**Negative relationship between the fertility rate and life expectancy**',
       x     = 'Fertility Rate',
       y     = 'Life Expectancy',
       subtitle = '',
       caption = "Source: World Bank")+
  theme(plot.title         = element_markdown(margin = margin(b=20),hjust=0),
        panel.background   = element_rect(fill='white',colour='white'),
        axis.title.y       = element_markdown(size   = 10, 
                                              vjust  = 1.075,
                                              angle  = 360, 
                                              margin = margin(r = -80)),
        panel.grid.major.y = element_blank(),
        axis.line          = element_blank(),
        axis.ticks         = element_blank(),
        axis.title.x       = element_text(margin = margin(t=10),size=10),
        plot.caption       = element_text(hjust = 0.01, 
                                          size  = 9)
  )+
  scale_y_continuous(breaks = seq(10,80,10),
                     labels = seq(10,80,10),
                     expand = c(0,0),
                     limits = c(10,86))+
  scale_x_continuous(breaks = 1:9,
                     labels = 1:9,
                     expand = c(0,0),
                     limits = c(1,9))

################################################################################
# 
#                         Bubble Plot
#

# The scatterplot can be transformed into a bubble plot (or bubble scatterplot) 
# by varying the sizes of the circles according to a third variable. 

# The data points don't have to be circles, they can be any other shape that 
# doesn't distort our perception of the data. The circles should be sized by 
# area, not radius. 
  
  p <- ggplot(sub_1960,
              aes(x=FR,y=LE,size=POP))+
    geom_point(alpha=0.25)
  
  p
  
  # custome theme elements
  
  p+
  labs(title = '**Negative relationship between the fertility rate and life expectancy**',
       x     = 'Fertility Rate',
       y     = 'Life Expectancy',
       subtitle = '',
       caption = "Source: World Bank")+
    theme(plot.title         = element_markdown(margin = margin(b=20),hjust=-0.06),
          panel.background   = element_rect(fill='white',colour='white'),
          axis.title.y       = element_markdown(size   = 10, 
                                                vjust  = 1.075,
                                                angle  = 360, 
                                                margin = margin(r = -80)),
          panel.grid.major.y = element_blank(),
          axis.line          = element_blank(),
          axis.ticks         = element_blank(),
          axis.title.x       = element_text(margin = margin(t=10),size=10),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9),
          legend.position      = 'top',
          legend.background    = element_blank(),
          legend.title         = element_text(),
          legend.justification = 'center',
          legend.key           = element_rect(color='white',fill='white'),
          legend.margin        = margin(t=-20,b=10) 
    )+
    scale_size(breaks = c(40000000,80000000,160000000,320000000,
                          640000000),
               labels = c("40M",'80M','160M','320M','640M'),
               name   = 'Population')+
    scale_y_continuous(breaks = seq(10,80,10),
                       labels = seq(10,80,10),
                       expand = c(0,0),
                       limits = c(10,86))+
    scale_x_continuous(breaks = 1:9,
                       labels = 1:9,
                       expand = c(0,0),
                       limits = c(1,9))
  
  
  # Color can help group or highlight certain points or direct the reader's 
  # attention to different parts of the graph.
  
  my_color <- rep('gray',length(unique(sub_1960$Country.Name)))
  names(my_color) <-unique(sub_1960$Country.Name)
  my_color['China'] <- '#be4c15'
  my_color
  
  p <- ggplot(sub_1960,
              aes(x=FR,y=LE,size=POP,color=Country.Name))+
    geom_point(alpha=1)+
    guides(color=FALSE)+
    scale_color_manual(values = my_color)
  
  p
  
  # Add label
  
  labels <- subset(sub_1960,Country.Name=='China')
  
  p +
    geom_text(data=labels,aes(x=FR,y=LE,label=Country.Name),
              size = 4,hjust=1.2,vjust=1)
  
  
  
  # Let's highlight the European countries with a different color
  
  my_color <- rep('gray',length(unique(sub_1960$Country.Name)))
  names(my_color) <- unique(sub_1960$Country.Name)

  europe   <- sub_1960[sub_1960$Continent=='Europe',]$Country.Name
  europe
  
  my_color[europe] <- '#be4c15'
  my_color
  
  p <- ggplot(sub_1960,
              aes(x=FR,y=LE,size=POP,color=Country.Name))+
    geom_point(alpha=1)+
    guides(color=FALSE)+
    scale_color_manual(values = my_color)
  
  p
  
  # Let's add some annotation to direct reader's attention
  
  p + 
    annotate('rect',
             xmin  = 1.9,
             xmax  = 3.05,
             ymin  = 63,
             ymax  = 75,
             alpha = 0.05,
             color = '#be4c15',
             fill  = '#be4c15')+
    annotate('segment',
             x     = 2.5,
             xend  = 2.5,
             y     = 63,
             yend  = 58,
             color = '#be4c15',
             arrow = arrow(length=unit(0.05,'inches')))+
    annotate('text',
             x     = 2.5,
             y     = 56.5,
             size  = 3,
             color = '#be4c15',
             label = 'Majority of \n European countries')+
    annotate('segment',
             x     = 6.46,
             y     = 54.4,
             xend  = 6.6,
             yend  = 61,
             color = '#be4c15',
             arrow = arrow(length=unit(0.05,'inches')))+
    annotate('text',
             x     = 6.6,
             y     = 62,
             size  = 2,
             color ='#be4c15',
             label = 'Albania')+
    annotate('segment',
             x     = 4.29,
             y     = 73.76,
             xend  = 4.5,
             yend  = 73.76,
             color = '#be4c15',
             arrow = arrow(length=unit(0.05,'inches')))+
    annotate('text',
             x     = 4.65,
             y     = 73.9,
             size  = 2,
             color ='#be4c15',
             label = 'Iceland')
    
    
  # Add other customization
  
  p + 
    annotate('rect',
             xmin  = 1.9,
             xmax  = 3.05,
             ymin  = 63,
             ymax  = 75,
             alpha = 0.05,
             color = '#be4c15',
             fill  = '#be4c15')+
    annotate('segment',
             x     = 2.5,
             xend  = 2.5,
             y     = 63,
             yend  = 58,
             color = '#be4c15',
             arrow = arrow(length=unit(0.05,'inches')))+
    annotate('text',
             x     = 2.5,
             y     = 56.5,
             size  = 3,
             color = '#be4c15',
             label = 'Majority of \n European countries')+
    annotate('segment',
             x     = 6.46,
             y     = 54.4,
             xend  = 6.6,
             yend  = 61,
             color = '#be4c15',
             arrow = arrow(length=unit(0.05,'inches')))+
    annotate('text',
             x     = 6.6,
             y     = 62,
             size  = 2,
             color ='#be4c15',
             label = 'Albania')+
    annotate('segment',
             x     = 4.29,
             y     = 73.76,
             xend  = 4.5,
             yend  = 73.76,
             color = '#be4c15',
             arrow = arrow(length=unit(0.05,'inches')))+
    annotate('text',
             x     = 4.65,
             y     = 73.9,
             size  = 2,
             color ='#be4c15',
             label = 'Iceland')+
  labs(title = '**European countries have the lowest fertility rate and highest life expectancy**',
       x     = 'Fertility Rate',
       y     = 'Life Expectancy',
       subtitle = 'Albania has the highest fertility rate and lowest life expectancy in Europe',
       caption = "Source: World Bank")+
    theme(plot.title         = element_markdown(margin = margin(b=5),hjust=0),
          plot.subtitle      = element_markdown(margin = margin(b=10),hjust=0), 
          panel.background   = element_rect(fill='white',colour='white'),
          axis.title.y       = element_markdown(size   = 10, 
                                                vjust  = 1.075,
                                                angle  = 360, 
                                                margin = margin(r = -80)),
          panel.grid.major.y = element_blank(),
          axis.line          = element_blank(),
          axis.ticks         = element_blank(),
          axis.title.x       = element_text(margin = margin(t=10),size=10),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9),
          legend.position      = 'top',
          legend.background    = element_blank(),
          legend.title         = element_text(),
          legend.justification = 'center',
          legend.key           = element_rect(color='white',fill='white'),
          legend.margin        = margin(t=0,b=15) 
    )+
    scale_size(breaks = c(40000000,80000000,160000000,320000000,
                          640000000),
               labels = c("40M",'80M','160M','320M','640M'),
               name   = 'Population')
  
# Labeling certain points or groups of points-with text, color, 
# or enclosing shapes-can help the reader navigate the chart and draw their 
# attention. The plot above added color and a shape to denote 
# European countries.

################################################################################
# 
#                    Animation
#

# It can be very effective to animate how the relationship between two variables
# change over time.
  
  
  library(ggplot2)
  library(gganimate)
  library(dplyr)

  # Creating the plot
  
  p <- ggplot(merged_data, aes(x = FR, y = LE, size = POP, color = Continent)) +
    geom_point(alpha = 0.7) + 
    scale_size_continuous(name = "Population")+
    guides(size=FALSE,color=FALSE)+
    labs(title = 'Change in relationship between fertility rate and life expectancy',
         x     = 'Fertility Rate',
         y     = 'Life Expectancy',
         subtitle = 'Year: {round(frame_time,0)}',
         caption = "Source: World Bank")+
    theme(plot.title         = element_markdown(margin = margin(b=10),hjust=0),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_blank(),
          axis.line          = element_blank(),
          axis.ticks         = element_blank(),
          axis.title.x       = element_text(margin = margin(t=10),size=10),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9),
          legend.position      = 'top',
          legend.background    = element_blank(),
          legend.title         = element_text(),
          legend.justification = 'center',
          legend.key           = element_rect(color='white',fill='white'),
          legend.margin        = margin(t=-20,b=10) 
    )+
    scale_size(breaks = c(40000000,80000000,160000000,320000000,
                          640000000),
               labels = c("40M",'80M','160M','320M','640M'),
               name   = 'Population')+
    scale_y_continuous(breaks = seq(10,80,10),
                       labels = seq(10,80,10),
                       expand = c(0,0),
                       limits = c(10,86))+
    scale_x_continuous(breaks = 1:9,
                       labels = 1:9,
                       expand = c(0,0),
                       limits = c(1,9))+
    transition_time(Year) +
    ease_aes('linear') 
  
  # Animating the plot
  
  animate(p, 
          duration = 20, 
          fps = 20, 
          width = 800, 
          height = 600, 
          renderer = gifski_renderer())
  
  # To save the animation
  anim_save("animated_plot.gif")
  
##########################################################
  
  # Highlight a particular data point in the animation
  
  my_color          <- rep('gray',length(unique(merged_data$Country.Name)))
  names(my_color)   <- unique(merged_data$Country.Name)
  my_color['China'] <- 'blue'
  my_color
  
  labels <- subset(merged_data,Country.Name=='China')
  
  p <- ggplot(merged_data, aes(x = FR, y = LE, size = POP,color=Country.Name)) +
    geom_point(alpha = 0.5) + 
    scale_size_continuous(name = "Population")+
    scale_color_manual(values = my_color)+
    geom_text(data=labels,aes(x=FR,y=LE,label=Country.Name),
              size = 4,hjust=1.2,vjust=1)+
    guides(size=FALSE,color=FALSE)+
    labs(title = 'Change in relationship between fertility rate and life expectancy',
         x     = 'Fertility Rate',
         y     = 'Life Expectancy',
         subtitle = 'Year: {round(frame_time,0)}',
         caption = "Source: World Bank")+
    theme(plot.title         = element_markdown(margin = margin(b=10),hjust=0),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_blank(),
          axis.line          = element_blank(),
          axis.ticks         = element_blank(),
          axis.title.x       = element_text(margin = margin(t=10),size=10),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9),
          legend.position      = 'top',
          legend.background    = element_blank(),
          legend.title         = element_text(),
          legend.justification = 'center',
          legend.key           = element_rect(color='white',fill='white'),
          legend.margin        = margin(t=-20,b=10) 
    )+
    scale_size(breaks = c(40000000,80000000,160000000,320000000,
                          640000000),
               labels = c("40M",'80M','160M','320M','640M'),
               name   = 'Population')+
    scale_y_continuous(breaks = seq(10,80,10),
                       labels = seq(10,80,10),
                       expand = c(0,0),
                       limits = c(10,86))+
    scale_x_continuous(breaks = 1:9,
                       labels = 1:9,
                       expand = c(0,0),
                       limits = c(1,9))+
    transition_time(Year) +
    ease_aes('linear') 
  
  # Animating the plot
  
  animate(p, 
          duration = 20, 
          fps = 20, 
          width = 800, 
          height = 600, 
          renderer = gifski_renderer())
  
  # To save the animation
  anim_save("animated_plot.gif")
  
################################################################################
# 
#                  HEATMAPs
#
#  Visualizing relationship among many variables
  
  require("likert")  # install.packages('likert')

  data(pisaitems)
  
  
  # Select only the first 11 items in the survey from USA
  
  items <- subset(pisaitems,
                  subset = (CNT== 'United States'),
                  select =  2:12)
  
  # Recode the responses
  
  # A custom function to recode responses into ordered factors
  
  likert_recode <- function(x) {
    y <- ifelse(is.na(x), NA,
                ifelse(x == "Strongly disagree",1,
                       ifelse(x == "Disagree",2,
                              ifelse(x == "Agree", 3, 4))))
    
    return(y)
  }
  
  items_numeric <- items %>%
    mutate_all(likert_recode)
  
  
  # Compute the correlation among items
  
  cormat <- items_numeric %>%
    cor(., use = "pairwise.complete.obs")
  
  cormat
  
  
  #############################################################################
  

  require("corrplot") # install.packages('corrplot')
  
  # Correlation matrix plot
  
    # this is very relevant to factor analysis
    # Suppose we think that there are two factors undelying these items
  
  corrplot(cormat,             # correlation matrix
           order   = "hclust", # hierarchical clustering of correlations
           addrect = 2)        # number of clusters
  
  ##############################################################################
  
  require("ggcorrplot") # install.packages('ggcorrplot')
  

  ggcorrplot(cormat, # correlation matrix
             type      = "lower", # print the lower part of the correlation matrix
             hc.order  = FALSE,    # hierarchical clustering of correlations
             lab       = TRUE)    # add correlation values as labels
  
    
  ggcorrplot(cormat, # correlation matrix
             type      = "lower", # print the lower part of the correlation matrix
             hc.order  = TRUE,    # hierarchical clustering of correlations
             lab       = TRUE)    # add correlation values as labels
  

  
  
  
  
  
  
  
  
   