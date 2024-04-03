################################################################################
# The examples and explanations in this script are taken from the following book.

# Schwabish, J. (2021). Better data visualizations: A guide for scholars, 
# researchers, and wonks. Columbia University Press.

# Data to reproduce the plots were compiled by Cengiz Zopluoglu
# using the sources printed on the book.

# R code to reproduce the plots is written by 
# Cengiz Zopluoglu, University of Oregon

# 2/4/2024

setwd('B:/UO Teaching/EDLD 652/Winter24/Week 4')

require(ggplot2)
require(ggtext)
require(tidyverse)
################################################################################
#
#                            BAR Charts
#
################################################################################
library(pacman)
library(ggplot2)
library(ggtext)
library(tidyverse)
library(here)

# Data Source
  # https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false

pop <- read.csv(here('Data/population.csv'))

  # I use fileEncoding argument otherwise, I get a weird column name for the 
  # first column


countries <- c('Brazil','Ethiopia','France','Germany','Italy','Japan',
               'Mexico','Turkiye','Russian Federation','Viet Nam')

sub_pop <- subset(pop,
                  subset = Country.Name%in%countries,
                  select=c('Country.Name','Country.Code','X2022'))

sub_pop$X2022 <- sub_pop$X2022/1000000

# Start with default settings

  ggplot(sub_pop, aes(x = Country.Name, y = X2022)) +
    geom_bar(stat = "identity") 

    # stat = "identity" is asking R to use data as is
  
    # geom_bar() understands the following aesthetics 
      # alpha
      # colour
      # fill
      # group
      # linetype
      # linewidth
      # Learn more about setting these aesthetics in 
   
      vignette("ggplot2-specs").

      ggplot(sub_pop, aes(x = Country.Name, y = X2022)) +
        geom_bar(stat = "identity",
                 fill='orange',           # color for inside the bar
                 colour = 'turquoise',    # color for the boundary line 
                 linetype='dashed',       
                 linewidth=2,
                 alpha = 0.5)             # transparency 

# Add a bit color, plot title, and y-axis title (vertical above axis),
# no xaxis title as it is obvious

  ggplot(sub_pop, aes(x = Country.Name, y = X2022)) +
    geom_bar(stat = "identity",fill='orange') +
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '(Millions of people)')+
    theme(plot.title   = element_markdown(margin = margin(b=25),hjust=-0.075),
          axis.title.y = element_text(size = 10, 
                                      vjust = 1.05,
                                      angle = 360, 
                                      margin = margin(r = -80)))
  
      # Hit Zoom, and change the figure size. The numbers may need adustment
      # based on your figure size

# Change background color and adjust the grid lines.
  
  # Set the Backgroun panel color to white
  # panel.background   = element_rect(fill='white',colour='white')
  
  # Gridlines help the reader see the specific values for each bar and 
  # are especially useful for the bars farthest from the axis label.
   
  # Because they serve as a visual guide, they can be rendered in a lighter 
  # color so the reader's eye stays on the data.
  # panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed")
  
  # I also made the bars a little transparent to make the grid lines visible 
  # behind the bars, alpha=0.60
  
  
  ggplot(sub_pop, aes(x = Country.Name, y = X2022)) +
    geom_bar(stat = "identity",fill='orange',alpha=0.60) +
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '(Millions of people)')+
    theme(plot.title   = element_markdown(margin = margin(b=25),hjust=-0.075),
          axis.title.y       = element_text(size = 10, 
                                          vjust = 1.05,
                                          angle = 360, 
                                          margin = margin(r = -80)),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed")
    )
  
# Improve the axis lines
  
  # Bar charts don't need tick marks between the bars. 
  # White space is an effective separator and deleting the tick marks 
  # reduces clutter.
  # axis.ticks = element_blank()
  
  # I added a visible emphasized x-axis line at the bottom
  #  axis.line.x= element_line(color = "black", linewidth = 0.5),
  
  # I reduced the space between x-axis text and x-axis line by fixing
  # the top margin to 0
  # axis.text.x = element_text(margin=margin(t=0))
  
  # Change the number of breaks and limit for the y-axis, and also removed the 
  # space between the x-axis line and the bottom of the bars
  # scale_y_continuous(breaks = c(0,50,100,150,200,250),
  #                   limits = c(0,250),
  #                   expand = c(0,0))
  
  # Custom x-axis labels so Russian Federation is printed in two lines
  # Note that x-axis is discrete because the values are character strings 
  
  ggplot(sub_pop, aes(x = Country.Name, y = X2022)) +
    geom_bar(stat = "identity",fill='orange',alpha=0.5) +
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '(Millions of people)')+
    theme(plot.title   = element_markdown(margin = margin(b=25),hjust=-0.075),
          axis.title.y       = element_text(size = 10, 
                                            vjust = 1.075,
                                            angle = 360, 
                                            margin = margin(r = -80)),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.x        = element_line(color = "black", linewidth = 0.5),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.ticks         = element_blank()
    )+
    scale_y_continuous(breaks = c(0,50,100,150,200,250),
                       limits = c(0,250),
                       expand = c(0,0))+
    scale_x_discrete(
      labels = c('Brazil','Ethiopia','France','Germany','Italy','Japan',
                 'Mexico','Russian \n Federation','Turkiye','Viet Nam'), # Custom labels
    )
  
# Add the data source  

  ggplot(sub_pop, aes(x = Country.Name, y = X2022)) +
    geom_bar(stat = "identity",fill='orange',alpha=0.5) +
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false")+
    theme(plot.title   = element_markdown(margin = margin(b=25),hjust=-0.075),
          axis.title.y       = element_text(size = 10, 
                                            vjust = 1.075,
                                            angle = 360, 
                                            margin = margin(r = -80)),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.x        = element_line(color = "black", linewidth = 0.5),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    )+
    scale_y_continuous(breaks = c(0,50,100,150,200,250),
                       limits = c(0,250),
                       expand = c(0,0))+
    scale_x_discrete(
      labels = c('Brazil','Ethiopia','France','Germany','Italy','Japan',
                 'Mexico','Russian \n Federation','Turkiye','Viet Nam'), # Custom labels
    )

# Sometimes, it makes easier when the data sorted according to their data values
# Highest and lowers values are easier to identify
  
  # Order from highest to lowest
  
  reorder(sub_pop$Country.Name, sub_pop$X2022, decreasing=TRUE)
  
  # Order from lowest to highest
  
  reorder(sub_pop$Country.Name, sub_pop$X2022, decreasing=FALSE)
  
  # Implement it directly within the aes() function
  
  ggplot(sub_pop, aes(x = reorder(Country.Name,X2022,decreasing=TRUE), 
                      y = X2022)) +
    geom_bar(stat = "identity",fill='orange',alpha=0.5) +
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false")+
    theme(plot.title   = element_markdown(margin = margin(b=25),hjust=-0.075),
          axis.title.y       = element_text(size = 10, 
                                            vjust = 1.075,
                                            angle = 360, 
                                            margin = margin(r = -80)),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.x        = element_line(color = "black", linewidth = 0.5),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    )+
    scale_y_continuous(breaks = c(0,50,100,150,200,250),
                       limits = c(0,250),
                       expand = c(0,0))+
    scale_x_discrete(
      labels = c('Brazil','Ethiopia','France','Germany','Italy','Japan',
                 'Mexico','Russian \n Federation','Turkiye','Viet Nam'), # Custom labels
    )
  
  
      # What is wrong here?
  
  
  ggplot(sub_pop, aes(x = reorder(Country.Name,X2022,decreasing=TRUE), 
                      y = X2022)) +
    geom_bar(stat = "identity",fill='orange',alpha=0.5) +
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false")+
    theme(plot.title   = element_markdown(margin = margin(b=25),hjust=-0.075),
          axis.title.y       = element_text(size = 10, 
                                            vjust = 1.075,
                                            angle = 360, 
                                            margin = margin(r = -80)),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.x        = element_line(color = "black", linewidth = 0.5),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    )+
    scale_y_continuous(breaks = c(0,50,100,150,200,250),
                       limits = c(0,250),
                       expand = c(0,0)) +
    scale_x_discrete(
       labels = c('Brazil','Russian \n Federation','Mexico','Japan','Ethiopia',
                  'Viet Nam','Turkiye','Germany','France','Italy'))
  
  
  
# Starting the axis of bar charts at zero is a rule of thumb many data
# visualization and authors agree.
# Starting at something other than zero may overemphasize the difference between
# the bars and skew viewer's perception
  
  # Because none of the countries in the chart are lower than 50 million
  ## you might be tempted to start the axis at 50 million
  
  ggplot(sub_pop, aes(x = reorder(Country.Name,X2022,decreasing=TRUE), 
                      y = X2022-50)) +
    geom_bar(stat = "identity",fill='orange',alpha=0.5) +
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false")+
    theme(plot.title   = element_markdown(margin = margin(b=25),hjust=-0.075),
          axis.title.y       = element_text(size = 10, 
                                            vjust = 1.075,
                                            angle = 360, 
                                            margin = margin(r = -80)),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.x        = element_line(color = "black", linewidth = 0.5),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    )+
    scale_y_continuous(breaks = c(0,50,100,150,200),
                       limits = c(0,200),
                       expand = c(0,0),
                       labels = c(50,100,150,200,250))



      # this was a little challenging as I couldn't make the following simple
      # code work
  
      #scale_y_continuous(breaks = c(0,50,100,150,200,250),
      #                   limits = c(50,250))
  
  
  # Here, it looks as though Brazil is orders of magnitude larger than Italy, 
  # when, in fact, it is only about three-and-a-half times greater. 
  # This isn't a matter of moving from accurate perception to general 
  # perception-it's a matter of moving from accurate to inaccurate.
  # Consider the concept of Lie Factor in this case.

# Sometimes, you may want to add the exact values as data labels to the graph
# Consider removing gridlines and y-axis all together in that case, 
# as they are not necessary 
  
  ggplot(sub_pop, aes(x = reorder(Country.Name,X2022,decreasing=TRUE), 
                      y = X2022)) +
    geom_bar(stat = "identity",fill='orange',alpha=0.5) +
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false")+
    theme(plot.title   = element_markdown(margin = margin(b=30),hjust=-0.01),
          axis.title.y       = element_text(size = 10, 
                                            vjust = 1.075,
                                            angle = 360, 
                                            margin = margin(r = -80)),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_blank(),
          axis.line.x        = element_line(color = "black", linewidth = 0.5),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.text.y        = element_blank(),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    )+
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,230))+
    geom_text(aes(label = round(X2022)), 
              vjust = -0.5, 
              colour = "black")
  
  
  
# You can flip the bar chart, but it requires adjusting all numbers, so all
# labels look as they are supposed to
  
  ggplot(sub_pop, aes(x = reorder(Country.Name,X2022,decreasing=TRUE), 
                      y = X2022)) +
    geom_bar(stat = "identity",fill='orange',alpha=0.5) +
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false")+
    theme(plot.title   = element_markdown(margin = margin(b=30),hjust=-0.01),
          axis.title.y       = element_text(size = 10, 
                                            vjust = 1.075,
                                            angle = 360, 
                                            margin = margin(r = -80)),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_blank(),
          axis.line.x        = element_line(color = "black", linewidth = 0.5),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.text.y        = element_blank(),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    )+
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,230))+
    geom_text(aes(label = round(X2022)), 
              vjust = -0.5, 
              colour = "black")+
    coord_flip()
  
  # If you are going to flip a bar chart, it is better to do it at the very 
  # beginning and then built everything on top of that
  
  # Adjusting the numbers
  
  ggplot(sub_pop, aes(x = reorder(Country.Name,X2022,decreasing=FALSE), 
                      y = X2022)) +
    geom_bar(stat = "identity",fill='orange',alpha=0.5) +
    coord_flip()+
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '',
         subtitle = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false")+
    theme(plot.title   = element_markdown(margin = margin(b=5),hjust=-0.01),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.y        = element_line(color = "black", linewidth = 0.5),
          axis.text.y        = element_text(margin=margin(t=0)),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    )+
    scale_y_continuous(breaks = seq(0,250,50),
                       expand = c(0,0),
                       limits = c(0,260))
################################################################################  
#                     Lollipop bar charts
  
  # There are other ways to modify the standard bar chart. 
  # One simple variation is to use other shapes in lieu of bars. 
  
  # The lollipop chart, for example, replaces the bar with a line and a dot at the end. 
  # This version lives a hair below the bar chart on our perceptual rankings, 
  # because it's not exactly clear which part of the circle encodes the value. 
  # But it removes a lot of ink from the page and gives you more white space to
  # add labels or other annotation.
  
  # To create a lollipop chart similar to the one attached, you would modify 
  # the geom_bar layer in your ggplot2 command to use geom_segment and geom_point. 
  # A lollipop chart is essentially a scatter plot with a segment (line) leading 
  # from a baseline to the point, emphasizing the magnitude of each value.
  
  # geom_segment() understands the following aesthetics 
  # xend
  # yend
  # alpha
  # colour
  # group
  # linetype
  # linewidth
  # ?geom_segment
  
  # geom_point() understands the following aesthetics (required aesthetics are in bold):
  # alpha
  # colour
  # fill
  # group
  # shape
  # size
  # stroke
  
  ggplot(sub_pop, aes(x = reorder(Country.Name, X2022, decreasing = FALSE), 
                      y = X2022)) +
    geom_segment(aes(x    = reorder(Country.Name, X2022, decreasing = FALSE), 
                     xend = reorder(Country.Name, X2022, decreasing = FALSE), 
                     y    = 0, 
                     yend = X2022), 
                 colour    = "orange",
                 linewidth = 1) +                      # This adds the 'stick' of the lollipop
    geom_point(size  = 5, 
               color = "orange", 
               alpha = 1) +  # This adds the 'head' of the lollipop
                             # default shape is circle
                             # you can change it using shape= argument
                             # Google "R geom_point shapes"
    coord_flip() +
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x = '',
         y = '',
         subtitle = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false") +
    theme(plot.title = element_markdown(margin = margin(b = 5), hjust = -0.01),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.y = element_line(color = "black", linewidth = 0.5),
          axis.text.y = element_text(margin = margin(t = 0)),
          axis.ticks = element_blank(),
          plot.caption = element_text(hjust = 0.01, size = 9, margin = margin(t = 0))) +
    scale_y_continuous(breaks = seq(0, 250, 50),
                       expand = c(0, 0),
                       limits = c(0, 260))
  
################################################################################  
#                     Radial bar charts
  
  # In this chart type, bars are represented as segments of a circle, 
  # with the length of the arc representing the value. 
  # This kind of chart can be visually appealing and is used for comparing 
  # the quantities via angular sizes.
  
  # This graph lies lowers on the perceptual ranking list because it is harder 
  # to compare the heights of the bars arranged around a circle than when 
  # they are arranged along a single flat axis. But this layout does allow you 
  # to fit more values in a compact space, and makes the radial bar chart 
  # well-suited for showing more data, frequent changes (such as monthly or daily), 
  # or changes over a long period of time.
  
  # Creating a radial bar chart in ggplot2 involves transforming a regular 
  # bar chart into polar coordinates using coord_polar()
  # ?coord_polar
  
  ggplot(sub_pop, aes(x = reorder(Country.Name, X2022, decreasing = TRUE), y = X2022)) +
    geom_bar(stat = "identity", fill = 'orange', alpha = 0.5) +
    coord_polar(theta='x', start = 2*pi,direction=1,clip='off') +  # This transforms the bar chart into a radial bar chart
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '',
         subtitle = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false")+
    theme(plot.title   = element_markdown(margin = margin(b=15),hjust=0),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.text.y        = element_blank(),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    )+
    scale_y_continuous(breaks = c(0,50,100,150,200,250),
                       limits = c(0,250),
                       expand = c(0,0))

################################################################################  
#                     Circular bar charts
  
  # coord_polar(theta='y') instead of coord_polar(theta='x') 
  
  sub_pop$temp <- 250
  
  ggplot(sub_pop) +
    geom_bar(aes(x = reorder(Country.Name, X2022, decreasing = FALSE), y = X2022),
             stat = "identity", fill = 'orange', alpha = 0.5) +
    geom_bar(aes(x = reorder(Country.Name, X2022, decreasing = TRUE), y = temp),
             stat = "identity", fill = 'gray', alpha = 0.5) +
    coord_polar(theta='y', start = pi,direction=1,clip='off') +  # This transforms the bar chart into a radial bar chart
    scale_y_continuous(limits = c(0,333),
                       breaks = c(0,50,100,200,250))+
    labs(title = '**The total population in Brazil exceeds that of other countries**',
         x     = '',
         y     = '',
         subtitle = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL?name_desc=false")+
    theme(plot.title   = element_markdown(margin = margin(b=15),hjust=0),
          panel.background   = element_rect(fill='white',colour='white'),
          axis.text.x        = element_text(),
          axis.text.y        = element_blank(),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    )+
    geom_text(aes(x = reorder(Country.Name, X2022, decreasing = FALSE), 
                  y = 0,
                  label = Country.Name), 
              vjust  = 0.5,
              hjust  = 0,
              colour = "black",
              size   = 3)
  
  # Perceptually speaking, the circular bar chart is problematic because 
  # it distorts our perception of the data in this case, the lengths of the bars 
  # don't correspond to their actual value.
  
  # Consider the case where the values of two bars are the same-the ends of the 
  # bars will line up in the same position, but the lengths of the bars are not 
  # actually the same because they lie along the circumference of two different 
  # circles. 
  
  # This is like each runner starting the same position in an Olympic race
  # It would be completely unfair.
  
  # Runners start at staggered positions on the track, but they all end up 
  # running the same distance because the runner on the outside lane has more 
  # distance to cover than the runner on the inside lane. 
  
  # Here, the visualization doesn't move down the perceptual ranking, 
  # but off of it altogether because it distorts the data and for that reason, 
  # you should probably avoid circular charts (and also radial bar charts).

################################################################################    
#                        Paired Bar Charts 
  
# If you want to show comparisons not ust across but also within countries for a
# grouping variable, the paired (or grouped) bar chart is a good option.
  
## First, read the group based population data for all countries
  
#  Data Source
#  https://data.worldbank.org/indicator/SP.POP.TOTL.MA.IN?name_desc=false
#  https://data.worldbank.org/indicator/SP.POP.TOTL.FE.IN?name_desc=false
  
  pop_m <- read.csv('population_male.csv',fileEncoding="UTF-8-BOM")
  pop_f <- read.csv('population_female.csv',fileEncoding="UTF-8-BOM")
  
  pop_m$group <- 'Men'
  pop_f$group <- 'Women'
  
      
  
  countries <- c('Brazil','Ethiopia','France','Germany','Italy','Japan',
                 'Mexico','Turkiye','Russian Federation','Viet Nam')
  
  sub_pop <- subset(rbind(pop_m,pop_f),
                    subset = Country.Name%in%countries,
                    select=c('Country.Name','Country.Code','X2022','group'))
  
  sub_pop$X2022 <- sub_pop$X2022/1000000
  
###################################################
  
  # aes(..., fill = group) is used to differentiate between the groups
  
  # geom_bar(..., position = position_dodge(width = 0.5)) 
  #   creates side-by-side bars for the groups with the desired distance
  
  # scale_fill_manual(...) is used to manually set the colors for the groups.
  
  # Create a named vector for colors
  # Names of the vector must match with the names of categories in the grouping
  # variable
  
  colors <- c("Men" = "#1b9e77", "Women" = "#d95f02")
  
  ggplot(sub_pop, aes(x    = reorder(Country.Name,X2022,decreasing=FALSE), 
                      y    = X2022,
                      fill = group)) + #name of the variable that differentiates nbetween women and men 
    geom_bar(stat     = "identity",
             alpha    = 1,
             position = position_dodge(width = 0.5), # without this the barchart would be stacked 
             width=0.5) +
    coord_flip()+
    labs(title = '**There are more women than men in each country except for Ethiopia**',
         x     = '',
         y     = '',
         subtitle = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL.MA.IN?name_desc=false \n https://data.worldbank.org/indicator/SP.POP.TOTL.FE.IN?name_desc=false")+
    theme(plot.title         = element_markdown(margin = margin(b=5),hjust=-0.01),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.y        = element_line(color = "black", linewidth = 0.5),
          axis.text.y        = element_text(margin=margin(t=0)),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0)),
          legend.position    = "top", # Position legend at the top
          legend.direction   = "horizontal", # Orient legend horizontally
          legend.title       = element_blank(), # No legend title
          legend.text        = element_text(size = 8), # Set legend text size
          legend.background  = element_rect(fill = "white", colour = "white"), # Legend background
          legend.key         = element_rect(fill = "white", colour = "white"),
          legend.justification = 'left',
          legend.margin = margin(l=-5)
    )+
    scale_y_continuous(breaks = seq(0,120,20),
                       expand = c(0,0),
                       limits = c(0,120))+
    scale_fill_manual(values = colors) 
  
################################################################################    
#                        Stacked Bar Charts 
  
  # Another variation is a stacked bar chart
  # While the paired bar chart shows two or more data values for each category, 
  # this chart subdivides the data within each category. 
  
  # The categories could sum to the same total, say, 100 percent, so that the 
  # total length of the bar is the same for every group. 
  
  # Or the totals may differ across the groups, in which case the total length 
  # of each bar may differ. 
  
  # You can plot a stacked bar chart by just removing the 
  # position = position_dodge(width = 0.5)
  # argument in the geom_bar()
  
  colors <- c("Men" = "#1b9e77", "Women" = "#d95f02")
  
  ggplot(sub_pop, aes(x    = reorder(Country.Name,X2022,decreasing=FALSE), 
                      y    = X2022,
                      fill = group)) +
    geom_bar(stat     = "identity",
             alpha    = 1) +
    coord_flip()+
    labs(title = '**There are more women than men in each country except for Ethiopia**',
         x     = '',
         y     = '',
         subtitle = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL.MA.IN?name_desc=false \n https://data.worldbank.org/indicator/SP.POP.TOTL.FE.IN?name_desc=false")+
    theme(plot.title         = element_markdown(margin = margin(b=5),hjust=-0.01),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.y        = element_line(color = "black", linewidth = 0.5),
          axis.text.y        = element_text(margin=margin(t=0)),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0)),
          legend.position    = "top", # Position legend at the top
          legend.direction   = "horizontal", # Orient legend horizontally
          legend.title       = element_blank(), # No legend title
          legend.text        = element_text(size = 8), # Set legend text size
          legend.background  = element_rect(fill = "white", colour = "white"), # Legend background
          legend.key         = element_rect(fill = "white", colour = "white"),
          legend.justification = 'left',
          legend.margin = margin(l=-5)
    )+
    scale_y_continuous(breaks = seq(0,250,50),
                       expand = c(0,0),
                       limits = c(0,250))+
    scale_fill_manual(values = colors) 
  
  
  # As with the bar charts we've looked at thus far, the stacked bar chart is 
  # familiar, easy to read, and easy to create. 
  
  # The biggest challenge, however, is that it can be difficult to compare
  # the different values of the segments within the chart. 
  
  # In the example above, it's easy to compare the values across the countries 
  # for Women, because the bar segments share the same vertical baseline. 
  # But that's harder to do with Men because they do not share a baseline. 
  # Which country has more men? 
  
  # One way to address the changing baseline is to break the graph apart 
  # so that each series sits on its own vertical baseline. This is a small 
  # multiples graph, arranged side by side. 
  
  # The tradeoff is that it is harder (if not impossible) to see the total 
  # values. But that too can be overcome: You can still break up the stacked 
  # graph and add a final segment that represents the total amount 
  
  colors <- c("Men" = "#1b9e77", "Women" = "#d95f02")
  
  ggplot(sub_pop, aes(x    = reorder(Country.Name,X2022,decreasing=FALSE), 
                      y    = X2022,
                      fill = group)) +
    geom_bar(stat     = "identity",
             alpha    = 1) +
    coord_flip()+
    labs(title = '**There are more women than men in each country except for Ethiopia**',
         x     = '',
         y     = '',
         subtitle = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL.MA.IN?name_desc=false \n https://data.worldbank.org/indicator/SP.POP.TOTL.FE.IN?name_desc=false")+
    theme(plot.title         = element_markdown(margin = margin(b=5),hjust=-0.01),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.y        = element_line(color = "black", linewidth = 0.5),
          axis.text.y        = element_text(margin=margin(t=0)),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0)),
          legend.position    = "none", # Remove the legend,
          strip.background = element_rect(fill = "white"),
    )+
    scale_y_continuous(breaks = seq(0,120,20),
                       expand = c(0,0),
                       limits = c(0,120))+
    scale_fill_manual(values = colors)+
    facet_wrap(. ~ group,ncol=2)
  
  
  
  # Adding a third panel for Total Population
  
  totals       <- aggregate(X2022 ~ Country.Name + Country.Code, sub_pop, sum)
  totals$group <- 'Total'
  
  combined_data <- rbind(sub_pop, totals)
  
  combined_data$group <- factor(combined_data$group,
                          levels = c('Men','Women','Total'),
                          labels = c('Men','Women','Total'))
  
  combined_data$group
         
  colors <- c("Men" = "#1b9e77", "Women" = "#d95f02", Total = "#7570b3")
  
  ggplot(combined_data, aes(x    = reorder(Country.Name,X2022,decreasing=FALSE), 
                      y    = X2022,
                      fill = group)) +
    geom_bar(stat     = "identity",
             alpha    = 1) +
    coord_flip()+
    labs(title = '**There are more women than men in each country except for Ethiopia**',
         x     = '',
         y     = '',
         subtitle = '(Millions of people)',
         caption = "Source: The World Bank \n https://data.worldbank.org/indicator/SP.POP.TOTL.MA.IN?name_desc=false \n https://data.worldbank.org/indicator/SP.POP.TOTL.FE.IN?name_desc=false")+
    theme(plot.title         = element_markdown(margin = margin(b=5),hjust=-0.01),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.y        = element_line(color = "black", linewidth = 0.5),
          axis.text.y        = element_text(margin=margin(t=0)),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0)),
          legend.position    = "none", # Remove the legend,
          strip.background = element_rect(fill = "white"),
    )+
    scale_y_continuous(breaks = seq(0,250,50),
                       expand = c(0,0),
                       limits = c(0,250))+
    scale_fill_manual(values = colors)+
    facet_wrap(. ~ group,ncol=3)
  
  
################################################################################  
#                    Diverging Bar Charts
  
# A variation on the stacked bar chart is one in which the stacks diverge 
# from a central baseline in opposite directions. These are often found in 
# surveys where the responses are arrayed in ranges from, for instance, 
# strongly disagree to strongly agree. 
# These are often called "Likert Scales"

  
# In this example, survey respondents were asked whether they believe it is 
# the government's responsibility to reduce income inequality. 
# By grouping the "disagrees" and the "agrees" together on either side of 
# a central baseline, we can compare the total sentiment across the different countries.
  
  
  survey <- read.csv('survey.csv')

  # Convert percentage columns to numeric by removing the '%' sign and converting to numeric
  #ANOTHER WAY TO HELP 
  survey <- survey %>%
    mutate(across(Strongly_Disagree:Strongly_Agree,
                  ~ as.numeric(str_remove(., "%")))) %>%
    mutate(Disapproval = Strongly_Disagree + Disagree ) #### new column THIS IS WHAT I NEED TO DO 
  
  # Convert to long format without Disapproval
  
  survey_long <- survey %>%
    pivot_longer(cols = Strongly_Disagree:Strongly_Agree, 
                 names_to = "Opinion", 
                 values_to = "Percentage")

  # Convert 'Strongly Disagree' and 'Disagree' values to negative for the diverging plot
  
  survey_long <- survey_long %>% ## switch to - 
    mutate(Percentage = ifelse(Opinion %in% c("Strongly_Disagree", "Disagree"), -Percentage, Percentage),
           Opinion = factor(Opinion, levels = c("Strongly_Disagree", "Disagree", "Agree", "Strongly_Agree")))
  
  
  # Create the diverginf plot
  
  colors <- c("Strongly_Disagree" = "#7b3294", 
              "Disagree"          = "#c2a5cf", 
              "Agree"             = "#a6dba0",
              "Strongly_Agree"    = "#008837")
  
  
  ggplot(survey_long, aes(x = reorder(Country,
                                      Disapproval,
                                      decreasing = FALSE), 
                          y = Percentage, 
                          fill = Opinion)) +
    geom_bar(stat = "identity")  +
    scale_y_continuous(breaks = seq(-60, 80, by = 20),
                       limits = c(-60, 90),
                       labels = c(paste0(rev(seq(from=0,to=60,by=20)),'%'),
                                  paste0(seq(from=20,to=80,by=20),'%'))) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid")+
    coord_flip() +
    labs(title = '**It is the responsibility of government to reduce differences in income <br> between people with high and low incomes**',
       x     = '',
       y     = 'Percent',
       subtitle = '',
       caption = "Source: Fake data for demonstration purposes")+
    theme(plot.title         = element_markdown(margin = margin(b=5),hjust=-0.01),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0)),
          legend.position    = "top", # Position legend at the top
          legend.direction   = "horizontal", # Orient legend horizontally
          legend.title       = element_blank(), # No legend title
          legend.text        = element_text(size = 8), # Set legend text size
          legend.background  = element_rect(fill = "white", colour = "white"), # Legend background
          legend.key         = element_rect(fill = "white", colour = "white"),
          legend.justification = 'left',
          legend.margin = margin(l=-5)
    )+
    scale_fill_manual(values = colors)
  
  
  # One advantage of this chart is that the sentiments are clearly presented-
  # the Disagrees jut out to the left (in what we might typically think of as
  # a negative direction) and the Agrees out to the right. 
  # This works well if your audience is most interested in the total sentiment 
  # of each side and not necessarily comparisons between each individual component. 
  
  # If the individual comparisons are the primary focal point, 
  # then a paired bar chart could do the job just as well.
  
  # As with the stacked bar chart, the challenge with visualizing these kinds 
  # of data is that we are comparing within and across the categories. 
  # Arranging the bars in opposite directions makes it difficult to 
  # compare the totals of the two groups. In other words, 
  # it's difficult to compare the total share of people who disagree 
  # with the total share of people who agree. 
  # That task is slightly easier in the paired bar chart, but then you lose 
  # the positive-negative connotation of the diverging chart. 
  # Depending on the patterns in your data and the number of categories and groups, 
  # you might find this chart looks cluttered and busy.
  

  # What if you have a Neutral Category
  
  
  survey <- read.csv('survey2.csv')
  
  # Convert percentage columns to numeric by removing the '%' sign and converting to numeric
  survey <- survey %>%
    mutate(across(c(Strongly_Disagree:Strongly_Agree,Neutral),
                  ~ as.numeric(str_remove(., "%")))) %>%
    mutate(Disapproval = Strongly_Disagree + Disagree ) %>%
    mutate(Neutral1 = Neutral/2) %>%
    mutate(Neutral2 = Neutral/2) %>%
    select(-Neutral) 
    
  
  # Convert to long format without Disapproval
  
  survey_long <- survey %>%
    pivot_longer(cols = c(Strongly_Disagree:Strongly_Agree,Neutral1,Neutral2), 
                 names_to = "Opinion", 
                 values_to = "Percentage")
  
  # Convert 'Strongly Disagree' and 'Disagree' values to negative for the diverging plot
  
  survey_long <- survey_long %>%
    mutate(Percentage = ifelse(Opinion %in% c("Strongly_Disagree", "Disagree","Neutral1"), -Percentage, Percentage),
           Opinion = factor(Opinion, 
                            levels = c("Strongly_Disagree", 
                                       "Disagree", 
                                       "Neutral1",
                                       "Strongly_Agree"                                       ,
                                       "Agree", 
                                       "Neutral2")))
  
  
  
  # Create the diverging plot
  
  colors <- c("Strongly_Disagree" = "#7b3294", 
              "Disagree"          = "#c2a5cf",
              "Neutral1"          = "gray",
              "Neutral2"          = "gray",
              "Agree"             = "#a6dba0",
              "Strongly_Agree"    = "#008837")
  
  
  ggplot(survey_long, aes(x = reorder(Country,
                                      Disapproval,
                                      decreasing = FALSE), 
                          y = Percentage, 
                          fill = Opinion)) +
    geom_bar(stat = "identity")  +
    scale_y_continuous(breaks = seq(-100, 100, by = 20),
                       limits = c(-65, 90),
                       labels = c(paste0(rev(seq(from=0,to=100,by=20)),'%'),
                                  paste0(seq(from=20,to=100,by=20),'%'))) +
    geom_segment(y=0,yend=0,x=0,xend=10.4,linetype='solid')+
    coord_flip() +
    labs(title = '**It is the responsibility of government to reduce differences in income <br> between people with high and low incomes**',
         x     = '',
         y     = 'Percent',
         subtitle = '',
         caption = "Source: Fake data for demonstration purposes")+
    theme(plot.title         = element_markdown(margin = margin(b=5),hjust=-0.01),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0)),
          legend.position = "top",
          legend.justification = 'left'
    )+
    scale_fill_manual(values = colors)
  
    # legend in this case is not a good one
    # I want to remove the original legend
    # and then type these categories with color coding
    # horizontally between the top of the plot and title
  
    # For this first you have to remove the original legend
    
    # legend.position = 'none
  
    # and add some space above the plot
  
  p <- ggplot(survey_long, aes(x = reorder(Country,
                                      Disapproval,
                                      decreasing = FALSE), 
                          y = Percentage, 
                          fill = Opinion)) +
    geom_bar(stat = "identity")  +
    scale_y_continuous(breaks = seq(-100, 100, by = 20),
                       limits = c(-65, 90),
                       labels = c(paste0(rev(seq(from=0,to=100,by=20)),'%'),
                                  paste0(seq(from=20,to=100,by=20),'%'))) +
    geom_segment(y=0,yend=0,x=0,xend=10.4,linetype='solid')+
    coord_flip() +
    labs(title = '**It is the responsibility of government to reduce differences in income <br> between people with high and low incomes**',
         x     = '',
         y     = 'Percent',
         subtitle = '',
         caption = "Source: Fake data for demonstration purposes")+
    theme(plot.title         = element_markdown(margin = margin(b=0),hjust=-0.01),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0)),
          legend.position = "none"
    )+
    scale_fill_manual(values = colors)+
    scale_x_discrete(expand = expansion(mult=c(0,0.25)))
    
    p
  
    # Then we will add custom annotations
  
    library(grid)
    text_sd <- textGrob("Strongly \n Disagree",
                        gp=gpar(fontsize=11, fontface="bold",col=colors[1]))
    text_d  <- textGrob("Disagree", 
                        gp=gpar(fontsize=11, fontface="bold",col=colors[2]))
    text_n  <- textGrob("Neutral", 
                        gp=gpar(fontsize=11, fontface="bold",col=colors[3]))
    text_a  <- textGrob("Agree", 
                        gp=gpar(fontsize=11, fontface="bold",col=colors[5]))
    text_sa  <- textGrob("Strongly \n Agree", 
                         gp=gpar(fontsize=11, fontface="bold",col=colors[6]))
  
    p +
    annotation_custom(text_sd,ymin=-50,ymax=-50,xmin=11.5,xmax=11.5)+
    annotation_custom(text_d,ymin=-25,ymax=-25,xmin=11.5,xmax=11.5)+
    annotation_custom(text_n,ymin=0,ymax=0,xmin=11.5,xmax=11.5)+
    annotation_custom(text_a,ymin=25,ymax=25,xmin=11.5,xmax=11.5)+
    annotation_custom(text_sa,ymin=50,ymax=50,xmin=11.5,xmax=11.5)
    
  # You must be especially careful using a diverging bar chart when you have 
  # a "neutral" category. By definition, the neutral survey response is neither 
  # agree nor disagree, and should therefore be grouped with neither category.
  
  # Placing the neutral category in the middle of the chart along the 
  # vertical baseline creates a misalignment between the two groups and implies 
  # the neutral responses are split between the two sentiments. 
    
  # It also means that none of the segments sit on a vertical baseline. 
  # Placing it to the side of the chart is a better strategy because 
  # the disagree, agree, and neutral categories now all sit on their own 
  # vertical axes, even though the neutral category is somewhat emphasized as 
  # it sits to the side
  
  # Another alternative-regardless of whether you have a neutral category-
  # is a stacked bar chart. In this view, the different 
  # categories sum to 100 percent, and one can more easily compare the 
  # totals between the countries. A good strategy is to mark specific 
  # aggregate values to guide the reader. 
  # for example, I have marked the 50-percent position to make it clear
  # for which countries the total "agree" and "disagree" sentiments are at least 
  # half of the total.
    
    survey <- read.csv('survey2.csv')
    
    # Convert percentage columns to numeric by removing the '%' sign and converting to numeric
    survey <- survey %>%
      mutate(across(c(Strongly_Disagree:Strongly_Agree,Neutral),
                    ~ as.numeric(str_remove(., "%")))) %>%
      mutate(Disapproval = Strongly_Disagree + Disagree )
    
    # Convert to long format without Disapproval
    
    survey_long <- survey %>%
      pivot_longer(cols = c(Strongly_Disagree:Strongly_Agree,Neutral), 
                   names_to = "Opinion", 
                   values_to = "Percentage") %>%
      mutate(Opinion = factor(Opinion, 
                              levels = rev(c("Strongly_Disagree", 
                                         "Disagree", 
                                         "Neutral",                          
                                         "Agree", 
                                         "Strongly_Agree"))))        
    
    colors <- c("Strongly_Disagree" = "#7b3294", 
                "Disagree"          = "#c2a5cf",
                "Neutral"          = "gray",
                "Agree"             = "#a6dba0",
                "Strongly_Agree"    = "#008837")
    
    text_sd <- textGrob("Strongly \n Disagree",
                        gp=gpar(fontsize=11, fontface="bold",col=colors[1]))
    text_d  <- textGrob("Disagree", 
                        gp=gpar(fontsize=11, fontface="bold",col=colors[2]))
    text_n  <- textGrob("Neutral", 
                        gp=gpar(fontsize=11, fontface="bold",col=colors[3]))
    text_a  <- textGrob("Agree", 
                        gp=gpar(fontsize=11, fontface="bold",col=colors[4]))
    text_sa  <- textGrob("Strongly \n Agree", 
                         gp=gpar(fontsize=11, fontface="bold",col=colors[5]))
    
    
    p <- ggplot(survey_long, aes(x = reorder(Country,
                                             Disapproval,
                                             decreasing = FALSE), 
                                 y = Percentage, 
                                 fill = Opinion)) +
      geom_bar(stat = "identity")  +
      scale_y_continuous(breaks = seq(0, 100, by = 20),
                         limits = c(0, 100),
                         labels = paste0(seq(from=0,to=100,by=20),'%')) +
      geom_segment(y=50,yend=50,x=0,xend=10.4,linetype='solid')+
      coord_flip() +
      labs(title = '**It is the responsibility of government to reduce differences in income <br> between people with high and low incomes**',
           x     = '',
           y     = 'Percent',
           subtitle = '',
           caption = "Source: Fake data for demonstration purposes")+
      theme(plot.title         = element_markdown(margin = margin(b=0),hjust=-0.01),
            panel.background   = element_rect(fill='white',colour='white'),
            panel.grid.major.x = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
            axis.ticks         = element_blank(),
            plot.caption       = element_text(hjust = 0.01, 
                                              size  = 9,
                                              margin=margin(t=0)),
            legend.position = "none"
      )+
      scale_fill_manual(values = colors)+
      scale_x_discrete(expand = expansion(mult=c(0,0.25)))+
      annotation_custom(text_sd,ymin=0,ymax=0,xmin=11.5,xmax=11.5)+
      annotation_custom(text_d,ymin=25,ymax=25,xmin=11.5,xmax=11.5)+
      annotation_custom(text_n,ymin=50,ymax=50,xmin=11.5,xmax=11.5)+
      annotation_custom(text_a,ymin=75,ymax=75,xmin=11.5,xmax=11.5)+
      annotation_custom(text_sa,ymin=100,ymax=100,xmin=11.5,xmax=11.5)
    
################################################################################  
#                    Dot  Plot

# The dot plot (sometimes called a dumbbell chart, barbell chart, or gap chart) 
# is a favorite alternative to a paired or stacked bar chart.
    
# The dot plot uses a symbol-often but not always a circle-corresponding to the 
# data value, connected by a line or arrow. The data values correspond to one 
# axis and the groups to the other, which do not necessarily need to be ordered 
# in a specific way, though sorting can help.
    
# The dot plot is an easy way to compare categories-especially many categories-
# when bars might add too much ink and clutter to the page. 

# For this example, let's look at scholastic test scores around the world from 
# the Programme for International Student Assessment (PISA), an international 
# set of achievement tests taken by fifteen-year-old students in reading, 
# mathematics, and science. We can easily plot the mathematics and reading 
# scores for a set of countries using a simple bar chart, but the twenty bars 
# makes the chart heavy and dense.
    
# By contrast, the dot plot shows the same data with a dot at each data 
# value connected by a line to show the range or difference. 
    
# The circles use less ink than the bars, which lightens the visual with more 
# empty space. The country labels are placed close to the leftmost dot, 
# If necessary, data values can be placed next to, above, or within each circle.
 
# Axes and gridlines can be included or not, depending on how important it is
# for the reader to determine the exact values.
install.packages('ggtext')
library(ggtext)
pisa <- read.csv('Data/pisa.csv')
    
pisa_long <- pisa %>%
             pivot_longer(cols = c(Math, Reading), 
                   names_to = "Subject", 
                   values_to = "Score")


colors <- c("Math" = "#1b9e77", "Reading" = "#d95f02")

ggplot(pisa_long, aes(x = reorder(Country, Score, decreasing = FALSE),
                      y = Score,
                      color = Subject)) +  # Use color instead of fill for points
  geom_segment(data = pisa, 
               aes(x    = reorder(Country, Math, decreasing = FALSE), 
                   xend = reorder(Country, Math, decreasing = FALSE), 
                   y    = Math, 
                   yend = Reading),
               color    = "black",
               linewidth = 1) +
  geom_point(size = 4) + # Use geom_point for dots
  coord_flip() +
  labs(title = '**PISA scores for math and reading among 10 OECD Countries**',
       x = '',
       y = '',
       subtitle = '',
       caption = "Source: Fake data for demonstration purposes") +
  theme(plot.title = element_markdown(margin = margin(b=5), hjust=-0.01),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color='black',linewidth = 1),
        axis.ticks.length.x = unit(0.5, "cm"),
        plot.caption = element_text(hjust = 0.01, size = 9, margin=margin(t=0)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "white", colour = "white"),
        legend.key = element_rect(fill = "white", colour = "white"),
        legend.justification = 'left',
        legend.margin = margin(l=-5)
  ) +
  scale_y_continuous(breaks = seq(375, 550, 25),
                     expand = expansion(mult=c(0,0.05)),
                     limits = c(375, 550)) +
  scale_x_discrete(expand = expansion(mult=c(0.1,0.1)))+
  scale_color_manual(values = colors)+
  geom_text(data=pisa,
            aes(x=Country,
                y=Math,
                label=Country), 
            vjust = 0,
            hjust = ifelse(pisa$Math>pisa$Reading,-0.2,1.25),
            colour = "black")






colors <- c("Math" = "#1b9e77", "Reading" = "#d95f02")

ggplot(pisa_long, aes(x = reorder(Country, Score, decreasing = FALSE),
                      y = Score,
                      color = Subject)) +  # Use color instead of fill for points
  geom_segment(data = pisa, 
               aes(x    = reorder(Country, Math, decreasing = FALSE), 
                   xend = reorder(Country, Math, decreasing = FALSE), 
                   y    = Math, 
                   yend = Reading),
               color    = "black",
               linewidth = 1) +
  geom_point(size = 4) + # Use geom_point for dots
  coord_flip() +
  labs(title = "**PISA scores for <span style='color:#1b9e77;'>math</span> and <span style='color:#d95f02;'>reading</span> among 10 OECD Countries**",
       x = '',
       y = '',
       subtitle = '',
       caption = "Source: Fake data for demonstration purposes") +
  theme(plot.title = element_markdown(margin = margin(b=0), hjust=-0.01),
        panel.background = element_rect(fill='white', colour='white'),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color='black',linewidth = 1),
        axis.ticks.length.x = unit(0.5, "cm"),
        plot.caption = element_text(hjust = 0.01, size = 9, margin=margin(t=0)),
        legend.position = "none"
  ) +
  scale_y_continuous(breaks = seq(375, 550, 25),
                     expand = expansion(mult=c(0,0.05)),
                     limits = c(375, 550)) +
  scale_x_discrete(expand = expansion(mult=c(0.1,0.1)))+
  scale_color_manual(values = colors)+
  geom_text(data=pisa,
            aes(x=Country,
                y=Math,
                label=Country), 
            vjust = 0,
            hjust = ifelse(pisa$Math>pisa$Reading,-0.2,1.25),
            colour = "black")



# A few points of caution about the dot pot. First, it's not entirely obvious 
# when the direction of the values change, as in the last chart. 
# Did you notice that math scores were higher than reading scores in four of
# the countries in the dot plot above? That difference is not immediately evident 
# unless the reader carefully examines the points and their coloring. 

# In this and other cases, we should consider how sufficient annotation, 
# clear labeling, and highlighting colors can help clarify different directions. 

# The data are sorted by math scores in the dot plot, 
# which helps organize the countries, but it is still not immediately clear that 
# in only the first four countries are math scores higher than reading scores.

# One approach is to split the graph into two groups, one for countries in which
# math scores are higher than reading scores and another for countries where 
# the opposite occurred. In these versions, 
# the groups are split and then sorted with larger, bold headers to distinguish 
# them. 

# Could you add annotation above Japan saying that Math > Reading?

# And, could you add another annotation between France and United States saying
# that Reading > Math? Also, you need to increase the space between countries
# for this one?







