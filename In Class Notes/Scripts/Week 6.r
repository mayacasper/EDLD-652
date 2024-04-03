################################################################################
# The examples and explanations in this script are taken from the following book.

# Schwabish, J. (2021). Better data visualizations: A guide for scholars, 
# researchers, and wonks. Columbia University Press.

# Data to reproduce the plots were compiled by Cengiz Zopluoglu
# using the sources printed on the book.

# R code to reproduce the plots is written by 
# Cengiz Zopluoglu, University of Oregon

# 2/8/2024

require(tidyverse)
require(ggplot2)
require(ggtext)
require(ggthemes)
require(gridExtra)
require(scales)
require(lubridate)
################################################################################
#
#                               TIME SERIES
#
################################################################################

setwd("B:/UO Teaching/EDLD 652/Winter24/Week 5")

# Health care spending (% of GDP) for OECD countries
# https://stats.oecd.org/

health <- read.csv('Data/oecd_health.csv')


# The line chart and the bar chart may be the most common charts in the world. 
# The line chart is easy to read, clear in its representation, and easily drawn 
# with pen and paper. Data values are connected by lines to show values over a 
# continuous period, tracking trends and patterns.


# This line chart shows the percent of gross domestic product (GDP) spent on 
# health care in the United States over the sixteen-year period from 2000 to 2022.

usa_health <- health[health$LOCATION=='USA',]

ggplot(usa_health,
       aes(x = Year, y = Value)) +
  geom_line()

    # Commonly used arguments for geom_line
    # linetype: "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"
    # linewidth: 
    # colour: 

    ggplot(usa_health,
           aes(x = Year, y = Value)) +
      geom_line(linetype='dotdash',linewidth=2,colour='blue')


ggplot(usa_health,
       aes(x = Year, y = Value)) +
  geom_line()+
  labs(title = '**Total healthcare spending in the United States peaks in 2020**',
       x     = '',
       y     = '*(Percent of GDP)*',
       caption = "Source: OECD Stats \nhttps://stats.oecd.org/")+
  theme(plot.title         = element_markdown(margin = margin(b=25),
                                              hjust  = -0.06),
        axis.title.y       = element_markdown(size   = 10, 
                                              vjust  = 1.075,
                                              angle  = 360, 
                                              margin = margin(r = -80)),
        panel.background   = element_blank(),
        panel.grid.major.y = element_line(color      = "grey80", 
                                          linewidth  = 0.5, 
                                          linetype   = "dashed"),
        axis.line.x        = element_line(colour='black'),
        axis.text.x        = element_text(margin=margin(t=0)),
        axis.ticks.x       = element_line(),
        axis.ticks.y       = element_blank(),
        axis.ticks.length  = unit(0.25, "cm"),
        plot.caption       = element_text(hjust = 0.01, 
                                          size  = 9,
                                          margin=margin(t=0))
  )+
  scale_y_continuous(breaks = c(0,5,10,15,20),
                     limits = c(0,20),
                     expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),
                     limits = c(2000,2022))
  

# As with the bar chart, the line chart sits near the top of the perceptual 
# ranking scale. With lines relative to the same horizontal axis, it is easy to 
# compare the values to each other and between different series.


################################################################################
# 
#  Line chart with multiple lines

# There is no hard rule to dictate the number of series you can include in a 
#single line graph.

# The key is not to worry about the sheer amount of data on the graph, 
# but instead about the purpose of the graph and how you can focus your reader's 
# attention to it. 

# For example, in a line graph with many series, you can highlight or emphasize 
# a subset of your data.

# Say we were interested in showing the share of government spending on health 
# care for the United States and Germany, but we also wanted to show them in 
# relation to the other countries that make up OECD. 

# To do that, instead of giving each line the same color saturation and thickness, 
# we might only color and thicken the lines for the United States and Germany 
# and leave the lines for the other countries gray and thin. 

  ggplot(health,
         aes(x = Year, y = Value,color=Country)) +
    geom_line()
  
  
    # Create a column in data, assign gray for all country except USA and Germany
    # Assign two distinct colors for USA and Germany
    
    my_color        <- rep('gray',length(unique(health$Country))) #creates everything grey 
    names(my_color) <- unique(health$Country)
    my_color["United States"] <- 'blue'#except 
    my_color["Germany"] <- 'orange'

    ggplot(health,
           aes(x = Year, y = Value,color=Country)) +
      geom_line() +
      scale_color_manual(values=my_color)+
      guides(color=FALSE)    # Hide the legend for color
  
    # I also want the lines for USA and Germany a bit thicker
    
    health <- health %>%
      mutate(LineWidth = ifelse(Country %in% c("United States", "Germany"), 1.5, 0.5))
  
    ggplot(health,
           aes(x = Year, y = Value,color=Country,size=LineWidth)) +
      geom_line(stat = 'identity')+
      scale_size_identity()+               # necessary to use line width as specified
      scale_color_manual(values=my_color)+
      guides(color=FALSE, size = FALSE)    # Hide the legends for color and size
    
  
# Complete with other customization  
  
    ggplot(health,
           aes(x = Year, y = Value,color=Country,size=LineWidth)) +
      geom_line(stat = 'identity')+
      scale_size_identity()+               # necessary to use line width as specified
      scale_color_manual(values=my_color)+
      guides(color=FALSE, size = FALSE) +   # Hide the legends for color and size
      labs(title = "**Total healthcare spending in the <span style='color:blue'>United States</span> and <span style='color:orange'>Germany</span> <br> increased between 2000 and 2015**",
           subtitle = 'Healthcare spending peaks in 2020 after COVID pandemic',
           x     = '',
           y     = '*(Percent of GDP)*',
           caption = "Source: OECD Stats \nhttps://stats.oecd.org/")+
      theme(plot.title         = element_markdown(margin = margin(b=10),
                                                  hjust  = 0),
            plot.subtitle      = element_markdown(margin = margin(b=35),
                                                  hjust  = 0),
            axis.title.y       = element_markdown(size   = 10, 
                                                  vjust  = 1.075,
                                                  angle  = 360, 
                                                  margin = margin(r = -80,b=-60)),
            panel.background   = element_blank(),
            panel.grid.major.y = element_line(color      = "grey80", 
                                              linewidth  = 0.5, 
                                              linetype   = "dashed"),
            axis.line.x        = element_line(colour='black'),
            axis.text.x        = element_text(margin=margin(t=0)),
            axis.ticks.x       = element_line(),
            axis.ticks.y       = element_blank(),
            axis.ticks.length  = unit(0.25, "cm"),
            plot.caption       = element_text(hjust = 0.01, 
                                              size  = 9,
                                              margin=margin(t=0))
      )+
      scale_y_continuous(breaks = c(0,5,10,15,20),
                         limits = c(0,20),
                         expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         limits = c(2000,2022))


# Color and line width are two of the preattentive attributes, thus our attention 
# is drawn to the thicker, colored lines. The advantage of the gray strategy is 
# that the reader can appreciate the general pattern for the entire sample and 
# yet focus on the two lines of interest.

################################################################################
# 
# Time series in multiple panels
    
# You might also take the line graph and break it into multiple graphs, 
# the small multiples approach. We might include just the line of interest in 
# each small graph, or include all of the lines and use the gray strategy. 
# The set of small multiple line graphs on the next page uses the former 
# approach and shows spending on health care for nine OECD countries. 

# Instead of forcing all nine lines on a single graph, each country has its own 
# panel. While we might lose some perspective of the relative values of spending 
# in each country, this layout provides more space for each country and thus 
# the opportunity to provide more detail, labels, or other annotation.
    
health_sub <- health[health$LOCATION %in% c('AUS','BEL','CHE','DNK','FIN','DEU',
                                     'IRL','ESP','USA'),]
    
    
    ggplot(health_sub,
           aes(x = Year, y = Value)) +
      geom_line(color='blue',linewidth=0.5)+
     labs(title = "**Healthcare spending across major countries has largely increased since 2000**", 
           subtitle = '',
           x     = '',
           y     = '*(Percent of GDP)*',
           caption = "Source: OECD Stats \nhttps://stats.oecd.org/")+
      theme(plot.title         = element_markdown(margin = margin(b=10),
                                                  hjust  = 0),
            plot.subtitle      = element_markdown(margin = margin(b=20),
                                                  hjust  = 0),
            axis.title.y       = element_markdown(size   = 8, 
                                                  vjust  = 1.075,
                                                  angle  = 360, 
                                                  margin = margin(r = -80,b=0)),
            panel.background   = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.line.x        = element_line(colour='black'),
            axis.text.x        = element_text(margin=margin(t=0)),
            axis.ticks.x       = element_line(),
            axis.ticks.y       = element_blank(),
            axis.ticks.length  = unit(0.25, "cm"),
            plot.caption       = element_text(hjust = 0.01, 
                                              size  = 9,
                                              margin=margin(t=0)),
            strip.background = element_rect(fill = "white", colour = "white") # Set facet label background to white
      )+
      scale_y_continuous(breaks = c(0,5,10,15,20),
                         limits = c(0,20),
                         expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         limits = c(2000,2022))+
      facet_wrap(. ~ Country)
install.packages("writexl")
    library("writexl")
    
    write_xlsx(overall_rate2, "Labs 2\\Data")
################################################################################
# 
# Where should the y-axis start?
#
    
# One of the few rules of thumb of visualizing data is that bar chart axes must 
# start at zero (see Chapter 4). Because we perceive the values in the bar from 
# the length of the bars, starting the axis at something other than zero 
# overemphasizes the differences in values.

# This does not hold true for line charts. The axis of a line chart does not
# necessarily need to start at zero. As with many aspects of visualizing data, 
# there are complications and different perspectives. If we say the axis does 
# not need to start at zero, what is an appropriate range? Where should we start 
# and end the axis?

# To illustrate, let's look more closely at changes in health care spending in 
# the United States. Each of the four charts below uses a different range in the
# vertical axis. 
    
    
  p1 <- ggplot(usa_health,
           aes(x = Year, y = Value)) +
      geom_line()+
      labs(title = '**Total healthcare spending in the United States peaks in 2020**',
           x     = '',
           y     = '*(Percent of GDP)*',
           caption = "Source: OECD Stats \nhttps://stats.oecd.org/")+
      theme(plot.title         = element_markdown(margin = margin(b=25),
                                                  hjust  = -0.06),
            axis.title.y       = element_markdown(size   = 10, 
                                                  vjust  = 1.075,
                                                  angle  = 360, 
                                                  margin = margin(r = -80)),
            panel.background   = element_blank(),
            panel.grid.major.y = element_line(color      = "grey80", 
                                              linewidth  = 0.5, 
                                              linetype   = "dashed"),
            axis.line.x        = element_line(colour='black'),
            axis.text.x        = element_text(margin=margin(t=0)),
            axis.ticks.x       = element_line(),
            axis.ticks.y       = element_blank(),
            axis.ticks.length  = unit(0.25, "cm"),
            plot.caption       = element_text(hjust = 0.01, 
                                              size  = 9,
                                              margin=margin(t=0))
      )+
      scale_y_continuous(breaks = c(0,5,10,15,20),
                         limits = c(0,20),
                         expand = c(0,0))+
      scale_x_continuous(expand = c(0,0),
                         limits = c(2000,2022))
    
    
    
    p2 <- p1 +
          scale_y_continuous(breaks = seq(10,19,by=2),
                             limits = c(10,19),
                             expand = c(0,0))
      
    p3 <- p1 +
      scale_y_continuous(breaks = seq(12.49,19,by=.5),
                         labels = seq(12.5,19,by=.5),
                         limits = c(12.49,19),
                         expand = c(0,0))
    
    p4 <- p1 +
      scale_y_continuous(breaks = seq(12,19,by=1),
                         limits = c(12,19),
                         expand = c(0,0))
    
    
    grid.arrange(p1,p2,p3,p4,ncol=2)
    
# As you can plainly see, those ranges affect our perception of 
# the level and the change in spending. In the top-left graph, where the axis 
# ranges from 0 to 20, we see a slight increase in spending. As you move clockwise 
# through the graphs and the axis range gets smaller and smaller, the change in 
# spending looks increasingly dramatic.
    
# There is no right answer to the choice of the vertical axis; the answer 
# depends on the data and your goal. If you need to demonstrate that the 
# economy will falter if spending reaches 17 percent of GDP, then the 
# bottom-right chart may be best. If you are telling a more general story, 
# then one of the graphs in the top row might be preferable, because it still 
# clearly shows the increase in spending over time. If you need to show a 
# detailed examination of spending in each year, you might want to consider 
# the graphs on the right.

################################################################################
# 
# Including data markers to mark specific values
#

# Data markers are symbols along the line to mark specific points in the series. 
# There isn't a right answer to the question of when to deploy them. 
# You may include data markers when you have only few lines or data points, or 
# for specific points you want to label or annotate. 
# The data markers give the graph more visual weight.

# These charts, for example, show health care spending as a share of GDP for 
# Germany and Spain. There are so few data points and the changes in the series 
# are so subtle that the addition of the circular data markers give the lines 
# more visual heft.
    
    
    health_sub <- health[health$LOCATION %in% c('ESP','DEU'),]
    
    
    ggplot(health_sub,
           aes(x = Year, y = Value,color=Country)) +
      geom_line(linewidth=0.5)+
      geom_point()+
      labs(title = "**Healthcare spending in Germany and Spain**", 
           subtitle = '',
           x     = '',
           y     = '*(Percent of GDP)*',
           caption = "Source: OECD Stats \nhttps://stats.oecd.org/")+
      theme(plot.title         = element_markdown(margin = margin(b=40),
                                                  hjust  = -0.05),
            plot.subtitle      = element_markdown(margin = margin(b=0),
                                                  hjust  = 0),
            axis.title.y       = element_markdown(size   = 8, 
                                                  vjust  = 1.075,
                                                  angle  = 360, 
                                                  margin = margin(r = -80,b=0)),
            panel.background   = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.line.x        = element_line(colour='black'),
            axis.text.x        = element_text(margin=margin(t=0)),
            axis.ticks.x       = element_line(),
            axis.ticks.y       = element_blank(),
            axis.ticks.length  = unit(0.25, "cm"),
            plot.caption       = element_text(hjust = 0.01, 
                                              size  = 9,
                                              margin=margin(t=0)),
            strip.background = element_rect(fill = "white", colour = "white") # Set facet label background to white
      )+
      scale_y_continuous(breaks = seq(6,14,by=2),
                         limits = c(6,15),
                         expand = c(0,0))+
      scale_x_continuous(expand = c(0,0.1),
                         limits = c(2000,2022))+
      guides(color=FALSE)+
      geom_text(data = data.frame(x=c(2000,2000),
                                  y=c(10,6.75),
                                  Country = c('Germany','Spain')),
                aes(x=x,y=y,label=Country),
                hjust = 0,
                vjust = -1,
                size = 5)
    
# You make my data markers circles, triangles, squares, or other shapes. 
# This is partly an aesthetic preference.
  
# Circles are perfectly symmetrical, and so it never matters where the line 
# intersects the circle. 
    
# With other shapes, like triangles, the line might intersect the thinner top 
# part or the thicker bottom part.

# Other shapes may be necessary if you or your organization are required to 
# comply with certain rules or laws that enable screen readers to differentiate 
# between objects on a screen for people with vision disabilities. 

# In the United States, federal government agencies are required to follow 
# Federal Section 508 regulations that make websites accessible to people with 
# disabilities.
    
# Even with different colors, most screen readers cannot differentiate between 
# the different series if the shapes are all the same. 
# In these cases, different data markers are a good choice.
    
    # https://sape.inf.usi.ch/quick-reference/ggplot2/shape
    
    my_colors <- c(Germany='#1b9e77',Spain='#d95f02') # this is how we specify the color and shaped we want to use 
    my_shapes <- c(Germany=16,Spain=15)
    
    ggplot(health_sub,
           aes(x = Year, y = Value,color=Country,shape=Country)) # changes color of line by country, shape = changes the shape of the dots 
      geom_line(linewidth=0.5)+
      geom_point(size = 3, alpha = 1)+ # alpha = transparency 
      scale_color_manual(values = my_colors)+ #this over rides the default shape or color 
      scale_shape_manual(values = my_shapes)+
      guides(color=FALSE,shape=FALSE)+ # this changes the legend to reflect the change in color and shape 
      labs(title = "**Healthcare spending in Germany and Spain**", 
           subtitle = '',
           x     = '',
           y     = '*(Percent of GDP)*',
           caption = "Source: OECD Stats \nhttps://stats.oecd.org/")+
      theme(plot.title         = element_markdown(margin = margin(b=40),
                                                  hjust  = -0.05),
            plot.subtitle      = element_markdown(margin = margin(b=0),
                                                  hjust  = 0),
            axis.title.y       = element_markdown(size   = 8, 
                                                  vjust  = 1.075,
                                                  angle  = 360, 
                                                  margin = margin(r = -80,b=0)),
            panel.background   = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.line.x        = element_line(colour='black'),
            axis.text.x        = element_text(margin=margin(t=0)),
            axis.ticks.x       = element_line(),
            axis.ticks.y       = element_blank(),
            axis.ticks.length  = unit(0.25, "cm"),
            plot.caption       = element_text(hjust = 0.01, 
                                              size  = 9,
                                              margin=margin(t=0)),
            strip.background = element_rect(fill = "white", colour = "white") # Set facet label background to white
      )+
      scale_y_continuous(breaks = seq(6,14,by=2),
                         limits = c(6,15),
                         expand = c(0,0.2))+
      scale_x_continuous(expand = c(0,0.2),
                         limits = c(2000,2022))+
      geom_text(data = data.frame(x=c(2000,2000),
                                  y=c(10,6.75),
                                  Country = c('Germany','Spain')),
                aes(x=x,y=y,label=Country),
                hjust = 0,
                vjust = -1,
                size = 5)
    
################################################################################
# 
# Using visual signals and annotations for missing data
#
    
# Missing data are truly missing when a regular series is interrupted because
# the data were not collected or available. In these cases, you should make it 
# clear that the data are incomplete. 

# In line charts, we can change the formatof the line (for instance, with dashes)
# or not connect the points at all to signal that those data points are missing.

# We can also place a note on the chart or below the chart to explain that 
# those data values are unavailable.
    
# Signal missing values with the gap and annotation
    
health_sub <- health[health$LOCATION %in% c('ESP','DEU'),]
    
health_sub[8:11,]$Value <- NA
health_sub[31:34,]$Value <- NA

      my_colors <- c(Germany='#1b9e77',Spain='#d95f02')
      my_shapes <- c(Germany=16,Spain=15)
############# finish creating fake missingness
      
      ggplot(health_sub,
             aes(x = Year, y = Value,color=Country,shape=Country)) +
        geom_line(linewidth=0.5)+
        geom_point(size = 3, alpha = 1)+
        scale_color_manual(values = my_colors)+
        scale_shape_manual(values = my_shapes)+
        guides(color=FALSE,shape=FALSE)+
        labs(title = "**Healthcare spending in Germany and Spain**", 
             subtitle = '',
             x     = '',
             y     = '*(Percent of GDP)*',
             caption = "Source: OECD Stats \nhttps://stats.oecd.org/")+
        theme(plot.title         = element_markdown(margin = margin(b=40),
                                                    hjust  = -0.05),
              plot.subtitle      = element_markdown(margin = margin(b=0),
                                                    hjust  = 0),
              axis.title.y       = element_markdown(size   = 8, 
                                                    vjust  = 1.075,
                                                    angle  = 360, 
                                                    margin = margin(r = -80,b=0)),
              panel.background   = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.line.x        = element_line(colour='black'),
              axis.text.x        = element_text(margin=margin(t=0)),
              axis.ticks.x       = element_line(),
              axis.ticks.y       = element_blank(),
              axis.ticks.length  = unit(0.25, "cm"),
              plot.caption       = element_text(hjust = 0.01, 
                                                size  = 9,
                                                margin=margin(t=0)),
              strip.background = element_rect(fill = "white", colour = "white") # Set facet label background to white
        )+
        scale_y_continuous(breaks = seq(6,14,by=2),
                           limits = c(6,15),
                           expand = c(0,0.2))+
        scale_x_continuous(expand = c(0,0.2),
                           limits = c(2000,2022))+
        geom_text(data = data.frame(x=c(2000,2000), ### Lab 2 
                                    y=c(10,6.75),
                                    Country = c('Germany','Spain')),
                  aes(x=x,y=y,label=Country),
                  hjust = 0,
                  vjust = -1,
                  size = 5)+
        annotate('text',x=2008.5,y=9.5,label='Gaps denote \n missing data') #### Lab 2 

# Signal missing values with a dashed line
  
  # Fill the missing data with hypothetical values so that the gap connects
  # the data points with a linear line
      
   missing     <- health_sub[c(7:12,30:35),] # Creates a data subset with the missing data and info from the one year before and after missing 
    

   missing[2:5,]$Value <- seq(health_sub[7,]$Value, #before missingingness 
                                 health_sub[12,]$Value, #after missingingness
                                 length=6)[2:5] #fill with values equal distance apart 
  #replaces NA values with the new vector numbers 
   
  #same for Germany 
   missing[8:11,]$Value <- seq(health_sub[30,]$Value,
                                  health_sub[36,]$Value,
                                  length=6)[2:5]
      

  my_colors <- c(Germany='#1b9e77',Spain='#d95f02')
  my_shapes <- c(Germany=16,Spain=15)
  
  ggplot(health_sub,
         aes(x = Year, y = Value,color=Country,shape=Country)) +
    geom_line(linewidth = 0.5)+
    geom_point(size = 3, alpha = 1)+
    scale_color_manual(values = my_colors)+
    scale_shape_manual(values = my_shapes)+
    guides(color=FALSE,shape=FALSE,linetype=FALSE)+
    labs(title = "**Healthcare spending in Germany and Spain**", 
         subtitle = '',
         x     = '',
         y     = '*(Percent of GDP)*',
         caption = "Source: OECD Stats \nhttps://stats.oecd.org/")+
    theme(plot.title         = element_markdown(margin = margin(b=40),
                                                hjust  = -0.05),
          plot.subtitle      = element_markdown(margin = margin(b=0),
                                                hjust  = 0),
          axis.title.y       = element_markdown(size   = 8, 
                                                vjust  = 1.075,
                                                angle  = 360, 
                                                margin = margin(r = -80,b=0)),
          panel.background   = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.line.x        = element_line(colour='black'),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.ticks.x       = element_line(),
          axis.ticks.y       = element_blank(),
          axis.ticks.length  = unit(0.25, "cm"),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0)),
          strip.background = element_rect(fill = "white", colour = "white") # Set facet label background to white
    )+
    scale_y_continuous(breaks = seq(6,14,by=2),
                       limits = c(6,15),
                       expand = c(0,0.2))+
    scale_x_continuous(expand = c(0,0.2),
                       limits = c(2000,2022))+
    geom_text(data = data.frame(x=c(2000,2000),
                                y=c(10,6.75),
                                Country = c('Germany','Spain')),
              aes(x=x,y=y,label=Country),
              hjust = 0,
              vjust = -1,
              size = 5)+
    geom_line(data = missing,aes(x=Year,y=Value),linetype='dashed')+ #adds the new dashed line 
    annotate('text',x=2008.5,y=9.5,
             label='Dashing lines denote \n missing data')
  

################################################################################
# 
# Dual axis line charts
#
  
housing_debt <- read.csv('Data/TDSP.csv') #data reports is quarter 
unemployment <- read.csv('Data/unemployment.csv')[,1:2] #data reports in month
  
merged_data <- inner_join(housing_debt, unemployment, 
                          by = c("DATE" = "Month")) # only pulls months that have the same info 

merged_data$DATE <- as.Date(merged_data$DATE,"%m/%d/%Y") #changes to a variable thats actually a date not a character 
  
  # ~ . *1.5 - 10
  
  # Found min, max, and desired range on the primary axis
  # max(merged_data$TDSP)
  # min(merged_data$TDSP)
  # 13.5 - 9.5 = 4
  
  # Found min, max, and desired range on the secondary axis
  # max(merged_data$Total)
  # min(merged_data$Total)
  # 10.5 - 3.5 = 7

  # One to one transformation between the primary and secondary axes


  # ratio,                 # ratio of the ranges between axes
  # max(merged_data$Total),# maximum of the secondary axes
  # ratio*(primary_axes) - (ratio*max(merged_data$TDSP)-max(merged_data$Total))
  
  # .*(7/4)-(23.625-10.5)

  # seq(9.5,13.5,0.5)*(7/4)-(23.625-10.5)


  ggplot(merged_data, aes(x = DATE)) + #doesn't provide a common y because their will be different y axis 
    geom_line(aes(y = TDSP, color = "blue"),linewidth=0.5) +
    scale_y_continuous(
      breaks   = seq(9.5,13.5,0.5),
      limits   = c(9.5,13.5),
      expand   = c(0,0),
      labels   = paste0(format(seq(9.5,13.5,0.5),digits = 2),'%'),
      sec.axis = sec_axis(~ .*(7/4)-(23.625-10.5), #adds new axis 
                          breaks    = seq(3.5,10.5,1),
                          labels   = paste0(format(seq(3.5,10.5,1),digits = 2),'%'))) + #labels axis 
    geom_line(aes(y = (Total+(23.625-10.5))/(7/4), color = "orange"),linewidth=0.5) + #adds new line
    scale_color_manual(
      values = c("blue" = "blue", "orange" = "orange"),
      labels = c("Housing Debt", "Unemployment Rate"))+
    labs(title = "**The economic climate for consumers in 2017 and 2018 was quite good**", 
         subtitle = '',
         x     = '',
         y     = '',
         caption = "Source: Household debt service ratio, Federal Reserve Board of Governors \n Unemployment rate, Bureau of Labor Statistics")+
      theme(plot.title         = element_markdown(margin = margin(b=0),
                                                  hjust  = -0.05),
            plot.subtitle      = element_markdown(margin = margin(b=0),
                                                  hjust  = 0),
            axis.title.y       = element_markdown(size   = 8, 
                                                  vjust  = 1.075,
                                                  angle  = 360, 
                                                  margin = margin(r = -80,b=0)),
            panel.background   = element_blank(),
            panel.grid.major.y = element_line(color='gray',linetype='dashed',size=0.05),
            axis.line.x        = element_line(colour='black'),
            axis.text.x        = element_text(margin=margin(t=0)), #### Lab 2
            axis.ticks.x       = element_line(),
            axis.ticks.y       = element_blank(),
            axis.ticks.length  = unit(0.25, "cm"),
            plot.caption       = element_text(hjust = 0.01, 
                                              size  = 9,
                                              margin=margin(t=0)),
            legend.position      = 'top',
            legend.background    = element_blank(),
            legend.title         = element_blank(),
            legend.justification = 'left',
            legend.key           = element_rect(color='white',fill='white')
            )+
      scale_x_date(
        expand = c(0,0),
        date_breaks = "1 year",
        date_labels = "%Y"
      )
  
  
# You might be inclined to add another vertical axis to your line chart when 
# comparing changes in two or more series of different units. 
  
# Resist that urge. Consider this dual-axis line chart that shows the share of 
# income devoted to paying for housing on the left axis and the quarterly unemployment
#rate on the right axis in the United States from 2004 to 2018. 

# There are three problems with plotting the data like this.
  
# 1) They are often hard to read. Did you intuitively know which lines 
#    corresponded to which axis? 
#    Even if the labels and axes were colored to match the lines 
#    (which many dual-axis charts don't include), it's hard to discern patterns 
#    in the data. They're extra work for the reader, especially when the labeling 
#    is not obvious.
  
# 2) Second, the gridlines may not match up. Notice how the horizontal gridlines
#    in this graph are associated with the left axis, which leaves the numbers on 
#    the right axis floating in space. 
  
# 3) Third, and most importantly, the point where the lines cross becomes a 
#    focal point, even though it may have no real meaning. In this graph, the 
#    eye is drawn to the middle of the chart where the two lines intersect, 
#    because that's where the most interesting thing is happening. 
#    But there's nothing special about 2009, it's just a coincidence that they 
#    crossed at that time. 
  

# The vertical axis in a line chart does not need to start at zero, 
# so this chart-with the left axis starting at 9.5 percent and the right axis 
# starting at 2.5 percent-is a perfectly reasonable way to plot the two series. 

# By that logic, we could arbitrarily change the dimensions of each axis to make
# the lines cross wherever we like. 
# And this is the problem with dual-axis line charts: the chart creator can 
# deliberately mislead readers about the relationship between the series.
  
  
  p <- ggplot(merged_data, aes(x = DATE)) +
    geom_line(aes(y = TDSP, color = "blue"),linewidth=0.5) +
    scale_color_manual(
      values = c("blue" = "blue", "orange" = "orange"),
      labels = c("Housing Debt", "Unemployment Rate")
    )+
    labs(title = "**The economic climate for consumers in 2017 and 2018 was quite good**", 
         subtitle = '',
         x     = '',
         y     = '',
         caption = "Source: Household debt service ratio, Federal Reserve Board of Governors \n Unemployment rate, Bureau of Labor Statistics")+
    theme(plot.title         = element_markdown(margin = margin(b=0),
                                                hjust  = -0.05),
          plot.subtitle      = element_markdown(margin = margin(b=0),
                                                hjust  = 0),
          axis.title.y       = element_markdown(size   = 8, 
                                                vjust  = 1.075,
                                                angle  = 360, 
                                                margin = margin(r = -80,b=0)),
          panel.background   = element_blank(),
          panel.grid.major.y = element_line(color='gray',linetype='dashed',size=0.05),
          axis.line.x        = element_line(colour='black'),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.ticks.x       = element_line(),
          axis.ticks.y       = element_blank(),
          axis.ticks.length  = unit(0.25, "cm"),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0)),
          legend.position      = 'top',
          legend.background    = element_blank(),
          legend.title         = element_blank(),
          legend.justification = 'left',
          legend.key           = element_rect(color='white',fill='white')
    )+
    scale_x_date(
      expand = c(0,0),
      date_breaks = "1 year",
      date_labels = "%Y"
    )

  
  p1 <- p + 
    scale_y_continuous(
      breaks   = seq(0,14,2),
      limits   = c(0,14),
      expand   = c(0,0),
      labels   = paste0(format(seq(0,14,2),digits = 1),'%'),
      sec.axis = sec_axis(~ ./2 + 3,
                          breaks    = seq(3,10,1),
                          labels   = paste0(format(seq(3,10,1),digits = 1),'%'))
    ) +
    geom_line(aes(y = (Total-3)*2, color = "orange"),linewidth=0.5)
    
    # are closely matched for a few years around 2010 and 2012;
  
  p2 <- p + 
    scale_y_continuous(
      breaks   = seq(9,15,1),
      limits   = c(9,15),
      expand   = c(0,0),
      labels   = paste0(format(seq(9,15,1),digits = 1),'%'),
      sec.axis = sec_axis(~ .*(10/6) - 15,
                          breaks    = seq(0,10,1),
                          labels   = paste0(format(seq(0,10,1),digits = 1),'%'))
    ) +
    geom_line(aes(y = (Total+15)/(10/6), color = "orange"),linewidth=0.5)

    # are closely matched for a few years around 2004 and 2008;
  
  
# By arbitrarily choosing the axes range, we can make different data series 
# look as correlated as we like. 
# On his website, Spurious Correlations, Tyler Vigen shows all kinds of 
# dual-axis charts in which arbitrary vertical axis scales create erroneous-and 
# humorous-visualizations for spurious correlations.
  
  # See an example
  # https://www.tylervigen.com/spurious/correlation/2733_the-distance-between-jupiter-and-the-sun_correlates-with_the-number-of-secretaries-in-alaska
  
  
# If it's important to annotate a specific point on the horizontal axis, 
# you could also vertically arrange the two and draw a line across both. 
# This will change the rotation of the final graphic, but is an easier way to 
# label a specific value or year.
  
  p1 <- ggplot(merged_data, aes(x=DATE, y = TDSP)) +
    geom_line(linewidth=1,color = 'blue')+
    labs(title = "**Housing debt in the United States has declined since 2018**", 
         subtitle = '',
         x     = '',
         y     = '',
         caption = '')+
    scale_y_continuous(
      breaks   = seq(8,14,1),
      limits   = c(8,14),
      expand   = c(0,0),
      labels   = paste0(format(seq(8,14,1),digits = 1),'%')
      )+
    geom_segment(aes(x    = ymd("2009-07-01"),
                     xend = ymd("2009-07-01"),
                     y    = 8,
                     yend = 14),color='black',linewidth=1)+
    theme(plot.title         = element_markdown(margin = margin(b=0),
                                                hjust  = -0.05,
                                                size   = 12),
          plot.margin        = margin(b=-20),
          plot.subtitle      = element_markdown(margin = margin(b=0),
                                                hjust  = 0),
          panel.background   = element_blank(),
          panel.grid.major.y = element_line(color='gray',linetype='dashed',size=0.05),
          axis.line.x        = element_line(colour='black'),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.ticks.x       = element_line(),
          axis.ticks.y       = element_blank(),
          axis.ticks.length  = unit(0.25, "cm"))+
    scale_x_date(
      expand = c(0,0),
      date_breaks = "2 year",
      date_labels = "%Y"
    )+
    annotate('text',x=ymd("2009-07-01"),y=13.5,hjust=-0.03,label = 'Recession ends, June 2009',size=3) ##Lab 2
    
  p2 <- ggplot(merged_data, aes(x=DATE, y = Total)) +
    geom_line(linewidth=1, color = 'orange')+
    labs(title = "**The unemployment rate in the United States has declined since 2010**", 
         subtitle = '',
         x     = '',
         y     = '',
         caption = '')+
    scale_y_continuous(
      breaks   = seq(0,12,2),
      limits   = c(0,12),
      expand   = c(0,0),
      labels   = paste0(format(seq(0,12,2),digits = 1),'%')
    )+
    geom_segment(aes(x    = ymd("2009-07-01"),
                     xend = ymd("2009-07-01"),
                     y    = 0,
                     yend = 12),color='black',linewidth=1)+
    theme(plot.title         = element_markdown(margin = margin(b=0),
                                                hjust  = -0.05,size=12),
          plot.margin        = margin(b=-20),
          plot.subtitle      = element_markdown(margin = margin(b=0),
                                                hjust  = 0),
          panel.background   = element_blank(),
          panel.grid.major.y = element_line(color='gray',linetype='dashed',size=0.05),
          axis.line.x        = element_line(colour='black'),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.ticks.x       = element_line(),
          axis.ticks.y       = element_blank(),
          axis.ticks.length  = unit(0.25, "cm"))+
    scale_x_date(
      expand = c(0,0),
      date_breaks = "2 year",
      date_labels = "%Y"
    )
  
  
  grid.arrange(p1,p2,ncol=1,heights=c(1,1)) #arrangement of plots 
  
################################################################################
# 
# Connected Scatterplot ordered by date
#
  
# If showing the changes in the associations between the two series is important,
# you can try a connected scatterplot. 

# The connected scatterplot is like a scatterplot with a horizontal and vertical 
# axis, but each point represents a different unit of time, 
# such as a quarter or a year. 
  
# It's easier to see how the relationship has changed over time between these 
# two metrics. You can also add more labels and annotation 
# to help the reader navigate the visual.
  
  
  merged_data <- inner_join(housing_debt, unemployment, 
                            by = c("DATE" = "Month"))
  
  merged_data$DATE <- as.Date(merged_data$DATE,"%m/%d/%Y")
  
  
  recession    <-   merged_data[16:23,]
  merged_data[17:22,]$TDSP  <- NA
  merged_data[17:22,]$Total <- NA
  
  
  ggplot() + 
    geom_point(data=merged_data,aes(x=Total,y=TDSP),color='#0c72af',size=2) +
    geom_path(data=merged_data,aes(x=Total,y=TDSP),color='#0c72af',linewidth=0.75) + #use instead of geom line 
    geom_point(data=recession,aes(x=Total,y=TDSP),color='#f7c20b',size=2) +
    geom_path(data=recession,aes(x=Total,y=TDSP),color='#f7c20b',linewidth=0.75)+
    scale_x_continuous(
      breaks   = seq(3,10,1),
      limits   = c(3,10.2),
      expand   = c(0,0),
      labels   = paste0(format(seq(3,10,1),digits = 1),'%')
    )+
    scale_y_continuous(
      breaks   = seq(8,14,1),
      limits   = c(8,14.5),
      expand   = c(0,0),
      labels   = paste0(format(seq(8,14,1),digits = 1),'%')
    )+
    labs(title = "**The U.S. economy appears supported of the consumer with <br> low-unemployment rate and housing debt**", 
         subtitle = '',
         x     = 'Unemployment rate',
         y     = '*(Household debt service ratio)*',
         caption = '')+
    annotate('text',
             x        = 5.75,
             y        = 12.22,
             hjust    = 0,
             vjust    = 0,
             label    = '1Q2004',
             size     = 3,
             color    = '#0c72af',
             fontface = 'bold')+
    annotate('text',
             x        = 4.7,
             y        = 13.3,
             hjust    = 0.5,
             vjust    = -1,
             label    = '4Q2007',
             size     = 3,
             color    = '#f7c20b',
             fontface = 'bold')+
    annotate('text',
             x        =  5.25,
             y        = 13.75,
             hjust    = 0,
             vjust    = 0,
             label    = 'Great Recession (4Q2007-2Q2009)',
             size     = 3,
             color    = '#f7c20b',
             fontface = 'bold')+
     annotate('text',
              x       = 5.25,
              y       = 13.5,
              hjust   = 0,
              vjust   = 0,
              label   = 'Unemployment rate rises swiftly and housing debt stabilizes',
             size     = 3,
             color    = '#f7c20b')+
    annotate('text',
             x        = 9.45,
             y        = 11.8,
             hjust    = 0,
             vjust    = 0,
             label    = '4Q2009',
             size     = 3,
             color    = '#0c72af',
             fontface = 'bold')+
    annotate('text',
             x        = 6.05,
             y        = 9.6,
             hjust    = 0,
             vjust    = 0,
             label    = '2009-2018',
             size     = 3,
             color    = '#0c72af',
             fontface = 'bold')+
    annotate('text',
             x        = 6.05,
             y        = 9.35,
             hjust    = 0,
             vjust    = 0,
             label    = 'Over the last decade, the unemployment rate has',
             size     = 3,
             color    = '#0c72af')+
    annotate('text',
             x        = 6.05,
             y        = 9.1,
             hjust    = 0,
             vjust    = 0,
             label    = 'fallen steadily while debt shares have leveled off',
             size     = 3,
             color    = '#0c72af')+
    annotate('text',
             x        = 3.05,
             y        = 9.6,
             hjust    = 0,
             vjust    = 0,
             label    = '4Q2018',
             size     = 3,
             color    = '#0c72af',
             fontface = 'bold')+
    annotate('text',
             x        = 3.05,
             y        = 9.35,
             hjust    = 0,
             vjust    = 0,
             label    = 'The unemployment rate is about',
             size     = 3,
             color    = '#0c72af')+
    annotate('text',
             x        = 3.05,
             y        = 9.1,
             hjust    = 0,
             vjust    = 0,
             label    = 'where it was in 2000, but the share',
             size     = 3,
             color    = '#0c72af')+
    annotate('text',
             x        = 3.05,
             y        = 8.85,
             hjust    = 0,
             vjust    = 0,
             label    = 'of income devoted to housing debt is',
             size     = 3,
             color    = '#0c72af')+
    annotate('text',
             x        = 3.05,
             y        = 8.6,
             hjust    = 0,
             vjust    = 0,
             label    = '1.7 perdentage points lower',
             size     = 3,
             color    = '#0c72af')+
    theme(plot.title         = element_markdown(margin = margin(b=10,l=-25),
                                                hjust  = 0,
                                                size=12),
          plot.subtitle      = element_markdown(margin = margin(b=0),
                                                hjust  = 0),
          plot.margin        = margin(r=25,b=5,t=5,l=5),
          panel.background   = element_blank(),
          panel.grid.major.y = element_line(color='gray',linetype='dashed',size=0.05),
          panel.grid.major.x = element_line(color='gray',linetype='dashed',size=0.05),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.title.y       = element_markdown(vjust  = 1.075,
                                                angle  = 360, 
                                                margin = margin(r = -150,t=20)),
          axis.ticks.x       = element_line(),
          axis.ticks.y       = element_blank(),
          axis.ticks.length  = unit(0.25, "cm"))
                  
################################################################################
# 
# Slope Chart
#  
  
  
# In some cases, it may not be necessary to show all of the data in your time 
# series. In these cases, a slope chart is a useful alternative.
  
# The paired bar chart is a standard way to visualize two data points for 
# multiple observations (also see page 84). Consider this 
# chart of changes in the unemployment rate for six states in the United States 
# between Dec 2022 and Dec 2023
  
  unemployment_state <- read.csv('Data/unemployment_states.csv')
  
  unemployment_state$diff <- abs(unemployment_state[,2]-unemployment_state[,3])  #created new 
  
  unemployment_state[order(unemployment_state$diff,decreasing=T),]
  
  df <- unemployment_state %>% 
    pivot_longer(cols      = c(Dec_22,Dec_23),
                 names_to  = 'Time',
                 values_to = 'Unemployment') %>%
    filter(State %in% c('New Jersey','Maryland','Oregon','California',
                        'Wyoming','District of Columbia')) #reorders data into long form
  
  
  colors <- c("Dec_22" = "#1b9e77", "Dec_23" = "#d95f02")
  
  ggplot(df, aes(x    = reorder(State,diff,decreasing=FALSE), 
                 y    = Unemployment,
                 fill = Time)) +
    geom_bar(stat     = "identity",
             alpha    = 1,
             position = position_dodge(width = 0.5),
             width=0.5) +
    labs(title = '**Biggest changes in the unemployment rate between December 22 and December 2023**',
         x     = '',
         y     = '',
         subtitle = '',
         caption = "Source: Bureau of Labor Statistics")+
    theme(plot.title         = element_markdown(margin = margin(b=5),hjust=-0.01),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5, linetype = "dashed"),
          axis.line.x        = element_line(color = "black", linewidth = 0.5),
          axis.text.x        = element_text(margin=margin(t=0)),
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
    scale_y_continuous(breaks = seq(0,5.5,0.5),
                       expand = c(0,0),
                       limits = c(0,5.5))+
    scale_fill_manual(values = colors) 
  
# With this kind of visualization, we ask our reader to process the level and 
# change in the unemployment rate between and across the six states. 
# There is a lot of ink in the graph, and it asks the reader to do lots of 
# mental math. We could, of course, just plot the change between the two time 
# periods, but we often want to show both the level and the change.

# The slope chart addresses this challenge by plotting each data point on a 
# separate vertical axis and connecting the two with a line. 
  
#May want to use in my final 
  
  my_colors <- c("Wyoming"              = '#e64173',
                 "California"           = '#20B2AA',
                 "Oregon"               = '#FFA500',
                 "Maryland"             = '#fb6107',
                 "New Jersey"           = '#3b3b9a',
                 "District of Columbia" = '#8bb174')
  
  ggplot(df, aes(x = Time, 
                 y = Unemployment, 
                 group = State, 
                 color = State)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = my_colors)+
    guides(color=FALSE)+
    labs(title = '**Biggest changes in the unemployment rate between December 22 and December 2023**',
         x     = '',
         y     = '',
         subtitle = '',
         caption = "Source: Bureau of Labor Statistics")+
    theme(plot.title         = element_markdown(margin = margin(b=5),hjust=-0.01),
          plot.margin        = margin(l=20,r=20),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_blank(),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.text.y        = element_blank(),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    ) +
    scale_x_discrete(labels = c('December 2022','December 2023'),
                     expand = c(0.25,0.1)) +
    geom_text(aes(label = ifelse(Time == "Dec_22", 
                                 paste0(State," ",Unemployment, "%"), 
                                 paste(Unemployment, "%"))),
              hjust = ifelse(df$Time == "Dec_22", 1.1, -.5), 
              vjust = .5,
              size = 3)
 
  
# There are many ways to slope the slope chart. 
# We can use two colors to denote increases and decreases. 
# We can include or exclude labels for levels and changes. We can even adjust the  
# thickness of the line to correspond to a third variable. 
  
# We could also rely on the Start with Gray strategy and add more data to the 
# basic slope chart. Here, every state is included in the nation but only 
# the six states of interest and the national average are highlighted.
  
  unemployment_state[52,] <- c('Nation',
                               round(mean(unemployment_state$Dec_22),1),
                               round(mean(unemployment_state$Dec_23),1),
                               NA)
  
  highlight <- c('New Jersey','Maryland','Oregon','California',
                 'Wyoming','District of Columbia','Nation')
  
  df <- unemployment_state %>% 
    pivot_longer(cols      = c(Dec_22,Dec_23),
                 names_to  = 'Time',
                 values_to = 'Unemployment')
  
  
  my_colors         <- rep('gray',nrow(df)/2)
  names(my_colors)  <- unique(df$State)
  my_colors[highlight] <- c('#e64173','#20B2AA','#FFA500','#fb6107',
                              '#3b3b9a','#8bb174','black')
  
  
  loc <- df$State %in% highlight
  
  df$label       <- NA
  df[loc,]$label <- ifelse(df[loc,]$Time == "Dec_22", 
                           paste0(df[loc,]$State," ",df[loc,]$Unemployment, "%"), 
                           paste(df[loc,]$Unemployment, "%")) #Adds labels #Lab 2
  
  df$Linewidth       <- 0.5
  df[loc,]$Linewidth <- 1
  
  ggplot(df, 
         aes(x = Time, 
             y = Unemployment, 
             group = State, 
             color = State,
             size = Linewidth)) +
    geom_line() +
    scale_size_identity()+
    scale_color_manual(values = my_colors)+
    guides(color=FALSE)+
    labs(title = '**Biggest changes in the unemployment rate between December 22 and December 2023**',
         x     = '',
         y     = '',
         subtitle = '',
         caption = "Source: Bureau of Labor Statistics")+
    theme(plot.title         = element_markdown(margin = margin(b=5),hjust=-0.01),
          plot.margin        = margin(l=20,r=20),
          panel.background   = element_rect(fill='white',colour='white'),
          panel.grid.major.y = element_blank(),
          axis.text.x        = element_text(margin=margin(t=0)),
          axis.text.y        = element_blank(),
          axis.ticks         = element_blank(),
          plot.caption       = element_text(hjust = 0.01, 
                                            size  = 9,
                                            margin=margin(t=0))
    ) +
    scale_x_discrete(labels = c('December 2022','December 2023'),
                     expand = c(0.25,0.1)) +
    geom_text(aes(label = label),
              hjust = ifelse(df$Time == "Dec_22", 1.1, -.5), 
              vjust = .5,
              size = 3)
  
  