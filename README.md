library(tidyverse) # for almost all data handling tasks
library(readxl) # to import Excel data
library(ggplot2) # to produce nice graphiscs
library(stargazer) # to produce nice results tables
# make sure you use the correct path



CKdata<- read_xlsx("CK_public.xlsx",na = ".")
str(CKdata) # prints some basic info on variables
CKdata$STATEf <- as.factor(CKdata$STATE) # translates a variable into a factor variable
levels(CKdata$STATEf) <- c("Pennsylvania","New Jersey") # changes the names of the categories
CKdata$CHAINf <- as.factor(CKdata$CHAIN)
levels(CKdata$CHAINf) <- c("Burger King","KFC", "Roy Rogers", "Wendy's")
Tab1 <- CKdata %>% group_by(STATEf) %>%
  summarise(n = n()) %>%
  print()
table(CKdata$STATUS2,CKdata$STATEf)

#The prop.table() function in R is used to compute proportions of observations in a contingency table, which is created using the table() function. The margin argument specifies which margins of the table to use for computing the proportions.
#In the code you provided, the table() function is used to create a contingency table with row and column variables CHAINf and STATEf, respectively, and the names "Chain" and "State" are assigned to the dimensions using the dnn argument. Then, the prop.table() function is applied to the table with margin = 2, which means that the proportions are computed by dividing each cell by the sum of its respective column, i.e., computing the proportion within each column.
prop.table(table(CKdata$CHAINf,CKdata$STATEf,dnn = c("Chain", "State")),margin = 2)


ggplot(CKdata, aes(WAGE_ST, stat(density), colour = STATEf)) +
  #the density of the WAGE_ST variable to the y-axis using the stat(density) function, and the STATEf variable to the color aesthetic.
  geom_freqpoly(bins = 10) +
  #he geom_freqpoly() function is used to create the frequency polygon plot with bins = 10 argument specifying the number of bins to use for the histogram.
  ggtitle(paste("Starting wage distribution, Feb/Mar 1992"))


ggplot(CKdata,aes(WAGE_ST, colour = STATEf), colour = STATEf) +
  # position="identity" specifies that the bars should be drawn directly on the x-axis, rather than stacked or dodged. aes(y = ..density..) specifies that the height of the bars should represent the density of observations rather than the count of observations. bins = 10 specifies that the histogram should be divided into 10 equal-width bins, and alpha = 0.2 sets the transparency of the bars to 20%.
  geom_histogram(position="identity",
                 aes(y = ..density..),
                 bins = 10,
                 alpha = 0.2) +
  ggtitle(paste("Starting wage distribution, Feb/Mar 1992"))


Tab1 <- CKdata %>% group_by(STATEf) %>%
  summarise(wage_FEB = mean(WAGE_ST,na.rm = TRUE),
            wage_DEC = mean(WAGE_ST2,na.rm = TRUE)) %>%
  print()


ggplot(CKdata, aes(WAGE_ST2, stat(density), colour = STATEf)) +
  geom_freqpoly(bins = 10) +
  ggtitle(paste("Starting wage distribution, Nov/Dec 1992"))


#line 50 and 51 use 2 different methods to create a variable and assigne it's values
CKdata$FTE <- CKdata$EMPFT + CKdata$NMGRS + 0.5*CKdata$EMPPT
CKdata <- CKdata %>% mutate(FTE2 = EMPFT2 + NMGRS2 + 0.5*EMPPT2)

TabDiD <- CKdata %>% group_by(STATEf) %>%
  summarise(meanFTE_FEB = mean(FTE,na.rm = TRUE),
            meanFTE_DEC = mean(FTE2,na.rm = TRUE)) %>%
  print()




ggplot(CKdata, aes(x=1992,y=FTE, colour = STATEf)) +
  geom_point(alpha = 0.2) +
  #geom_point(aes(1993,FTE2),alpha = 0.2): This adds a second layer to the plot that overlays a set of additional points on top of the existing plot. These points represent a different variable (FTE2) that was not included in the original data frame, and are plotted at the x-value 1993
  geom_point(aes(1993,FTE2),alpha = 0.2) +
  labs(x = "Time") +
  #labs(x = "Time"): This sets the x-axis label to "Time".
  ggtitle(paste("Employment, FTE"))



#The points are very tight together which make it a little difficult to see how many there really are. Instead of
#the geom_point we use the geom_jitter command which jitters the points slightly in the time direction.
ggplot(CKdata, aes(1992,FTE, colour = STATEf)) +
  #package in R that can be used to add jitter to a plot, which means adding random noise to the position of the points along the x or y-axis. This can be useful for reducing overplotting in a scatterplot where multiple points have the same x and y values.
  geom_jitter(alpha = 0.2) +
  geom_jitter(aes(1993,FTE2),alpha = 0.2) +
  labs(x = "Time") +
  ggtitle(paste("Employment, FTE"))


#When we calculate a DiD estimator we are basically calculating the means of the two staes in the two periods,
#i.e. four points. We calculated these four values for TabDiD and we can plot them. They basically summarise the four clouds of points in the previous picture.




ggplot(TabDiD, aes(1992,meanFTE_FEB, colour = STATEf)) +
  geom_point(size = 3) +
  geom_point(aes(1993,meanFTE_DEC),size=3) +
  #ylim() is a function in R that sets the limits of the y-axis in a plot.
  ylim(17, 24) +
  labs(x = "Time") +
  ggtitle(paste("Employment, mean FTE"))




ggplot(TabDiD, aes(1992,meanFTE_FEB, colour = STATEf)) +
  geom_point(size = 3) +
  geom_point(aes(1993,meanFTE_DEC),size=3) +
  ylim(17, 24) +
  geom_segment(aes(x = 1992, y = TabDiD[[1,2]], # TabDiD[[1,2]] row 1 column 2
                   xend = 1993, yend = TabDiD[[1,3]]), color = "red") +
  geom_segment(aes(x = 1992, y = TabDiD[[2,2]],
                   xend = 1993, yend = TabDiD[[2,2]]+(TabDiD[[1,3]]-TabDiD[[1,2]]))) +
  labs(x = "Time") +
  ggtitle(paste("Employment, mean FTE"))



TabDiD[[1,2]]
TabDiD[[1,3]]
a <- TabDiD[[2,2]]+(TabDiD[[1,3]]-TabDiD[[1,2]])
