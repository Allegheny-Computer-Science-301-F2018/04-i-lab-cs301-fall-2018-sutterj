# Name: Jacob Sutter
# Date: 100618


# Run the below only if the library is not already installed.
# install.packages(dslabs)


library(dslabs)
library(dplyr)
library(tidyverse)
data(us_contagious_diseases)


#Question 1.
dat <- filter(us_contagious_diseases, state != 'Hawaii',  state != 'Alaska', disease == 'Measles')
dat <- dat %>% mutate(((count * 100000) / population) * (weeks_reporting / 52))


#Question 2.
measles_california <- filter(us_contagious_diseases, state == 'California', disease == "Measles")
ggplot(data = measles_california) + geom_point(mapping = aes(x = year, y = (((count * 100000) / population) * (weeks_reporting / 52)))) + geom_vline(xintercept = 1965)

# With the first commannd, I filtered the data to include only the Measels cases in California.
# I then plotted this data using the year as the x-axis and the per 100000 rate as the y-axis.
# In order to create the line at the proper year, I gave it an x-intercept of 1965.


#Question 3.
dat_caliFocus <- filter(us_contagious_diseases, state == "California", year < 1980, year >= 1950)
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1950] <- "1950’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1960] <- "1960’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1970] <- "1970’s"
ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))
ggplot(data = dat_caliFocus ) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

# The first command excludes the years that are not in the desired date range.
# The next three commands create the year blocks for the 50's, 60's, and 70's.
# The last two commands are the plot commands.
# The first does not apply the square root transformation and the second does.
# The transformation decreses the difference between the tops of the bars across the decades.
# This allows us to get a better idea of what comparisons that we can make because there is not as much variance between the decades.


#Question 4.
dat_Focus <- filter(us_contagious_diseases, year < 1980, year >= 1950)
dat_Focus$yearBlock[dat_Focus$year >= 1950] <- "1950’s"
dat_Focus$yearBlock[dat_Focus$year >= 1960] <- "1960’s"
dat_Focus$yearBlock[dat_Focus$year >= 1970] <- "1970’s"
ggplot(data = dat_Focus ) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))
ggplot(data = dat_Focus ) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

# The commands here are the same as in Question 3 except that the filter for California has been removed.
# The trend holds true from state to state in that the count decreases across the decades.
# It is almost always the case that the 50's has the highest count and the 70's has the lowest count.
# The trend is the same both with and without the transformation, but it is much easier to see the trends with the transformation applied because there is not as large of a difference in the heights of the bars.


#Question 5.
ggplot(dat_Focus, aes(x = yearBlock, y = state)) + geom_tile(aes(fill = sqrt(count)), color = "grey50")

# This plot provides a different representation of the data and while it is easier to see the distinct states, it is harder to determine the rates.
# The color gives and idea of the rate, but the exact value is impossible to determine.


#Question 6.
library(readr)
autismReport <- read_csv("autismReport.csv")
# https://www.dds.ca.gov/Autism/docs/AutismReport_2007.pdf
ggplot(data = autismReport, mapping = aes(x = Year, y = Population)) + geom_line()

# As vaccines have been introduced, disease rates have been reduced and autism rates have increased.
# The autistic population has been growing at a loosely exponential since the time after vaccines were introduced, but the timing does not coincide and there is no strong connection that can be drawn from comparing these plots about disease rates and autism.
# The increase in the autistic population has many other contributing factors that can make the impact of vaccines negligible if there exists a connection at all.
# More research into autism, greater public awareness, and other health changes have much more to do with the increase in the autistic population than the imporbable connection with vaccines.

