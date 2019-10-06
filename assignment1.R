# Assignment 1

scorecard = readRDS("college_scorecard.rds")

#2
nrow(scorecard)

#3
ncol(scorecard)

#4
range(scorecard$academic_year)
table(scorecard$academic_year == 2012)
table(scorecard$academic_year == 2013)
table(scorecard$academic_year == 2014)
table(scorecard$academic_year == 2015)
table(scorecard$academic_year == 2016)

#5
install.packages("plyr")
library("plyr")

scorecard2016 = subset(scorecard, scorecard$academic_year == 2016)

state_freq = count(scorecard2016, "state")
state_freq
sort(state_freq$freq)

# largest college counts: 717 454 454 417 382
# fewest college counts: 1 1 1 1 1

table(scorecard$state == "CA" & scorecard$academic_year == 2016)
table(scorecard$state == "TX" & scorecard$academic_year == 2016)
table(scorecard$state == "NY" & scorecard$academic_year == 2016)
table(scorecard$state == "FL" & scorecard$academic_year == 2016)
table(scorecard$state == "PA" & scorecard$academic_year == 2016)

table(scorecard$state == "AS" & scorecard$academic_year == 2016)
table(scorecard$state == "FM" & scorecard$academic_year == 2016)
table(scorecard$state == "MH" & scorecard$academic_year == 2016)
table(scorecard$state == "MP" & scorecard$academic_year == 2016)
table(scorecard$state == "PW" & scorecard$academic_year == 2016)

#6

library(ggplot2)
scorecard2014 = subset(scorecard, scorecard$academic_year == 2014)

# public schools
ggplot(scorecard2014, aes(x = avg_net_price.public, y = earn_10_yrs_after_entry.median)) + 
  geom_point() +
  labs(title = "Average Net Price v. Median Student Earnings After 10 Years for Public Schools in 2014",
       x = "Average Net Price", y = "Median Student Earnings After 10 Years") +
  coord_cartesian(xlim = c(0, 75000), ylim = c(0, 125000))

#7 private, for-profit schools
ggplot(scorecard2014[scorecard2014$ownership == "Private for-profit", ], aes(x = avg_net_price.private, 
                                                                           y = earn_10_yrs_after_entry.median)) +
  geom_point() +
  labs(title = "Average Net Price v. Median Student Earnings After 10 Years for Private For-Profit Schools in 2014",
       x = "Average Net Price", y = "Median Student Earnings After 10 Years") +
  coord_cartesian(xlim = c(0, 75000), ylim = c(0, 125000))

#8 private, non-profit schools
ggplot(scorecard2014[scorecard2014$ownership == "Private nonprofit", ], aes(x = avg_net_price.private, 
                                                                             y = earn_10_yrs_after_entry.median)) +
  geom_point() +
  labs(title = "Average Net Price v. Median Student Earnings After 10 Years for Private Nonprofit Schools in 2014",
       x = "Average Net Price", y = "Median Student Earnings After 10 Years") +
  coord_cartesian(xlim = c(0, 75000), ylim = c(0, 125000))

#9

ggplot(scorecard, aes(x = academic_year, fill = ownership)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Schools of Different Types from 2012 to 2016", x = "Academic Year", y = "Number of Schools") +
  guides(fill = guide_legend(title = "School Type"))

#10

# UC Davis scorecard
davis_scorecard = subset(scorecard, scorecard$id == 110644)

ggplot(davis_scorecard, aes(x = academic_year, y = admission_rate.overall, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "UC Davis Overall Admission Rate from 2012 to 2016", x = "Academic Year", y = "Overall Admission Rate")













