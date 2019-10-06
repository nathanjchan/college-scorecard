# STA 141A Assignment 2
# Nathan Chan

scorecard = readRDS("college_scorecard.rds")

# 1. Exploring missing values

check_na = function(data) {
  # Prints out the column names that have an NA in them
  # Prints out number of columns
  count = 0
  for (i in 1:ncol(data)) {
    if (any(is.na(data[, i])) == TRUE) {
      print(colnames(data)[i])
      count = count + 1
    }
  }
  cat("Number of columns with NA:", count)
}

explore_na = function(data, number) {
  # See how many columns have more NA's than the given number
  # Prints out number of columns
  count = 0
  for (i in 1:ncol(data)) {
    if (sum(is.na(data[, i])) >= number) {
      print(colnames(data)[i])
      count = count + 1
    }
  }
  cat("Number of columns with specific number of NA:", count)
}

number_of_na = function(data) {
  # Returns ordered dataframe of number of NA's per column
  col1 = c()
  col2 = c()
  for (i in 1:ncol(data)) {
    col1[i] = colnames(data)[i]
    col2[i] = sum(is.na(data[, i]))
  }
  NA_number = data.frame("Feature" = col1, "Number_of_NA" = col2)
  sorted_NA_number = NA_number[order(NA_number$Number_of_NA),]
  return(sorted_NA_number)
}

check_na(scorecard) # which columns have NA in them
explore_na(scorecard, 38068) # how many columns are empty
sorted_NA_number = number_of_na(scorecard)

# As in Nick's suggestion, look at NA's over the years.
sum(is.na(subset(scorecard, scorecard$academic_year == 2012)))
sum(is.na(subset(scorecard, scorecard$academic_year == 2013)))
sum(is.na(subset(scorecard, scorecard$academic_year == 2014)))
sum(is.na(subset(scorecard, scorecard$academic_year == 2015)))
sum(is.na(subset(scorecard, scorecard$academic_year == 2016)))

sum(is.na(subset(scorecard, scorecard$ownership == "Public")))
sum(is.na(subset(scorecard, scorecard$ownership == "Private nonprofit")))
sum(is.na(subset(scorecard, scorecard$ownership == "Private for-profit")))

# 2. Exploring student populations

create_list = function(data, variable1, variable2) {
  # Returns ordered dataframe of two given variables
  col1 = c()
  col2 = c()
  for (i in 1:nrow(data)) {
    col1[i] = data[i, grep("name", colnames(data))]
    col2[i] = data[i, grep("size", colnames(data))]
  }
  df = data.frame(variable1 = col1, variable2 = col2)
  ordered_df = df[order(df$variable2),]
  return(ordered_df)
}

scorecard2016 = subset(scorecard, scorecard$academic_year == 2016)
name_size = create_list(scorecard, scorecard$name, scorecard$size) # create list of data with name and size
name_size_2016 = create_list(scorecard2016, scorecard$name, scorecard$size) # create list with only 2016 data

library(ggplot2)
ggplot(scorecard2016, aes(x = size)) + geom_density() + xlim(0, 70000) # see the distribution of sizes
ggplot(scorecard2016, aes(x = size)) + geom_bar() + xlim(0, 70000)

install.packages("dplyr")
library(dplyr)
size2016 = select(scorecard2016, name, size, grad_students) # does same thing as create_list function

ggplot(scorecard2016, aes(x = size, y = grad_students)) + geom_point() +
  labs(title = "Number of undergraduate v. Number of graduate students in college in 2016", 
       x = "Number of undergraduate students", y = "Number of graduate students")
length(name_size_2016[,1])
NA_size = sum(is.na(scorecard2016$size))

more_grad = scorecard2016$name[scorecard2016$grad_students > scorecard2016$size] # which schools have more grads

# 3. Looking at program percentages

summary(scorecard2016) # look at the distribution for program percentages

# 4. Tuition and states

names(scorecard2016)

instate = aggregate(tuition.in_state ~ state, scorecard, mean, na.rm = TRUE)
outstate = aggregate(tuition.out_of_state ~ state, scorecard, mean, na.rm = TRUE)

tuition2016 = select(scorecard2016, name, tuition.in_state, tuition.out_of_state)

sorted_instate = instate[order(instate$tuition.in_state),]
sorted_outstate = outstate[order(outstate$tuition.out_of_state),]

in_vs_out = function(data1, data2) {
  # Prints out whichever states or territories have larger or equal in-state tuition
  for (i in 1:nrow(data1)) {
    if (data1[i, 2] >= data2[i, 2]) {
      print(data1[i, 1])
    } else {
      print("Nope.")
    }
  }
}

in_vs_out(instate, outstate)

ggplot(instate, aes(x = tuition.in_state)) + geom_histogram(binwidth = 1000) +
  labs(x = "Average In-state Tuition (from 2012 to 2016)", y = "Number of States with particular Tuition",
       title = "Number of US states and territories with particular average in-state tuition") +
  xlim(0, 35000) + ylim(0, 8)

ggplot(outstate, aes(x = tuition.out_of_state)) + geom_histogram(binwidth = 1000) +
  labs(x = "Average Out-of-state Tuition (from 2012 to 2016)", y = "Number of States with particular Tuition",
       title = "Number of states and territories with particular average out-of-state tuition") +
  xlim(0, 35000) + ylim(0, 8)

# Looking at relationship between tuition and number of colleges
library("plyr")

scorecard2016 = subset(scorecard, scorecard$academic_year == 2016)

state_freq = count(scorecard2016, "state")
state_freq
sort(state_freq$freq)

# Individual scatter plots
ggplot(state_freq, aes(x = state_freq$freq, y = instate$tuition.in_state)) + geom_point() +
  labs(x = "Number of colleges per state", y = "Average in-state tuition", 
       title = "Number of colleges per state v. Average in-state tuition") + ylim(0, 32000)

ggplot(state_freq, aes(x = state_freq$freq, y = outstate$tuition.out_of_state)) + geom_point() +
  labs(x = "Number of colleges per state", y = "Averate out-of-state tuition",
       title = "Number of colleges per state v. Averate out-of-state tutition") + ylim(0, 32000)

# Combined scatter plot
ggplot(state_freq) + geom_point(aes(x = state_freq$freq, y = instate$tuition.in_state, 
                                    color = "In-state tuition")) +
  geom_point(aes(x = state_freq$freq, y = outstate$tuition.out_of_state, color = "Out-of-state tuition")) +
  labs(x = "Number of colleges per state", y = "Average tuition", 
       title = "Number of colleges per state v. Average tuition from 2012 to 2016") +
  guides(color = guide_legend(title = "Tuition type"))
  
# 5. Diversity

find_diversity = function(data, ethnic_group_number, percentage) {
  # Prints name of colleges that have three ethnic groups, with each group being 30% of population
  for (i in 1:nrow(data)) {
    count = 0
    for (j in 87:95) {
      if (is.na(data[i, j]) == TRUE) {
        next
      } else if (data[i, j] > percentage) {
        count = count + 1
        if (count >= ethnic_group_number) {
          print(data[i, 4])
        }
      }
    }
  }
}

find_diversity(scorecard2016, 3, .3) # three ethnic groups make up 30% of student population
find_diversity(scorecard2016, 3, .2) # three ethnic groups make up 20% of student population
find_diversity(scorecard2016, 4, .2) # four ethnic groups make up 20% of student population
find_diversity(scorecard2016, 3, .3)

diverse_schools = subset(scorecard2016, scorecard2016$name == "Old Town Barber College-Wichita" |
                           scorecard2016$name == "Arlington Career Institute" |
                           scorecard2016$name == "Remington College-Fort Worth Campus" |
                           scorecard2016$name == "Institute of Professional Careers")

diverse_schools$size
mean(scorecard2016$size, na.rm = TRUE)
diverse_schools$ownership

# 6. My own questions

# 1) Enrollment of women over time

women_year = aggregate(demographics.women ~ academic_year, scorecard, mean, na.rm = TRUE)

ggplot(women_year, aes(x = academic_year, y = demographics.women, group = 1)) + geom_point() + geom_line() +
  labs(x = "Academic year", y = "Average percentage of students who are women in college", 
       title = "Average percentage of students who are women in college from 2012 to 2016")

# 2) SAT and ACT scores and future earnings

ggplot(subset(scorecard, !is.na(sat_scores.average.overall)), 
       aes(y = earn_10_yrs_after_entry.median, x = sat_scores.average.overall, na.rm == TRUE)) + geom_point() +
  labs(y = "Median earnings after 10 years of entry into college, per college", 
       x = "Average overall SAT score per college", 
       title = "How SAT scores affects earnings after college")

ggplot(subset(scorecard, !is.na(act_scores.midpoint.cumulative)), 
       aes(y = earn_10_yrs_after_entry.median, x = act_scores.midpoint.cumulative, na.rm == TRUE)) + geom_point() +
  labs(y = "Median earnings after 10 years of entry into college, per college", 
       x = "Median overall ACT score per college", 
       title = "How ACT scores affects earnings after college")

# 8. Follow-up questions

# 1) Women enrollment, again

percentage_of_women_per_year = function(data) {
  # Prints out percentage of women enrolled in college per year
  col1 = c(2012, 2013, 2014, 2015, 2016)
  col2 = c()
  for (i in 1:5) {
    year = i + 2011
    remove_na = subset(data, !is.na(data$demographics.women))
    yearly_data = subset(remove_na, remove_na$academic_year == year)
    number_of_women = 0
    total_people = 0
    for (j in 1:nrow(yearly_data)) {
      number_of_women = number_of_women + (yearly_data[j, 86] * yearly_data[j, 141])
      total_people = total_people + yearly_data[j, 86]
    }
    col2[i] = number_of_women / total_people
  }
    percentage = data.frame("Year" = col1, "Percentage of women in college" = col2)
    return(percentage)
}

percentage = percentage_of_women_per_year(scorecard)

ggplot(percentage, aes(x = Year, y = Percentage.of.women.in.college, group = 1)) + geom_point() + geom_line() +
  labs(x = "Academic Year", y = "Percentage of students who are women in college", 
       title = "Percentage of students who are women in college from 2012 to 2016")

# 2) SAT and ACT scores for private vs. public schools

private = subset(scorecard, ownership == "Private nonprofit" | ownership == "Private for-profit")
public = subset(scorecard, ownership == "Public")

ggplot(private, aes(x = sat_scores.average.overall)) + geom_histogram(binwidth = 50) +
  labs(x = "Average SAT score", y = "Number of colleges with that score", 
       title = "Number of private colleges with particular SAT scores") + xlim(500, 1600)

ggplot(public, aes(x = sat_scores.average.overall)) + geom_histogram(binwidth = 50) +
  labs(x = "Average SAT score", y = "Number of colleges with that score",
       title = "Number of public colleges with particular SAT scores") + xlim(500, 1600)
