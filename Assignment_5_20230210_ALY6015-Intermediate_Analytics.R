print("Vartika Joshi")

getwd()
setwd("/Users/vj/Documents/ALY 6015 IntermediateAnalytics")


# Load the "wilcox.test" function
library(stats)

##Section 13-2
#Game attendance
# Define the sample data
x <- c(6210, 3150, 2700, 3012, 4875, 3540, 6127, 2581, 2642, 2573, 2792, 2800, 2500, 3700, 6030, 5437, 2758, 3490, 2851, 2720)

# Perform the Wilcoxon signed-rank test
result <- wilcox.test(x, mu = 3000, alternative = "two.sided")

# Print the results
print(result)

#Lottery Tickets Sale

# Create the data frame
tickets_sold <- c(180, 198, 210, 185, 178, 193, 195, 200, 185, 198, 210, 200, 188, 180, 190, 192, 198, 198, 215, 200, 180, 185, 185, 195, 198, 198, 210, 180, 195, 193, 185, 200, 198, 200, 200, 215, 193, 210, 198, 200)
days <- data.frame(Day = 1:length(tickets_sold), Tickets_Sold = tickets_sold)

# Plot the data using ggplot
library(ggplot2)
ggplot(days, aes(x = "", y = Tickets_Sold)) +
  geom_boxplot() +
  ggtitle("Lottery Tickets Sold per Day") +
  xlab("") +
  ylab("Tickets Sold")


# Define the sample data
x <- c(180, 198, 210, 185, 178, 193, 195, 200, 185, 198, 210, 200, 188, 180, 190, 192, 198, 198, 215, 200, 180, 185, 185, 195, 198, 198, 210, 180, 195, 193, 185, 200, 198, 200, 200, 215, 193, 210, 198, 200)

# Perform the one-sample Wilcoxon signed-rank test
result <- wilcox.test(x, mu = 200, alternative = "less")

# Print the results
print(result)

##Section 13-3
#Length of prisoners sentences
# Define the sample data
male <- c(8, 12, 6, 14, 22, 27, 3, 2, 2, 2, 4, 6, 19, 15, 13)
female <- c(7, 5, 2, 3, 21, 26, 3, 9, 4, 0, 17, 23, 12, 11, 16)

# Perform the Wilcoxon rank sum test
result <- wilcox.test(male, female, alternative = "two.sided")

# Print the results
print(result)

#Winning Baseball Games
NL <- c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
AL <- c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

result <- wilcox.test(NL, AL, alternative = "two.sided")
print(result)


##Section 13-4
install.packages("coin")
library(coin)
n1 <- 15
n2 <- 15
alpha <- 0.01
critical_value_upper <- qwilcox(1-alpha/2, n1, n2, lower.tail = FALSE)
critical_value_lower <- qwilcox(alpha/2, n1, n2, lower.tail = TRUE)
ws <- 13
if(ws >= critical_value_lower & ws <= critical_value_upper) {
  print("Fail to reject the null hypothesis")
} else {
  print("Reject the null hypothesis")
}

##Section 13-5
# Create data frames for each region
western_hemisphere <- c(527, 406, 474, 381, 411)
europe <- c(520, 510, 513, 548, 496)
eastern_asia <- c(523, 547, 547, 391, 549)

# Combine the data frames into a single data frame
data <- data.frame(Region = rep(c("Western Hemisphere", "Europe", "Eastern Asia"), each = 5),
                   Score = c(western_hemisphere, europe, eastern_asia))

# Perform the Kruskal-Wallis test
kruskal.test(Score ~ Region, data = data)


install.packages("ggplot2")
library(ggplot2)
math_scores <- data.frame(
  Region = rep(c("Western Hemisphere", "Europe", "Eastern Asia"), each = 5),
  Score = c(527, 406, 474, 381, 411, 520, 510, 513, 548, 496, 523, 547, 547, 391, 549)
)

ggplot(math_scores, aes(x = Region, y = Score, fill = Region)) +
  geom_boxplot() +
  ggtitle("Mathematics Literacy Scores by Region") +
  xlab("Region") +
  ylab("Score")

##Section 13-6
subway_trips <- c(845, 494, 425, 313, 108, 41)
rail_trips <- c(39, 291, 142, 103, 33, 38)
correlation_coefficient <- cor(subway_trips, rail_trips, method = "spearman")

# State the hypotheses
H0 <- "There is no relationship between subway and rail trips."
Ha <- "There is a relationship between subway and rail trips."

# Find the critical value
alpha <- 0.05
df <- length(subway_trips) - 2
t_critical <- qt(1-alpha/2, df)

# Make the decision
t_value <- correlation_coefficient * sqrt(df) / sqrt(1 - correlation_coefficient^2)
if (abs(t_value) <= t_critical) {
  print("Fail to reject the null hypothesis")
} else {
  print("Reject the null hypothesis")
}

# Summarize the results
result <- paste("The correlation coefficient is", round(correlation_coefficient, 2), ".")
print(result)

data <- data.frame(subway_trips, rail_trips)

ggplot(data, aes(x = subway_trips, y = rail_trips)) + 
  geom_point() + 
  geom_smooth(method = "lm", col = "red") +
  xlab("Subway Trips (in thousands)") + 
  ylab("Rail Trips (in thousands)") +
  ggtitle("Relationship between Subway and Rail Trips")

##Section 14-3
#Prizes in the Caramel Corn Boxes
# Set the seed for reproducibility
set.seed(123)

# Function to find the average number of boxes needed to get all four prizes
get_average_boxes <- function(repetitions) {
  boxes_needed <- numeric(repetitions)
  for (i in 1:repetitions) {
    prizes_found <- numeric(4)
    box_number <- 0
    while (length(unique(prizes_found)) < 4) {
      box_number <- box_number + 1
      prize_found <- sample(1:4, 1)
      prizes_found[box_number] <- prize_found
    }
    boxes_needed[i] <- box_number
  }
  mean(boxes_needed)
}

# Repeat the experiment 1000 times
repetitions <- 1000
average_boxes <- get_average_boxes(repetitions)
average_boxes

#Lottery Winner
# Set the seed for reproducibility
set.seed(123)

# Function to find the average number of tickets needed to win the prize
get_average_tickets <- function(repetitions) {
  tickets_needed <- numeric(repetitions)
  for (i in 1:repetitions) {
    letters_found <- c("b"=FALSE, "i"=FALSE, "g"=FALSE)
    ticket_number <- 0
    while (sum(letters_found) < 3) {
      ticket_number <- ticket_number + 1
      letter_found <- sample(c("b", "i", "g"), 1, prob = c(0.6, 0.3, 0.1))
      letters_found[letter_found] <- TRUE
    }
    tickets_needed[i] <- ticket_number
  }
  mean(tickets_needed)
}

# Repeat the experiment 1000 times
repetitions <- 1000
average_tickets <- get_average_tickets(repetitions)
average_tickets

