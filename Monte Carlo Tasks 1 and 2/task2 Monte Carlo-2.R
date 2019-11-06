# cost function
# p1 : exam points
# p2 : project points
# p3 : activity points
# w1 : weight for p1 exam
# w2 : weight for p2 project
# w3 : weight for p3 activities 
# pts : total points


###### Check, installation and loading of required packages #######
requiredPackages = c("triangle") # list of required packages
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) } 


# Initialise the data frame with 7 cols:
df <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("w1", "p1", "w2", "p2", "w3", "p3", "Points")
colnames(df) <- x

# Initialise the counter and set the iteration
i = 1
iter = 1000


# iterating:
while (i <= iter){
  
  
  w1 <- runif(1, min = 0.2, max = 0.4)
  w2 <- runif(1, min = 0.2, max = 0.4)
  w3 <- 1 - (w1 + w2)
  
  p1 <- rtriangle(1, 79, 95, 82) # assume exam pts is triangle distribution
  p2 <- rnorm(1, mean = 80, sd = 15) # assume project pts is normal distribution 
  p3 <- runif(1, min = 50, max = 95) # assume activity pts is uniform distribution 
  
  # ensure all pts are within (0, 100] range
  if ((0 < p1) & (p1 <= 100) & (0 < p1) & (p1 <= 100) &(0 < p1) & (p1 <= 100)){
    
    # the objective function
    pts <- p1 * w1 + p2 * w2 + p3 * w3
    
    # ensure the pts are within (0, 100] range
    if (pts > 0 & pts <= 100){
      
      # append rows to the dataframe & increase the counter
      df[nrow(df)+1,] <- c(w1, p1, w2, p2, w3, p3, pts)
      i = i + 1
    } 
  }
  
}

# mapping from numerical grade to GPA:

suppressPackageStartupMessages(library(tidyverse))

df <- df %>%
  mutate(Grade = case_when(
    df$Points <= 100 & df$Points > 95 ~5.5,
    df$Points <= 95 & df$Points > 90 ~5,
    df$Points <= 90 & df$Points > 80 ~4.5,    
    df$Points <= 80 & df$Points > 70 ~4,
    df$Points <= 70 & df$Points > 60 ~3.5,
    df$Points <= 60 & df$Points > 50 ~3,
    df$Points <= 50 & df$Points >= 0 ~2
  ))

# if exam p1 is less than 30 pts, or project and activities combined are less than 60 pts, we fail: 

for (row in 1:nrow(df)){
  
  if (df[row,"p1"] < 30 | (df[row,"p2"] + df[row,"p3"] < 60)){
    df[row,"Grade"] <- 2
  }
  
}

# view the dataframe 
View(df)

# view the histogram of the Points and Grades:
hist(df$Points,main="Histogram of the Simulated Points", col="aquamarine3", breaks=50, probability=TRUE)
hist(df$Grade,main="Histogram of the Simulation Grade", col="aquamarine3", probability=TRUE)


# The average weights w1_mean, w2_mean, w3_mean that give us 4.0 Grades or higher:
high_grade <- df[df$Grade >= 5,]
w1_mean <- mean(high_grade$w1)
w2_mean <- mean(high_grade$w2)
w3_mean <- mean(high_grade$w3)
