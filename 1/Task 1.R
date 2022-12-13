# Question 1. What are the column names of the data frame?
df <- read.csv('/sample_data/airquality.csv')
colnames(df)

# Question 2. What are the row names of the data frame?
rownames(df)

# Question 3. Extract the first 6 rows of the data frame and print them to the console
head(df, 6)

# Question 4. How many observations (i.e. rows) are in this data frame?
nrow(df)

# Question 5. Extract the last 6 rows of the data frame and print them to the console
tail(df, 6)

# Question 6. How many missing values are in the “Ozone” column of this data frame?
sum(is.na(df$Ozone))

# Question 7. What is the mean of the “Ozone” column in this dataset? Exclude missing values (coded as NA) from this calculation.
mean(df$Ozone, na.rm=TRUE)

# Question 8. Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90.
df[df$Ozone > 31  & df$Temp > 90, ]

# Question 9. Use a for loop to create a vector of length 6 containing the mean of each column in the data frame (excluding all missing values).
vec = vector()
for (x in 1:ncol(df)) {
  vec <- append(vec, mean(df[,x], na.rm=TRUE))
}
vec

# Question 10. Use the apply function to calculate the standard deviation of each column in the data frame (excluding all missing values).
sapply(df, sd, na.rm=TRUE)

# Question 11. Calculate the mean of “Ozone” for each Month in the data frame and create a vector containing the monthly means (exclude all missing values).
library(dplyr)

df %>%
  group_by(Month) %>%
  summarise_at(vars(Ozone), list(name = mean), na.rm = TRUE)

Question 12. Draw a random sample of 5 rows from the data frame
sample_n(df, 5)