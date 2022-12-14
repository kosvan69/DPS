## Part 1
df <- readRDS('hike_data.rds')
head(df, 6)

# Create a new dataset clean_hike_trails with the next updates:
# 1. Convert columns gain, highpoint, rating to numeric values.
# 2. Add new column trip with the type of trip from column length (“roundtrip”, “trails”, “one-way”).
# 3. Add new column length_total with the route length from column length, considering that for “one-way” trip you must double the route length.
# 4. Add new column location_general with location from column location (a part before “–”).
# 5. Add column id with row number
library(stringr)
clean_hike_trails <- df
clean_hike_trails[, 4:6] <- sapply(clean_hike_trails[, 4:6], as.numeric)
clean_hike_trails$trip <- sub('..', '', str_extract(df$length,",.*"))
clean_hike_trails$length_numeric<-sapply(gsub("[^0-9]","",clean_hike_trails$length), as.numeric)
clean_hike_trails$length_total <- with(clean_hike_trails, ifelse(trip == "one-way", clean_hike_trails$length_numeric*2, clean_hike_trails$length_numeric))
clean_hike_trails$location_general <- sub(' --', '',  str_extract(df$location,".*--"))
clean_hike_trails$ID <- seq.int(nrow(clean_hike_trails))
head(clean_hike_trails$features, 6)

# Question 1. How many routes have rating more than 4.9
library(dplyr)
n_distinct(clean_hike_trails[clean_hike_trails$rating > 4.9, ])

# Question 2. How many routes are “Good for kids” (hint: you can use (unnest function)?
n_distinct(clean_hike_trails[clean_hike_trails$features == "Good for kids", ])

# Question 3. Which unique features can routes have?
library(tidyr)
clean_hike_trails %>% nest(data = c("features"))
unique(clean_hike_trails$features)

# Question 4. What is the most common rating of a route?
head(clean_hike_trails$rating, 6)
install.packages("DescTools")                
library("DescTools") 
tail(names(sort(table(clean_hike_trails$rating))), 1)

# Question 5. Check if there are no cases where both dogs are allowed and not
clean_hike_trails$id <- 1:nrow(clean_hike_trails)
hike <- clean_hike_trails %>%
  tidyr::unnest(features, keep_empty = TRUE)
hike_dogs <- hike %>% 
  mutate(dogs_info = stringr::str_extract(features, "Dogs (not )?allowed(.+)?")) %>% 
  filter(!is.na(dogs_info))
hike_dogs %>% dplyr::count(id) %>% arrange(desc(n))

# Road 1618 is weird!