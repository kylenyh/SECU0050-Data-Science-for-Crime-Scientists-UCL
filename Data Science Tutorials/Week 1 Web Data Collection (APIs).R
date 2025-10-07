---
title: "Web Data Collection"
format: html
editor: visual
---

### Task 1: Building an access pipeline to The Guardian

{r include=FALSE}

# Load packages
library(guardianapi)
library(plyr)
library(rvest)


{r echo=FALSE, message=FALSE}

# Function will ask you to enter API key
gu_api_key()


{r echo=FALSE, message=FALSE}

# Searches content containing crime and security phrases between from data and to date
request.crimesec <- gu_content(query = '"crime" AND "security"',
                               from_date = "2021-01-01",
                               to_date   = "2021-03-31")


{r echo=FALSE, message=FALSE}

# Gets the dimension of the data 
dim(request.crimesec)


{r echo=FALSE, message=FALSE}

# Gets the names of the object (Guardian API)
names(request.crimesec)


{r echo=FALSE, message=FALSE}

# Returns the data view (Guardian API)
View (request.crimesec)


{r echo=FALSE, message=FALSE}

# Shows the headline of the 100th article (Guardian API)
request.crimesec$headline[100]


{r echo=FALSE, message=FALSE}

# Shows the body text of the 100th article (Guardian API)
request.crimesec$body_text[100]


{r echo=FALSE, message=FALSE}

# Gets articles from the technology section
crimesec_tech <- request.crimesec[request.crimesec$section_id=="technology",]

# Prints data dimension of the section
dim(crimesec_tech)



{r echo=FALSE, message=FALSE}

# Prints the entire list of headlines from the technology section
crimesec_tech$headline


{r echo=FALSE, message=FALSE}

# Creates a frequency table of how many articles came from each Guardian section
sectioncounts <- table(request.crimesec$section_id)

# Prints the data
sectioncounts


{r echo=FALSE, message=FALSE}

# Barplot and Pie Chart
# Number of observations per section using table()
sections_table <- table(request.crimesec$section_id)  

# Plot barplot
barplot(sections_table, main = "Number of Guardian Articles by Section", xlab = "Section", ylab = "Count")


{r echo=FALSE, message=FALSE}

# Convert table to data frame
df <- as.data.frame(sections_table)

# Filter sections with more than 2 articles
filtered_df <- df[df$Freq > 2, ]



{r echo=FALSE, message=FALSE}

# Plot pie chart if there are multiple sections
if (nrow(filtered_df) > 0) {
  pie(filtered_df$Freq, labels = filtered_df$Var1, main = "Sections with More Than 2 Articles")
}



{r echo=FALSE, message=FALSE}

# Task: Run multiple search queries for 6 months
# Search queries for 6 months
my_query_july <- gu_content(query = '"crime" AND "security"',
                            from_date = "2022-07-01",
                            to_date = "2022-07-31")

my_query_aug <- gu_content(query = '"crime" AND "security"',
                           from_date = "2022-08-01",
                           to_date = "2022-08-31")

my_query_sept <- gu_content(query = '"crime" AND "security"',
                            from_date = "2022-09-01",
                            to_date = "2022-09-30")

my_query_oct <- gu_content(query = '"crime" AND "security"',
                           from_date = "2022-10-01",
                           to_date = "2022-10-31")

my_query_nov <- gu_content(query = '"crime" AND "security"',
                           from_date = "2022-11-01",
                           to_date = "2022-11-30")

my_query_dec <- gu_content(query = '"crime" AND "security"',
                           from_date = "2022-12-01",
                           to_date = "2022-12-31")



{r echo=FALSE, message=FALSE}

# Join the six variables
guardian_query_six_months <- do.call(rbind.fill, list(my_query_july,
                                                      my_query_aug,
                                                      my_query_sept,
                                                      my_query_oct,
                                                      my_query_nov,
                                                      my_query_dec))



### Task 2: Identifying patterns in retrieved text 

{r echo=FALSE, message=FALSE}

# Examine the data for outliers in the article length
guardian_query_six_months$word_count_converted <- as.numeric(as.character(guardian_query_six_months$wordcount))

# Plot the word count
plot(guardian_query_six_months$word_count_converted, main = "Word Count Distribution", ylab = "Word Count")



{r echo=FALSE, message=FALSE}

# Calculate mean + 3 SD
mean_val <- mean(guardian_query_six_months$word_count_converted, na.rm = TRUE)
sd_val <- sd(guardian_query_six_months$word_count_converted, na.rm = TRUE)
mean_plus_3sd <- mean_val + 3 * sd_val



{r echo=FALSE, message=FALSE}

# Table of outliers
table(guardian_query_six_months$word_count_converted > mean_plus_3sd)

# Looking for the temporal development of article length
guardian_query_six_months$date <- substr(guardian_query_six_months$web_publication_date, 1, 10)
guardian_query_six_months$date <- as.Date(guardian_query_six_months$date, format = "%Y-%m-%d")

# Sort by date
guardian_query_six_months <- guardian_query_six_months[order(guardian_query_six_months$date), ]



{r echo=FALSE, message=FALSE}

# Plot the word count over time
plot(guardian_query_six_months$date, guardian_query_six_months$word_count_converted,
     main = "Article Word Count Over Time",
     xlab = "Date",
     ylab = "Word Count")


{r echo=FALSE, message=FALSE}

# Assess how the article frequency changes over time
guardian_query_six_months$month <- format(guardian_query_six_months$date, "%Y-%m")

# Aggregate by month (assuming 'id' is a unique identifier for articles)
monthly_counts <- aggregate(id ~ month, data = guardian_query_six_months, FUN = length)



### Task 3: Retrieving the HTML structure of a webpage using R 

{r echo=FALSE, message=FALSE}

# Set the base URL
base_url <- "https://www.ucl.ac.uk/security-crime-science"

# Access the full page using read_html
target <- read_html(base_url)

# Extract text using the identified class (e.g., '.text-formatted')
target |>
  html_nodes('.text-formatted') |> 
  html_text()
