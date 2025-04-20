# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(topicmodels)
library(tm)
library(SnowballC)
library(wordcloud)
library(scales)
library(plotly)

# Load data
df <- read.csv("filtered_2comments_combined.csv", stringsAsFactors = FALSE)

# Convert date column
df$date <- as.Date(df$date)

# Count missing values
colSums(is.na(df))

# Daily comment count
daily_comments <- df %>%
  group_by(date) %>%
  summarise(count = n())

# Daily upvotes
daily_upvotes <- df %>%
  group_by(date) %>%
  summarise(upvotes = sum(upvotes, na.rm = TRUE))

# Plot daily comment count and daily upvotes
par(mfrow = c(1, 2))
plot(daily_comments$date, daily_comments$count, type = "l", col = "steelblue",
     main = "Comment Count Over Time", xlab = "Date", ylab = "Number of Comments")
plot(daily_upvotes$date, daily_upvotes$upvotes, type = "l", col = "tomato",
     main = "Total Upvotes Over Time", xlab = "Date", ylab = "Total Upvotes")

# Add week column
df$week <- floor_date(df$date, "week")

weekly_data <- df %>%
  group_by(week) %>%
  summarise(weekly_comments = n(),
            weekly_upvotes = sum(upvotes, na.rm = TRUE))

# Plot weekly comment count and weekly upvotes
par(mfrow = c(1, 2))
plot(weekly_data$week, weekly_data$weekly_comments, type = "l", col = "darkgreen",
     main = "Weekly Comment Count", xlab = "Week", ylab = "Number of Comments")
plot(weekly_data$week, weekly_data$weekly_upvotes, type = "l", col = "darkred",
     main = "Weekly Total Upvotes", xlab = "Week", ylab = "Total Upvotes")

# Preprocessing comments for topic modeling
df <- df %>% filter(!is.na(comment))
df <- df %>% filter(!grepl("iamabot", comment), nchar(comment) < 500)

corpus <- VCorpus(VectorSource(df$comment))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
# Removed stemDocument to keep full words

dtm <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(10, Inf))))
dtm <- removeSparseTerms(dtm, 0.99)

row_totals <- apply(dtm, 1, sum)
dtm <- dtm[row_totals > 0, ]

# Fit LDA model
lda_model <- LDA(dtm, k = 5, control = list(seed = 42))

# Extract top 10 words for each topic
top_terms <- terms(lda_model, 10)
top_terms_df <- data.frame(top_terms)  # Convert matrix to dataframe

# Rename columns to match topic numbers (0 to 4, since topics() uses 0-based indexing)
colnames(top_terms_df) <- paste0("Topic_", 0:4)

# Reshape to get one word per row
top_terms_long <- top_terms_df %>%
  pivot_longer(cols = everything(), names_to = "topic", values_to = "word") %>%
  mutate(topic = gsub("Topic_", "", topic))  # Clean topic names to match dominant_topic (0-4)

# Create topic labels with "Topic X:" prefix
topic_labels <- top_terms_long %>%
  group_by(topic) %>%
  summarise(label = paste(word, collapse = ", ")) %>%
  mutate(label = paste0("Topic ", topic, ": ", label)) %>%  # Format as "Topic X: word1, word2, ..."
  mutate(topic = as.character(topic))  # Ensure topic is character for joining

# Keep only rows from df that are used in dtm
filtered_df <- df[row.names(dtm), ]  # Align with dtm rows

# Assign dominant topics (0-based indexing from LDA)
filtered_df$dominant_topic <- as.numeric(topics(lda_model))

# Convert date to Date format if not already
filtered_df$date <- as.Date(filtered_df$date)

# Group by date and topic, filter out NA or invalid topics
topic_trend <- filtered_df %>%
  group_by(date, dominant_topic) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(!is.na(dominant_topic) & dominant_topic %in% 0:4) %>%  # Ensure topics are 0-4
  pivot_wider(names_from = dominant_topic, values_from = count, values_fill = 0)

# Reshape for plotting
topic_trend_long <- topic_trend %>%
  pivot_longer(-date, names_to = "topic", values_to = "count") %>%
  mutate(topic = as.character(topic)) %>%  # Ensure topic is character for joining
  filter(!is.na(topic))  # Remove any NA topics

# Merge with topic labels
topic_trend_long <- topic_trend_long %>%
  left_join(topic_labels, by = "topic")

# Plot with ggplot (topics over time)
p <- ggplot(topic_trend_long, aes(x = date, y = count, color = label)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +  # Improved date readability
  labs(
    title = "Topic Trends Over Time",
    x = "Date",
    y = "Number of Comments",
    color = "Topic"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display ggplot
print(p)

# Create interactive plot with Plotly
plotly_p <- plot_ly(topic_trend_long,
                    x = ~date,
                    y = ~count,
                    color = ~label,
                    type = 'scatter',
                    mode = 'lines',
                    line = list(width = 2),
                    hoverinfo = 'text',
                    text = ~paste("Date:", date, "<br>Count:", count, "<br>Topic:", label)) %>%
  layout(
    title = "Topic Trends Over Time",
    xaxis = list(title = "Date", tickangle = 45),
    yaxis = list(title = "Number of Comments"),
    legend = list(orientation = "h", x = 0.1, y = -0.2),
    hovermode = "x unified"
  )

# Display Plotly plot
print(plotly_p)





