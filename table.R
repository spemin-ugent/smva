# Load required library
library(DT)

# Define the topic data with appropriate labels
topic_data <- data.frame(
  topic = 1:4,
  label = c(
    "Viewer Intentions and Excitement for Nezha 2",
    "Movie Quality and Chinese Cinema Reception",
    "Box Office Success and Chinese Film Impact",
    "Initial Impressions and Comparisons to First Film"
  ),
  words = c(
    "will, see, film, nezha, new, really, much, people, even, films",
    "movie, good, one, like, movies, chinese, time, still, last, get",
    "chinese, movie, think, film, like, office, billion, really, even, box",
    "zha, first, movie, one, just, like, movies, even, now, dont"
  )
)

# Create a graphical table using DT::datatable
datatable(topic_data[, c("topic", "label", "words")],
          caption = "Topic Summary for Nezha 2 Comments",
          colnames = c("Topic", "Label", "Top Words"),
          options = list(pageLength = 5, lengthMenu = c(5, 10)),
          class = "display")

