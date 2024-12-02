
# Analyze Detractors in the NPS Dataset with Bigram Analysis and Word Cloud

# Load libraries
library(tidyverse)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(RColorBrewer)

# Load the dataset
data <- read.csv('final_nps_variation_dataset.csv')

# Categorize customers as detractors, passives, or promoters
data <- data %>%
  mutate(Category = case_when(
    NPS_Score >= 9 ~ "Promoter",
    NPS_Score >= 7 ~ "Passive",
    TRUE ~ "Detractor"
  ))

# Proportion of detractors by region
detractors_by_region <- data %>%
  filter(Category == "Detractor") %>%
  count(Region) %>%
  left_join(data %>% count(Region, name = "Total"), by = "Region") %>%
  mutate(Proportion = n / Total * 100)

print("Proportion of Detractors by Region:")
print(detractors_by_region)

# Visualize proportion of detractors by region
ggplot(detractors_by_region, aes(x = Region, y = Proportion)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Proportion of Detractors by Region", x = "Region", y = "Percentage of Detractors") +
  theme_minimal()

# Age distribution of detractors
ggplot(data %>% filter(Category == "Detractor"), aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Age Distribution of Detractors", x = "Age", y = "Count") +
  theme_minimal()

# Average premiums by category
avg_premiums <- data %>%
  group_by(Category) %>%
  summarize(Average_Premium = mean(Average_Premium_Paid))

print("Average Premium Paid by Category:")
print(avg_premiums)

# Visualize average premiums by category
ggplot(avg_premiums, aes(x = Category, y = Average_Premium, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "gray", "green")) +
  labs(title = "Average Premium Paid by Category", x = "Category", y = "Average Premium Paid") +
  theme_minimal()

# Detractor feedback analysis
detractor_feedback <- data %>%
  filter(Category == "Detractor") %>%
  select(Feedback) %>%
  unnest_tokens(word, Feedback)

# Remove stop words
detractor_feedback <- detractor_feedback %>%
  anti_join(stop_words, by = "word")

# Most common words
common_words <- detractor_feedback %>%
  count(word, sort = TRUE) %>%
  top_n(20)

print("Most Common Words in Detractor Feedback:")
print(common_words)

# Visualize most common words
ggplot(common_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Common Words in Detractor Feedback", x = "Words", y = "Count") +
  theme_minimal()

# Bigram analysis
bigrams <- data %>%
  filter(Category == "Detractor") %>%
  select(Feedback) %>%
  unnest_tokens(bigram, Feedback, token = "ngrams", n = 2)

common_bigrams <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  top_n(20)

print("Most Common Bigrams in Detractor Feedback:")
print(common_bigrams)

# Visualize common bigrams
ggplot(common_bigrams, aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Common Bigrams in Detractor Feedback", x = "Bigrams", y = "Count") +
  theme_minimal()

# Word cloud for detractor feedback
wordcloud(words = common_words$word, freq = common_words$n, min.freq = 1,
          max.words = 100, colors = brewer.pal(8, "Reds"), random.order = FALSE)
