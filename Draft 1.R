library(httr)
library(jsonlite)
library(dplyr)
library(rvest)
library(syuzhet)
library(tidyr)
library(ggplot2)

get_month_url <- function(year, month) {
  month_urls <- list(
    "01" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-01-31&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-01-01&types=article"),
    "02" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-02-28&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-02-01&types=article"),
    "03" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-03-31&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-03-01&types=article"),
    "04" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-04-30&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-04-01&types=article"),
    "05" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-05-31&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-05-01&types=article"),
    "06" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-06-30&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-06-01&types=article"),
    "07" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-07-31&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-07-01&types=article"),
    "08" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-08-31&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-08-01&types=article"),
    "09" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-09-30&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-09-01&types=article"),
    "10" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-10-31&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-10-01&types=article"),
    "11" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-11-30&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-11-01&types=article"),
    "12" = paste0("https://www.nytimes.com/search?dropmab=false&endDate=", year, "-12-31&lang=en&query=Artificial%20intelligence&sort=best&startDate=", year, "-12-01&types=article")
  )
  return(month_urls[[month]])
}

scrape_nyt_search_data <- function(url) {
  page <- tryCatch({
    read_html(url)
  }, error = function(e) NULL)  
  
  if (is.null(page)) {
    return(data.frame(title = character(0), author = character(0)))  
  }
  
  titles <- page %>%
    html_nodes(".css-nsjm9t") %>%
    html_text(trim = TRUE)
  
  authors <- page %>%
    html_nodes(".css-1engk30") %>%
    html_text(trim = TRUE) %>%
    gsub("^By\\s+", "", .)  
  
  max_length <- max(length(titles), length(authors))
  
  titles <- c(titles, rep(NA, max_length - length(titles)))
  authors <- c(authors, rep(NA, max_length - length(authors)))
  
  articles_df <- data.frame(
    title = titles,
    author = authors,
    stringsAsFactors = FALSE
  )
  
  return(articles_df)
}

# ==== 2020-2024 ====
all_articles1 <- list()

for (year in 2020:2024) {
  for (month in sprintf("%02d", 1:12)) {
    month_url <- get_month_url(year, month)
    nyt_articles <- scrape_nyt_search_data(month_url)
    
    if (nrow(nyt_articles) > 0) {
      nyt_articles$month <- paste0(month, "_", year)
      all_articles1[[paste0(month, "_", year)]] <- nyt_articles
    }
  }
}

combined_articles1 <- bind_rows(all_articles1)

sentiment_scores1 <- get_nrc_sentiment(combined_articles1$title)

combined_articles1$sentiment <- apply(sentiment_scores1, 1, function(x) {
  if (sum(x[1:8]) > sum(x[9:10])) {
    return("Positive")
  } else if (sum(x[9:10]) > sum(x[1:8])) {
    return("Negative")
  } else {
    return("Neutral")
  }
})

combined_articles1$sentiment_numeric <- ifelse(combined_articles1$sentiment == "Positive", 1, 0)
combined_articles1$year <- as.numeric(sub("^\\d+_", "", combined_articles1$month))

table(combined_articles1$year, combined_articles1$sentiment)

anova_result1 <- aov(sentiment_numeric ~ factor(year), data = combined_articles1)
summary(anova_result1)
















# ==== 2015–2019 ====
all_articles2 <- list()

for (year in 2015:2019) {
  for (month in sprintf("%02d", 1:12)) {
    month_url <- get_month_url(year, month)
    nyt_articles <- scrape_nyt_search_data(month_url)
    
    if (nrow(nyt_articles) > 0) {
      nyt_articles$month <- paste0(month, "_", year)
      all_articles2[[paste0(month, "_", year)]] <- nyt_articles
    }
  }
}

combined_articles2 <- bind_rows(all_articles2)

sentiment_scores2 <- get_nrc_sentiment(combined_articles2$title)

combined_articles2$sentiment <- apply(sentiment_scores2, 1, function(x) {
  if (sum(x[1:8]) > sum(x[9:10])) {
    return("Positive")
  } else if (sum(x[9:10]) > sum(x[1:8])) {
    return("Negative")
  } else {
    return("Neutral")
  }
})

combined_articles2$sentiment_numeric <- ifelse(combined_articles2$sentiment == "Positive", 1, 0)
combined_articles2$year <- as.numeric(sub("^\\d+_", "", combined_articles2$month))

table(combined_articles2$year, combined_articles2$sentiment)

anova_result2 <- aov(sentiment_numeric ~ factor(year), data = combined_articles2)
summary(anova_result2)











summary(combined_articles1$sentiment)
summary(combined_articles2$sentiment)
length(unique(combined_articles1$title))
length(unique(combined_articles2$title))





table(combined_articles1$year)
table(combined_articles2$year)






















run_post_anova_analysis <- function(data, sentiment_var = "sentiment_numeric", year_var = "year") {
  # Load only base or commonly available packages
  library(car)        # For Levene's test
  library(effectsize) # For eta-squared
  library(ggplot2)
  library(dplyr)
  
  # Build formula
  formula <- as.formula(paste(sentiment_var, "~ factor(", year_var, ")"))
  
  # === ANOVA ===
  cat("\n=== ANOVA Summary ===\n")
  aov_model <- aov(formula, data = data)
  print(summary(aov_model))
  
  # === Levene's Test ===
  cat("\n=== Levene's Test (Equal Variances) ===\n")
  lev_test <- leveneTest(as.formula(paste(sentiment_var, "~ as.factor(", year_var, ")")), data = data)
  print(lev_test)
  
  # === Pairwise t-tests (as Tukey alternative) ===
  cat("\n=== Pairwise t-tests with Bonferroni Correction ===\n")
  pw <- pairwise.t.test(data[[sentiment_var]], as.factor(data[[year_var]]), p.adjust.method = "bonferroni")
  print(pw)
  
  # === Logistic Regression ===
  cat("\n=== Logistic Regression Summary ===\n")
  glm_model <- glm(as.formula(paste(sentiment_var, "~ factor(", year_var, ")")), data = data, family = "binomial")
  print(summary(glm_model))
  
  # === Simple Linear Regression (ADDED) ===
  cat("\n=== Simple Linear Regression Summary ===\n")
  lm_model <- lm(as.formula(paste(sentiment_var, "~ ", year_var)), data = data)
  print(summary(lm_model))
  
  # Calculate confidence interval for the slope
  conf_int <- confint(lm_model)
  cat("\nConfidence interval for the linear trend:\n")
  print(conf_int)
  
  # === Boxplot Visualization (ADDED) ===
  cat("\n=== Creating Boxplot Visualization ===\n")
  # Create boxplot
  boxplot <- ggplot(data, aes(x = factor(get(year_var)), y = get(sentiment_var))) +
    geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 2) +
    geom_jitter(width = 0.2, height = 0, alpha = 0.3, color = "darkblue") +
    labs(
      title = "Sentiment Distribution by Year",
      subtitle = "AI-related NYT Article Titles",
      x = "Year",
      y = "Sentiment Score (1 = Positive, 0 = Negative/Neutral)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0)
    )
  
  # Add mean points connected by line
  mean_by_year <- data %>%
    group_by(!!sym(year_var)) %>%
    summarize(mean_sentiment = mean(!!sym(sentiment_var), na.rm = TRUE))
  
  boxplot <- boxplot +
    geom_line(data = mean_by_year, 
              aes(x = factor(get(year_var)), y = mean_sentiment, group = 1),
              color = "darkred", linewidth = 1) +
    geom_point(data = mean_by_year,
               aes(x = factor(get(year_var)), y = mean_sentiment),
               color = "darkred", size = 3)
  
  print(boxplot)
  
  # Return the plot object for potential further usage
  return(list(boxplot = boxplot, lm_model = lm_model, aov_model = aov_model))
}


# Run the analysis for the 2020-2024 period
results_2020_2024 <- run_post_anova_analysis(combined_articles1)

# Run the analysis for the 2015-2019 period
results_2015_2019 <- run_post_anova_analysis(combined_articles2)



















# ==== Unified Emotion Visualization (Fixed) ====

positive_emotions <- c("joy", "anticipation", "trust", "surprise")
negative_emotions <- c("anger", "sadness", "disgust", "fear")

# Tag datasets
combined_articles1$dataset <- "2020_2024"
combined_articles2$dataset <- "2015_2019"

# Combine datasets
combined_all <- bind_rows(combined_articles1, combined_articles2)

# Recalculate sentiment scores
sentiment_scores_all <- get_nrc_sentiment(combined_all$title)

# Add emotion columns
combined_all <- combined_all %>%
  mutate(
    joy = sentiment_scores_all$joy,
    anticipation = sentiment_scores_all$anticipation,
    trust = sentiment_scores_all$trust,
    surprise = sentiment_scores_all$surprise,
    anger = sentiment_scores_all$anger,
    sadness = sentiment_scores_all$sadness,
    disgust = sentiment_scores_all$disgust,
    fear = sentiment_scores_all$fear
  )

# Tidy up for visualization
emotion_counts_all <- combined_all %>%
  pivot_longer(cols = c(joy, anticipation, trust, surprise, anger, sadness, disgust, fear),
               names_to = "emotion", values_to = "count") %>%
  mutate(
    sentiment = ifelse(emotion %in% positive_emotions, "Positive", "Negative")
  ) %>%
  group_by(year, sentiment, emotion) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  mutate(
    emotion = factor(emotion, levels = c(positive_emotions, negative_emotions)),
    year = as.integer(year)
  )

# Plot with 2 facets (Positive / Negative)
ggplot(emotion_counts_all, aes(x = year, y = count, fill = emotion)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ sentiment, scales = "free_y") +
  scale_fill_manual(values = c(
    "joy" = "lightblue", "anticipation" = "skyblue", "trust" = "deepskyblue", "surprise" = "dodgerblue",
    "anger" = "darkred", "sadness" = "firebrick", "disgust" = "brown", "fear" = "darkorange"
  )) +
  labs(
    x = "Year",
    y = "Emotion Count",
    title = "Emotion Categories by Sentiment and Year (2015–2024)",
    fill = "Emotion"
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )










summarize_emotions_by_year <- function(df) {
  emotion_summary <- df %>%
    group_by(year) %>%
    summarise(
      joy = sum(joy, na.rm = TRUE),
      anticipation = sum(anticipation, na.rm = TRUE),
      trust = sum(trust, na.rm = TRUE),
      surprise = sum(surprise, na.rm = TRUE),
      anger = sum(anger, na.rm = TRUE),
      sadness = sum(sadness, na.rm = TRUE),
      disgust = sum(disgust, na.rm = TRUE),
      fear = sum(fear, na.rm = TRUE),
      total_positive = joy + anticipation + trust + surprise,
      total_negative = anger + sadness + disgust + fear,
      .groups = "drop"
    ) %>%
    arrange(year) %>%
    mutate(year = as.character(year))  # Convert year to character
  
  total_row <- emotion_summary %>%
    summarise(across(-year, ~ sum(.x, na.rm = TRUE))) %>%
    mutate(year = "Total") %>%
    select(names(emotion_summary))  # Ensure column order matches
  
  bind_rows(emotion_summary, total_row)
}


emotion_summary_by_year <- summarize_emotions_by_year(combined_all)
print(emotion_summary_by_year)







# Improved Emotion Trend Line Chart

emotion_trends <- emotion_summary_by_year %>%
  filter(year != "Total") %>%
  mutate(year = as.integer(year)) %>%
  pivot_longer(cols = c(joy, anticipation, trust, surprise, anger, sadness, disgust, fear),
               names_to = "emotion", values_to = "count")

ggplot(emotion_trends, aes(x = year, y = count, color = emotion)) +
  geom_line(size = 1.3) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "joy" = "lightblue",
    "anticipation" = "skyblue",
    "trust" = "deepskyblue",
    "surprise" = "dodgerblue",
    "anger" = "darkred",
    "sadness" = "firebrick",
    "disgust" = "brown",
    "fear" = "darkorange"
  )) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title = "Emotion Trends in AI-Related NYT Articles (2015–2024)",
    subtitle = "Year-wise counts of NRC emotion categories in article titles",
    x = "Year",
    y = "Emotion Count",
    color = "Emotion"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic")
  )


  
  
#write.csv(combined_all, "combined_all.csv", row.names = FALSE)
