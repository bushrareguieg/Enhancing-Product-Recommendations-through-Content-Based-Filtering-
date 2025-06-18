library(readr)
products <- read.csv("C:\\Users\\bochr\\Downloads\\archive\\product_info.csv", stringsAsFactors = FALSE)
reviews1 <- read.csv("C:\\Users\\bochr\\Downloads\\archive\\reviews_0-250.csv", stringsAsFactors = FALSE)
reviews2 <- read.csv("C:\\Users\\bochr\\Downloads\\archive\\reviews_1250-end.csv", stringsAsFactors = FALSE)
reviews3 <- read.csv("C:\\Users\\bochr\\Downloads\\archive\\reviews_250-500.csv", stringsAsFactors = FALSE)
reviews4 <- read.csv("C:\\Users\\bochr\\Downloads\\archive\\reviews_500-750.csv", stringsAsFactors = FALSE)
reviews5 <- read.csv("C:\\Users\\bochr\\Downloads\\archive\\reviews_750-1250.csv", stringsAsFactors = FALSE)

str(products)
#str(reviews1)
#str(reviews2)
#str(reviews3)
#str(reviews4)
#str(reviews5)



#phase_1:data cleaning
library(dplyr)
# Combine all review files into one dataframe
reviews <- rbind(reviews1, reviews2, reviews3, reviews4, reviews5)

# Check the structure of the combined dataframe
str(reviews)
#Select Relevant Columns
products <- products %>%
  select(product_id, product_name, brand_name, rating, price_usd, loves_count)
#str(products)
reviews <- reviews %>%
  select(author_id, product_id, rating, is_recommended, helpfulness, review_text, skin_tone, skin_type)

#Merge Reviews with Product Info
data <- merge(reviews, products, by = "product_id", all.x = TRUE)
#str(data)
#Handle Missing Values
# Drop rows with critical missing values
library(tidyverse)
data <- data %>%
  filter(!is.na(rating.x) & !is.na(price_usd)) %>%
  mutate(skin_tone = replace_na(skin_tone, "Unknown"),
         skin_type = replace_na(skin_type, "Unknown"))

#check_check
data <- reviews %>%
  left_join(products, by = "product_id") %>%
  distinct()
user_data <- data %>% filter(author_id == "1741593524", rating.x >= 4)
top_products <- data %>%
  group_by(product_id, product_name, brand_name) %>%
  summarise(
    avg_rating = mean(rating.y, na.rm = TRUE),
    loves_count = max(loves_count, na.rm = TRUE),
    price_usd = max(price_usd, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_rating))  # Optional: Sort by average rating

#number_of_orders Formula
# Step: Calculate number_of_orders
base_score <- 3.0

data <- data %>%
  mutate(
    is_recommended = ifelse(is.na(is_recommended), 0, is_recommended),
    helpfulness = ifelse(is.na(helpfulness), 0, helpfulness),
    rating = ifelse(is.na(rating.x), 0, rating.x),
    loves_count = ifelse(is.na(loves_count), 0, loves_count),
    reviews = ifelse(is.na(rating.y), 0, rating.y),  # rating.y holds the reviews count from product info
    number_of_orders = round(
      base_score * 
        (1 + 0.2 * is_recommended + 0.05 * helpfulness) *
        log1p(loves_count + reviews) *
        rating / 5,
      0
    )
  )

# Save the combined reviews + number_of_orders to a new CSV
write.csv(data, "C:\\Users\\bochr\\Downloads\\all_reviews_with_orders.csv", row.names = FALSE)


#Phase_2:feature engineering
# Feature Engineering
features <- data %>%
  group_by(product_id, product_name, brand_name) %>%
  summarise(
    avg_rating = mean(rating.y, na.rm = TRUE),
    loves_count = max(loves_count, na.rm = TRUE),
    price_usd = max(price_usd, na.rm = TRUE),
    count_reviews = n(),  # Number of reviews per product
    .groups = "drop"
  ) %>%
  mutate(
    normalized_rating = scale(avg_rating, center = TRUE, scale = TRUE),
    normalized_loves = scale(loves_count, center = TRUE, scale = TRUE),
    normalized_price = scale(price_usd, center = TRUE, scale = TRUE)
  )

# Add a final feature vector combining normalized values
features <- features %>%
  rowwise() %>%
  mutate(feature_vector = list(c(normalized_rating, normalized_loves, normalized_price))) %>%
  ungroup()

print(head(features))






#Phase_3:Similarity Calculation
# Step 1: Verify product IDs
if (anyDuplicated(features$product_id) > 0) {
  stop("Duplicate product IDs found in features$product_id")
}
if (any(is.na(features$product_id))) {
  stop("Missing product IDs found in features$product_id")
}
print(length(features$product_id))  # Should match the number of products
# Step 2: Validate and construct feature matrix
if (any(sapply(features$feature_vector, length) != length(features$feature_vector[[1]]))) {
  stop("Inconsistent lengths in feature vectors")
}
feature_matrix <- do.call(rbind, features$feature_vector)

if (!is.matrix(feature_matrix) || !is.numeric(feature_matrix)) {
  stop("Feature matrix is not a numeric matrix")
}
print(dim(feature_matrix))  # Should match the number of products and feature dimensions
# Step 3: Compute cosine similarity
library(lsa)
product_similarity <- cosine(feature_matrix)
# Rebuild feature matrix (rows: products, columns: features)
feature_matrix <- do.call(rbind, features$feature_vector)
# Verify feature matrix dimensions
if (nrow(feature_matrix) != length(features$product_id)) {
  stop("Mismatch between feature rows and product IDs")
}
print(dim(feature_matrix))  # Expected: 2351 x number_of_features
# Transpose the feature matrix
transposed_feature_matrix <- t(feature_matrix)
# Compute cosine similarity (now between rows of the original matrix)
library(lsa)
product_similarity <- cosine(transposed_feature_matrix)
# Validate similarity matrix dimensions
if (nrow(product_similarity) != ncol(product_similarity)) {
  stop("Product similarity matrix is not square")
}
if (nrow(product_similarity) != length(features$product_id)) {
  stop("Mismatch between product similarity matrix and product IDs")
}
# Assign row and column names
rownames(product_similarity) <- features$product_id
colnames(product_similarity) <- features$product_id
# Check final similarity matrix
print(dim(product_similarity))  # Expected: 2351 x 2351
print(head(product_similarity))  # Preview matrix






# Phase 4: Recommendation System + Evaluation Test
# Function to recommend products based on similarity
recommend_products <- function(user_id, data, product_similarity, top_n) {
  # Get the products the user has interacted with
  user_products <- data %>%
    filter(author_id == user_id) %>%
    pull(product_id)
  
  if (length(user_products) == 0) {
    cat("No products found for the given user.\n")
    # Fallback to recommending popular products if no ratings exist
    popular_products <- data %>%
      group_by(product_id) %>%
      summarise(avg_rating = mean(rating.x, na.rm = TRUE), total_loves = sum(loves_count, na.rm = TRUE)) %>%
      arrange(desc(total_loves), desc(avg_rating)) %>%
      head(top_n)
    return(popular_products$product_id)
  }
  
  # Find similar products using the similarity matrix
  similar_scores <- product_similarity[user_products, , drop = FALSE] %>%
    as.data.frame() %>%
    colSums(na.rm = TRUE) %>%
    sort(decreasing = TRUE)
  
  # Exclude products the user has already interacted with
  similar_scores <- similar_scores[!names(similar_scores) %in% user_products]
  
  if (length(similar_scores) < top_n) {
    cat("Warning: Fewer recommendations available than requested.\n")
  }
  
  # Return the top N recommended products
  top_recommendations <- names(similar_scores)[1:top_n]
  return(top_recommendations)
}

# Function to get user recommendations with detailed information
get_user_recommendations <- function(user_id, data, product_similarity, top_n) {
  user_data <- data %>% filter(author_id == user_id)
  
  if (nrow(user_data) == 0) {
    cat("User does not exist in the history. Showing top", top_n, "popular products:\n")
    top_products <- data %>%
      group_by(product_id, product_name, brand_name, price_usd) %>%
      summarise(
        avg_rating = mean(rating.x, na.rm = TRUE), 
        total_loves = sum(loves_count, na.rm = TRUE)
      ) %>%
      arrange(desc(avg_rating), desc(total_loves)) %>%
      head(top_n)
    
    return(top_products)
  } else {
    recommendations <- recommend_products(user_id, data, product_similarity, top_n)
    
    if (is.null(recommendations)) {
      cat("No recommendations generated for the user.\n")
      return(NULL)
    }
    
    # Fetch detailed recommendation information
    detailed_recommendations <- data %>%
      filter(product_id %in% recommendations) %>%
      group_by(product_id, product_name, brand_name, price_usd) %>%
      summarise(
        avg_rating = mean(rating.x, na.rm = TRUE), 
        total_loves = sum(loves_count, na.rm = TRUE)
      ) %>%
      distinct()
    
    return(detailed_recommendations)
  }
}

# Function to evaluate recommendations (Precision & Recall)
evaluate_recommendations <- function(user_id, data, product_similarity, top_n) {
  # Get the products the user has rated
  user_ratings <- data %>% filter(author_id == user_id)
  actual_rated_products <- user_ratings$product_id
  
  # Get recommendations
  recommendations <- get_user_recommendations(user_id, data, product_similarity, top_n)
  
  if (is.null(recommendations)) {
    return(list(precision = NA, recall = NA))
  }
  
  # True positives: recommended products that the user has rated
  true_positives <- length(intersect(recommendations$product_id, actual_rated_products))
  
  # Precision = TP / (TP + FP)
  precision <- true_positives / length(recommendations$product_id)
  
  # Recall = TP / (TP + FN)
  recall <- true_positives / length(actual_rated_products)
  
  # Return precision and recall
  return(list(precision = precision, recall = recall))
}

# Interactive Input
user_id <- readline(prompt = "Enter the user ID: ")
top_n <- as.numeric(readline(prompt = "Enter the number of top recommendations: "))

# Get recommendations for the given user interactively
recommendations <- get_user_recommendations(user_id, data, product_similarity, top_n)

# Print recommendations in a readable format
if (!is.null(recommendations) && nrow(recommendations) > 0) {
  cat("\nRecommended Products:\n")
  for (i in 1:nrow(recommendations)) {
    cat("Product ID:", recommendations$product_id[i], "\n")
    cat("Product Name:", recommendations$product_name[i], "\n")
    cat("Brand Name:", recommendations$brand_name[i], "\n")
    cat("Price (USD):", recommendations$price_usd[i], "\n")
    cat("Average Rating:", recommendations$avg_rating[i], "\n")
    cat("Total Loves:", recommendations$total_loves[i], "\n")
    cat("\n")
  }
} else {
  cat("No recommendations available.\n")
}

# Evaluate the recommendations (Precision & Recall)
evaluation <- evaluate_recommendations(user_id, data, product_similarity, top_n)
cat("Precision: ", evaluation$precision, "\n")
cat("Recall: ", evaluation$recall, "\n")

# Plot Histograms for Recommendations
library(ggplot2)

# Check if recommendations are not null or empty
if (!is.null(recommendations) && nrow(recommendations) > 0) {
  
  # Bar plot for Average Rating of Recommended Products
  avg_rating_plot <- ggplot(recommendations, aes(x = product_name, y = avg_rating, fill = brand_name)) +
    geom_bar(stat = "identity") +
    coord_flip() +  # Flip coordinates for better readability
    labs(title = "Average Rating of Recommended Products",
         x = "Product Name", y = "Average Rating") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(avg_rating_plot)
  
  # Bar plot for Total Loves of Recommended Products
  total_loves_plot <- ggplot(recommendations, aes(x = product_name, y = total_loves, fill = brand_name)) +
    geom_bar(stat = "identity") +
    coord_flip() +  # Flip coordinates for better readability
    labs(title = "Total Loves for Recommended Products",
         x = "Product Name", y = "Total Loves") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(total_loves_plot)
  
  # Scatter plot for Price vs Total Loves
  price_loves_plot <- ggplot(recommendations, aes(x = price_usd, y = total_loves, color = brand_name)) +
    geom_point(size = 4) +
    labs(title = "Price vs Total Loves for Recommended Products",
         x = "Price (USD)", y = "Total Loves") +
    theme_minimal()
  
  print(price_loves_plot)
  
} else {
  cat("No recommendations available.\n")
}



#Export Final Data from R
# Save final recommendations
write.csv(recommendations, "C:\\Users\\bochr\\Downloads\\final_recommendations.csv", row.names = FALSE)

# Save evaluation scores
evaluation_df <- data.frame(
  user_id = user_id,
  precision = evaluation$precision,
  recall = evaluation$recall
)
write.csv(evaluation_df, "C:\\Users\\bochr\\Downloads\\evaluation_scores.csv", row.names = FALSE)



getwd()


