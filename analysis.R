
# E-Commerce Usage and Consumer Buying Decisions
# STEP 4: STATISTICAL ANALYSIS

library(ggplot2)

# Load dataset
ecommerce_data <- read.csv("ecommerce_consumer_buying.csv")

# 1. Basic descriptive statistics
cat("TOTAL RESPONDENTS:", nrow(ecommerce_data), "\n")

cat("AVERAGE AGE:",
    mean(ecommerce_data$Age), "\n")

cat("AVERAGE MONTHLY ORDERS:",
    mean(ecommerce_data$Average_Orders_Per_Month), "\n")

cat("AVERAGE MONTHLY SPENDING:",
    mean(ecommerce_data$Average_Spending_PHP), "\n")

cat("AVERAGE BUYING SATISFACTION:",
    mean(ecommerce_data$Buying_Satisfaction_Score), "\n")

# 2. Preferred E-Commerce Platforms
platform_counts <- as.data.frame(
  table(ecommerce_data$Preferred_Platform)
)

colnames(platform_counts) <- c("Platform", "Respondents")

# GRAPH 1
graph1 <- ggplot(
  platform_counts,
  aes(x = Platform, y = Respondents, fill = Platform)
) +
  geom_col() +
  labs(
    title = "Preferred E-Commerce Platforms",
    x = "E-Commerce Platform",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph1)

ggsave(
  "graph1_preferred_platforms.png",
  graph1,
  width = 8,
  height = 5
)

# 3. Monthly Spending by Shopping Frequency
# GRAPH 2
graph2 <- ggplot(
  ecommerce_data,
  aes(
    x = Shopping_Frequency,
    y = Average_Spending_PHP,
    fill = Shopping_Frequency
  )
) +
  geom_boxplot() +
  labs(
    title = "Monthly Spending by Shopping Frequency",
    x = "Shopping Frequency",
    y = "Average Monthly Spending (PHP)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph2)

ggsave(
  "graph2_spending_by_frequency.png",
  graph2,
  width = 8,
  height = 5
)

# 4. Factors Influencing Consumer Buying Decisions
factor_counts <- as.data.frame(
  table(ecommerce_data$Purchase_Decision_Factor)
)

colnames(factor_counts) <- c("Factor", "Respondents")

# GRAPH 3
graph3 <- ggplot(
  factor_counts,
  aes(x = reorder(Factor, Respondents),
      y = Respondents,
      fill = Factor)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Factors Influencing Consumer Buying Decisions",
    x = "Purchase Decision Factor",
    y = "Number of Respondents"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph3)

ggsave(
  "graph3_buying_decision_factors.png",
  graph3,
  width = 8,
  height = 5
)

# 5. Summary of Results
cat("\n--- ANALYSIS SUMMARY ---\n")

cat("Most preferred platform:\n")
print(names(which.max(table(
  ecommerce_data$Preferred_Platform
))))

cat("\nMost common purchase decision factor:\n")
print(names(which.max(table(
  ecommerce_data$Purchase_Decision_Factor
))))

cat("\nAnalysis completed successfully!\n")

