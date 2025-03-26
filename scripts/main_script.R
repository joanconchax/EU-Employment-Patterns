### Main Script

# Load all packages/libraries, and install them if they are not already installed:
packages <- c(
  "readxl", "dplyr", "tidyr", "knitr", "stringr", "ggplot2", "reshape2",
  "zoo", "mice", "corrplot", "purrr", "grid", "gridExtra", "ggrepel",
  "RColorBrewer", "psych", "readr", "caret", "cluster", "factoextra",
  "plotly", "mclust", "DT", "tidytext", "openxlsx", "tmap", "eurostat", "sf"
)

# Install any packages that are missing:
missing <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(missing)) install.packages(missing)

# Load all specified packages:
invisible(lapply(packages, library, character.only = TRUE))


# Define the directory that contains the .tsv files:
directory <- "./tsv_files"

# Read the auxiliary Excel files:
dataset_names <- read_excel("aux_rename_datasets.xlsx")
nuts_codes <- read_excel("NUTS2Final.xlsx")

# Two auxiliary scripts with helper functions were imported:
source("aux_functions.R")


##############  CREATION AND CLEANING OF VARIABLES  ##############

# Extract the 'variable_name' column as the list of dataset names:
variable_names <- dataset_names$variable_name
valid_nuts_codes <- nuts_codes$`NUTS Code`
print(valid_nuts_codes)

# Read and assign each .tsv file to its corresponding variable:
for (var_name in variable_names) {
  file_path <- file.path(directory, paste0(var_name, ".tsv"))
  
  if (file.exists(file_path)) {
    df <- read.delim(file_path, header = TRUE, sep = "\t")
    
    # Remove columns X2010 and X2011 if they exist:
    df <- df[, !names(df) %in% c("X2010", "X2011")]
    
    # Create the NUTS code column:
    df$NUTS_Code <- extract_nuts_code(df[[1]])
    df <- df[, c(1, ncol(df), 2:(ncol(df)-1))]
    
    # Clean the labels:
    df[, -c(1,2)] <- lapply(df[, -c(1,2)], clean_value)
    
    # Remove the first column:
    df <- df[, -1]
    
    # Filter rows to retain only valid NUTS codes:
    df <- df %>% filter(NUTS_Code %in% valid_nuts_codes)
    
    # Dynamically assign the cleaned dataframe to a variable:
    assign(var_name, df)
    message(paste("Loaded, processed, and filtered:", var_name))
  } else {
    message(paste("File not found:", file_path))
  }
  rm(df)
}


##############  COUNTING THE % OF MISSING VALUES FOR ALL DATASETS  ##############

# Create list as placeholder for results:
year_missing_counts <- list()

# Count the number of datasets:
total_datasets <- 0

for (var_name in variable_names) {
  if (exists(var_name)) {
    df <- get(var_name)
    
    # Identify columns corresponding to years:
    year_columns <- grep("^X[0-9]{4}$", names(df), value = TRUE)
    
    # Iterate over year columns in each dataset (e.g., X2019, X2020):
    for (year_col in year_columns) {
      # Count occurrences of ":" and 0 (i.e., values starting with ":" and zero):
      missing_count <- sum(df[[year_col]] == ":", na.rm = TRUE)
      print(missing_count)
      total_missing <- missing_count
      
      # If the year is not yet in year_missing_counts, add it with the current missing count:
      if (!year_col %in% names(year_missing_counts)) {
        year_missing_counts[[year_col]] <- total_missing
      } else { # Otherwise, increment the existing count:
        year_missing_counts[[year_col]] <- year_missing_counts[[year_col]] + total_missing
      }
    }
    total_datasets <- total_datasets + 1
    message(paste("Processed dataset:", var_name))
  }
}

# Calculate the average percentage of missing values per year:
mean_missing_percentages <- sapply(year_missing_counts, function(missing_count) {
  (missing_count / (242 * total_datasets)) * 100
})

# Create a dataframe summarizing the percentage of missing values:
mean_missing_summary_df <- data.frame(
  Year = names(mean_missing_percentages),
  MeanMissingPercentage = mean_missing_percentages
)
View(mean_missing_summary_df)


##############  CREATION OF THE FINAL DATASET CONSIDERING ONLY THE YEAR 2019  ##############

# Rename NUTS Code column to NUTS_Code:
colnames(nuts_codes)[colnames(nuts_codes) == "NUTS Code"] <- "NUTS_Code"

datasets_list <- list()

for (var_name in variable_names) {
  if (exists(var_name)) {
    df <- get(var_name)
    
    # If the dataset contains the X2019 column, proceed:
    if ("X2019" %in% names(df)) {
      # Extract the 2019 column and NUTS code:
      df_2019 <- df %>%
        select(NUTS_Code, X2019) %>%
        rename(!!var_name := X2019)  # Rename 2019 to the dataset name
      
      # Merge with the list of NUTS codes:
      df_2019 <- nuts_codes %>%
        select(NUTS_Code) %>%
        left_join(df_2019, by = "NUTS_Code")  # Left join to preserve all NUTS codes
      
      # Add the dataframe to the list:
      datasets_list[[var_name]] <- df_2019
    }
  }
} # Repeat for each dataset

# Combine all datasets from the list, aligning by NUTS code:
combined_df <- reduce(datasets_list, full_join, by = "NUTS_Code")
#View(combined_df)

# Store total number of rows for later use:
total_rows <- nrow(combined_df)

# Count missing values (":", and NA) in each column:
missing_counts <- sapply(combined_df, function(column) {
  sum(column == ":" | is.na(column))
})

# Calculate the percentage of missing values for each column:
missing_percentages <- (missing_counts / total_rows) * 100

# Identify columns with more than 20% missing values:
columns_to_remove <- names(missing_percentages[missing_percentages > 20])

# Remove columns with more than 20% missing values:
combined_df_cleaned <- combined_df[, !(names(combined_df) %in% columns_to_remove)]
#View(combined_df_cleaned)

# Clean the labels:
combined_df_cleaned[, -1] <- lapply(combined_df_cleaned[, -1], clean_value)

# Replace ":" with NA and convert all columns (except the first) to numeric:
combined_df_cleaned <- combined_df_cleaned %>%
  mutate(across(2:ncol(combined_df_cleaned), ~as.numeric(na_if(., ":"))))


##########  EXPLORATORY DATA ANALYSIS  #############

# Assess the normality of variables before imputation:
normality_results <- sapply(combined_df_cleaned, test_normality)

# Identify non-normally distributed columns (p-value < 0.05):
non_normal_cols <- names(normality_results[normality_results < 0.05])

# Extract numeric columns from the combined dataset:
numeric_cols <- names(combined_df_cleaned)[sapply(combined_df_cleaned, is.numeric)]

# Generate Q-Q plots for every 4 columns:
for (i in seq(1, length(numeric_cols), 4)) {
  cols_to_plot <- numeric_cols[i:min(i + 3, length(numeric_cols))]
  p_values <- normality_results[cols_to_plot]
  #print(create_qq_plots(combined_df_cleaned, cols_to_plot, p_values))
}


##################  PHASE 1: MISSING VALUE IMPUTATION IN EACH INDIVIDUAL DATASET  ##################

# Loop through each column in the cleaned dataset (excluding the first column):
for (col_name in names(combined_df_cleaned)[-1]) {
  # Retrieve the corresponding dataset:
  eurostat_data <- get(col_name)
  
  # Exclude columns after X2019:
  eurostat_data <- eurostat_data[, 1:which(names(eurostat_data) == "X2019")]
  
  # Replace ":" with NA and convert columns to numeric:
  eurostat_data <- eurostat_data %>%
    mutate(across(-1, ~if(is.numeric(.)) . else as.numeric(na_if(., ":"))))
  
  # Auxiliary variable: define the year columns for boxplot visualization:
  year_columns <- c("X2014", "X2015", "X2016", "X2017", "X2018", "X2019")
  
  # Display boxplot for the selected years (pre-imputation):
  create_boxplot_imp(eurostat_data, year_columns, col_name, dataset_names, "(pre-imputation phase 1)")
  
  # Auxiliary calculation of original variance before imputation:
  original_variance <- sapply(eurostat_data[, year_columns], var, na.rm = TRUE)
  
  # Display missing data pattern (excluding the first geo code column):
  print(md.pattern(eurostat_data[, 2:ncol(eurostat_data)], rotate.names = TRUE))
  grid.text(paste("Missing Data Pattern -", col_name, "(pre-imputation phase 1)"),
            x = 0.5, y = unit(0.96, "npc"), gp = gpar(fontsize = 19, fontface = "bold"))
  
  # Create a predictor matrix and perform imputation:
  predictor_matrix <- matrix(0, ncol = ncol(eurostat_data) - 1, nrow = ncol(eurostat_data) - 1)
  predictor_matrix[ncol(predictor_matrix), 1:(ncol(predictor_matrix)-1)] <- 1
  imputed_data <- mice(eurostat_data[, -1], m = 5, method = 'pmm', seed = 123,
                       predictorMatrix = predictor_matrix)
  
  # Replace only the X2019 column in the original dataset:
  eurostat_data$X2019 <- complete(imputed_data)$X2019
  
  # Calculate variance after imputation:
  imputed_variance <- sapply(eurostat_data[, year_columns], var, na.rm = TRUE)
  
  # Compare original and imputed variances:
  var_comparison <- data.frame(Year = year_columns, Original = original_variance, Imputed = imputed_variance,
                               Diff_Percent = (imputed_variance - original_variance) / original_variance * 100)
  diff_percent_x2019 <- var_comparison$Diff_Percent[var_comparison$Year == "X2019"]
  cat("\nPercentage Difference for variable", col_name, "in year 2019:", round(diff_percent_x2019, 2), "%\n")
  
  # Create a bar plot to visualize variance differences for col_name:
  print(
    ggplot(var_comparison, aes(x = Year, y = Diff_Percent)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      theme_minimal(base_size = 15) +
      labs(
        title = paste("Variance Change -", col_name, "(post-imputation phase 1)"),
        x = "Years",
        y = "Percentage Difference (%)"
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1.2) +
      theme(
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),  # Center the title
        axis.text.x = element_text(size = 13),  # Size of x-axis labels
        axis.text.y = element_text(size = 11),  # Size of y-axis labels
        axis.title.x = element_text(size = 21),  # Size of x-axis title
        axis.title.y = element_text(size = 21),  # Size of y-axis title
        panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a border
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank() 
      ) +
      scale_y_continuous(
        labels = scales::percent_format(scale = 1), 
        breaks = scales::pretty_breaks(n = 10)
      ) +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
  )
  
  # Display missing data pattern after imputation (excluding first column):
  print(md.pattern(eurostat_data[, 2:ncol(eurostat_data)], rotate.names = TRUE))
  grid.text(paste("Missing Data Pattern -", col_name, "(post-imputation phase 1)"),
            x = 0.5, y = unit(0.96, "npc"), gp = gpar(fontsize = 19, fontface = "bold"))
  
  # Join the new 2019 column with the cleaned combined dataset:
  combined_df_cleaned <- combined_df_cleaned %>%
    left_join(select(eurostat_data, NUTS_Code, X2019), by = "NUTS_Code") %>%
    mutate(!!sym(col_name) := coalesce(X2019, !!sym(col_name))) %>%
    select(-X2019)
  
  # Reassign the updated data back to its original variable:
  assign(col_name, eurostat_data)
  
  # Display boxplot for the selected years (post-imputation):
  create_boxplot_imp(eurostat_data, year_columns, col_name, dataset_names, "(post-imputation phase 1)")
  print(paste("Imputation completed for X2019 in", col_name))
}


##################  PHASE 2: MISSING VALUE IMPUTATION IN THE FINAL TABLE  ##################

# Print missing data pattern (pre-imputation):
print(md.pattern(combined_df_cleaned[, -1], rotate.names = TRUE))
grid.text(paste("Missing Data Pattern for 2019 (pre-imputation phase 2)"),
          x = 0.5, y = unit(0.95, "npc"), gp = gpar(fontsize = 15.5, fontface = "bold"))

# Assume the first column is an identifier (e.g., NUTS_Code):
data_for_imputation <- combined_df_cleaned


# Specify the predictor columns (all except the first one):
predictor_cols <- names(data_for_imputation)[-1]

# Perform CART-based imputation using the mice package:
imputed_data <- mice(data_for_imputation[, predictor_cols], m = 5, method = 'rf', seed = 123)

# Complete the imputed dataset:
completed_data <- complete(imputed_data)

# Combine the completed data with the NUTS_Code column:
final_combined_df <- data_for_imputation %>%
  select(NUTS_Code) %>%
  bind_cols(completed_data)

# Display the missing data pattern (post-imputation):
print(md.pattern(final_combined_df[, -1], rotate.names = TRUE))
grid.text(paste("Missing Data Pattern for 2019 (post-imputation phase 2)"),
          x = 0.5, y = unit(0.95, "npc"), gp = gpar(fontsize = 15.5, fontface = "bold"))

# Print summary of the completed dataset:
print(summary(final_combined_df))

# Optionally, assign the final dataset back to combined_df_cleaned:
combined_df_cleaned <- final_combined_df

# Columns that were imputed in phase 2:
column_names <- c("Cult_Emp", "NEET_M", "NEET_F", "Y_EmpRate_M", "Y_EmpRate_F", "Y_EmpRate_Age")

# Display boxplots for the selected columns (post-imputation):
for (col_name in column_names) {
  create_boxplot_2019(combined_df_cleaned, col_name, dataset_names)
}

# Export the final cleaned dataset to a TSV file:
write.table(combined_df_cleaned, file = "cleaned_dataset.tsv", sep = "\t", row.names = FALSE, quote = FALSE)


###########  IMPUTATION QUALITY ASSESSMENT  ###########    

stripplot(imputed_data, pch = 20, main = "Imputation Quality – Phase 2", 
          col = c("blue", "red"), 
          cex = 1.2, 
          layout = c(5, 1),
          subset = .imp > 0 & .imp <= 5)

# Calculate and compare variances:
original_variance <- sapply(data_for_imputation[, -1], var, na.rm = TRUE)
imputed_variance <- sapply(final_combined_df[, -1], var)
var_comparison <- data.frame(Original = original_variance, Imputed = imputed_variance)

# Compute the percentage difference:
var_comparison$diff_percent <- (var_comparison$Imputed - var_comparison$Original) / var_comparison$Original * 100
var_comparison_sorted <- var_comparison[order(var_comparison$diff_percent), ]

# Generate a plot to show the percentage difference across variables:
ggplot(var_comparison_sorted, aes(x = reorder(rownames(var_comparison_sorted), diff_percent), y = diff_percent)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  coord_flip() +  
  theme_minimal(base_size = 15) + 
  labs(
    title = "Variance Changes (post-imputation phase 2)",
    x = "Variables",
    y = "Difference (%)"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),  # Center the title
    axis.text.y = element_text(size = 11),  # y-axis label size
    axis.text.x = element_text(size = 13),  # x-axis label size
    axis.title.x = element_text(size = 21),  # x-axis title size
    axis.title.y = element_text(size = 21),  # y-axis title size
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()  
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1.2) + 
  scale_y_continuous(
    labels = scales::percent_format(scale = 1), 
    breaks = scales::pretty_breaks(n = 10) 
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))


########## PRINCIPAL COMPONENT ANALYSIS ##########

# dataset <- read_tsv("dataset_final.tsv")
dataset <- combined_df_cleaned

# View dataset structure and sample rows:
str(dataset)
head(dataset)

# Assess PCA assumptions:
# Visualize dispersion and detect outliers:
pairs(dataset[,2:14], pch = 19, lower.panel = NULL)
pairs(dataset[,15:27], pch = 19, lower.panel = NULL)
pairs(dataset[,28:40], pch = 19, lower.panel = NULL)
pairs(dataset[,41:52], pch = 19, lower.panel = NULL)

# Visualize correlation matrix:
correlation <- cor(dataset[,2:52])
par(oma = c(2, 2, 2, 2))  # Add outer margins for text
corrplot.mixed(correlation,
               order = "hclust",  # Cluster variable ordering
               tl.pos = "lt",  # Text placement: left and top
               upper = "ellipse",
               lower = "ellipse")

# Remove display limit to allow full PCA component output:
options(max.print = 1e6)

# Correlation matrix (rounded to 3 decimal places):
round(correlation, 3)

# Perform Bartlett’s test and compute KMO statistic:
cortest.bartlett(correlation)
KMO(correlation)

### SELECTING NUMBER OF COMPONENTS ###

# Normalize the data:
data_scaled <- scale(dataset[,2:52])

# Extract components without rotation:
pc51 <- principal(data_scaled, nfactors = 51, rotate = "none", scores = TRUE)
pc51  # View component metrics

# Kaiser criterion:
round(pc51$values, 3)

# Scree plot:
plot(pc51$values, type = "b", main = "Scree plot for Employment dataset", xlab = "Number of PC", ylab = "Eigenvalue")

# Check variable loadings:
pc51$loadings

# Check communalities of retained variables:
pc51$communality

# Extract a 4-component solution:
pc4 <- principal(data_scaled, nfactors = 4, rotate = "none")
pc4

# Extract a 5-component solution:
pc5 <- principal(data_scaled, nfactors = 5, rotate = "none")
pc5

### INTERPRETATION AND ROTATION ###

# Rotate the 5 components using varimax:
pc5r <- principal(data_scaled, nfactors = 5, rotate = "varimax")
pc5r$loadings  # View rotated loadings

# View communalities:
round(pc5r$communality, 2)

## Interpretation ##
# Evaluate the first principal component:
pc5sc <- principal(data_scaled, nfactors = 5, rotate = "none", scores = TRUE)
round(pc5sc$scores, 3)

# Compute the mean:
mean(pc5sc$scores[,1])

# Compute the standard deviation:
sd(pc5sc$scores[,1])


# Normalize the eigenvalues (variance explained by each component) applied to the PCA model with 5 components (example):
normalized_scores <- normalize_pca_scores(pc5sc)

# View the new normalized scores:
head(normalized_scores)

### Component Assessment ###
# Add the normalized scores to the original dataset:
dataset$Dimensão_Laboral <- normalized_scores[,1]
dataset$Faces_do_Mercado_de_Trabalho <- normalized_scores[,2]
dataset$Emprego_Industrial <- normalized_scores[,3]
dataset$Inovação_vs_Tradição <- normalized_scores[,4]
dataset$Raízes_do_Desemprego <- normalized_scores[,5]

# Save the dataset to an Excel file:
write.xlsx(dataset, file = "Employment_EuroStat.xlsx",
           sheetName = "PCAscores", colNames = TRUE,
           rowNames = TRUE, append = FALSE)

# Scatterplot of PC1 vs PC2:
plot(dataset$Dimensão_Laboral, dataset$Faces_do_Mercado_de_Trabalho, pch = 19,
     xlim = c(-2, 3), ylim = c(-3, 2), 
     xlab = "Dimensão Laboral", ylab = "Faces do Mercado de Trabalho", 
     main = "Scores: PC1 vs PC2 (Normalized)")
text(dataset$Dimensão_Laboral, dataset$Faces_do_Mercado_de_Trabalho - 0.1, dataset[,2])  # (x, y, labels)


################### Clustering #########################

nutscode <- dataset$NUTS_Code 
geodata <- get_eurostat_geospatial(nuts_level = 2, year = 2016)
set.seed(123)

### Hierarchical Clustering ###
rownames(dataset) <- nutscode
dist_matrix <- dist(dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")])
hclust_result <- hclust(dist_matrix, method = "ward.D2")
k <- 3
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "", labels = rownames(dataset), cex.lab = 0.1, hang = -3)
rect.hclust(hclust_result, k = k, border = "red")

# Add hierarchical clusters to the dataset:
clusters_hierarchical <- cutree(hclust_result, k)
dataset$cluster_hierarchical <- clusters_hierarchical

# Hierarchical Silhouette Plot:
silhouette_scores <- silhouette(clusters_hierarchical, dist_matrix)
plot(silhouette_scores, main = paste("Silhouette Plot for Hierarchical"))

# Cluster profiles by average scores in each dimension:
cluster_profiles <- aggregate(dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")],
                              by = list(cluster = clusters_hierarchical), mean)
print(cluster_profiles)


### Elbow Method ###
wssplot(dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")], nc = 15)

### K-Means Clustering ###

# 4 Clusters:
kmeans_result4 <- kmeans(dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")], centers = 4, nstart = 25)
dataset$cluster_kmeans4 <- kmeans_result4$cluster
resultskmeaens4 <- data.frame(
  nutscode = nutscode,  
  cluster_kmeans4 = dataset$cluster_kmeans4
)
clusters_split4 <- split(resultskmeaens4$nutscode, resultskmeaens4$cluster_kmeans4)
clusters_split4

# Silhouette Plot for K-Means with 4 clusters:
silhouette_scores_kmeans4 <- silhouette(kmeans_result4$cluster, dist_matrix)
plot(silhouette_scores_kmeans4, main = "Silhouette Plot for K-Means Clustering 4 Clusters")

# K-Means Cluster Map:
map_data <- geodata %>%
  inner_join(dataset, by = c("geo" = "NUTS_Code"))  

custom_palette <- c("#1b9e77", "#d95f02", "#7570b3", "#fdae55")  

map_data$cluster_kmeans4 <- factor(map_data$cluster_kmeans4,
                                   levels = c(1, 2, 3, 4),  
                                   labels = c("Eastern and Portuguese Emergence",
                                              "Hard Work", 
                                              "Urban Centers and Economic Capitals",
                                              "Traditional Europe"))

mapk4 <- tm_shape(geodata,
                  projection = "EPSG:3035",  
                  xlim = c(2400000, 7800000),  
                  ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +  
  tm_shape(map_data) +
  tm_polygons("cluster_kmeans4",  
              title = "K-Means Clusters",  
              palette = custom_palette,  
              legend.labels = c("Hard Work", "Traditional Europe", "Eastern and Portuguese Emergence", "Urban Centers and Economic Capitals")  # Custom cluster names
  ) +
  tm_layout(legend.outside = TRUE)
print(mapk4)


### PAM Clustering ###

pam_result <- pam(dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")], k = 4, metric = "euclidean")
dataset$cluster_pam <- pam_result$clustering
clusplot(pam_result, labels = 4, main = "PAM Clustering of PCA Components")
resultspam <- data.frame(
  nutscode = nutscode,  
  cluster_pam = dataset$cluster_pam
)
clusters_splitpam <- split(resultspam$nutscode, resultspam$cluster_pam)
clusters_splitpam

# Silhouette Plot for PAM:
silhouette_scores_pam <- silhouette(resultspam$cluster, dist_matrix)
plot(silhouette_scores_pam, main = "Silhouette Plot for PAM Clustering")

# PAM Cluster Map:
map_data <- geodata %>%
  inner_join(dataset, by = c("geo" = "NUTS_Code")) 

mapp4 <- tm_shape(geodata,
                  projection = "EPSG:3035",  
                  xlim = c(2400000, 7800000), 
                  ylim = c(1320000, 5650000)
) +
  tm_fill("white") +  
  tm_shape(map_data) +
  tm_polygons("cluster_pam",  
              title = "PAM 4 Clusters",
              palette = "Set3"  
  ) +
  tm_layout(legend.outside = TRUE)

print(mapp4)


### Gaussian Mixture Model (GMM) Clustering ###

pca_data <- dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")]
gmm_result <- Mclust(pca_data)
summary(gmm_result)

# Plot BIC scores to evaluate different cluster counts:
plot(gmm_result, what = "BIC")

# Assign GMM clusters to the dataset:
dataset$cluster_gmm <- gmm_result$classification

# Silhouette Plot for GMM:
dist_matrix <- dist(pca_data)
silhouette_scores <- silhouette(dataset$cluster_gmm, dist_matrix)
plot(silhouette_scores, main = "Silhouette Plot for GMM Clustering")


### Profiling ###

# Extract the country code from NUTS codes:
dataset$Country <- substr(nutscode, 1, 2)
head(dataset$Country)
unique(dataset$Country)
all(substr(nutscode, 1, 2) == dataset$Country)

# Summarize number and percentage of regions per country and cluster:
country_summary <- dataset %>%
  group_by(Country, cluster_kmeans4) %>%
  summarise(Regions = n()) %>%
  group_by(Country) %>%
  mutate(Percentage = (Regions / sum(Regions)) * 100)

# GDP data:
gdp_data <- read.csv("nama_10r_2gdp$defaultview_page_spreadsheet.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
str(gdp_data)
gdp_data <- gdp_data %>%
  select(NUTS_Codes, GDP)
dataset <- dataset %>%
  left_join(gdp_data, by = c("NUTS_Code" = "NUTS_Codes"))

# Urbanization percentage data:
urbanization_data <- read.csv("lfst_r_lfsd2hh$defaultview_page_spreadsheet.csv", 
                              header = TRUE, 
                              sep = ";", 
                              stringsAsFactors = FALSE)
urbanization_data <- urbanization_data %>%
  select(NUTS_Code, Urbanisation)
dataset <- dataset %>%
  left_join(urbanization_data, by = c("NUTS_Code" = "NUTS_Code"))

# Define capital regions:
capital_regions <- c("AT13", "BE10", "DE30", "DK01", "ES30", "EL30", "FR10", 
                     "FI1B", "ITI4", "IE06", "LU00", "NL32", "PT17", "SE11", 
                     "BG41", "CY00", "CZ01", "EE00", "HU11", "LT01", "LV00", 
                     "MT00", "PL91", "RO32", "SI04", "SK01")

# Create binary variable indicating if region is a capital:
dataset$Is_Capital <- ifelse(dataset$NUTS_Code %in% capital_regions, 1, 0)


# Analysis using profile variables:

# Convert GDP to numeric format:
dataset$GDP <- as.numeric(gsub(",", ".", gsub("\\.", "", dataset$GDP)))

# Compute summary statistics for GDP per cluster:
gdp_stats <- dataset %>%
  group_by(cluster_kmeans4) %>%
  summarize(
    Min_GDP = min(GDP, na.rm = TRUE),
    Max_GDP = max(GDP, na.rm = TRUE),
    Mean_GDP = mean(GDP, na.rm = TRUE),
    Median_GDP = median(GDP, na.rm = TRUE)
  )
print(gdp_stats)
View(gdp_stats)

# Rename clusters with descriptive labels:
gdp_stats$cluster_kmeans4 <- factor(gdp_stats$cluster_kmeans4,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("Hard Work",
                                               "Traditional Europe",
                                               "Eastern and Portuguese Emergence",
                                               "Urban Centers and Economic Capitals"))

# Convert GDP stats to long format for plotting:
gdp_long <- gdp_stats %>%
  pivot_longer(cols = c(Min_GDP, Max_GDP, Mean_GDP, Median_GDP),
               names_to = "Metric", values_to = "Value")

# Create bar chart comparing GDP metrics by cluster:
ggplot(gdp_long, aes(x = cluster_kmeans4, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set2", labels = c("Maximum GDP", "Mean GDP", "Median GDP", "Minimum GDP")) +
  theme_minimal() +
  labs(title = "Comparison of GDP Metrics by Cluster",
       x = "Clusters",
       y = "GDP (in monetary units)",
       fill = "GDP Metrics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(face = "bold", size = 14))


# Urbanization data:

# Convert Urbanisation to numeric format:
dataset$Urbanisation <- as.numeric(gsub(",", ".", gsub("\\.", "", dataset$Urbanisation)))

# Compute summary statistics for Urbanisation per cluster:
urbanization_stats <- dataset %>%
  group_by(cluster_kmeans4) %>%
  summarize(
    Min_Urbanization = min(Urbanisation, na.rm = TRUE),
    Max_Urbanization = max(Urbanisation, na.rm = TRUE),
    Mean_Urbanization = mean(Urbanisation, na.rm = TRUE),
    Median_Urbanization = median(Urbanisation, na.rm = TRUE)
  )
print(urbanization_stats)
View(urbanization_stats)

# Rename clusters with descriptive labels:
urbanization_stats$cluster_kmeans4 <- factor(urbanization_stats$cluster_kmeans4,
                                             levels = c(1, 2, 3, 4),
                                             labels = c("Hard Work",
                                                        "Traditional Europe",
                                                        "Eastern and Portuguese Emergence",
                                                        "Urban Centers and Economic Capitals"))

# Convert to long format for plotting:
urbanization_long <- urbanization_stats %>%
  pivot_longer(cols = c(Min_Urbanization, Max_Urbanization, Mean_Urbanization, Median_Urbanization),
               names_to = "Metric", values_to = "Value")

# Create bar chart comparing urbanization metrics by cluster:
ggplot(urbanization_long, aes(x = cluster_kmeans4, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set2", labels = c("Maximum Urbanization", "Mean Urbanization", "Median Urbanization", "Minimum Urbanization")) +
  theme_minimal() +
  labs(title = "Comparison of Urbanization Level by Cluster",
       x = "Clusters",
       y = "Urbanization Level",
       fill = "Urbanization Metrics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(face = "bold", size = 14))


# Prepare cluster labels:
cluster_names <- c("Hard Work", 
                   "Traditional Europe", 
                   "Eastern and Portuguese Emergence", 
                   "Urban Centers and Economic Capitals")

country_summary$cluster_kmeans4 <- factor(country_summary$cluster_kmeans4,
                                          levels = c(1, 2, 3, 4),
                                          labels = cluster_names)

# View the unique cluster labels:
print(unique(country_summary$cluster_kmeans4))       

# Create bar chart showing the percentage of regions per cluster in each country:
ggplot(country_summary, aes(x = Country, y = Percentage, fill = cluster_kmeans4)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Percentage of Regions per Country in Each Cluster",
    x = "Country",
    y = "Percentage of Regions",
    fill = "Clusters"  
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 8),  
    axis.text.y = element_text(size = 8)   
  )

# Check for capital regions missing from the dataset:
missing_capitals <- setdiff(capital_regions, dataset$NUTS_Code)
cat("Capital regions missing from dataset:\n", missing_capitals, "\n")

# Re-assign capital indicator:
dataset$Is_Capital <- ifelse(dataset$NUTS_Code %in% capital_regions, 1, 0)
table(dataset$Is_Capital)

# List capital regions:
capital_counts <- dataset %>%
  filter(Is_Capital == 1) %>%
  group_by(cluster_kmeans4) %>%
  summarize(Total_Capitals = n())
print(capital_counts)

# List capitals per cluster:
capitals_by_cluster <- dataset %>%
  filter(Is_Capital == 1) %>%
  group_by(cluster_kmeans4) %>%
  summarize(Capitals = list(NUTS_Code))  
for (i in seq_len(nrow(capitals_by_cluster))) {
  cat("\nCluster:", capitals_by_cluster$cluster_kmeans4[i], "\n")
  cat("Capitals:", paste(capitals_by_cluster$Capitals[[i]], collapse = ", "), "\n")
}

# Summarize capitals per cluster and country:
capitals_summary <- dataset %>%
  filter(Is_Capital == 1) %>%
  group_by(cluster_kmeans4) %>%
  summarize(
    Num_Capitals = n(),
    Capitals = paste(unique(substr(NUTS_Code, 1, 2)), collapse = "\n")  # One country per line
  )

# Assign descriptive labels to cluster levels:
capitals_summary$cluster_kmeans4 <- factor(capitals_summary$cluster_kmeans4,
                                           levels = c(1, 2, 3, 4),
                                           labels = c("Hard Work", 
                                                      "Traditional Europe", 
                                                      "Eastern and Portuguese Emergence", 
                                                      "Urban Centers and Economic Capitals"))

# Pie chart showing the distribution of capital regions by cluster:
ggplot(capitals_summary, aes(x = "", y = Num_Capitals, fill = cluster_kmeans4)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  
  coord_polar("y", start = 0) +  
  geom_text(aes(label = Capitals),  
            position = position_stack(vjust = 0.5), size = 3, color = "black", fontface = "bold", lineheight = 0.8) +
  labs(
    title = "Distribuição de Capitais por Cluster",
    fill = "Clusters"
  ) +
  theme_void() +  
  theme(legend.position = "right",  
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
