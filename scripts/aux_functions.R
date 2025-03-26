### Data Cleaning auxiliar functions

# Function to clean labels:
clean_value <- function(x) {
  if (is.character(x)) {
    x <- trimws(x)
    x <- gsub("\\s.*|[a-zA-Z].*", "", x)
  }
  return(x)
}

# Function to extract only alphabetic characters, and transform empty string to NAs:
extract_letters <- function(x) {
  if (is.character(x)) {
    x <- gsub("[^a-zA-Z]", "", x)
    x[x == ""] <- NA
  }
  return(x)
}

# Function to extract the NUTS code:
extract_nuts_code <- function(x) {
  sapply(strsplit(x, ","), tail, 1)
}

# Function to perform the Shapiro-Wilk test and return the p-value:
test_normality <- function(x) {
  if (is.numeric(x) && length(unique(x)) > 1) {
    return(shapiro.test(x)$p.value)
  } else {
    return(NA)
  }
}

# Function to create Q-Q plots with formatted p-values:
create_qq_plots <- function(data, cols, p_values) {
  data_long <- data %>%
    select(all_of(cols)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
  
  # Combine p-values with long-format data:
  data_long <- data_long %>%
    mutate(p_value = format(p_values[variable], scientific = TRUE, digits = 3)) %>%
    group_by(variable) %>%
    mutate(label = paste0("p-value: ", unique(p_value)))
  
  ggplot(data_long, aes(sample = value)) +
    stat_qq() +
    stat_qq_line(color = "blue", size = 0.8) +
    facet_wrap(~ variable, scales = "free", nrow = 2) +
    geom_text(aes(x = -Inf, y = Inf, label = label), 
              size = 5, 
              color = "red", 
              vjust = 1.3,  # Position below
              hjust = -0.1) +  # Align to the right
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),  # Remove grid
      strip.background = element_blank(),  # Remove background from facet titles
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border around plots
      axis.ticks = element_line(color = "black", size = 0.5),  # Axis ticks on X and Y
      axis.ticks.length = unit(0.25, "cm"),  # Length of ticks
      axis.line = element_line(color = "black", size = 0.5),  # Axis lines
      strip.text = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 17, face = "bold", hjust = 0.5),  # Center the main title
      plot.margin = margin(10, 10, 10, 10)  # Add margins for spacing
    ) +
    labs(
      title = "Q-Q Plots",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    )
}

# Function to create boxplots with custom labels for outliers (pre- or post-imputation):
create_boxplot_imp <- function(data, col_names, dataset_name, dataset_names, imputation_type) {
  # Retrieve the corresponding title and y-axis label:
  title <- dataset_names$variable_title[dataset_names$variable_name == dataset_name]
  ylabel <- dataset_names$variable_ylabel[dataset_names$variable_name == dataset_name]
  
  # Reshape data to long format for ggplot2:
  data_long <- data %>%
    select(all_of(c("NUTS_Code", col_names))) %>%
    pivot_longer(cols = -all_of("NUTS_Code"), names_to = "Year", values_to = "Value") %>%
    mutate(Year = str_remove(Year, "X"))
  
  # Identify outliers:
  outliers_data <- data_long %>%
    group_by(Year) %>%
    mutate(
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      IQR = IQR(Value, na.rm = TRUE),
      Lower_Bound = Q1 - 1.5 * IQR,
      Upper_Bound = Q3 + 1.5 * IQR,
      Is_Outlier = (Value < Lower_Bound | Value > Upper_Bound)
    ) %>%
    filter(Is_Outlier) %>%
    ungroup()
  
  # Assign a unique color for each NUTS code:
  outliers_data <- outliers_data %>%
    mutate(
      NUTS_Color = factor(!!sym("NUTS_Code"), 
                          levels = outliers_data %>%
                            arrange(desc(Value)) %>% 
                            pull("NUTS_Code") %>% 
                            unique())
    )
  
  # Generate sufficient colors using the Paired palette:
  n_colors <- length(unique(outliers_data$NUTS_Color))
  paired_colors <- colorRampPalette(brewer.pal(12, "Paired"))(n_colors)
  
  # Create the boxplot:
  boxplot <- ggplot(data_long, aes(x = Year, y = Value)) +
    geom_boxplot(fill = "skyblue", color = "black", outlier.shape = NA) +
    geom_point(data = outliers_data, aes(x = Year, y = Value, color = NUTS_Color), size = 3) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_color_manual(
      values = colorRampPalette(brewer.pal(12, "Paired"))(length(unique(outliers_data$NUTS_Color))),
      name = "Outlier NUTS Codes"
    ) +
    geom_text_repel(
      data = outliers_data,
      aes(x = Year, y = Value, label = get("NUTS_Code")),
      size = 5, color = "darkred", max.overlaps = 12
    ) +
    labs(
      title = paste(str_wrap(title, width = 50), imputation_type),  # Append imputation type to title
      x = "Year",
      y = ylabel
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 29, margin = margin(b = 10), lineheight = 1.2, face = "bold"),
      plot.title.position = "plot",
      axis.title.x = element_text(size = 26),
      axis.title.y = element_text(size = 26),
      axis.text.x = element_text(size = 21),
      axis.text.y = element_text(size = 21),
      panel.grid = element_blank(),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.ticks.length = unit(5, "pt"),
      panel.border = element_rect(color = "black", fill = NA, size = 1)
    )
  #print(boxplot)
}

# Function to create boxplots with custom labels for outliers (post-imputation):
create_boxplot_2019 <- function(data, col_name, dataset_names) {
  # Retrieve the corresponding title and y-axis label:
  title <- dataset_names$variable_title[dataset_names$variable_name == col_name]
  ylabel <- dataset_names$variable_ylabel[dataset_names$variable_name == col_name]
  
  # Remove the phrase " per year" from the title, if present:
  title <- str_remove(title, " por ano$")
  
  # Reshape data to long format for ggplot2:
  data_long <- data %>%
    select(NUTS_Code, all_of(col_name)) %>%
    rename(Value = !!sym(col_name))  # Rename for consistency
  
  # Identify outliers:
  outliers_data <- data_long %>%
    mutate(
      Q1 = quantile(Value, 0.25, na.rm = TRUE),
      Q3 = quantile(Value, 0.75, na.rm = TRUE),
      IQR = IQR(Value, na.rm = TRUE),
      Lower_Bound = Q1 - 1.5 * IQR,
      Upper_Bound = Q3 + 1.5 * IQR,
      Is_Outlier = (Value < Lower_Bound | Value > Upper_Bound)
    ) %>%
    filter(Is_Outlier)
  
  # Assign a unique color for each NUTS code:
  outliers_data <- outliers_data %>%
    mutate(
      NUTS_Color = factor(NUTS_Code, 
                          levels = outliers_data %>%
                            arrange(desc(Value)) %>% 
                            pull(NUTS_Code) %>% 
                            unique())
    )
  
  # Generate a sufficient number of colors using the Paired palette:
  n_colors <- length(unique(outliers_data$NUTS_Color))
  paired_colors <- colorRampPalette(brewer.pal(12, "Paired"))(n_colors)
  
  # Create the boxplot:
  boxplot <- ggplot(data_long, aes(x = "", y = Value)) +
    geom_boxplot(fill = "skyblue", color = "black", outlier.shape = NA) +
    geom_point(data = outliers_data, aes(x = "", y = Value, color = NUTS_Color), size = 3) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_color_manual(
      values = paired_colors,
      name = "Outlier NUTS Codes"
    ) +
    geom_text_repel(
      data = outliers_data,
      aes(x = "", y = Value, label = NUTS_Code),
      size = 5, color = "darkred", max.overlaps = 12
    ) +
    labs(
      title = paste0(str_wrap(title, width = 50), " (post-imputation phase 2)"),
      x = "2019",
      y = ylabel
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 29, margin = margin(b = 10), lineheight = 1.2, face = "bold"),
      plot.title.position = "plot",
      axis.title.x = element_text(size = 26),
      axis.title.y = element_text(size = 26),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 21),
      panel.grid = element_blank(),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.ticks.length = unit(5, "pt"),
      panel.border = element_rect(color = "black", fill = NA, size = 1)
    )
  #print(boxplot)
}


# Principal Component Analysis auxiliary functions:

# Normalization of principal component scores:
normalize_pca_scores <- function(pca_object) {
  # Retrieve the original scores and eigenvalues (variance explained by each component):
  scores <- pca_object$scores
  eigenvalues <- pca_object$values[1:ncol(scores)]
  
  # Normalize the scores by dividing by the square root of the corresponding eigenvalues:
  normalized_scores <- sweep(scores, 2, sqrt(eigenvalues), FUN = "/")
  
  return(normalized_scores)
}


# Clustering auxiliary functions:

# Elbow method to evaluate the optimal number of clusters:
wssplot <- function(data, nc = 10, seed = 123) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$tot.withinss)
  }
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-group Sum of Squares")
}
