# Packages
library(readxl)
library(dplyr)
library(tidyr)
library(knitr)
library(stringr)
library(ggplot2)
library(reshape2)
library(zoo)
library(mice)
library(corrplot)
library(purrr)
library(grid)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)
library(psych)
library(readr)
library(caret)
library(cluster)
library(factoextra)
library(plotly)
library(mclust)
library(DT)
library(tidytext)
library(openxlsx)
library(tmap)
library(eurostat)
library(sf)
library(tidyr)

# Definição do diretório que contém os ficheiros .tsv
directory <- "./tsv_files"

# Leitura do ficheiroS Excel auxiliares
dataset_names <- read_excel("aux_rename_datasets.xlsx")
nuts_codes <- read_excel("NUTS2Final.xlsx")


##############  FUNÇÕES  ##############

# Função para limpar os labels
clean_value <- function(x) {
  if (is.character(x)) {
    x <- trimws(x)
    x <- gsub("\\s.*|[a-zA-Z].*", "", x)
  }
  return(x)
}

extract_letters <- function(x) {
  if (is.character(x)) {
    # Manter apenas os caracteres que são letras
    x <- gsub("[^a-zA-Z]", "", x)
    x[x == ""] <- NA
  }
  return(x)
}

# Função para extrair o código NUTS
extract_nuts_code <- function(x) {
  sapply(strsplit(x, ","), tail, 1)
}

# Função para realizar o teste de Shapiro-Wilk e retornar o p-valor
test_normality <- function(x) {
  if (is.numeric(x) && length(unique(x)) > 1) {
    return(shapiro.test(x)$p.value)
  } else {
    return(NA)
  }
}

# Função para criar Q-Q plots com p-valores formatados
create_qq_plots <- function(data, cols, p_values) {
  data_long <- data %>%
    select(all_of(cols)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
  
  # Combinar os valores de p com os dados longos
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
              vjust = 1.3,  # Posicionar abaixo
              hjust = -0.1) +  # Alinhar à direita
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),  # Remove a grade
      strip.background = element_blank(),  # Remove o quadro extra nos títulos
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # Quadro ao redor dos gráficos
      axis.ticks = element_line(color = "black", size = 0.5),  # Ticks nos eixos X e Y
      axis.ticks.length = unit(0.25, "cm"),  # Comprimento dos ticks
      axis.line = element_line(color = "black", size = 0.5),  # Linhas dos eixos
      strip.text = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 17, face = "bold", hjust = 0.5),  # Centralizar o título principal
      plot.margin = margin(10, 10, 10, 10)  # Margens para espaçamento
    ) +
    labs(
      title = "Q-Q Plots",
      x = "Quantis Teóricos",
      y = "Quantis Amostrais"
    )
}

# Função para criar os boxplots, com custom labels para os outliers (pré-imputação ou pós-imputação)
create_boxplot_imp <- function(data, col_names, dataset_name, dataset_names, imputation_type) {
  # Obter o título e o ylabel correspondentes
  title <- dataset_names$variable_title[dataset_names$variable_name == dataset_name]
  ylabel <- dataset_names$variable_ylabel[dataset_names$variable_name == dataset_name]
  
  # Reshape data to long format for ggplot2
  data_long <- data %>%
    select(all_of(c("NUTS_Code", col_names))) %>%
    pivot_longer(cols = -all_of("NUTS_Code"), names_to = "Year", values_to = "Value") %>%
    mutate(Year = str_remove(Year, "X"))
  
  # Identify outliers
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
  
  # Assign a unique color for each NUTS code
  outliers_data <- outliers_data %>%
    mutate(
      NUTS_Color = factor(!!sym("NUTS_Code"), 
                          levels = outliers_data %>%
                            arrange(desc(Value)) %>% 
                            pull("NUTS_Code") %>% 
                            unique())
    )
  
  # Gerar cores suficientes usando a paleta Paired
  n_colors <- length(unique(outliers_data$NUTS_Color))
  paired_colors <- colorRampPalette(brewer.pal(12, "Paired"))(n_colors)
  
  # Create the boxplot
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
      title = paste(str_wrap(title, width = 50), imputation_type),  # Adicionando tipo de imputação no título
      x = "Ano",
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
  
  print(boxplot)
}

# Função para criar os boxplots, com custom labels para os outliers (pós-imputação)
create_boxplot_2019 <- function(data, col_name, dataset_names) {
  # Obter o título e o ylabel correspondentes
  title <- dataset_names$variable_title[dataset_names$variable_name == col_name]
  ylabel <- dataset_names$variable_ylabel[dataset_names$variable_name == col_name]
  
  # Remove a parte " por ano" do título, se necessário
  title <- str_remove(title, " por ano$")
  
  # Reshape data para formato longo para ggplot2
  data_long <- data %>%
    select(NUTS_Code, all_of(col_name)) %>%
    rename(Value = !!sym(col_name))  # Renomear para consistência
  
  # Identificar outliers
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
  
  # Atribuir uma cor única para cada NUTS code
  outliers_data <- outliers_data %>%
    mutate(
      NUTS_Color = factor(NUTS_Code, 
                          levels = outliers_data %>%
                            arrange(desc(Value)) %>% 
                            pull(NUTS_Code) %>% 
                            unique())
    )
  
  # Gerar cores suficientes usando a paleta Paired
  n_colors <- length(unique(outliers_data$NUTS_Color))
  paired_colors <- colorRampPalette(brewer.pal(12, "Paired"))(n_colors)
  
  # Criar o boxplot
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
      title = paste0(str_wrap(title, width = 50), " (pós-imputação 2ª fase)"),
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
  
  print(boxplot)
}


##############  CRIAÇÃO E LIMPEZA DAS VARIÁVEIS  ##############

# Extração da coluna 'variable_name' como a lista de nomes de variáveis
variable_names <- dataset_names$variable_name
valid_nuts_codes <- nuts_codes$`NUTS Code`
print(valid_nuts_codes)
# Leitura e atribuição de cada ficheiro .tsv à sua variável correspondente
for (var_name in variable_names) {
  file_path <- file.path(directory, paste0(var_name, ".tsv"))
  
  if (file.exists(file_path)) {
    df <- read.delim(file_path, header = TRUE, sep = "\t")
    
    # Remoção das colunas X2010 e X2011 se existirem
    df <- df[, !names(df) %in% c("X2010", "X2011")]
    
    # Criação da coluna com o NUTS Code
    df$NUTS_Code <- extract_nuts_code(df[[1]])
    df <- df[, c(1, ncol(df), 2:(ncol(df)-1))]
    
    # Limpeza dos labels
    df[, -c(1,2)] <- lapply(df[, -c(1,2)], clean_value)
    
    # Remoção da primeira coluna
    df <- df[, -1]
    
    # Filtragem das linhas com NUTS Codes válidos
    df <- df %>% filter(NUTS_Code %in% valid_nuts_codes)
    
    # Atribuição dinâmica do dataframe modificado a uma variável
    assign(var_name, df)
    message(paste("Carregado, processado, e filtrado:", var_name))
  } else {
    message(paste("Ficheiro não encontrado:", file_path))
  }
  rm(df)
}


##############  CONTAGEM DA % DE MISSINGS, PARA TODOS OS DATASETS  ##############

#Criação de lista para place holder
year_missing_counts <- list()

#Contagem da quantidade de datasets
total_datasets <- 0

for (var_name in variable_names) {
  if (exists(var_name)) {
    df <- get(var_name)
    
    #Encontrar as colunas que representam os anos
    year_columns <- grep("^X[0-9]{4}$", names(df), value = TRUE)
    
    # For loop para os anos de cada dataset (e.g., X2019, X2020)
    for (year_col in year_columns) {
      # Contagem dos ":" e 0 (CONTAGEM DE VALORES QUE COMEÇAM COM ":" E SAO 0)
      missing_count <- sum(df[[year_col]] == ":", na.rm = TRUE)
      print(missing_count)
      total_missing <- missing_count
      
      #se a coluna de ano ainda não estiver na variável year missing counts então adiciona e atribui-lhe o número de total missings
      if (!year_col %in% names(year_missing_counts)) {
        year_missing_counts[[year_col]] <- total_missing
      } else { #se já estiver adiciona os novos missing counts
        year_missing_counts[[year_col]] <- year_missing_counts[[year_col]] + total_missing
      }
    }
    total_datasets <- total_datasets + 1
    message(paste("Processed dataset:", var_name))
  }
}

# Calcular a média 
mean_missing_percentages <- sapply(year_missing_counts, function(missing_count) {
  (missing_count / (242 * total_datasets)) * 100
})

#criação de uma dataframe com a demonstração da percentagem de missings
mean_missing_summary_df <- data.frame(
  Year = names(mean_missing_percentages),
  MeanMissingPercentage = mean_missing_percentages
)
View(mean_missing_summary_df)


##############  CRIAÇÃO DO DATASET FINAL, CONSIDERANDO APENAS O ANO 2019  ##############

#Renomear NUTS Code para NUTS_Code
colnames(nuts_codes)[colnames(nuts_codes) == "NUTS Code"] <- "NUTS_Code"

datasets_list <- list()

for (var_name in variable_names) {
  if (exists(var_name)) {
    df <- get(var_name)
    
    #Se o dataset tem a coluna X2019 entra no loop
    if ("X2019" %in% names(df)) {
      #Extrair a coluna 2019 e a coluna nuts
      df_2019 <- df %>%
        select(NUTS_Code, X2019) %>%
        rename(!!var_name := X2019)  # Renomear 2019 para o nome do dataset
      
      # Fazer merge com a lista dos nuts
      df_2019 <- nuts_codes %>%
        select(NUTS_Code) %>%
        left_join(df_2019, by = "NUTS_Code")  # Left join para manter todos os códigos do nuts
      
      # Adicionar a dataframe à lista
      datasets_list[[var_name]] <- df_2019
    }
  }
} #repetir para cada dataset

#Combinar todos os datasets que estavam na lista, alinhando com o código NUTs
combined_df <- reduce(datasets_list, full_join, by = "NUTS_Code")
#View(combined_df)

#Contar número total de linhas para usar mais tarde
total_rows <- nrow(combined_df)

# Contar os valores missing (":", e NA) em cada coluna  (MUDAR AQUI TAMBEM)
missing_counts <- sapply(combined_df, function(column) {
  sum(column == ":" | is.na(column))
})

# Calcular a percentagem de valores missing para cada coluna
missing_percentages <- (missing_counts / total_rows) * 100

# Identificar colunas com percentagem de valores missing superior a 20%
columns_to_remove <- names(missing_percentages[missing_percentages > 20])

# Remover colunas com percentagem de valores missing superior a 20%
combined_df_cleaned <- combined_df[, !(names(combined_df) %in% columns_to_remove)]
#View(combined_df_cleaned)

# Limpeza dos labels 
combined_df_cleaned[, -1] <- lapply(combined_df_cleaned[, -1], clean_value)

# Limpar os ":" e transformar as colunas a partir da segunda em numérico
combined_df_cleaned <- combined_df_cleaned %>%
  mutate(across(2:ncol(combined_df_cleaned), ~as.numeric(na_if(., ":"))))


##########  EXPLORATORY DATA ANALYSIS  #############

# Verificar a normalidade das variáveis antes da imputação
normality_results <- sapply(combined_df_cleaned, test_normality)

# Identificar colunas não normais (p-valor < 0.05)
non_normal_cols <- names(normality_results[normality_results < 0.05])

# Obter colunas numéricas a partir da tabela combined_df_cleaned
numeric_cols <- names(combined_df_cleaned)[sapply(combined_df_cleaned, is.numeric)]

# Criar Q-Q plots de 4 em 4 colunas
for (i in seq(1, length(numeric_cols), 4)) {
  cols_to_plot <- numeric_cols[i:min(i + 3, length(numeric_cols))]
  p_values <- normality_results[cols_to_plot]
  print(create_qq_plots(combined_df_cleaned, cols_to_plot, p_values))
}


##################   1ª fase: IMPUTAÇÃO DOS VALORES MISSING EM CADA DATASET  ##################

# Loop através das colunas do conjunto de dados de teste (excluindo a primeira coluna)
for (col_name in names(combined_df_cleaned)[-1]) {
  # Get the corresponding dataset
  eurostat_data <- get(col_name)
  
  # Excluir as colunas após X2019
  eurostat_data <- eurostat_data[, 1:which(names(eurostat_data) == "X2019")]
  
  # Transformar os valores ":" para NA, e converter as colunas a numéricas
  eurostat_data <- eurostat_data %>%
    mutate(across(-1, ~if(is.numeric(.)) . else as.numeric(na_if(., ":"))))
  
  # Variável auxiliar: definir as colunas a estudar no boxplot
  year_columns <- c("X2014", "X2015", "X2016", "X2017", "X2018", "X2019")
  
  # Mostrar o boxplot para os anos selecionados (pré-imputação)
  create_boxplot_imp(eurostat_data, year_columns, col_name, dataset_names, "(pré-imputação 1ª fase)")
  
  # Cálculo auxiliae da variância original antes da imputação
  original_variance <- sapply(eurostat_data[, year_columns], var, na.rm = TRUE)
  
  # Verificar o padrão de NAs (excluindo a 1ª coluna dos geo codes)
  print(md.pattern(eurostat_data[, 2:ncol(eurostat_data)], rotate.names = TRUE))
  grid.text(paste("Padrão de Dados Ausentes -", col_name, "(pré-imputação 1ª fase)"),
            x = 0.5, y = unit(0.96, "npc"), gp = gpar(fontsize = 19, fontface = "bold"))
  
  # Criar uma matriz preditora, e realizar a imputação
  predictor_matrix <- matrix(0, ncol = ncol(eurostat_data) - 1, nrow = ncol(eurostat_data) - 1)
  predictor_matrix[ncol(predictor_matrix), 1:(ncol(predictor_matrix)-1)] <- 1
  imputed_data <- mice(eurostat_data[, -1], m = 5, method = 'pmm', seed = 123,
                       predictorMatrix = predictor_matrix)
  
  # Plot para avaliar a qualidade das imputações
  print(stripplot(imputed_data, pch = 20,
                  main = paste("Estudo da Qualidade das Imputações -", col_name, "(1ª fase)"),
                  col = c("blue", "red"),
                  cex = 1.2,
                  layout = c(5, 1),
                  subset = .imp > 0 & .imp <= 5))
  
  # Substituir apenas a coluna X2019 no dataset original
  eurostat_data$X2019 <- complete(imputed_data)$X2019
  
  # Calcular a variância após a imputação
  imputed_variance <- sapply(eurostat_data[, year_columns], var, na.rm = TRUE)
  
  # Comparar variâncias (original vs imputada)
  var_comparison <- data.frame(Year = year_columns, Original = original_variance, Imputed = imputed_variance,
    Diff_Percent = (imputed_variance - original_variance) / original_variance * 100)
  diff_percent_x2019 <- var_comparison$Diff_Percent[var_comparison$Year == "X2019"]
  cat("\nDiferença Percentual para a variável", col_name, "para o ano 2019:", round(diff_percent_x2019, 2), "%\n")
  
  # Criar o gráfico para ver  a diferença percentual de variâncias para o col_name
  print(
    ggplot(var_comparison, aes(x = Year, y = Diff_Percent)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      theme_minimal(base_size = 15) +
      labs(
        title = paste("Variação da Variância -", col_name, "(pós-imputação 1ª fase)"),
        x = "Anos",
        y = "Diferença Percentual (%)"
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1.2) +
      theme(
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5),  # Centralizar o título
        axis.text.x = element_text(size = 13),  # Tamanho dos rótulos do eixo x
        axis.text.y = element_text(size = 11),  # Tamanho dos rótulos do eixo y
        axis.title.x = element_text(size = 21),  # Tamanho do título do eixo x
        axis.title.y = element_text(size = 21),  # Tamanho do título do eixo y
        panel.border = element_rect(color = "black", fill = NA, size = 1),  # Adicionar um quadro ao redor
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank() 
      ) +
      scale_y_continuous(
        labels = scales::percent_format(scale = 1), 
        breaks = scales::pretty_breaks(n = 10)
      ) +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
  )
  
  # Verificar o padrão de NAs após imputação (excluindo a 1ª coluna dos geo codes)
  print(md.pattern(eurostat_data[, 2:ncol(eurostat_data)], rotate.names = TRUE))
  grid.text(paste("Padrão de Dados Ausentes -", col_name, "(pós-imputação 1ª fase)"),
            x = 0.5, y = unit(0.96, "npc"), gp = gpar(fontsize = 19, fontface = "bold"))
  
  # Realizar o join com combined_df_cleaned usando NUTS_Code
  combined_df_cleaned <- combined_df_cleaned %>%
    left_join(select(eurostat_data, NUTS_Code, X2019), by = "NUTS_Code") %>%
    mutate(!!sym(col_name) := coalesce(X2019, !!sym(col_name))) %>%
    select(-X2019)
  
  # Atribuir os dados atualizados de volta à variável original
  assign(col_name, eurostat_data)
  
  # Mostrar o boxplot para os anos selecionados (pós-imputação)
  create_boxplot_imp(eurostat_data, year_columns, col_name, dataset_names, "(pós-imputação 1ª fase)")
  print(paste("Imputação concluída para X2019 em", col_name))
}


##################   2ª fase: IMPUTAÇÃO DOS VALORES MISSING NA TABELA FINAL ##################

# Imprimir o padrão de dados ausentes (pré-imputação)
print(md.pattern(combined_df_cleaned[, -1], rotate.names = TRUE))
grid.text(paste("Padrão de Dados Ausentes para o ano 2019 (pré-imputação 2ª fase)"),
          x = 0.5, y = unit(0.95, "npc"), gp = gpar(fontsize = 15.5, fontface = "bold"))

# Supondo que a primeira coluna seja um identificador (por exemplo, NUTS_Code)
data_for_imputation <- combined_df_cleaned

# Especificar as colunas preditoras (todas exceto a primeira)
predictor_cols <- names(data_for_imputation)[-1]

# Realizar a imputação CART usando o mice
imputed_data <- mice(data_for_imputation[, predictor_cols], m = 5, method = 'rf', seed = 123)

# Completar o conjunto de dados imputado
completed_data <- complete(imputed_data)

# Combinar os dados completos com a coluna NUTS_Code
final_combined_df <- data_for_imputation %>%
  select(NUTS_Code) %>%
  bind_cols(completed_data)

# Imprimir o padrão de dados ausentes (pós-imputação)
print(md.pattern(final_combined_df[, -1], rotate.names = TRUE))
grid.text(paste("Padrão de Dados Ausentes para o ano 2019 (pós-imputação 2ª fase)"),
          x = 0.5, y = unit(0.95, "npc"), gp = gpar(fontsize = 15.5, fontface = "bold"))

# Imprimir o resumo do conjunto de dados completo
print(summary(final_combined_df))

# Opcionalmente, pode atribuir o conjunto de dados final de volta a combined_df_cleaned
combined_df_cleaned <- final_combined_df

# Colunas que tiveram imputações na 2ª fase:
column_names <- c("Cult_Emp", "NEET_M", "NEET_F", "Y_EmpRate_M", "Y_EmpRate_F", "Y_EmpRate_Age")

# Mostrar o boxplot para os anos selecionados (pós-imputação)
for (col_name in column_names) {
  create_boxplot_2019(combined_df_cleaned, col_name, dataset_names)
}

# Exportação do ficheiro TSV final
write.table(combined_df_cleaned, file = "dataset_final.tsv", sep = "\t", row.names = FALSE, quote = FALSE)


###########  ESTUDO DA QUALIDADE DA IMPUTAÇÃO  ###########    
stripplot(imputed_data, pch = 20, main = "Qualidade das Imputações - 2ª fase", 
          col = c("blue", "red"), 
          cex = 1.2, 
          layout = c(5, 1),
          subset = .imp > 0 & .imp <= 5)

# Calcular e comparar as variâncias
original_variance <- sapply(data_for_imputation[, -1], var, na.rm = TRUE)
imputed_variance <- sapply(final_combined_df[, -1], var)
var_comparison <- data.frame(Original = original_variance, Imputed = imputed_variance)

# Calcular a diferença percentual
var_comparison$diff_percent <- (var_comparison$Imputed - var_comparison$Original) / var_comparison$Original * 100
var_comparison_sorted <- var_comparison[order(var_comparison$diff_percent), ]

# Criar o gráfico para mostrar a diferença percentual das variáveis
ggplot(var_comparison_sorted, aes(x = reorder(rownames(var_comparison_sorted), diff_percent), y = diff_percent)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  coord_flip() +  
  theme_minimal(base_size = 15) + 
  labs(
    title = "Variação das Variâncias (pós-imputação 2ª fase)",
    x = "Variáveis",
    y = "Diferença (%)"
  ) +
  theme(
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5),  # Centralizar o título
    axis.text.y = element_text(size = 11),  # Tamanho dos rótulos do eixo y
    axis.text.x = element_text(size = 13),  # Tamanho dos rótulos do eixo x
    axis.title.x = element_text(size = 21),  # Tamanho do título do eixo x
    axis.title.y = element_text(size = 21),  # Tamanho do título do eixo y
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Adicionar um quadro ao redor
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()  
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1.2) + 
  scale_y_continuous(
    labels = scales::percent_format(scale = 1), 
    breaks = scales::pretty_breaks(n = 10) 
  ) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))


########## PCA ##########

dataset <- read_tsv("dataset_final.tsv")

### Visualização do dataset ###
str(dataset)
head(dataset)

### Adequação do PCA ###
# Visualização da dispersão e dos outliers
pairs(dataset[,2:14], pch = 19, lower.panel = NULL)
pairs(dataset[,15:27], pch = 19, lower.panel = NULL)
pairs(dataset[,28:40], pch = 19, lower.panel = NULL)
pairs(dataset[,41:52], pch = 19, lower.panel = NULL)

# Visualização da correlação
correlation <- cor(dataset[,2:52])
par(oma = c(2, 2, 2, 2)) # space around for text
corrplot.mixed(correlation,
               order = "hclust", #order of variables
               tl.pos = "lt", #text left + top
               upper = "ellipse",
               lower = "ellipse")

# Tirar o limite de visualização para demonstrar as componentes todas
options(max.print = 1e6)

# Matriz de correlação
round(correlation, 3)

# Teste Bartlett e KMO
cortest.bartlett(correlation)
KMO(correlation)

### Número do PCs ###
# Normalização dos dados
data_scaled <- scale(dataset[,2:52])

# Extração
pc51 <- principal(data_scaled, nfactors=51, rotate="none", scores=TRUE) #Sempre sem rotation
# Visualizar as métricas das componentes
pc51

# Critério Kaiser
round(pc51$values,3)

#Scree plot
plot(pc51$values, type = "b", main = "Scree plot for Employment dataset", xlab = "Number of PC", ylab = "Eigenvalue")

#Verificação dos pesos das variáveis
pc51$loadings

# Verificação de comunalidade das variáveis retidas
pc51$communality

# Extração de uma solução com 4 componentes
pc4 <- principal(data_scaled, nfactors=4, rotate="none")
pc4

# Extração de uma solução com 5 componentes
pc5 <- principal(data_scaled, nfactors=5, rotate="none")
pc5

### Interpretação e rotação ###
## Rotação ##
# Rotação das 4 componentes usando varimax
#pc4r <- principal(data_scaled, nfactors=4, rotate="varimax")
#pc4r$loadings

# Verificação das comunalidades
#round(pc4r$communality,2)

# Rotação das 5 componentes usando varimax
pc5r <- principal(data_scaled, nfactors=5, rotate="varimax")
pc5r$loadings

# Verificação das comunalidades
round(pc5r$communality,2)

## Interpretação ##
# Avaliação do melhor PC
pc5sc <- principal(data_scaled, nfactors=5, rotate="none", scores=TRUE)
round(pc5sc$scores,3)

# Cálculo da média
mean(pc5sc$scores[,1])

# Cálculo do desvio padrão
sd(pc5sc$scores[,1])

# Normalização das avaliações das componentes principais
normalize_pca_scores <- function(pca_object) {
  # Obter os scores originais
  scores <- pca_object$scores
  
  # Obter os valores próprios (variância explicada por cada componente)
  eigenvalues <- pca_object$values[1:ncol(scores)]
  
  # Normalizar os scores dividindo-os pela raiz quadrada dos valores próprios correspondentes
  normalized_scores <- sweep(scores, 2, sqrt(eigenvalues), FUN = "/")
  
  return(normalized_scores)
}

# Aplicar a função ao modelo PCA com 5 componentes (exemplo)
normalized_scores <- normalize_pca_scores(pc5sc)

# Verificar os novos scores normalizados
head(normalized_scores)

### Avaliação das componentes ###
# Adicionar os scores normalizados ao dataset original
dataset$Dimensão_Laboral <- normalized_scores[,1]
dataset$Faces_do_Mercado_de_Trabalho <- normalized_scores[,2]
dataset$Emprego_Industrial <- normalized_scores[,3]
dataset$Inovação_vs_Tradição <- normalized_scores[,4]
dataset$Raízes_do_Desemprego <- normalized_scores[,5]

#Guardar a base de dados num excel
write.xlsx(dataset, file = "Employment_EuroStat.xlsx",
           sheetName = "PCAscores", colNames = TRUE,
           rowNames = TRUE, append = FALSE)

# Scatterplot de PC1 vs PC2
plot(dataset$Dimensão_Laboral, dataset$Faces_do_Mercado_de_Trabalho, pch = 19,
     xlim = c(-2, 3), ylim = c(-3, 2), 
     xlab = "Dimensão Laboral", ylab = "Faces do Mercado de Trabalho", 
     main = "Scores: PC1 vs PC2 (Normalized)")
text(dataset$Dimensão_Laboral, dataset$Faces_do_Mercado_de_Trabalho - 0.1, dataset[,2]) #(x,y,labels)


################### Clustering #########################

nutscode <- dataset$NUTS_Code 
geodata <- get_eurostat_geospatial(nuts_level = 2, year = 2016)
set.seed(123)
### Hierarchical Clustering
rownames(dataset) <- nutscode
dist_matrix <- dist(dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")])
hclust_result <- hclust(dist_matrix, method = "ward.D2")
k <- 3
plot(hclust_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "", labels = rownames(dataset), cex.lab = 0.1, hang = -3)
rect.hclust(hclust_result, k = k, border = "red")

##Versão que exporta o gráfico para melhor visualização
#png("dendrograma_ajustado.png", width = 2500, height = 1000)
#plot(hclust_result, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "", labels = rownames(dataset), cex.lab = 0.5, hang = -1)
#rect.hclust(hclust_result, k = k, border = "red")
#dev.off()

# Adicionando Clusters
clusters_hierarchical <- cutree(hclust_result, k)
dataset$cluster_hierarchical <- clusters_hierarchical

# Silhouette Hierarchical
silhouette_scores <- silhouette(clusters_hierarchical, dist_matrix)
plot(silhouette_scores, main = paste("Silhouette Plot for Hierarchical"))
# Profiles
cluster_profiles <- aggregate(dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")],
                              by = list(cluster = clusters_hierarchical), mean)
print(cluster_profiles)


### Elbow ###
wssplot <- function(data, nc = 10, seed = 123) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$tot.withinss)
  }
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-group Sum of Squares")
}
wssplot(dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")], nc = 15)

### K-Means Clustering ###

# 4 Clusters
kmeans_result4 <- kmeans(dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")], centers = 4, nstart = 25)
dataset$cluster_kmeans4 <- kmeans_result4$cluster
resultskmeaens4 <- data.frame(
  nutscode = nutscode,  
  cluster_kmeans4 = dataset$cluster_kmeans4
)
clusters_split4 <- split(resultskmeaens4$nutscode, resultskmeaens4$cluster_kmeans4)
clusters_split4
#Visualização
# plot_ly(
#   data = dataset, 
#   x = ~pc1_normalized, 
#   y = ~pc2_normalized, 
#   z = ~pc3_normalized, 
#   color = as.factor(dataset$cluster_kmeans),
#   colors = c("red", "blue", "green", "orange"),
#   type = "scatter3d", 
#   mode = "markers+text",
#   marker = list(size = 5, opacity = 0.8),
#   text = ~nutscode,
#   textposition = "top center"
# ) %>%
#   layout(
#     title = "3D Visualization of K-Means Clusters",
#     scene = list(
#       xaxis = list(title = "PC1"),
#       yaxis = list(title = "PC2"),
#       zaxis = list(title = "PC3")
#     )
#   )

#Silhouette K-Means 4
silhouette_scores_kmeans4 <- silhouette(kmeans_result4$cluster, dist_matrix)
plot(silhouette_scores_kmeans4, main = "Silhouette Plot for K-Means Clustering 4 Clusters")

#Mapa K-Means 4
map_data <- geodata %>%
  inner_join(dataset, by = c("geo" = "NUTS_Code"))  


custom_palette <- c("#1b9e77", "#d95f02", "#7570b3", "#fdae55")  

map_data$cluster_kmeans4 <- factor(map_data$cluster_kmeans4,
                                   levels = c(1, 2, 3, 4),  
                                   labels = c("Emergência Oriental e de Portugal",
                                              "Trabalho árduo", 
                                              "Centros Urbanos e Capitais Económicas",
                                              "Europa tradicional"))


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
              legend.labels = c("Trabalho árduo", " Europa tradicional", "Emergência Oriental e de Portugal", "Centros Urbanos e Capitais Económicas")  # Custom cluster names
  ) +
  tm_layout(legend.outside = TRUE)
print(mapk4)


### PAM ###

pam_result <- pam(dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")], k = 4, metric = "euclidean")
dataset$cluster_pam <- pam_result$clustering
clusplot(pam_result, labels = 4, main = "PAM Clustering of PCA Components")
resultspam <- data.frame(
  nutscode = nutscode,  
  cluster_pam = dataset$cluster_pam
)
clusters_splitpam <- split(resultspam$nutscode, resultspam$cluster_pam)
clusters_splitpam


#Silhouette PAM 4
silhouette_scores_pam <- silhouette(resultspam$cluster, dist_matrix)
plot(silhouette_scores_pam, main = "Silhouette Plot for PAM Clustering")

#Mapa PAM 4
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


### Gaussian Mixture Model ###
pca_data <- dataset[, c("Dimensão_Laboral", "Faces_do_Mercado_de_Trabalho", "Emprego_Industrial", "Inovação_vs_Tradição", "Raízes_do_Desemprego")]
gmm_result <- Mclust(pca_data)
summary(gmm_result)

plot(gmm_result, what = "BIC") #Testar diferentes N de Clusters
dataset$cluster_gmm <- gmm_result$classification

dist_matrix <- dist(pca_data)
silhouette_scores <- silhouette(dataset$cluster_gmm, dist_matrix)
plot(silhouette_scores, main = "Silhouette Plot for GMM Clustering")

### Profiling ###

# Países
dataset$Country <- substr(nutscode, 1, 2)
head(dataset$Country)
unique(dataset$Country)
all(substr(nutscode, 1, 2) == dataset$Country)

country_summary <- dataset %>%
  group_by(Country, cluster_kmeans4) %>%
  summarise(Regions = n()) %>%
  group_by(Country) %>%
  mutate(Percentage = (Regions / sum(Regions)) * 100)

# GDP 
gdp_data <- read.csv("nama_10r_2gdp$defaultview_page_spreadsheet.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
str(gdp_data)
gdp_data <- gdp_data %>%
  select(NUTS_Codes, GDP)
dataset <- dataset %>%
  left_join(gdp_data, by = c("NUTS_Code" = "NUTS_Codes"))


# Urbanization Percentage
urbanization_data <- read.csv("lfst_r_lfsd2hh$defaultview_page_spreadsheet.csv", 
                              header = TRUE, 
                              sep = ";", 
                              stringsAsFactors = FALSE)
urbanization_data <- urbanization_data %>%
  select(NUTS_Code, Urbanisation)
dataset <- dataset %>%
  left_join(urbanization_data, by = c("NUTS_Code" = "NUTS_Code"))


# Capitais
capital_regions <- c("AT13", "BE10", "DE30", "DK01", "ES30", "EL30", "FR10", 
                     "FI1B", "ITI4", "IE06", "LU00", "NL32", "PT17", "SE11", 
                     "BG41", "CY00", "CZ01", "EE00", "HU11", "LT01", "LV00", 
                     "MT00", "PL91", "RO32", "SI04", "SK01")

dataset$Is_Capital <- ifelse(dataset$NUTS_Code %in% capital_regions, 1, 0)


# Análises com variáveis de Profile
#GDP
dataset$GDP <- as.numeric(gsub(",", ".", gsub("\\.", "", dataset$GDP)))

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

gdp_stats$cluster_kmeans4 <- factor(gdp_stats$cluster_kmeans4,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("Trabalho árduo",
                                               "Europa tradicional",
                                               "Emergência Oriental e de Portugal",
                                               "Centros Urbanos e Capitais Económicas"))


gdp_long <- gdp_stats %>%
  pivot_longer(cols = c(Min_GDP, Max_GDP, Mean_GDP, Median_GDP),
               names_to = "Metric", values_to = "Value")


ggplot(gdp_long, aes(x = cluster_kmeans4, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set2", labels = c("GDP Máximo", "GDP Médio", "GDP Mediana", "GDP Mínimo")) +
  theme_minimal() +
  labs(title = "Comparação de Métricas de GDP por Cluster",
       x = "Clusters",
       y = "GDP (em unidades monetárias)",
       fill = "Métricas de GDP") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(face = "bold", size = 14))


#Urbanization
dataset$Urbanisation <- as.numeric(gsub(",", ".", gsub("\\.", "", dataset$Urbanisation)))

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



urbanization_stats$cluster_kmeans4 <- factor(urbanization_stats$cluster_kmeans4,
                                             levels = c(1, 2, 3, 4),
                                             labels = c("Trabalho árduo",
                                                        "Europa tradicional",
                                                        "Emergência Oriental e de Portugal",
                                                        "Centros Urbanos e Capitais Económicas"))


urbanization_long <- urbanization_stats %>%
  pivot_longer(cols = c(Min_Urbanization, Max_Urbanization, Mean_Urbanization, Median_Urbanization),
               names_to = "Metric", values_to = "Value")


ggplot(urbanization_long, aes(x = cluster_kmeans4, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set2", labels = c("Urbanização Máxima", "Urbanização Média", "Urbanização Mediana", "Urbanização Mínima")) +
  theme_minimal() +
  labs(title = "Comparação de Grau de Urbanização por Cluster",
       x = "Clusters",
       y = "Grau de Urbanização",
       fill = "Métricas de Urbanização") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(face = "bold", size = 14))




cluster_names <- c("Trabalho árduo", 
                   "Europa tradicional", 
                   "Emergência Oriental e de Portugal", 
                   "Centros Urbanos e Capitais Económicas")


country_summary$cluster_kmeans4 <- factor(country_summary$cluster_kmeans4,
                                          levels = c(1, 2, 3, 4),
                                          labels = cluster_names)

print(unique(country_summary$cluster_kmeans4))       
labels = cluster_names
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

# Capitais em Clusters

missing_capitals <- setdiff(capital_regions, dataset$NUTS_Code)
cat("Capital regions missing from dataset:\n", missing_capitals, "\n")
dataset$Is_Capital <- ifelse(dataset$NUTS_Code %in% capital_regions, 1, 0)
table(dataset$Is_Capital)

#Lista de Capitais
capital_counts <- dataset %>%
  filter(Is_Capital == 1) %>%
  group_by(cluster_kmeans4) %>%
  summarize(Total_Capitals = n())

print(capital_counts)

# Capitais por cluster
capitals_by_cluster <- dataset %>%
  filter(Is_Capital == 1) %>%
  group_by(cluster_kmeans4) %>%
  summarize(Capitals = list(NUTS_Code))  
for (i in seq_len(nrow(capitals_by_cluster))) {
  cat("\nCluster:", capitals_by_cluster$cluster_kmeans4[i], "\n")
  cat("Capitals:", paste(capitals_by_cluster$Capitals[[i]], collapse = ", "), "\n")
}


capitals_summary <- dataset %>%
  filter(Is_Capital == 1) %>%
  group_by(cluster_kmeans4) %>%
  summarize(
    Num_Capitals = n(),
    Capitals = paste(unique(substr(NUTS_Code, 1, 2)), collapse = "\n")  # Coloca um país por linha
  )

capitals_summary$cluster_kmeans4 <- factor(capitals_summary$cluster_kmeans4,
                                           levels = c(1, 2, 3, 4),
                                           labels = c("Trabalho árduo", 
                                                      "Europa tradicional", 
                                                      "Emergência Oriental e de Portugal", 
                                                      "Centros Urbanos e Capitais Económicas"))

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


