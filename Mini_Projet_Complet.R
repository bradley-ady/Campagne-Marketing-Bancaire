# ============================================================
# Bradley Belizaire - B3 DATA ET IA
# Projet : Analyse exploratoire - Campagne Marketing Bancaire
# ============================================================

# ── 0. PACKAGES
packages <- c("tidyverse", "ggplot2", "corrplot", "class",
              "caret", "shiny", "shinydashboard", "DT",
              "ggcorrplot", "gridExtra")
installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(class)
library(caret)
library(shiny)
library(shinydashboard)
library(DT)
library(gridExtra)

data <- read.csv(
  "C:/Users/Bradley/OneDrive/YNOV/Analyse de donnée/bank-additional-full.csv",
  sep = ";",
  stringsAsFactors = TRUE
)

cat("=== DIMENSIONS ===\n")
dim(data)
str(data)
head(data)


# -- 2.1 Valeurs manquantes 
cat("\n=== VALEURS MANQUANTES ===\n")
print(colSums(is.na(data)))

# Les "unknown" sont encodés en facteur — on les conserve
# mais on les identifie
for (col in names(data)) {
  if (is.factor(data[[col]])) {
    n_unk <- sum(data[[col]] == "unknown", na.rm = TRUE)
    if (n_unk > 0) cat(sprintf("  %s : %d 'unknown'\n", col, n_unk))
  }
}

# -- 2.2 Détection et traitement des outliers (IQR) ----------
cat("\n=== OUTLIERS (méthode IQR) ===\n")
num_vars <- c("age", "duration", "campaign", "pdays",
              "previous", "emp.var.rate", "cons.price.idx",
              "cons.conf.idx", "euribor3m", "nr.employed")

detect_outliers_iqr <- function(x) {
  Q1  <- quantile(x, 0.25, na.rm = TRUE)
  Q3  <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  sum(x < (Q1 - 1.5 * IQR_val) | x > (Q3 + 1.5 * IQR_val), na.rm = TRUE)
}

for (v in num_vars) {
  n_out <- detect_outliers_iqr(data[[v]])
  pct   <- round(100 * n_out / nrow(data), 2)
  cat(sprintf("  %-20s : %d outliers (%.1f%%)\n", v, n_out, pct))
}

# Winsorisation de 'duration' (variable très asymétrique)
q99_dur       <- quantile(data$duration, 0.99)
data$duration <- pmin(data$duration, q99_dur)
cat(sprintf("\nduration winsorisée au 99e percentile (%.0f s)\n", q99_dur))


cat("\n=== STATISTIQUES DESCRIPTIVES (variables numériques) ===\n")
desc_stats <- data %>%
  select(all_of(num_vars)) %>%
  summarise(across(
    everything(),
    list(
      moyenne  = ~round(mean(.x, na.rm = TRUE), 2),
      mediane  = ~round(median(.x, na.rm = TRUE), 2),
      ecart_type = ~round(sd(.x, na.rm = TRUE), 2),
      min      = ~round(min(.x, na.rm = TRUE), 2),
      max      = ~round(max(.x, na.rm = TRUE), 2)
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

print(desc_stats)

# Variable cible
cat("\n=== DISTRIBUTION DE LA VARIABLE CIBLE (y) ===\n")
print(table(data$y))
print(round(prop.table(table(data$y)) * 100, 1))

# -- Histogrammes variables numériques
plots_hist <- lapply(num_vars, function(v) {
  ggplot(data, aes_string(x = v, fill = "y")) +
    geom_histogram(position = "identity", alpha = 0.55, bins = 30) +
    scale_fill_manual(values = c("no" = "#E74C3C", "yes" = "#2ECC71")) +
    labs(title = paste("Distribution :", v), x = v, y = "Effectif") +
    theme_minimal(base_size = 9)
})
# Affichage en grille (4 colonnes)
gridExtra::grid.arrange(grobs = plots_hist, ncol = 3)

# -- Boxplots 
plots_box <- lapply(num_vars, function(v) {
  ggplot(data, aes_string(x = "y", y = v, fill = "y")) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c("no" = "#E74C3C", "yes" = "#2ECC71")) +
    labs(title = paste("Boxplot :", v)) +
    theme_minimal(base_size = 9) +
    theme(legend.position = "none")
})
gridExtra::grid.arrange(grobs = plots_box, ncol = 3)

# -- Distribution variables catégoriques 
cat_vars <- c("job", "marital", "education", "default",
              "housing", "loan", "contact", "month",
              "day_of_week", "poutcome")

plots_cat <- lapply(cat_vars, function(v) {
  ggplot(data, aes_string(x = v, fill = "y")) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("no" = "#E74C3C", "yes" = "#2ECC71")) +
    labs(title = paste("Taux de souscription par", v),
         y = "Proportion") +
    theme_minimal(base_size = 8) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})
gridExtra::grid.arrange(grobs = plots_cat[1:5], ncol = 2)
gridExtra::grid.arrange(grobs = plots_cat[6:10], ncol = 2)


cat("\n=== CORRÉLATION DE PEARSON ===\n")
mat_cor <- cor(data[, num_vars], use = "complete.obs", method = "pearson")
print(round(mat_cor, 2))

ggcorrplot(mat_cor,
           method = "circle",
           type   = "lower",
           lab    = TRUE,
           lab_size = 2.5,
           colors = c("#E74C3C", "white", "#2ECC71"),
           title  = "Matrice de corrélation (Pearson)",
           ggtheme = theme_minimal())


ggplot(data, aes(x = duration, y = euribor3m, color = y)) +
  geom_point(alpha = 0.3, size = 0.8) +
  scale_color_manual(values = c("no" = "#E74C3C", "yes" = "#2ECC71")) +
  labs(title = "Duration vs Euribor3m selon souscription") +
  theme_minimal()

ggplot(data, aes(x = age, y = duration, color = y)) +
  geom_point(alpha = 0.3, size = 0.8) +
  scale_color_manual(values = c("no" = "#E74C3C", "yes" = "#2ECC71")) +
  labs(title = "Âge vs Durée selon souscription") +
  theme_minimal()


# -- 5.1 TEST ANOVA : durée selon le type d'emploi 
cat("\n=== TEST ANOVA : duration ~ job ===\n")
anova_job <- aov(duration ~ job, data = data)
summary(anova_job)
# Interprétation : si p < 0.05, il existe une différence
# significative de durée d'appel selon le type d'emploi.

# ANOVA : durée selon souscription
cat("\n=== TEST ANOVA : duration ~ y ===\n")
anova_y <- aov(duration ~ y, data = data)
summary(anova_y)

# ANOVA : age selon souscription
cat("\n=== TEST ANOVA : age ~ y ===\n")
anova_age <- aov(age ~ y, data = data)
summary(anova_age)

# -- 5.2 TEST KHI² : variables catégoriques ~ y -------------
cat("\n=== TESTS KHI² ===\n")
chi2_vars <- c("job", "marital", "education", "contact",
               "poutcome", "housing", "loan", "default")

for (v in chi2_vars) {
  tbl    <- table(data[[v]], data$y)
  result <- chisq.test(tbl)
  cat(sprintf("  %-15s : X² = %7.2f, df = %d, p = %.4f %s\n",
              v,
              result$statistic,
              result$parameter,
              result$p.value,
              ifelse(result$p.value < 0.05, "***", "")))
}
# Interprétation : p < 0.05 → la variable est significativement
# associée à la souscription.

# Conversion variable cible
data$y <- factor(data$y, levels = c("no", "yes"))

set.seed(123)
index <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[index, ]
test  <- data[-index, ]

cat(sprintf("\nTrain : %d obs | Test : %d obs\n", nrow(train), nrow(test)))

# -- 6.1 RÉGRESSION LOGISTIQUE --------------------------------
cat("\n=== RÉGRESSION LOGISTIQUE ===\n")
model_glm <- glm(y ~ ., data = train, family = "binomial")
summary(model_glm)

pred_prob_glm <- predict(model_glm, test, type = "response")
pred_glm      <- ifelse(pred_prob_glm > 0.5, "yes", "no")
pred_glm      <- factor(pred_glm, levels = c("no", "yes"))

# Matrice de confusion complète
cm_glm <- confusionMatrix(pred_glm, test$y, positive = "yes")
cat("\n--- Métriques Régression Logistique ---\n")
cat(sprintf("  Accuracy  : %.4f\n", cm_glm$overall["Accuracy"]))
cat(sprintf("  Precision : %.4f\n", cm_glm$byClass["Precision"]))
cat(sprintf("  Recall    : %.4f\n", cm_glm$byClass["Recall"]))
cat(sprintf("  F1-Score  : %.4f\n", cm_glm$byClass["F1"]))
print(cm_glm$table)

# -- 6.2 KNN 
cat("\n=== KNN (k = 5) ===\n")
knn_feats <- c("age", "duration", "campaign", "pdays",
               "previous", "euribor3m", "emp.var.rate", "nr.employed")

normalize <- function(x) (x - min(x)) / (max(x) - min(x))

train_knn_x <- as.data.frame(lapply(train[, knn_feats], normalize))
test_knn_x  <- as.data.frame(lapply(test[,  knn_feats], normalize))

pred_knn <- knn(train = train_knn_x,
                test  = test_knn_x,
                cl    = train$y,
                k     = 5)

cm_knn <- confusionMatrix(pred_knn, test$y, positive = "yes")
cat("\n--- Métriques KNN ---\n")
cat(sprintf("  Accuracy  : %.4f\n", cm_knn$overall["Accuracy"]))
cat(sprintf("  Precision : %.4f\n", cm_knn$byClass["Precision"]))
cat(sprintf("  Recall    : %.4f\n", cm_knn$byClass["Recall"]))
cat(sprintf("  F1-Score  : %.4f\n", cm_knn$byClass["F1"]))
print(cm_knn$table)

# -- 6.3 COMPARAISON DES MODÈLES 
cat("\n=== COMPARAISON MODÈLES ===\n")
comp <- data.frame(
  Modele    = c("Regression Logistique", "KNN (k=5)"),
  Accuracy  = as.numeric(c(cm_glm$overall["Accuracy"],  cm_knn$overall["Accuracy"])),
  Precision = as.numeric(c(cm_glm$byClass["Precision"], cm_knn$byClass["Precision"])),
  Recall    = as.numeric(c(cm_glm$byClass["Recall"],    cm_knn$byClass["Recall"])),
  F1        = as.numeric(c(cm_glm$byClass["F1"],        cm_knn$byClass["F1"]))
)
comp[, -1] <- round(comp[, -1], 4)
print(comp)

# -- Visualisation matrices de confusion 
plot_confusion <- function(cm, title) {
  as.data.frame(cm$table) %>%
    ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), size = 5, fontface = "bold") +
    scale_fill_gradient(low = "white", high = "#3498DB") +
    labs(title = title, x = "Réel", y = "Prédit") +
    theme_minimal()
}

p1 <- plot_confusion(cm_glm, "Matrice - Régression Logistique")
p2 <- plot_confusion(cm_knn,  "Matrice - KNN (k=5)")
gridExtra::grid.arrange(p1, p2, ncol = 2)


ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Marketing Bancaire"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Données",       tabName = "data_tab",   icon = icon("table")),
      menuItem("Exploration",   tabName = "explore_tab", icon = icon("chart-bar")),
      menuItem("Statistiques",  tabName = "stats_tab",  icon = icon("calculator")),
      menuItem("Modèles",       tabName = "model_tab",  icon = icon("robot")),
      menuItem("Simulation",    tabName = "sim_tab",    icon = icon("sliders-h"))
    )
  ),

  dashboardBody(
    tabItems(

      # ── Onglet Données 
      tabItem(tabName = "data_tab",
        fluidRow(
          valueBox(nrow(data),   "Clients",           icon = icon("users"),       color = "blue"),
          valueBox(ncol(data)-1, "Variables",         icon = icon("columns"),     color = "green"),
          valueBox(
            paste0(round(mean(data$y == "yes") * 100, 1), "%"),
            "Taux de souscription",
            icon  = icon("percent"),
            color = "yellow"
          )
        ),
        fluidRow(
          box(title = "Aperçu des données", width = 12,
              DTOutput("table_preview"))
        )
      ),

      # ── Onglet Exploration 
      tabItem(tabName = "explore_tab",
        fluidRow(
          box(title = "Variable à explorer", width = 3,
              selectInput("var_explore", "Variable numérique :",
                          choices = num_vars, selected = "age"),
              selectInput("plot_type", "Type de graphique :",
                          choices = c("Histogramme", "Boxplot", "Densité"))
          ),
          box(title = "Graphique", width = 9,
              plotOutput("plot_explore", height = "400px"))
        ),
        fluidRow(
          box(title = "Taux de souscription par variable catégorielle", width = 6,
              selectInput("var_cat", "Variable catégorielle :",
                          choices = cat_vars, selected = "job"),
              plotOutput("plot_cat", height = "350px")),
          box(title = "Corrélation", width = 6,
              plotOutput("plot_corr", height = "350px"))
        )
      ),

      # ── Onglet Statistiques ─────────────────────────────────
      tabItem(tabName = "stats_tab",
        fluidRow(
          box(title = "Statistiques descriptives", width = 12,
              DTOutput("table_stats"))
        ),
        fluidRow(
          box(title = "Test ANOVA (duration ~ variable catégorielle)", width = 6,
              selectInput("anova_var", "Variable catégorielle :",
                          choices = cat_vars, selected = "job"),
              verbatimTextOutput("anova_result")),
          box(title = "Test Khi² (variable catégorielle ~ y)", width = 6,
              selectInput("chi2_var", "Variable catégorielle :",
                          choices = chi2_vars, selected = "job"),
              verbatimTextOutput("chi2_result"))
        )
      ),

      # ── Onglet Modèles ──────────────────────────────────────
      tabItem(tabName = "model_tab",
        fluidRow(
          box(title = "Comparaison des modèles", width = 12,
              DTOutput("model_comparison"))
        ),
        fluidRow(
          box(title = "Matrice de confusion — Régression Logistique", width = 6,
              plotOutput("cm_glm_plot", height = "300px")),
          box(title = "Matrice de confusion — KNN", width = 6,
              plotOutput("cm_knn_plot", height = "300px"))
        )
      ),

      # ── Onglet Simulation ───────────────────────────────────
      tabItem(tabName = "sim_tab",
        h3("Simulation : probabilité de souscription d'un client"),
        fluidRow(
          box(title = "Profil du client", width = 4,
              sliderInput("sim_age",      "Âge :",              18, 95,  40),
              sliderInput("sim_duration", "Durée appel (s) :",   0, 5000, 300),
              sliderInput("sim_campaign", "Nb contacts campagne :", 1, 50, 2),
              selectInput("sim_job",    "Emploi :",    levels(data$job),    "admin."),
              selectInput("sim_marital","Marital :",   levels(data$marital),"married"),
              selectInput("sim_edu",    "Éducation :", levels(data$education),"university.degree"),
              selectInput("sim_poutcome","Résultat camp. préc. :", levels(data$poutcome),"nonexistent"),
              selectInput("sim_contact", "Contact :",  levels(data$contact), "cellular"),
              sliderInput("sim_euribor", "Euribor 3m :", 0, 6, 1.3, step = 0.1),
              actionButton("sim_run", "Calculer", class = "btn-primary")
          ),
          box(title = "Résultat", width = 4,
              h4("Probabilité de souscription :"),
              verbatimTextOutput("sim_result"),
              h4("Recommandation :"),
              verbatimTextOutput("sim_recomm")
          ),
          box(title = "Interprétation des variables clés", width = 4,
              h5("Facteurs qui augmentent la probabilité :"),
              tags$ul(
                tags$li("Durée d'appel élevée"),
                tags$li("Résultat précédent = success"),
                tags$li("Contact par mobile"),
                tags$li("Mois de mars ou octobre"),
                tags$li("Profil : retraité ou étudiant")
              ),
              h5("Facteurs qui la diminuent :"),
              tags$ul(
                tags$li("Euribor élevé"),
                tags$li("Trop de contacts (saturation)"),
                tags$li("Contact par téléphone fixe"),
                tags$li("Résultat précédent = failure")
              )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # -- Données --
  output$table_preview <- renderDT({
    datatable(head(data, 200), options = list(scrollX = TRUE, pageLength = 10))
  })

  # -- Exploration variable numérique --
  output$plot_explore <- renderPlot({
    v <- input$var_explore
    if (input$plot_type == "Histogramme") {
      ggplot(data, aes_string(x = v, fill = "y")) +
        geom_histogram(position = "identity", alpha = 0.55, bins = 30) +
        scale_fill_manual(values = c("no" = "#E74C3C", "yes" = "#2ECC71")) +
        theme_minimal() + labs(title = paste("Distribution :", v))
    } else if (input$plot_type == "Boxplot") {
      ggplot(data, aes_string(x = "y", y = v, fill = "y")) +
        geom_boxplot(alpha = 0.7) +
        scale_fill_manual(values = c("no" = "#E74C3C", "yes" = "#2ECC71")) +
        theme_minimal() + labs(title = paste("Boxplot :", v))
    } else {
      ggplot(data, aes_string(x = v, fill = "y")) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = c("no" = "#E74C3C", "yes" = "#2ECC71")) +
        theme_minimal() + labs(title = paste("Densité :", v))
    }
  })

  # -- Exploration variable catégorielle --
  output$plot_cat <- renderPlot({
    v <- input$var_cat
    ggplot(data, aes_string(x = v, fill = "y")) +
      geom_bar(position = "fill") +
      scale_fill_manual(values = c("no" = "#E74C3C", "yes" = "#2ECC71")) +
      labs(y = "Proportion", title = paste("Souscription par", v)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # -- Corrélation --
  output$plot_corr <- renderPlot({
    ggcorrplot(mat_cor, method = "circle", type = "lower", lab = TRUE,
               lab_size = 2.5,
               colors = c("#E74C3C", "white", "#2ECC71"),
               ggtheme = theme_minimal())
  })

  # -- Statistiques descriptives --
  output$table_stats <- renderDT({
    datatable(round(as.data.frame(desc_stats), 2),
              options = list(pageLength = 15))
  })

  # -- ANOVA --
  output$anova_result <- renderPrint({
    req(input$anova_var)
    form   <- as.formula(paste("duration ~", input$anova_var))
    result <- aov(form, data = data)
    cat("ANOVA : duration ~", input$anova_var, "\n\n")
    print(summary(result))
    p_val <- summary(result)[[1]][["Pr(>F)"]][1]
    cat("\n→ p-value =", round(p_val, 6))
    cat("\n→", ifelse(p_val < 0.05,
                      "Différence SIGNIFICATIVE (p < 0.05)",
                      "Pas de différence significative"))
  })

  
  output$chi2_result <- renderPrint({
    req(input$chi2_var)
    tbl    <- table(data[[input$chi2_var]], data$y)
    result <- chisq.test(tbl)
    cat("Test Khi² :", input$chi2_var, "~ y\n\n")
    print(result)
    cat("\n→", ifelse(result$p.value < 0.05,
                      "Association SIGNIFICATIVE avec y (p < 0.05)",
                      "Pas d'association significative"))
  })

  
  output$model_comparison <- renderDT({
    datatable(round(comp, 4), options = list(dom = "t"), rownames = FALSE)
  })

  output$cm_glm_plot <- renderPlot({ plot_confusion(cm_glm, "Régression Logistique") })
  output$cm_knn_plot <- renderPlot({ plot_confusion(cm_knn,  "KNN (k=5)") })

  
  sim_data <- eventReactive(input$sim_run, {
    new_obs <- data[1, ]
    new_obs$age       <- input$sim_age
    new_obs$duration  <- input$sim_duration
    new_obs$campaign  <- input$sim_campaign
    new_obs$job       <- factor(input$sim_job,     levels = levels(data$job))
    new_obs$marital   <- factor(input$sim_marital, levels = levels(data$marital))
    new_obs$education <- factor(input$sim_edu,     levels = levels(data$education))
    new_obs$poutcome  <- factor(input$sim_poutcome,levels = levels(data$poutcome))
    new_obs$contact   <- factor(input$sim_contact, levels = levels(data$contact))
    new_obs$euribor3m <- input$sim_euribor
    # Valeurs fixes pour les autres
    new_obs$pdays     <- 999
    new_obs$previous  <- 0
    new_obs$default   <- factor("no",      levels = levels(data$default))
    new_obs$housing   <- factor("yes",     levels = levels(data$housing))
    new_obs$loan      <- factor("no",      levels = levels(data$loan))
    new_obs$month     <- factor("may",     levels = levels(data$month))
    new_obs$day_of_week <- factor("mon",   levels = levels(data$day_of_week))
    new_obs$emp.var.rate   <- median(data$emp.var.rate)
    new_obs$cons.price.idx <- median(data$cons.price.idx)
    new_obs$cons.conf.idx  <- median(data$cons.conf.idx)
    new_obs$nr.employed    <- median(data$nr.employed)
    new_obs$y     <- factor("no", levels = c("no","yes"))
    new_obs
  })

  output$sim_result <- renderPrint({
    req(sim_data())
    prob <- predict(model_glm, newdata = sim_data(), type = "response")
    cat(sprintf("Probabilité de souscription : %.1f%%\n", prob * 100))
    cat(sprintf("Décision (seuil 50%%) : %s\n",
                ifelse(prob > 0.5, "✅ SOUSCRIPTION PROBABLE", "❌ REFUS PROBABLE")))
  })

  output$sim_recomm <- renderPrint({
    req(sim_data())
    prob <- predict(model_glm, newdata = sim_data(), type = "response")
    if (prob > 0.6) {
      cat("→ Cibler en PRIORITÉ : profil très favorable.\n")
      cat("  Proposer une offre personnalisée avec durée d'appel suffisante.")
    } else if (prob > 0.35) {
      cat("→ Cibler si BUDGET LE PERMET : profil moyen.\n")
      cat("  Adapter le script d'appel et choisir le bon mois.")
    } else {
      cat("→ FAIBLE PRIORITÉ : risque de refus élevé.\n")
      cat("  Concentrer les ressources sur d'autres segments.")
    }
  })
}


shinyApp(ui = ui, server = server)
