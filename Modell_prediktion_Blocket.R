library(tidyverse)
data <- read.csv("C:/users/ykper/Downloads/dataset_blocket.csv", header = TRUE, fileEncoding = "UTF-8", sep = ";",stringsAsFactors = FALSE)

EDA

head(data)  # Inspektera de första raderna
summary(data)
glimpse(data)

nrow(data)
#[1] 899

ncol(data)
#[1] 13

table(is.na(data$Försäljningspris))

colSums(is.na(data))

#Bygg ett vettigt dataset med kategoriska varibler

library(tidyverse)
library(recipes)

###Skapa lista i datasetet, på de variabler som ska omkodas, 

data_cleaned <- data %>%
  mutate(across(c(Säljare, Bränsle, Växellåda, Biltyp, Drivning, Färg, Modell, Region), as.factor))

str(data_cleaned)

library(dplyr)
library(forcats)

# Funktion som säkert rensar små kategorier från ALLA faktorvariabler

rens_kategorier_säkert <- function(df, min_obs = 3) {
  df %>%
    mutate(across(where(is.factor), ~ {
      # Behåll endast nivåer med tillräckligt många observationer
      fct <- fct_lump_min(.x, min = min_obs, other_level = NA)
      # Omvandla NA till riktiga NA (för borttagning)
      fct_explicit_na(fct, na_level = "TA_BORT")
    })) %>%
    # Ta bort rader där någon kategori markerats för borttagning
    filter(if_all(where(is.factor), ~ .x != "TA_BORT")) %>%
    # Återställ faktorernas nivåer (ta bort tomma)
    mutate(across(where(is.factor), fct_drop))
}

data_rensad <- data_cleaned %>% rens_kategorier_säkert(min_obs = 3)

# Kontrollera resultatet
glimpse(data_rensad)


# Visa antal kvarvarande nivåer per variabel
data_rensad %>%
  select(where(is.factor)) %>%
  summarise(across(everything(), ~nlevels(.x))) %>%
  pivot_longer(everything(), names_to = "Variabel", values_to = "Antal_nivåer")

library(recipes)

final_recipe <- recipe(~ ., data = data_rensad) %>%
  update_role(Försäljningspris, new_role = "id") %>%  # Behåll oförändrad
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  prep()

final_data <- bake(final_recipe, new_data = data_rensad)

str(final_data)


###Uppgift 1 Prediktion Försäljningspris, Blocket Data,  Regressionsmodellering

lin_reg <- lm(Försäljningspris ~ ., data = final_data)
summary(lin_reg)

library(robustbase)
lin_reg_rob <- lmrob(Försäljningspris ~ ., data = final_data)
summary(lin_reg_rob)


# Skapa en ny datauppsättning med både "Försäljningspris" och de 12 utvalda prediktorerna
selected_data <- final_data[, c("Försäljningspris", selected_vars)]

library(leaps)
# Utför best subset selection med max 10 variabler
subset_results <- regsubsets(Försäljningspris ~ Modellår + Hästkrafter + 
                               Biltyp_SUV + Modell_X740 + Modell_X245 + Modell_XC90 + 
                               Biltyp_Sedan + Modell_S60 + Modell_V90_Cross_Country + 
                               Färg_Grå + Modell_X940 + Färg_Mörkblå + Modell_S40 + 
                               Modell_S80 + Modell_S90 + Modell_EC40 + Modell_EX40 + 
                               Färg_Grön + Region_Västernorrland + Färg_LjusGrå + Modell_V90, 
                             data = final_data, 
                             nvmax = 10)

# För att titta på resultaten med diverse kriterier (t ex BIC)
subset_summary <- summary(subset_results)
print(subset_summary$bic)

best_model_coefs <- coef(subset_results, best_model_index)
print(best_model_coefs)


# Extrahera de variabelnamn som ingår i din 10-variabla modell (exkludera interceptet)
selected_vars <- names(best_model_coefs)[-1]

# Skapa ett reducerat dataset som innehåller utfallsvariabeln och de valda prediktorerna
reduced_data <- final_data[, c("Försäljningspris", selected_vars)]
summary(reduced_data)  # Kontrollera att du fått med rätt variabler

# Bygg en linjär modell på det reducerade datasetet
model_lin_reg <- lm(Försäljningspris ~ ., data = reduced_data)
summary(model_lin_reg)

# Bygg en linjär modell på det reducerade datasetet
model_lin_rob <- lmrob(Försäljningspris ~ ., data = reduced_data)
summary(model_lin_rob)

set.seed(123)  # Sätter en seed för reproducerbarhet
n <- nrow(reduced_data)

# 70 % träningsdata
train_idx <- sample(seq_len(n), size = round(0.70 * n))
train_data <- reduced_data[train_idx, ]

# Resterande 30 % (för validation och test)
remaining_data <- reduced_data[-train_idx, ]
n_remaining <- nrow(remaining_data)

# Dela resterande data i hälften: 15 % validering och 15 % test
val_idx <- sample(seq_len(n_remaining), size = round(0.5 * n_remaining))
val_data <- remaining_data[val_idx, ]
test_data <- remaining_data[-val_idx, ]

# Kontrollera uppdelningen
cat("Train set:", nrow(train_data), "observationer\n")
cat("Validation set:", nrow(val_data), "observationer\n")
cat("Test set:", nrow(test_data), "observationer\n")

# Bygg en linjär modell på det reducerade datasetet
lin_reg <- lm(Försäljningspris ~ ., data = train_data)
summary(lin_reg)

# Bygg en linjär modell på det reducerade datasetet
lin_rob <- lmrob(Försäljningspris ~ ., data = train_data)
summary(lin_rob)

# Prediktioner med den basic lin‑modellen:
pred_val_lin <- predict(lin_reg, newdata = val_data)
pred_test_lin <- predict(lin_reg, newdata = test_data)

# Funktion för att beräkna RMSE
calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# ---- Basic LM ----
# Prediktioner på validation- och testsetet med modellen tränad på train_data
pred_val_lin  <- predict(lin_reg, newdata = val_data)
pred_test_lin <- predict(lin_reg, newdata = test_data)

# Utvärderingsmått för basic LM
rmse_lin_val  <- calc_rmse(val_data$Försäljningspris, pred_val_lin)
mae_lin_val   <- mean(abs(val_data$Försäljningspris - pred_val_lin))
rmse_lin_test <- calc_rmse(test_data$Försäljningspris, pred_test_lin)
mae_lin_test  <- mean(abs(test_data$Försäljningspris - pred_test_lin))
adj_r2_lin    <- summary(lin_reg)$adj.r.squared

# ---- Robust LM ----
# Prediktioner på validation- och testsetet med den robusta modellen
pred_val_rob  <- predict(lin_rob, newdata = val_data)
pred_test_rob <- predict(lin_rob, newdata = test_data)

# Utvärderingsmått för robust LM
rmse_rob_val  <- calc_rmse(val_data$Försäljningspris, pred_val_rob)
mae_rob_val   <- mean(abs(val_data$Försäljningspris - pred_val_rob))
rmse_rob_test <- calc_rmse(test_data$Försäljningspris, pred_test_rob)
mae_rob_test  <- mean(abs(test_data$Försäljningspris - pred_test_rob))
adj_r2_rob    <- summary(lin_rob)$adj.r.squared

# Om vi utgår från ditt tidigare resultat
results_df <- data.frame(
  Model           = c("Basic LM", "Robust LM"),
  RMSE_Validation = c(rmse_lin_val, rmse_rob_val),
  MAE_Validation  = c(mae_lin_val, mae_rob_val),
  RMSE_Test       = c(rmse_lin_test, rmse_rob_test),
  MAE_Test        = c(mae_lin_test, mae_rob_test),
  Adjusted_R2     = c(adj_r2_lin, adj_r2_rob)
)

# Runda av numeriska värden (t.ex. till 2 decimaler)
results_df[, -1] <- round(results_df[, -1], 2)

# Om du vill byta namn på kolumnerna
names(results_df) <- c("Modell", "RMSE_Val", "MAE_Val", "RMSE_Test", "MAE_Test", "Adj_R2")

# Skriv ut den justerade dataframe
print(results_df)


#####Lasso modellering Regression

library(glmnet)
# Skapa designmatris och responsvektor
x_all <- model.matrix(Försäljningspris ~ ., data = final_data)[, -1]
y_all <- final_data$Försäljningspris

cv_lasso_all <- cv.glmnet(x_all, y_all, alpha = 1)
optimal_lambda <- cv_lasso_all$lambda.min
lasso_model_all <- glmnet(x_all, y_all, alpha = 1, lambda = optimal_lambda)

# Visa koefficienterna - de med värde 0 exkluderas
coeffs <- as.matrix(coef(lasso_model_all))
print(coeffs)

library(glmnet)

# 1. Skapa designmatris och responsvektor från final_data
x_all <- model.matrix(Försäljningspris ~ ., data = final_data)[, -1]  # Ta bort den automatiska intercept-kolumnen
y_all <- final_data$Försäljningspris

# 2. Definiera candidate-värden för alpha
alphas <- seq(0, 1, by = 0.1)
cv_errors <- numeric(length(alphas))  # vektor för att spara CV-felen för varje alpha
cv_models <- list()  # för att spara cv-modellerna

# Loop över varje alpha
for(i in seq_along(alphas)){
  alpha_val <- alphas[i]
  
  # Kör inbyggd cross-validation för aktuellt alpha
  cv_model <- cv.glmnet(x_all, y_all, alpha = alpha_val)
  
  # Spara minsta CV-fel (oberoende MSE) från denna cv-modell.
  cv_errors[i] <- min(cv_model$cvm)
  
  # Spara modellen om du vill kunna återanvända den senare
  cv_models[[i]] <- cv_model
  
  cat("Alpha:", alpha_val, "CV Error:", cv_errors[i], "\n")
}

# 3. Välj alpha med lägst CV-fel
best_index <- which.min(cv_errors)
best_alpha <- alphas[best_index]
cat("Bästa alpha är:", best_alpha, "\n")

# Hämta den cv-modell som motsvarar best_alpha
best_cv_model <- cv_models[[best_index]]
optimal_lambda <- best_cv_model$lambda.min
cat("Optimal lambda:", optimal_lambda, "\n")

# 4. Träna slutgiltig modell på hela datasetet med det bästa alpha och optimal lambda
final_model <- glmnet(x_all, y_all, alpha = best_alpha, lambda = optimal_lambda)

# Extrahera koefficienterna som en matris
coeffs <- as.matrix(coef(final_model))
print(coeffs)

# Om du vill skapa en variabelvektor med de utvalda variablerna (dvs. koefficienter != 0, exklusive intercept)
selected_vars <- rownames(coeffs)[coeffs[, 1] != 0]
selected_vars <- setdiff(selected_vars, "(Intercept)")
cat("Utvalda variabler:\n")
print(selected_vars)



# Extrahera koefficienterna som en matris
coeffs <- as.matrix(coef(final_model))
print(coeffs)

# Ta bort interceptet:
coeffs_noint <- coeffs[-1, 1]  # exkludera "(Intercept)"

# Sortera koefficienterna efter absolutvärde i fallande ordning
sorted_coeffs <- sort(abs(coeffs_noint), decreasing = TRUE)

# Plocka ut de 10 högsta värdena
top10_coeffs <- head(sorted_coeffs, 10)

# Hämta variabelnamnen för de 10 utvalda
selected_vars <- names(top10_coeffs)

# Visa de utvalda variablerna tillsammans med deras ursprungliga (med tecken) koefficienter
top10_vals <- coeffs_noint[selected_vars]
result <- data.frame(Variabel = selected_vars, Koefficient = top10_vals)
print(result)

lasso_data_set <- final_data[, c(selected_vars, "Försäljningspris")]

# Visa strukturen på den nya datasetet
str(lasso_data_set)

set.seed(123)  # För reproducerbarhet

# Totalt antal observationer
n <- nrow(lasso_data_set)

# Skapa index för träningsdata (70%)
train_idx <- sample(seq_len(n), size = round(0.7 * n))
train_data <- lasso_data_set[train_idx, ]

# Resterande data
remaining_data <- lasso_data_set[-train_idx, ]

# Dela de övriga 30% lika: 15% validering, 15% test
temp_n <- nrow(remaining_data)
val_idx <- sample(seq_len(temp_n), size = round(0.5 * temp_n))
val_data <- remaining_data[val_idx, ]
test_data <- remaining_data[-val_idx, ]

# Kontrollera fördelningen
cat("Träningsset:", nrow(train_data), "observationer\n")
cat("Valideringsset:", nrow(val_data), "observationer\n")
cat("Testset:", nrow(test_data), "observationer\n")

library(glmnet)

# Skapa designmatris och responsvektor för träningsdata
# Här exkluderar vi kolumnen "Försäljningspris" från prediktorerna
x_train <- as.matrix(train_data[, colnames(train_data) != "Försäljningspris"])
y_train <- train_data$Försäljningspris

# Kör cross-validation för att hitta optimal lambda för Lasso
cv_lasso_train <- cv.glmnet(x_train, y_train, alpha = 1)  # alpha=1 motsvarar Lasso
optimal_lambda <- cv_lasso_train$lambda.min
cat("Optimal lambda från träningen:", optimal_lambda, "\n")

# Träna den slutgiltiga Lasso-modellen på träningsdata
lasso_model_train <- glmnet(x_train, y_train, alpha = 1, lambda = optimal_lambda)

# För valideringssetet
x_val <- as.matrix(val_data[, colnames(val_data) != "Försäljningspris"])
y_val <- val_data$Försäljningspris
pred_val <- predict(lasso_model_train, s = optimal_lambda, newx = x_val)

# För testsetet
x_test <- as.matrix(test_data[, colnames(test_data) != "Försäljningspris"])
y_test <- test_data$Försäljningspris
pred_test <- predict(lasso_model_train, s = optimal_lambda, newx = x_test)

# Funktion för att räkna ut RMSE
calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Beräkna RMSE för validation och test
rmse_val <- calc_rmse(y_val, pred_val)
rmse_test <- calc_rmse(y_test, pred_test)

cat("RMSE på valideringssetet:", rmse_val, "\n")
cat("RMSE på testsetet:", rmse_test, "\n")


library(glmnet)

# Skapa designmatris och responsvektor för träningsdata
x_train <- as.matrix(train_data[, colnames(train_data) != "Försäljningspris"])
y_train <- train_data$Försäljningspris

# Definiera ett galler med candidate alpha-värden, t.ex. från 0.1 till 0.9
alphas <- seq(0.1, 0.9, by = 0.2)
cv_errors <- numeric(length(alphas))    # vektor för att spara CV-fel för varje alpha
cv_models <- vector("list", length(alphas))  # lista för att spara cv.glmnet-modeller

# Loop över alpha-värden för Elastic Net
for (i in seq_along(alphas)) {
  a_val <- alphas[i]
  cv_enet <- cv.glmnet(x_train, y_train, alpha = a_val)
  cv_errors[i] <- min(cv_enet$cvm)
  cv_models[[i]] <- cv_enet
  cat("Alpha:", a_val, "CV Error:", cv_errors[i], "\n")
}

# Välj det alpha-värde som gav lägst CV-fel:
best_index <- which.min(cv_errors)
best_alpha <- alphas[best_index]
best_cv_model <- cv_models[[best_index]]
optimal_lambda <- best_cv_model$lambda.min

cat("Bästa alpha för Elastic Net:", best_alpha, "\n")
cat("Optimal lambda för Elastic Net:", optimal_lambda, "\n")

# Träna den slutgiltiga Elastic Net-modellen på träningsdata med bästa alpha och optimal lambda:
enet_model <- glmnet(x_train, y_train, alpha = best_alpha, lambda = optimal_lambda)

# Utvärdera modellen på valideringsdata
x_val <- as.matrix(val_data[, colnames(val_data) != "Försäljningspris"])
y_val <- val_data$Försäljningspris
pred_val_enet <- predict(enet_model, s = optimal_lambda, newx = x_val)

# Utvärdera modellen på testdata
x_test <- as.matrix(test_data[, colnames(test_data) != "Försäljningspris"])
y_test <- test_data$Försäljningspris
pred_test_enet <- predict(enet_model, s = optimal_lambda, newx = x_test)

# Funktion för att räkna ut RMSE
calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

rmse_val_enet <- calc_rmse(y_val, pred_val_enet)
rmse_test_enet <- calc_rmse(y_test, pred_test_enet)

cat("Elastic Net RMSE på valideringssetet:", rmse_val_enet, "\n")
cat("Elastic Net RMSE på testsetet:", rmse_test_enet, "\n")




leverage_values <- hatvalues(model_lmrob_revised)
high_leverage_obs <- which(leverage_values > (2 * mean(leverage_values)))
trainData[high_leverage_obs, ]


ggplot(trainData[high_leverage_obs, ], aes(x = Miltal, y = Försäljningspris)) +
  geom_point(color = "red") +
  labs(title = "High Leverage-punkter: Miltal vs Försäljningspris", x = "Miltal", y = "Försäljningspris")


library(car)
library(dplyr)

# Beräkna VIF
vif_values <- vif(lin_reg_rob)  

# Konvertera till dataframe
vif_df <- data.frame(Variabel = names(vif_values), VIF = vif_values)

# Sortera efter VIF-värde i fallande ordning
vif_df <- vif_df %>% arrange(desc(VIF))
 
#Filtrera endast variabler med VIF över 10
high_vif_df <- vif_df %>% filter(VIF > 10)
print(high_vif_df)

# Visa resultatet
print(high_vif_df)



high_vif_vars <- vif_df %>% filter(VIF > 10) %>% pull(Variabel)  # Plocka ut variabelnamnen
total_data <- final_data[, !names(final_data) %in% high_vif_vars]  # Ta bort dem från datasetet


AIC_values <- data.frame(
  Model = c("Linear Regression", "Robust Regression (rlm)", "Robust MM-estimator (lmrob)"),
  AIC = c(AIC(lin_reg), AIC(lin_reg_rob), AIC_lmrob)
)
print(AIC_values)

formula_top10 <- as.formula(paste("Försäljningspris ~ 1 +", paste(top_vars, collapse = " + ")))
model_auto_top10 <- lmrob(formula_top10, data = final_data)
summary(model_auto_top10)

print(top_vars)

library(car)
vif(lin_reg_robo)


# Skapa DataFrame med variabler, estimate och p-värden
df_results <- data.frame(
  Variable = rownames(model_summary$coefficients),
  Estimate = model_summary$coefficients[, "Estimate"],
  P_value = model_summary$coefficients[, "Pr(>|t|)"]
)

df_filtered <- df_results %>%
  filter(abs(Estimate) < 0.15, P_value < 0.05)
print(df_results)

library(dplyr)

df_results <- data.frame(
  Variable = names(coef(lin_reg_modell)),
  Estimate = coef(lin_reg_modell)
)

df_final <- dataset_final %>%
  select(all_of(df_results$Variable)) %>%
  mutate(across(everything(), ~ . * df_results$Estimate))

df_final <- df_final %>%
  mutate(Total_Bidrag = rowSums(select(., -Variable)))

print(df_final)

library(ggplot2)
ggplot(df_results, aes(x = Variable, y = Estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal()

# Förbered data
x <- model.matrix(Försäljningspris ~ ., final_data)[,-1]
y <- final_data$Försäljningspris

#HIT STOP


# baserad på Lasso

library(glmnet)
x <- model.matrix(Försäljningspris ~ .,data = final_data)[,-1]
y <- total_data$Försäljningspris

set.seed(123)  # För reproducerbara resultat  
trainIndex <- createDataPartition(filtered_data$Försäljningspris, p = 0.7, list = FALSE)
train_data <- final_data[trainIndex, ]
remainingData <- final_data[-trainIndex, ]

valIndex <- createDataPartition(remainingData$Försäljningspris, p = 0.5, list = FALSE)
val_data <- remainingData[valIndex, ]
test_data <- remainingData[-valIndex, ]

lasso_model <- cv.glmnet(x, y, alpha = 1)
summary(lasso_model)

alpha_values <- seq(0, 1, by = 0.1)  # Testa från 0 till 1 med steg om 0.1

alpha_opt <- lapply(alpha_values, function(a) {
  cv.glmnet(x, y, alpha = a)
})

cv_errors <- sapply(alpha_opt, function(model) min(model$cvm))
best_alpha <- alpha_values[which.min(cv_errors)]

print(best_alpha)  # Visar den optimala alpha-värdet

lasso_model <- cv.glmnet(x, y, alpha = 0.9)
print(lasso_model$lambda.min)  # Visa optimal lambda

lasso_model <- glmnet(x, y, alpha = 0.9, lambda = 1793.21)
coef(lasso_model, s = 1793.21)

coeff_matrix <- as.matrix(coef(lasso_model, s = 1793.21))  # Konvertera till en vanlig matris
selected_vars <- rownames(coeff_matrix)[coeff_matrix[,1] != 0]  # Filtrera bort nollade variabler
print(selected_vars)  # Visar de viktigaste variablerna

set.seed(123)  # Sätter seed för reproducerbarhet
cv_fit <- cv.glmnet(x, y, alpha = 0.9, nfolds = 10)  # K = 10-fold cross-validation

print(cv_fit$lambda.min)  # Visar optimalt lambda-värde
print(cv_fit$cvm)  # CV-fel för varje testat lambda

final_lasso <- glmnet(x, y, alpha = 0.9, lambda = 2159.928)

# Visa vilka variabler som har överlevt
coef(final_lasso)

vars_lambda_28 <- rownames(as.matrix(coef(final_lasso_lower_lambda)))[as.matrix(coef(final_lasso_lower_lambda))[,1] != 0]
vars_lambda_51 <- rownames(as.matrix(coef(final_lasso)))[as.matrix(coef(final_lasso))[,1] != 0]

print(setdiff(vars_lambda_51, vars_lambda_28))  # Variabler som tillkom efter optimering
print(setdiff(vars_lambda_28, vars_lambda_51))  # Variabler som försvann

final_lasso_lower_lambda <- glmnet(x, y, alpha = 0.9, lambda = 1500)

coeff_matrix_lower_lambda <- as.matrix(coef(final_lasso_lower_lambda))  # Konvertera till vanlig matris
selected_vars_lower_lambda <- rownames(coeff_matrix_lower_lambda)[coeff_matrix_lower_lambda[,1] != 0]  # Filtrera bort nollade variabler
print(selected_vars_lower_lambda)  # Visa de mest signifikanta variablerna

plot(cv_fit)

### Ridge Linjär regression
# Designmatriser för träningsdata
x_train <- model.matrix(Försäljningspris ~ ., data = train_data)[,-1]
y_train <- train_data$Försäljningspris

# Designmatriser för valideringsdata
x_val <- model.matrix(Försäljningspris ~ ., data = val_data)[,-1]
y_val <- val_data$Försäljningspris

# Designmatriser för testdata
x_test <- model.matrix(Försäljningspris ~ ., data = test_data)[,-1]
y_test <- test_data$Försäljningspris


# Träna Ridge-modellen
optimized_model_ridge <- cv.glmnet(x_train, y_train, alpha = 0)

# Hämta optimal lambda
optimized_lambda_ridge <- optimized_model_ridge$lambda.min
print(optimized_lambda_ridge)

# Extrahera koefficienter från Ridge-modellen vid optimal lambda
optimized_coef_ridge <- coef(optimized_model_ridge, s = optimized_lambda_ridge)

# Visa koefficienterna
print(optimized_coef_ridge)

barplot(
  optimized_coef_ridge[-1],  # Exkludera intercept
  names.arg = rownames(optimized_coef_ridge)[-1],
  main = "Effekten av variabler i Ridge-modellen",
  col = "steelblue",
  horiz = TRUE,
  las = 2
)

# Prediktioner för valideringsdata
predictions_val_ridge <- predict(optimized_model_ridge, s = optimized_lambda_ridge, newx = x_val)

# Prediktioner för testdata
predictions_test_ridge <- predict(optimized_model_ridge, s = optimized_lambda_ridge, newx = x_test)

# RMSE för valideringsdata
rmse_val <- sqrt(mean((y_val - predictions_val_ridge)^2))

# RMSE för testdata
rmse_test <- sqrt(mean((y_test - predictions_test_ridge)^2))

# MAE för valideringsdata
mae_val <- mean(abs(y_val - predictions_val_ridge))

# MAE för testdata
mae_test <- mean(abs(y_test - predictions_test_ridge))

# Beräkning av R² för valideringsdata
rss_val <- sum((y_val - predictions_val_ridge)^2)
tss_val <- sum((y_val - mean(y_val))^2)
r_squared_val <- 1 - (rss_val / tss_val)
adjusted_r_squared_val <- 1 - ((1 - r_squared_val) * ((nrow(x_val) - 1) / (nrow(x_val) - ncol(x_val) - 1)))

# Beräkning av R² för testdata
rss_test <- sum((y_test - predictions_test_ridge)^2)
tss_test <- sum((y_test - mean(y_test))^2)
r_squared_test <- 1 - (rss_test / tss_test)
adjusted_r_squared_test <- 1 - ((1 - r_squared_test) * ((nrow(x_test) - 1) / (nrow(x_test) - ncol(x_test) - 1)))


results <- data.frame(
  Dataset = c("Valideringsdata", "Testdata"),
  RMSE = c(rmse_val, rmse_test),
  Adjusted_R2 = c(adjusted_r_squared_val, adjusted_r_squared_test),
  MAE = c(mae_val, mae_test)
)

print(results)
print(optimized_model_ridge$glmnet.fit$alpha)

########### Prediktion på validering och test, för 4 lin.reg modeller

# Beräkna utvärderingsmått för robust linjär regression, valideringsdata

rmse_rob_val <- sqrt(mean((pred_rob_val - val_data$Försäljningspris)^2))
mae_rob_val<- mean(abs(pred_rob_val - val_data$Försäljningspris))
adjusted_r2_rob <- summary(rob_model)$adj.r.squared

# Beräkna utvärderingsmått för robust linjär regression, testdata
rmse_rob_test <- sqrt(mean((pred_rob_test - test_data$Försäljningspris)^2))
mae_rob_test <- mean(abs(pred_rob_test - test_data$Försäljningspris))

rmse_robo_val <- sqrt(mean((pred_robo_val - val_data$Försäljningspris)^2))
mae_robo_val<- mean(abs(pred_robo_val - val_data$Försäljningspris))
adjusted_r2_robo <- summary(robo_model)$adj.r.squared

# Beräkna utvärderingsmått för robust linjär regression, testdata
rmse_robo_test <- sqrt(mean((pred_robo_test - test_data$Försäljningspris)^2))
mae_robo_test <- mean(abs(pred_robo_test - test_data$Försäljningspris))

eval_df <- data.frame(
  Dataset = c("Validering", "Test"),
  Modell = rep(c("Linjär Regression", "Robust Regression lib MASS ", "Robust regression, lib RobuBase"), each = 3),
  RMSE = c(rmse_lin_val, rmse_lin_test, rmse_rob_val, rmse_rob_test,rmse_robo_val, rmse_robo_test)
  MAE = c(mae_lin_val, mae_lin_test, mae_rob_val, mae_rob_test, mae_robo_val, mae_robo_test)
  Justerat_R2 = c(adjusted_r2_lin, adjusted_r2_lin, adjusted_r2_rob, adjusted_r2_rob, adjusted_r2_robo, adjusted_r2_robo)
)

# Skriv ut resultatet snyggt
print(eval_df)




coef(lasso_model, s = "lambda.min")

# Extrahera koefficienterna från Lasso-modellen
lasso_coefs <- coef(lasso_model, s = "lambda.min")

# Konvertera till dataframe
lasso_df <- data.frame(Variabel = rownames(lasso_coefs), Koefficient = as.vector(lasso_coefs))

# Sortera efter absolutvärde på koefficienten (störst först)
lasso_df <- lasso_df %>% arrange(desc(abs(Koefficient)))

# Visa resultatet
print(lasso_df)

# Extrahera koefficienter vid optimalt lambda
lasso_coefs <- coef(lasso_model, s = "lambda.min") %>% 
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  rename(Coefficient = 2) %>%
  filter(Coefficient != 0) %>%  # Filtrerar bort de variabler som fick nollvärde
  arrange(desc(abs(Coefficient)))  # Sorterar efter absolutvärde


library(MASS)

# Skapa en stepwise-modell med LASSO-variablerna
stepwise_model <- stepAIC(lm(Försäljningspris ~ ., data = data_new), 
                          direction = "both", 
                          scope = list(lower = ~1, upper = reformulate(lasso_filtered$Variable)))

summary(stepwise_model)

library(MASS)

# Skapa en stepwise-modell där vi begränsar variabelurvalet till max 10
stepwise_model <- stepAIC(lm(Försäljningspris ~ ., data = data_new), 
                          direction = "both",
                          trace = FALSE, # Tar bort utskrift av varje steg
                          k = log(nrow(data_new)))  # Justerar AIC-kriteriet för att vara mer strikt

# Filtrera ner till max 10 variabler
selected_vars <- names(coef(stepwise_model))[2:11]  # Tar ut de 10 starkaste variablerna

print(selected_vars)

library(dplyr)

# Skapa dataset med endast de valda variablerna
data_selected <- data_new[, c("Försäljningspris", "Miltal", "Modellår", "Hästkrafter", 
                              "BiltypSUV", "FärgGrå", "FärgGrön", "FärgLjusGrå", 
                              "FärgMörkblå", "FärgVit", "Modell240_2.1_DL")]

print(names(data_selected))  # Verifiera att variablerna är korrekt valda



leverage_df$VerkligtPris <- test_data$Försäljningspris[match(leverage_df$Observation, rownames(test_data))]
valid_leverage <- high_leverage[high_leverage %in% rownames(test_data)]
rownames(test_data) <- seq_len(nrow(test_data))


# Visa de första raderna av resultatet
print(head(result_df))


print(results_df)

# Breusch-Pagan-test för heteroskedasticitet
bptest(lm_model_pca_cleaned)  

# Visa resultatet av modelljämförelsen
print(comparison_models)

lm_model_restored <- lm(model_formula_pca, data = data_cleaned)
summary(lm_model_restored)


library(robustbase)
data$Variabel_Winsor <- winsorize(data$Variabel, probs = c(0.05, 0.95))

plot(linear_model, which = 4) # Cook's Distance

library(lmtest)  # För DW-test
library(nlme)    # För GLS-modellen
library(ggplot2) # För visualiseringar


# Histogram över DW-värde
hist(dw_stat, main = "Durbin-Watson Statistic Distribution", col = "lightblue", xlab = "DW Value")
abline(v = 2, col = "red", lty = 2)  # Referenslinje vid DW = 2

# Autokorrelation av residualer
acf(ts(residuals(lm_model_restored)), main = "Autocorrelation of Residuals")

qqnorm(residuals(lm_model_restored))  
qqline(residuals(lm_model_restored), col = "red")  # Jämför med ideal normalfördelning


dw_stat <- dwtest(lm_model_restored)
hist(dw_stat, main = "Durbin-Watson Statistic Distribution", col = "lightblue", xlab = "DW Value")
abline(v = 2, col = "red", lty = 2)  # Referenslinje vid DW = 2

acf(residuals(lm_model_restored), main = "Autocorrelation of Residuals")

plot(residuals(lm_model_restored), type = "p", col = "blue", pch = 20, main = "Residual Plot", ylab = "Residuals", xlab = "Observation Index")
abline(h = 0, col = "red", lty = 2)


install.packages("nortest")
library(nortest)

ad.test(residuals(lm_model_restored))

plot(fitted(lm_model_restored), residuals(lm_model_pca_cleaned),
     main = "Residual Plot", xlab = "Förväntade värden", ylab = "Residualer")
abline(h = 0, col = "red")


ks.test(residuals(lm_model_restored), "pnorm", mean(residuals(updated_model)), sd(residuals(updated_model)))

qqnorm(residuals(updated_model))
qqline(residuals(updated_model), col = "red")


