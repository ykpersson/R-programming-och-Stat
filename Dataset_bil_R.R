library(tidyverse)
data <- read.csv("C:/users/ykper/Downloads/dataset_blocket.csv", header = TRUE, fileEncoding = "UTF-8", sep = ";",stringsAsFactors = FALSE)
str(data)

EDA

head(data)  # Inspektera de första raderna
#Försäljningspris Säljare Bränsle Växellåda Miltal Modellår    Biltyp       Drivning Hästkrafter  Färg Motorstorlek Modell
#1            21000  Privat  Bensin   Manuell  35630     2006     Kombi Tvåhjulsdriven         210 Svart         2521    V70
#2            34000  Privat  Bensin   Manuell  32781     2009     Sedan Tvåhjulsdriven         141   Blå         2435    S60
#3            19000  Privat  Bensin   Manuell  29250     2007     Kombi Tvåhjulsdriven         126   Grå         1798    V50
#4            73999  Privat  Bensin   Manuell  16166     2012 Halvkombi Tvåhjulsdriven         116   Vit         1560    C30
#5            28000  Privat  Bensin   Manuell  30900     1998     Kombi Tvåhjulsdriven         194   Röd         2435    V70
#6             7500  Privat  Bensin   Manuell  37766     2004     Sedan Tvåhjulsdriven         141   Grå         2435    S40
#Region Säljare_företag Växellåda_Automat Drivning_Fyrhjulsdriven FärgMörkblå BiltypSUV ModellXC90
#1 Värmland               0                 0                       0           0         0          0
#2 Värmland               0                 0                       0           0         0          0
#3 Värmland               0                 0                       0           0         0          0
#4 Värmland               0                 0                       0           0         0          0
#5 Värmland               0                 0                       0           0         0          0
#6 Värmland               0                 0                       0           0         0          0
 
nrow(data)
#[1] 899

ncol(data)
#[1] 13

#korrigera felskrivning i Excel filen blocket data

data$Säljare <- ifelse(data$Säljare == "privat", "Privat", data$Säljare)
unique(data$Säljare) 

table(is.na(data$Försäljningspris))
#FALSE 
#  899
  
colSums(is.na(data))
###Försäljningspris          Säljare          Bränsle        Växellåda           Miltal         Modellår           Biltyp 
###               0                0                0                0                0                0                0 
###        Drivning      Hästkrafter             Färg     Motorstorlek           Modell           Region 
###               0                0                0                0                0                0 
dim(data)
 
####### Kontroll Normalfördeling dataset####

library(ggplot2)              
               
#QQ plot Normalfördelning
qqnorm(data$Försäljningspris, 
       main = "QQ-Plot för Försäljningspris",
       xlab = "Teoretiska kvantiler",
       ylab = "Observerade kvantiler")
qqline(data$Försäljningspris, col = "red",lwd = 2)

####################Punkt1##############

#Resultat samtliga kontrollerade variabler har p värden mycket långt under p = 0.05, och följer inte en normalfördelning.


skewness(data$Försäljningspris)
#[1] 0.6072405

library(e1071)
# Beräkna medelvärde och standardavvikelse
mean_value <- mean(data$Försäljningspris, na.rm = TRUE)
sd_value <- sd(data$Försäljningspris, na.rm = TRUE)


plot(density(data$Försäljningspris), 
     main="Densitet plot Försäljningspris, Blocketdata, med Gauss-kurva", 
     ylab="Frekvens", 
     sub=paste("Skewness:", round(e1071::skewness(data$Försäljningspris), 2))) 

polygon(density(data$Försäljningspris), col="red")

# Lägg till Gauss-kurvan
curve(dnorm(x, mean = mean_value, sd = sd_value), 
      col = "blue", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data", "Gauss-kurva"), 
       fill = c("red", "blue"), bty = "n")


# Beräkna medelvärde och standardavvikelse
mean_value <- mean(data$Miltal, na.rm = TRUE)
sd_value <- sd(data$Miltal, na.rm = TRUE)

plot(density(data$Miltal), 
     main="Densitet plot Miltal Blocket-data med Gauss-kurva", 
     ylab="Frekvens", 
     sub=paste("Skewness:", round(e1071::skewness(data$Miltal), 2))) 

polygon(density(data$Miltal), col="red")

# Lägg till Gauss-kurvan
curve(dnorm(x, mean = mean_value, sd = sd_value), 
      col = "blue", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data", "Gauss-kurva"), 
       fill = c("red", "blue"), bty = "n")


# Beräkna medelvärde och standardavvikelse
mean_value <- mean(data$Hästkrafter, na.rm = TRUE)
sd_value <- sd(data$Hästkrafter, na.rm = TRUE)

plot(density(data$Hästkrafter, adjust = 0.8),
     main="Densitet plot Hästkrafter Blocket-data med Gauss-kurva", 
     ylab="Frekvens", 
     sub=paste("Skewness:", round(e1071::skewness(data$Hästkrafter), 2))) 

polygon(density(data$Hästkrafter), col="red")

# Lägg till Gauss-kurvan
curve(dnorm(x, mean = mean_value, sd = sd_value), 
      col = "blue", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data", "Gauss-kurva"), 
       fill = c("red", "blue"), bty = "n")

# Beräkna medelvärde och standardavvikelse
mean_value <- mean(data$Motorstorlek, na.rm = TRUE)
sd_value <- sd(data$Motorstorlek, na.rm = TRUE)

plot(density(data$Motorstorlek, adjust = 0.9),
     main="Densitet plot Motorstorlek Blocket-data med Gauss-kurva", 
     ylab="Frekvens", 
     sub=paste("Skewness:", round(e1071::skewness(data$Motorstorlek), 2))) 

polygon(density(data$Motorstorlek), col="red")

# Lägg till Gauss-kurvan
curve(dnorm(x, mean = mean_value, sd = sd_value), 
      col = "blue", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data", "Gauss-kurva"), 
       fill = c("red", "blue"), bty = "n")

# Beräkna medelvärde och standardavvikelse
mean_value <- mean(data$Modellår, na.rm = TRUE)
sd_value <- sd(data$Modellår, na.rm = TRUE)

plot(density(data$Modellår, adjust = 0.9),
     main="Densitet plot Modellår Blocket-data med Gauss-kurva", 
     ylab="Frekvens", 
     sub=paste("Skewness:", round(e1071::skewness(data$Modellår), 2))) 

polygon(density(data$Modellår), col="red")

# Lägg till Gauss-kurvan
curve(dnorm(x, mean = mean_value, sd = sd_value), 
      col = "blue", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data", "Gauss-kurva"), 
       fill = c("red", "blue"), bty = "n")


#Skapa värden för 3 kategoriska variabler av binär typ

data$Säljare_företag <- ifelse(data$Säljare == "Företag", 1, 0)
data$Växellåda_Automat <- ifelse(data$Växellåda == "Automat", 1, 0)
data$Drivning_Fyrhjulsdriven <- ifelse(data$Drivning == "Fyrhjulsdriven", 1, 0)

table(data$Säljare_företag)     #alternativ Företag eller Privat
table(data$Växellåda_Automat)   # alternativ manuell, Automat
table(data$Drivning_Fyrhjulsdriven) #alternativ Fyrhjulsdriven, Tvåhjulsdriven

#Borttagning av de tidigare kolumner som fått binär kodning

data_encoded <- model.matrix(~ Bränsle + Biltyp + Färg + Modell + Region - 1, data = data)               
data <- cbind(data, data_encoded)   
head(data_encoded)
colnames(data_encoded)

#Borttagning av de tidigare kolumner som fått one hot encoder variabler
data <- subset(data, select = -c(Bränsle, Biltyp, Modell, Färg, Region))
data <- subset(data, select = -c(Säljare, Växellåda, Drivning))

library(dplyr)

# Standardisera numeriska kolumner
data_standardized <- data %>%
  mutate(across(where(is.numeric), ~ scale(.)))


# Kör linjär regression med alla numeriska variabler
lm_model <- lm(Försäljningspris ~ ., data = data_standardized)

# Visa sammanfattningen av modellen
summary(lm_model)

# Hämta koefficienter och p-värden från regressionsmodellen
model_summary <- summary(lm_model)

# Bygg dataframe med variabelnamn, estimat och p-värden
coefficients_df <- data.frame(
  Variable = rownames(model_summary$coefficients),
  Estimator = model_summary$coefficients[, "Estimate"],
  P_Value = model_summary$coefficients[, "Pr(>|t|)"]
)

# Sortera dataframe baserat på p-värden (lägst först)
coefficients_sorted <- coefficients_df %>%
  arrange(P_Value)

# Visa dataframe
print(coefficients_sorted)

significant_vars <- coefficients_sorted %>%
  filter(P_Value < 0.05)
print(significant_vars)

coefficients_sorted <- coefficients_sorted %>%
  arrange(desc(abs(Estimator)))
print(coefficients_sorted)


top_vars <- coefficients_sorted %>%
  filter(P_Value < 0.05) %>%
  arrange(P_Value) %>%
  head(20) %>%
  pull(Variable)
  print(top_vars)
  
top_vars <- make.names(top_vars)
# Convert to a vector
top_vars <- unlist(top_vars)

data$Modell<- unique(modell_columns)
print(data$Modell)  # Kontrollera värdena i den nya kolumnen
data$Modell <- gsub(" ","", data$Modell)  # Ersätter mellanslag med punkter

lm_top <- lm(Försäljningspris ~ ., data = data[, c("Försäljningspris", top_vars)])
summary(lm_top)

    
top_vars <- gsub(" ", "", top_vars)  # Ersätter mellanslag med punkter
print(top_vars)
top_vars<-unique(top_vars)


# Check the structure to confirm it's now a vector
print(top_vars)
is.vector(top_vars)  # Should return TRUE

lin_reg <- lm(Försäljningspris ~ ., data = data[, c("Försäljningspris", top_vars)])
summary(lin_reg)

library(robustbase)
# Skapa en robust regressionsmodell
lin_reg_rob <- lmrob(Försäljningspris ~ ., data = data_standardized[, c("Försäljningspris", top_vars)])

# Sammanfattning av den robusta modellen
summary(lin_reg_rob)

###Uppdelning av Blocket Bildata, i 15 % Validideringsdata 15 % testdata och 70 % träningsdata för en standard
###modell Linreg

set.seed(123)  # För reproducerbarhet
train_index <- sample(1:nrow(data_standardized), 0.8 * nrow(data_standardized))
train_data <- data_standardized[train_index, ]
test_data <- data_standardized[-train_index, ]

lin_reg<- lm(Försäljningspris ~., data = train_data)
summary(lin_reg) 

# Skapa en robust regressionsmodell
lin_reg_rob <- lmrob(Försäljningspris ~ ., data = train_data)

# Sammanfattning av den robusta modellen
summary(lin_reg_rob)

Y_predlinR_val <- predict(lin_reg, newdata = val_data)
Y_predlinR_test <- predict(lin_reg, newdata = test_data)

YpredRo_val <- predict(lin_reg_rob, newdata = val_data)
YpredRo_test <- predict(lin_reg_rob, newdata = test_data)

# Beräkna utvärderingsmått för linjär regression
rmse_lin_val <- sqrt(mean((linreg_val_predictions - val_data$Försäljningspris)^2))
mae_lin_val<- mean(abs(linreg_val_predictions - val_data$Försäljningspris))
adjusted_r2_lin <- summary(lin_reg)$adj.r.squared

# Beräkna utvärderingsmått för robust regression
rmse_rob_val <- sqrt(mean((linrob_val_predictions - val_data$Försäljningspris)^2))
mae_rob_val <- mean(abs(linrob_val_predictions - val_data$Försäljningspris))
adjusted_r2_rob <- summary(lin_reg_rob)$adj.r.squared

# Beräkna utvärderingsmått för linjär regression
rmse_lin_test <- sqrt(mean((linreg_test_predictions - test_data$Försäljningspris)^2))
mae_lin_test <- mean(abs(linreg_test_predictions - test_data$Försäljningspris))
adjusted_r2_lin <- summary(lin_reg)$adj.r.squared

# Beräkna utvärderingsmått för robust regression
rmse_rob_test <- sqrt(mean((linrob_test_predictions - test_data$Försäljningspris)^2))
mae_rob_test<- mean(abs(linrob_test_predictions - test_data$Försäljningspris))
adjusted_r2_rob <- summary(lin_reg_rob)$adj.r.squared

eval_df <- data.frame(
  Dataset = c("Validering", "Test"),
  Modell = rep(c("Linjär Regression", "Robust Regression"), each = 2),
  RMSE = c(rmse_lin_val, rmse_lin_test, rmse_rob_val, rmse_rob_test),
  MAE = c(mae_lin_val, mae_lin_test, mae_rob_val, mae_rob_test),
  Justerat_R2 = c(adjusted_r2_lin, adjusted_r2_lin, adjusted_r2_rob, adjusted_r2_rob)
)

# Skriv ut resultatet snyggt
print(eval_df)

library(glmnet)

lasso_model <- cv.glmnet(X, Y, alpha = 1)

# Extrahera koefficienterna med det bästa lambda-värdet
best_lambda <- lasso_model$lambda.min
lasso_coef <- coef(lasso_model, s = best_lambda)

# Visa variabler med icke-noll koefficienter
selected_vars <- rownames(lasso_coef)[lasso_coef[, 1] != 0]
print(selected_vars)

# Välj det bästa Lambda-värdet (regulariseringens styrka)
best_lambda <- lasso_model$lambda.min

# Extrahera koefficienterna
lasso_coef <- coef(lasso_model, s = best_lambda)
print(lasso_coef)  # Visa vilka variabler som är inkluderade (icke-noll koefficienter)

sorted_coef <- lasso_coef[order(-abs(lasso_coef[, 1])), , drop = FALSE]
sorted_coef_df <- data.frame(
Variabel = rownames(sorted_coef),
Koefficient = as.numeric(sorted_coef[, 1])
)
print(sorted_coef_df)

# Filtrera rader där Koefficient är större än 0.1
filtered_coef_df <- sorted_coef_df[sorted_coef_df$Koefficient > 0.20, ]

# Visa det filtrerade resultatet
print(filtered_coef_df)

filtered_lasso_vars <- lasso_importance[lasso_importance$Coefficient > 0.1, "Variable"]
print(filtered_lasso_vars)  # Visa variabler med koefficient över 0.25

missing_vars <- filtered_lasso_vars[!filtered_lasso_vars %in% colnames(data)]
print(missing_vars)  # Variabler som inte finns i data

filtered_lasso_vars <- filtered_lasso_vars[filtered_lasso_vars %in% colnames(data)]

final_model <- lm(Försäljningspris ~ . -1, data = selected_data_significant)
summary(final_model)

significant_vars <- names(coef(final_model))[which(summary(final_model)$coefficients[, "Pr(>|t|)"] <= 0.05)]
print(significant_vars)  # Visa variabler med p ≤ 0.05
selected_data_significant <- data[, c("Försäljningspris", significant_vars)]


# Skapa en dataram med korrelation och p-värde
analysis_df <- data.frame(
  Variable = names(p_values),  # Variabelnamn
  Correlation = correlation_with_price,  # Korrelation med försäljningspris
  P_Value = p_values  # p-värden från modellen
)
# Visa datarammen
print(analysis_df)

# Filtrera variabler med stark korrelation och lågt p-värde
key_vars_df <- analysis_df[abs(analysis_df$Correlation) >= 0.15 & analysis_df$P_Value < 0.05, ]
print(key_vars_df)  # Visa nyckelvariabler



selected_vars <- key_vars_df$Variable  # Behåll nyckelvariablerna
selected_data_key <- data[, c("Försäljningspris", selected_vars)]
final_model_key <- lm(Försäljningspris ~ ., data = selected_data_key)
summary(final_model_key)


selected_vars <- key_vars_df$Variable  # Behåll nyckelvariablerna
selected_data_key <- data[, c("Försäljningspris", selected_vars)]

print(selected_data_key)

x_key <- model.matrix(Försäljningspris ~ ., data = selected_data_key)[,-1]  # Tar bort intercept
y_key <- selected_data_key$Försäljningspris

# Kör Lasso-modellen
lasso_model<- glmnet(x_key, y_key, alpha = 1)

predictions_val <- predict(lasso_model, s = best_lambda, newx = x_val)
rmse_val <- sqrt(mean((y_val - predictions_val)^2))
print(rmse_val)

selected_vars <- unlist(selected_data_key)  # Konverterar listan till en vektor
print(selected_vars)

x_train <- model.matrix(Försäljningspris ~ ., data = train_data[, c("Försäljningspris", selected_vars)])[,-1]
y_train <- train_data$Försäljningspris

x_val <- model.matrix(Försäljningspris ~ ., data = val_data[, c("Försäljningspris", selected_vars)])[,-1]
y_val <- val_data$Försäljningspris

x_test <- model.matrix(Försäljningspris ~ ., data = test_data[, c("Försäljningspris", selected_vars)])[,-1]
y_test <- test_data$Försäljningspris

print(selected_vars)  # Visa variabelnamn
print(colnames(train_data))  # Visa kolumnnamn i train_data

library(glmnet)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)
best_lambda <- lasso_model$lambda.min

predictions_val <- predict(lasso_model, s = best_lambda, newx = x_val)
rmse_val <- sqrt(mean((y_val - predictions_val)^2))
print(rmse_val)

predictions_test <- predict(lasso_model, s = best_lambda, newx = x_test)
rmse_test <- sqrt(mean((y_test - predictions_test)^2))
print(rmse_test)


lasso_coefficients <- coef(lasso_model, s = best_lambda)
barplot(
  lasso_coefficients[-1, 1],  # Exkludera intercept
  names.arg = rownames(lasso_coefficients)[-1],
  main = "Lasso Koefficienter",
  col = "skyblue",
  las = 2,
  horiz = TRUE
)

library(glmnet)

# Lista över alpha-värden att testa
alpha_vals <- seq(0.01, 1, by = 0.01)  # Justera intervallet för precision
results <- data.frame(alpha = numeric(), lambda = numeric(), rmse_val = numeric())

# Loop för att optimera alpha
for (alpha in alpha_vals) {
  model <- cv.glmnet(x_train, y_train, alpha = alpha)
  best_lambda <- model$lambda.min
  predictions_val <- predict(model, s = best_lambda, newx = x_val)
  rmse_val <- sqrt(mean((y_val - predictions_val)^2))
  
  # Spara resultatet
  results <- rbind(results, data.frame(alpha = alpha, lambda = best_lambda, rmse_val = rmse_val))


print(colnames(x_train))  # Variabler i träningsdata
print(colnames(x_val))    # Variabler i valideringsdata
print(colnames(x_test))   # Variabler i testdata

selected_vars <- c("Modellår", "Hästkrafter", "Växellåda_Automat", 
                   "BiltypSUV", "Säljare_företag", "Motorstorlek", 
                   "BiltypKombi", "ModellXC90", "BränsleBensin")

x_train <- model.matrix(Försäljningspris ~ ., data = train_data[, c("Försäljningspris", selected_vars)])[,-1]
y_train <- train_data$Försäljningspris


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



install.packages("broom")


library(MASS)
library(broom)
step_model <- stepAIC(lm_model_restored, direction = "both")
tidy(step_model)

comparison_models <- data.frame(
  Model = c("Original Modell", "Stepwise Modell"),
  Adjusted_R2 = c(summary(lm_model_restored)$adj.r.squared, summary(step_model)$adj.r.squared),
  Residual_Std_Error = c(summary(lm_model_restored)$sigma, summary(step_model)$sigma)
)
print(comparison_models)

qqnorm(residuals(step_model))
qqline(residuals(step_model), col = "red")


library(MASS)
lm_model_robust <- rlm(model_formula, data = data_cleaned)
summary(lm_model_robust)


result_df <- data.frame(
  VerkligtPris = test_data$Försäljningspris,
  PrediceratPris = predict(lin_model, newdata = test_data[, valid_vars]),
  Residual = test_data$Försäljningspris - predict(lin_model, newdata = test_data[, valid_vars])
)

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

####Outliers punkt nr 5

if (!any(grepl("^z_", colnames(data_standardized)))) {
z_scores <- as.data.frame(scale(data_standardized[, top_vars]))
colnames(z_scores) <- paste0("z_", top_vars)
data_standardized <- cbind(data_standardized, z_scores)


  z_scores <- as.data.frame(scale(data_standardized[, top_vars]))
  colnames(z_scores) <- paste0("z_", top_vars)
  data_standardized <- cbind(data_standardized, z_scores)
}


for (var in top_vars) {
  p <- ggplot(data_standardized, aes_string(x = var, y = paste0("z_", var))) +
    geom_point(color = "blue") +
    geom_hline(yintercept = 3, color = "red", linetype = "dashed") +
    geom_hline(yintercept = -3, color = "red", linetype = "dashed") +
    labs(title = paste(var, "vs Z-Score"), x = var, y = "Z-Score")
  print(p)
}

outlier_data <- data_standardized[outlier_indices, ]
print(outlier_data)

data_standardized <- data_standardized[, c(top_vars, paste0("z_", top_vars))]
print(colnames(data_standardized))
data_standardized <- data_standardized[, !grepl("^z_", colnames(data_standardized))]
print(colnames(data_standardized))


outlier_indices <- which(data_standardized$Försäljningspris > 3 | data_standardized$Försäljningspris < -3)
print(outlier_indices)

data_without_outliers <- data_standardized[-outlier_indices, ]

outlier_indices <- list()  # En lista för att lagra outliers för varje variabel

for (var in top_vars) {
  outlier_indices[[var]] <- which(data_standardized[[var]] > 3 | data_standardized[[var]] < -3)
}

#Kombinera alla index
all_outlier_indices <- unique(unlist(outlier_indices))
print(all_outlier_indices)

data_without_outliers <- data_standardized[-all_outlier_indices, ]
print(data_without_outliers)

model_without_outliers<- lm(Försäljningspris ~ ., data = data_without_outliers[, c("Försäljningspris", top_vars)])
summary(model_without_outliers)

duplicated_columns <- colnames(data_standardized)[duplicated(colnames(data_standardized))]
print(duplicated_columns)
data_standardized <- data_standardized[, !duplicated(colnames(data_standardized))]

anyDuplicated(colnames(data_standardized))  # Ska returnera 0
data_standardized <- data_standardized[, !duplicated(colnames(data_standardized))]

####### High leverage points punkt nr 6
# Identifiera high leverage points
high_leverage <- which(hat_values > (2 * mean(hat_values)))
print(high_leverage)

plot(hat_values, main = "Hat-värden för observationer", xlab = "Index", ylab = "Hat-värde")
abline(h = 2 * mean(hat_values), col = "red", lwd = 2)

# Beräkna Cook's Distance
cooks_distance <- cooks.distance(lm_top)

# Plotta Cook's Distance
plot(cooks_distance, main = "Cook's Distance för varje observation", 
     xlab = "Observation", ylab = "Cook's Distance")
abline(h = 4 / nrow(data_standardized), col = "red", lty = 2)  # Tröskellinje

outlier_index <- which(cooks_distance > 0.080)
print(outlier_index)

print(data_standardized[outlier_index,])

data_standardized$original_index <- 1:nrow(data_standardized)

lm_top <- lm(Försäljningspris ~ ., data = data_standardized[, c("Försäljningspris", top_vars)], 
             subset = rownames(data_standardized) != "775")
summary(lm_top)

# Skapa ett nytt dataset utan high leverage points
filtered_data <- data[-high_leverage, ]
Y <- data$Försäljningspris[-high_leverage]
print(length(Y))
nrow(filtered_data) == length(Y)  # Ska returnera TRUE

# Kontrollera hur många observationer som togs bort
print(paste("Antal observationer borttagna:", length(high_leverage)))

high_leverage_df$Cooks_Distance <- cooks.distance(lin_reg_pca)[high_leverage_df$Observation]

# Visa dataframe
print(high_leverage_df)

library(ggplot2)

# Skapa plot
ggplot(high_leverage_df, aes(x = Observation, y = HatValue)) +
  geom_point(color = "red", size = 3) +  # Röda punkter för att markera high leverage
  theme_minimal() +
  labs(title = "High Leverage Points",
       x = "Observation",
       y = "Hat Value") +
  geom_hline(yintercept = 2 * mean(hat_values), linetype = "dashed", color = "blue")  # Konventionell gräns

#########################7##############################

# Punkt 7, Kollinaritet och multikollariet

# Skapa en data frame med variabler och deras VIF-värden
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values
)

# Visa data frame
print(vif_df)

# Sortera data frame efter VIF-värden
vif_df_sorted <- vif_df[order(vif_df$VIF, decreasing = TRUE), ]
print(vif_df_sorted)


cor_matrix <- cor(x_train)
print(cor_matrix)

high_corr <- which(abs(cor_matrix) > 0.75 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr)

library(car)
vif(lin_reg) # Kontrollera vilka variabler som har höga VIF-värden (> 5 kan vara problematiskt)

reduced_data <- subset(reduced_data, select = -c(BiltypKombi))
reduced_data <- reduced_data[, -which(names(reduced_data) %in% c("Säljare_företag", "ModellXC60", "BränsleEl"))]
