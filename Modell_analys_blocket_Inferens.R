library(tidyverse)
data <- read.csv("C:/users/ykper/Downloads/dataset_blocket.csv", header = TRUE, fileEncoding = "UTF-8", sep = ";",stringsAsFactors = FALSE)

####### Teoretiska förutsättningar statistik 

####punkt 1, Normalfördelning, icke linjära 
###samband mellan beroende och oberoende variabler, linjäritet

#########################Punkt1##############################

library(ggplot2)              

#QQ plot Normalfördelning
qqnorm(final_data$Försäljningspris, 
       main = "QQ-Plot för Försäljningspris",
       xlab = "Teoretiska kvantiler",
       ylab = "Observerade kvantiler")
qqline(data$Försäljningspris, col = "red",lwd = 2)

####################Punkt1##############Normalfördelning/skevhet

library(e1071)
skewness(final_data$Försäljningspris)
skewness(final_data$Miltal)
skewness(final_data$Modellår)
skewness(final_data$Försäljningspris)
skewness(final_data$Motorstorlek)

# Beräkna medelvärde och standardavvikelse
mean_value <- mean(final_data$Försäljningspris, na.rm = TRUE)
sd_value <- sd(final_data$Försäljningspris, na.rm = TRUE)

plot(density(total_data$Försäljningspris), 
     main="Densitet plot Försäljningspris, Blocketdata, med Gauss-kurva", 
     ylab="Frekvens", 
     sub=paste("Skewness:", round(e1071::skewness(total_data$Försäljningspris), 2))) 

polygon(density(final_data$Försäljningspris), col="red")

# Lägg till Gauss-kurvan
curve(dnorm(x, mean = mean_value, sd = sd_value), 
      col = "blue", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data", "Gauss-kurva"), 
       fill = c("red", "blue"), bty = "n")

# Beräkna medelvärde och standardavvikelse
mean_value <- mean(final_data$Miltal, na.rm = TRUE)
sd_value <- sd(final_data$Miltal, na.rm = TRUE)

plot(density(final_data$Miltal), 
     main="Densitet plot Miltal Blocket-data med Gauss-kurva", 
     ylab="Frekvens", 
     sub=paste("Skewness:", round(e1071::skewness(data$Miltal), 2))) 

polygon(density(final_data$Miltal), col="red")

# Lägg till Gauss-kurvan
curve(dnorm(x, mean = mean_value, sd = sd_value), 
      col = "blue", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data", "Gauss-kurva"), 
       fill = c("red", "blue"), bty = "n")

# Beräkna medelvärde och standardavvikelse
mean_value <- mean(final_data$Hästkrafter, na.rm = TRUE)
sd_value <- sd(final_data$Hästkrafter, na.rm = TRUE)

plot(density(final_data$Hästkrafter, adjust = 0.8),
     main="Densitet plot Hästkrafter Blocket-data med Gauss-kurva", 
     ylab="Frekvens", 
     sub=paste("Skewness:", round(e1071::skewness(data$Hästkrafter), 2))) 

polygon(density(final_data$Hästkrafter), col="red")

# Lägg till Gauss-kurvan
curve(dnorm(x, mean = mean_value, sd = sd_value), 
      col = "blue", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data", "Gauss-kurva"), 
       fill = c("red", "blue"), bty = "n")

# Beräkna medelvärde och standardavvikelse
mean_value <- mean(final_data$Motorstorlek, na.rm = TRUE)
sd_value <- sd(final_data$Motorstorlek, na.rm = TRUE)

plot(density(final_data$Motorstorlek, adjust = 0.9),
     main="Densitet plot Motorstorlek Blocket-data med Gauss-kurva", 
     ylab="Frekvens", 
     sub=paste("Skewness:", round(e1071::skewness(data$Motorstorlek), 2))) 

polygon(density(final_data$Motorstorlek), col="red")

# Lägg till Gauss-kurvan
curve(dnorm(x, mean = mean_value, sd = sd_value), 
      col = "blue", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data", "Gauss-kurva"), 
       fill = c("red", "blue"), bty = "n")

# Beräkna medelvärde och standardavvikelse
mean_value <- mean(final_data$Modellår, na.rm = TRUE)
sd_value <- sd(final_data$Modellår, na.rm = TRUE)

plot(density(final_data$Modellår, adjust = 0.9),
     main="Densitet plot Modellår Blocket-data med Gauss-kurva", 
     ylab="Frekvens", 
     sub=paste("Skewness:", round(e1071::skewness(data$Modellår), 2))) 

polygon(density(final_data$Modellår), col="red")

# Lägg till Gauss-kurvan
curve(dnorm(x, mean = mean_value, sd = sd_value), 
      col = "blue", 
      lwd = 2, 
      add = TRUE)
legend("topright", legend = c("Data", "Gauss-kurva"), 
       fill = c("red", "blue"), bty = "n")


############Punkt 2 Korrelerade residualer, icke oberoende


############Punkt 3 Heteroskedasticitet


############Punkt 4 Icke normalfördelade residualer


########nr 5 OUtliers



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


#######Punkt nr 6 High leverage punkter

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


### Modell Inferens

### I vilken grad de teoretiska antaganden är uppfyllda 

###Signifikanta variabler

###Effekt styrka 

###Konfidensintervall 

###Hypotesprövning


