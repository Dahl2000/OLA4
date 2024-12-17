library(dkstat)
library(danstat)
library(devtools)
library(gt)
library(tidyverse)
###########################################
# OPG. 3.1

# indlæs data via API
# Hent dataen
NKHCO21 <- dst_meta(table = "NKHCO21", lang = "da")

# Filtrer data
realforbrug.filter <- list( 
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

NKHC021_data <- dst_get_data(table = "NKHC021", query = realforbrug.filter, lang = "da")

# Sørg for, at TID-kolonnen er i kvartalsformat
NKHC021_data$TID <- paste0(format(as.Date(NKHC021_data$TID), "%Y"), quarters(as.Date(NKHC021_data$TID)))

# Beregning af realvækst
NKHC021_data$Realvækst <- (NKHC021_data$value / dplyr::lag(NKHC021_data$value, 4) - 1) * 100  # Beregn realvækst år-over-år

# subset fra 1998Q1 - Find rækken, hvor TID er "1998Q1"
start_row <- which(NKHC021_data$TID == "1998Q1")

# Subset fra "1998Q1" til slutningen
realforbrug_data_filtreret <- NKHC021_data[start_row:nrow(NKHC021_data), ]

######
# Dummy variabel
realforbrug_data_filtreret$`op/ned` <- ifelse(realforbrug_data_filtreret$Realvækst > 0, "Op", 
                                              ifelse(realforbrug_data_filtreret$Realvækst < 0, "Ned", "Neutral"))

realforbrug_data_filtreret$dummy <- ifelse(realforbrug_data_filtreret$`op/ned` == "Op", 1,0)

table(realforbrug_data_filtreret$`op/ned`)

############################
# OPG. 3.2
################

# Hent metadata via API
meta_data <- dst_meta("FORV1")
str(meta_data)

# Hent dataen
FORV1 <- dst_meta(table = "FORV1", lang = "da")

# Filtrer data
FORV1_filter <- list(
  INDIKATOR = "*",  # Brug "*" for at inkludere alle indikatorer
  Tid = "*"         # Brug "*" for at inkludere alle tidspunkter
)
FORV1Data <- dst_get_data(table = "FORV1", query = FORV1_filter, lang = "da")

# Konverter data til bred format
FORV1Data_wide <- FORV1Data %>%
  pivot_wider(names_from = INDIKATOR, values_from = value)

# Sørg for, at TID-kolonnen er i kvartalsformat
FORV1Data_wide$TID <- paste0(format(as.Date(FORV1Data_wide$TID), "%Y"), quarters(as.Date(FORV1Data_wide$TID)))

# Summér alle kolonner gruppevis baseret på samme kvartal
FORV1Data_wide <- aggregate(. ~ TID, data = FORV1Data_wide, FUN = mean, na.rm = TRUE)

# subset fra 1998Q1 - Find rækken, hvor TID er "1998Q1"
start_row <- which(FORV1Data_wide$TID == "1998Q1")

# Subset fra "1998Q1" til slutningen
FORV1Data_wide <- FORV1Data_wide[start_row:nrow(FORV1Data_wide), ]

# Opret et nyt dataframe med DI-FTI relaterede kolonner

new_row <- realforbrug_data_filtreret[108,]
realforbrug_data_filtreret[108,] <- new_row

realforbrug_data_filtreret <- cbind(realforbrug_data_filtreret,
                                    FORV1Data_wide$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`,
                                    FORV1Data_wide$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`,
                                    FORV1Data_wide$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`,
                                    FORV1Data_wide$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`)

realforbrug_data_filtreret$`op/ned` <- as.factor(realforbrug_data_filtreret$`op/ned`)
rownames(realforbrug_data_filtreret) <- NULL

realforbrug_data_filtreret$DI_indikator <- rowMeans(realforbrug_data_filtreret[, c(9:12)])

# Udfør logistisk regression med præcise variabelnavne
logit_model <- glm(`op/ned` ~ DI_indikator,
                   data = realforbrug_data_filtreret, 
                   family = binomial)

summary(logit_model)

prediction <- data.frame(predict(logit_model,type = "response"))

realforbrug_data_filtreret$`op/ned` <- relevel(realforbrug_data_filtreret$`op/ned`, ref = "Ned")
contrasts(realforbrug_data_filtreret$`op/ned`)

# predictdata
f_pred <- data.frame(DI_indikator=-10.2750000)
predict(logit_model, f_pred, type = "response") # 0.6427198 

prediction$prd_direction <- ifelse(prediction$predict.logit_model..type....response..>= 0.6427198, 1,0)

###########################
# OPG. 3.3
########################
confusion_matrix <- data.frame(
  prd_direction = prediction$prd_direction,
  op_ned = realforbrug_data_filtreret$dummy[1:107]
)

confusion_matrix$prd_direction <- factor(confusion_matrix$prd_direction, levels = c(0, 1))
confusion_matrix$op_ned <- factor(confusion_matrix$op_ned, levels = c(0, 1))
confusion_di <- confusionMatrix(data = confusion_matrix$prd_direction, reference = confusion_matrix$op_ned)
confusion_di
table(confusion_matrix$prd_direction)
table(confusion_matrix$op_ned)

roc_curve <- roc(realforbrug_data_filtreret$dummy[1:107], prediction$predict.logit_model..type....response..)

# Plot ROC-kurven
roc_curve$sensitivities
roc_curve$specificities

fpr <- 1 - roc_curve$specificities
tpr <- roc_curve$sensitivities

# Plot TPR (y) mod FPR (x) manuelt
plot(fpr, tpr, type = "l", col = "blue", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     xlim = c(0, 1), ylim = c(0, 1), main = "ROC Curve")

# Tilføj diagonal linje for tilfældig klassifikation (reference)
abline(a = 0, b = 1, col = "gray", lty = 2)
# Tilføj AUC-værdi på plottet
legend("bottomright", legend = paste("AUC =", round(roc_curve$auc, 3)), col = "blue", lwd = 2)

#######################
# OPG. 3.4
#####################

# Scenarie 1: Tilføj interaktion mellem Familiens økonomiske situation og Danmarks økonomiske situation
logit_model_interaction <- glm(`op/ned` ~ 
                                 realforbrug_data_filtreret$`FORV1Data_wide$\`Familiens økonomiske situation i dag, sammenlignet med for et år siden\`` * realforbrug_data_filtreret$`FORV1Data_wide$\`Danmarks økonomiske situation i dag, sammenlignet med for et år siden\`` +
                                 realforbrug_data_filtreret$`FORV1Data_wide$\`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket\`` +
                                 realforbrug_data_filtreret$`FORV1Data_wide$\`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.\``,
                               data = realforbrug_data_filtreret, 
                               family = binomial)

summary(logit_model_interaction)

# Forudsigelse og konfusion matrix for interaktionsmodellen
glm.probs_interaction <- predict(logit_model_interaction, type = "response")
glm.pred_interaction <- ifelse(glm.probs_interaction > 0.50, "OP", "NED")
confusion_matrix_interaction <- table(glm.pred_interaction, realforbrug_data_filtreret$Realvækst[1:107])

# Beregn nøjagtighed for interaktionsmodellen
accuracy_interaction <- mean(glm.pred_interaction == realforbrug_data_filtreret$Realvækst[1:107]) * 100
print(paste("Nøjagtighed for interaktionsmodellen:", round(accuracy_interaction, 2), "%"))


# Scenarie 2: Tilføjelse af kvadratiske termer
logit_model_quadratic <- glm(realforbrug_data_filtreret$`op/ned` ~ 
                               poly(realforbrug_data_filtreret$`FORV1Data_wide$\`Familiens økonomiske situation i dag, sammenlignet med for et år siden\``, 2) + 
                               poly(realforbrug_data_filtreret$`FORV1Data_wide$\`Danmarks økonomiske situation i dag, sammenlignet med for et år siden\``, 2) +
                               realforbrug_data_filtreret$`FORV1Data_wide$\`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket\`` +
                               realforbrug_data_filtreret$`FORV1Data_wide$\`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.\``,
                             data = realforbrug_data_filtreret, 
                             family = binomial)

# Se resultaterne af den nye model
summary(logit_model_quadratic)

# Forudsigelse og konfusion matrix for kvadratmodellen
glm.probs_quadratic <- predict(logit_model_quadratic, type = "response")
glm.pred_quadratic <- ifelse(glm.probs_quadratic > 0.50, "OP", "NED")

# Opret en konfusion matrix
confusion_matrix_quadratic <- table(glm.pred_quadratic, realforbrug_data_filtreret$Realvækst[1:107])

# Beregn nøjagtighed for kvadratmodellen
accuracy_quadratic <- mean(glm.pred_quadratic == realforbrug_data_filtreret$Realvækst[1:107]) * 100
print(paste("Nøjagtighed for kvadratmodellen:", round(accuracy_quadratic, 2), "%"))




