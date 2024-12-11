library(tidyverse)
library(readr)

untar("log (1).tar")
contents <- untar("log (1).tar", list = TRUE)
print(contents)
View(contents)

log <- list.files(pattern = "access*", path = ".")
setwd("/users/kasperdahl 1/Documents/CPH Business/Dataanalyse 2024-2026/OLA 4")
log
dflog <- data.frame(text = unlist(lapply(log, read_lines)))


# Opret en dataframe fra log entries
dflog <- data.frame(log_entry = dflog, stringsAsFactors = FALSE)

# Opret en tom dataframe til at gemme resultaterne
log_df <- data.frame(
  IP_Address = character(),
  User_Identifier = character(),
  Auth_User = character(),
  Timestamp = character(),
  Request_Method = character(),
  Request_Resource = character(),
  Protocol = character(),
  Status_Code = integer(),
  Size = numeric(),
  Referrer = character(),
  User_Agent = character(),
  stringsAsFactors = FALSE
)

# Loop gennem hver række i dflog
for (i in 1:nrow(dflog)) {
  entry <- dflog$text[i]
  
  log_df <- log_df %>%
    add_row(
      IP_Address = str_extract(entry, "^[^ ]+"),
      User_Identifier = str_extract(entry, "-"),
      Auth_User = str_extract(entry, "-"),
      Timestamp =  str_extract(entry, "\\[(.*?)\\]") %>% str_remove_all("[\\[\\]]"),
      Request_Method = str_extract(entry, "\"(\\w+)"),
      Request_Resource = str_extract(entry, "\"[A-Z]+ (.*?) HTTP"),
      Protocol = str_extract(entry, "HTTP/\\d\\.\\d"),
      Status_Code = as.integer(str_extract(entry, "\\s(\\d{3})\\s")),
      Size = as.numeric(str_extract(entry, "\\s\\d+\\s$")),
      Referrer = str_extract(entry, "\"-\""),
      User_Agent = str_extract(entry, "\".*\"$") %>% str_remove_all("\"")
    )
}


#IP antal i alt
# Definer funktionen
top_ip <- function(log_df, top_n = 10) {
  # Tæl forekomste af hver værdi
  frequency_table <- table(log_df[["IP_Address"]])
  
  # Konverter til dataframe for sortering
  freq_df <- as.data.frame(frequency_table)
  
  # Sortér dataframe i faldende rækkefølge
  sorted_df <- freq_df[order(-freq_df$Freq), ]
  
  # Returner de øverste 'top_n' resultater
  return(head(sorted_df, top_n))
}
# Brug funktionen
top_10 <- top_ip(log_df, 10)
# Vis resultatet
print(top_10)

#Plot med top 10 mest forekommede IP-adresser
# Opret plot
ggplot(top_10, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity") +  # Brug geom_bar til søjlediagram
  labs(title = "Antal forekomster af de 10 mest hyppige IP-adresser",
       x = "IP Adresse",
       y = "Antal Forekomster") +
  theme_minimal() +
  coord_flip()


# Aktive IP-adresser pr dag
#Omdan timestamp til datoformat
log_df$Timestamp <- gsub("[/:]", "-", log_df$Timestamp)

# Definer funktionen
active_ips_per_day <- function(log_df) {
  
  # Omdan Timestamp kolonne til POSIXct format
  log_df$Timestamp <- as.POSIXct(log_df$Timestamp, format = "%Y-%m-%d %H:%M:%S")
  
  # Ekstraher datoen fra Timestamp
  log_df$Date <- as.Date(log_df$Timestamp)
  
  # Tæl unikke IP-adresser pr. dag
  active_ips <- log_df %>%
    group_by(Date) %>%
    summarise(active_ip_count = n_distinct(IP_Address), .groups = 'drop')
  # Returner resultaterne
  return(active_ips)
}
# Brug funktionen
active_ips_summary <- active_ips_per_day(log_df)

# Vis resultaterne
print(active_ips_summary)
active_ips_summary$Date <- as.factor(active_ips_summary$Date)





ggplot(active_ips_summary, aes(x = Date, y = active_ip_count)) +
  geom_bar(stat = "identity", fill = "blue") +  # Søjler
  labs(title = "Aktive IP-adresser pr. Dag",
       x = "Dato",
       y = "Antal Aktive IP-adresser") +
  theme_minimal() +              # Minimalistisk tema
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Skrå tekst for datoer
  geom_text(aes(label = active_ip_count), vjust = -0.5)

######################################
#Find 404
kode404 <- log_df %>% 
  filter(Status_Code=="404") %>% 
  nrow()
print(kode404)

# Opret subset med rækker, hvor Status_Code er 404
Code404sub <- subset(log_df, Status_Code == 404)

# Definer funktionen
top_ip404 <- function(Code404sub, top_n = 10) {
  # Tæl forekomster af hver værdi
  frequency_table404 <- table(Code404sub[["IP_Address"]])
  
  # Konverter til dataframe for sortering
  freq_df404 <- as.data.frame(frequency_table404)
  
  # Sortér dataframe i faldende rækkefølge
  sorted_df404 <- freq_df404[order(-freq_df404$Freq), ]
  
  # Returner de øverste 'top_n' resultater
  return(head(sorted_df404, top_n))
}

# Brug funktionen med Code404sub dataframen
top_10_404 <- top_ip404(Code404sub, 10)

# Vis resultatet
print(top_10_404)

#Plot med 404 IP-adresser
ggplot(top_10_404, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity") +  # Brug geom_bar til søjlediagram
  labs(title = "Antal forekomster af de 10 mest hyppige IP-adresser med 404 statuskode",
       x = "IP Adresse",
       y = "Antal Forekomster") +
  theme_minimal() +
  coord_flip()

