library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(httr)


contents <- untar("log (1).tar", list = TRUE)
dflog <- data.frame(text = unlist(lapply(contents, read_lines)))


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
  new_row <- data.frame(
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
    User_Agent = str_extract(entry, "\".*\"$") %>% str_remove_all("\""),
    stringsAsFactors = FALSE
    )
  log_df <- rbind(log_df, new_row)
}

log_df_clean <- cbind(log_df[, 1:3], 
                as.Date(log_df$Timestamp, format = "%d/%b/%Y"),  # Ny kolonne 4
                log_df[, 4:ncol(log_df)])  # Behold de resterende kolonner
log_df_clean$`as.Date(log_df$Timestamp, format = "%d/%b/%Y")` <- as.Date(log_df_clean$`as.Date(log_df$Timestamp, format = "%d/%b/%Y")`)

# Omdøb kolonne
colnames(log_df_clean)[4] <- "Date"

# fjern " fra Request_method
log_df_clean$Request_Method <- gsub('"', '', log_df_clean$Request_Method)

# hurtig oversigt over date
table(log_df_clean$Date)

###################
# TOP 10 IP ADRESSER
# Tæl antallet af forekomster af hver IP-adresse
ip_counts <- table(log_df_clean$IP_Address)
unique_ip_count <- length(unique(log_df_clean$IP_Address)) # 1630 unikke ip adresser

# Omdan til dataframe for lettere håndtering
ip_counts_df <- as.data.frame(ip_counts)

# Sortér efter antallet i faldende rækkefølge
sorted_ips <- ip_counts_df[order(-ip_counts_df$Freq), ]

# Vælg de 10 mest forekommende IP-adresser
top_ips <- head(sorted_ips, 10)

#Plot med top 10 mest forekommede IP-adresser
ggplot(top_ips, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Freq), 
            hjust = 0.5,              # Centreret horisontalt
            vjust = -0.5,             # Placeret lige over søjlen
            size = 3.5,
            color = "black") + 
  labs(#title = "Én IP adresse udgår næsten 27% af antallet af IP adresser",
       x = "Top 10 IP adresser",
       y = "Antal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################
# WHO IS INFO
# geo locate
{
library(httr)
library(jsonlite)

# Funktion til at hente geolokalisering af en IP-adresse
get_ip_data <- function(ip) {
  url <- paste0("http://ip-api.com/json/", ip)
  response <- GET(url)
  ip_data <- fromJSON(content(response, "text"))
  return(ip_data)
}

# Liste af IP-adresser
ip_addresses <- c("192.0.102.40", "46.101.144.32", "5.179.80.205")

# Hent geolokalisering for hver IP
ip_data_list <- lapply(ip_addresses, get_ip_data)

# Ekstraher lat og lon fra IP-dataene
ip_df <- data.frame(
  IP = ip_addresses,
  Lat = sapply(ip_data_list, function(x) x$lat),
  Lon = sapply(ip_data_list, function(x) x$lon),
  City = sapply(ip_data_list, function(x) x$city)
)

library(ggplot2)
library(maps)

# Opret et kort med ggplot2
world_map <- map_data("world")

# Plot kortet med punkt for IP-adressen
ggplot(data = world_map) +  # Grundlæggende ggplot objekt for kortet
  borders("world", colour = "gray85", fill = "gray90") +  # Tilføj grænser for verden
  geom_point(data = ip_df, aes(x = Lon, y = Lat), color = "red", size = 3) +  # Tilføj punkter for IP-adresser
  geom_text(data = ip_df, aes(x = Lon, y = Lat, label = City), color = "black", size = 4, vjust = -1, hjust = 1) +  # Tilføj bynavnene som tekst
  labs(#title = "IP-adresser på kortet",
       x = "Longitude", y = "Latitude")+
  theme_minimal()
}

####################
# TOP 10 DATE

date_counts <- table(log_df_clean$Date)

# Omdan til dataframe for lettere håndtering
date_counts_df <- as.data.frame(date_counts)

# Sortér efter antallet i faldende rækkefølge
sorted_dates <- date_counts_df[order(-date_counts_df$Freq), ]

# Vælg de 10 mest forekommende Dates
top_dates <- head(sorted_dates, 10)

ggplot(top_dates, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Freq), 
            hjust = 0.5,              # Centreret horisontalt
            vjust = -0.5,             # Placeret lige over søjlen
            size = 3.5,
            color = "black") +  
  labs(#title = "Over 14% af forekomsterne kom fra én dag",
       x = "Dato",
       y = "Antal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####################
# Hvilken IP fremgår mest den ene dag
filtered_date <- subset(log_df_clean, Date == "2023-10-31") # 2163 forekomster

date_counts_ip <- table(filtered_date$IP_Address)

# Omdan til dataframe for lettere håndtering
date_ip_df <- as.data.frame(date_counts_ip)

# Sortér efter antallet i faldende rækkefølge
sorted_dates_ip <- date_ip_df[order(-date_ip_df$Freq), ]

# Vælg de 10 mest forekommende Dates
top_dates_ip <- head(sorted_dates_ip, 3)

ggplot(top_dates_ip, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Freq), 
            hjust = 0.5,              # Centreret horisontalt
            vjust = -0.5,             # Placeret lige over søjlen
            size = 3.5,
            color = "black") +  
  labs(#title = "Den 2023-10-31 stod én IP adresse for 53% af forekomsterne",
       x = "IP adresser",
       y = "Antal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############
# hvilken status kode fik ip adressen

filtered_date_status <- subset(filtered_date, IP_Address == "5.179.80.205")

date_status_counts <- table(filtered_date_status$Status_Code)

# Omdan til dataframe for lettere håndtering
date_status_counts_df <- as.data.frame(date_status_counts)

# Sortér efter antallet i faldende rækkefølge
sorted_date_status <- date_status_counts_df[order(-date_status_counts_df$Freq), ]

# Vælg de 10 mest forekommende status Dates
top_date_status <- head(sorted_date_status, 3)

ggplot(top_date_status, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Freq), 
            hjust = 0.5,
            vjust = -0.5,
            size = 3.5,
            color = "black") +  
  labs(#title = "",
    x = "Status code",
    y = "Antal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

oktober_31 <- data.frame(table(filtered_date_status$Request_Method))

######################################
# se overblik over status koder

status_counts <- data.frame(table(log_df_clean$Status_Code))

# Omdan til dataframe for lettere håndtering
status_counts_df <- as.data.frame(status_counts)

# Sortér efter antallet i faldende rækkefølge
sorted_status <- status_counts_df[order(-status_counts_df$Freq), ]

# Vælg de 10 mest forekommende Dates
top_status <- head(sorted_status, 3)

ggplot(top_status, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Freq), 
            hjust = 0.5,              # Centreret horisontalt
            vjust = -0.5,             # Placeret lige over søjlen
            size = 3.5,
            color = "black") +  
  labs(#title = "82% lykkeds med deres HTTP-anmodning",
       x = "Status code",
       y = "Antal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Find 404
########################
kode404 <- log_df_clean %>% 
  filter(Status_Code=="404") %>% 
  nrow()

# Opret subset med rækker, hvor Status_Code er 404
Code404sub <- subset(log_df_clean, Status_Code == 404)

######################
filtered_404 <- subset(log_df_clean, Status_Code == 404) # 2163 forekomster

counts404 <- table(filtered_404$IP_Address)

counts404_df <- as.data.frame(counts404)


sorted_404 <- counts404_df[order(-counts404_df$Freq), ]

# Vælg de 10 mest forekommende IP adresser med 404 status kode
top_404 <- head(sorted_404, 10)

ggplot(top_404, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Freq), 
            hjust = 0.5,              # Centreret horisontalt
            vjust = -0.5,             # Placeret lige over søjlen
            size = 3.5,
            color = "black") +  
  labs(#title = "Én IP adresse stod for over 16% af 404 status koderne",
       x = "IP adresser",
       y = "Antal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################
# se request method
method_counts <- table(log_df_clean$Request_Method)

# Omdan til dataframe for lettere håndtering
method_counts_df <- as.data.frame(method_counts)

# Sortér efter antallet i faldende rækkefølge
sorted_method <- method_counts_df[order(-method_counts_df$Freq), ]

# Vælg de 10 mest forekommende Dates
top_method <- head(sorted_method, 3)

ggplot(top_method, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Freq), 
            hjust = 0.5,              # Centreret horisontalt
            vjust = -0.5,             # Placeret lige over søjlen
            size = 3.5,
            color = "black") +  
  labs(#title = "Tre request metoder udgør næsten alle observationer",
       x = "Request method",
       y = "Antal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################
# 200 der burde fejle

burde_fejle_counts <- table(filtered_date_status$Status_Code == 200 & str_detect(filtered_date_status$Request_Resource, "admin|wp-login|config"))

# Gem resultatet i en data frame
burde_fejle_df <- as.data.frame(burde_fejle_counts)
# Omdan TRUE/FALSE til "Adgang" og "Ikke adgang"
burde_fejle_df$Var1 <- ifelse(burde_fejle_df$Var1 == TRUE, "Adgang", "Ikke adgang")


ggplot(burde_fejle_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = Freq), 
            hjust = 0.5,
            vjust = -0.5,
            size = 3.5,
            color = "black") +  
  labs(#title = "",
    x = "",
    y = "Antal") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))
