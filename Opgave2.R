library(DBI)
library(RMariaDB)
library(dplyr)
library(rvest)



urlHCAB <- "https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB"
urlAnholt <- "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO"
urlAARH3 <- "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3"
urlRisoe <- "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE"


headerhcab <- add_headers(
  "accept" = "*/*",
  "accept-language" = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
  "origin" = "https://envs2.au.dk",
  "referer" = "https://envs2.au.dk/",
  "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
)

base_url <- "https://envs2.au.dk"

# HCAB
initial_res_hcab <- GET(paste0(base_url, "/Luftdata/Presentation/table/Copenhagen/HCAB"))
initial_html_hcab <- read_html(initial_res_hcab)

# Få token
token <- initial_html_hcab %>%
  html_element("#__AjaxAntiForgeryForm input[name='__RequestVerificationToken']") %>%
  html_attr("value")

# Lav POST request for hovedtabellen
hcab_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Copenhagen/HCAB"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded"
  ),
  body = list(
    "__RequestVerificationToken" = token
  ),
  encode = "form"
)

# Parse responset
table_html <- content(hcab_table_res, "text")
Hcab_data <- read_html(table_html) %>%
  html_table() %>%
  .[[1]]  # Tag første tabel

# Anholt
initial_res_anholt <- GET(paste0(base_url, "/Luftdata/Presentation/table/Rural/ANHO"))
initial_html_anholt <- read_html(initial_res_anholt)


# Lav POST request for hovedtabellen
anholt_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Copenhagen/HCAB"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded"
  ),
  body = list(
    "__RequestVerificationToken" = token
  ),
  encode = "form"
)

# Parse responset
anholt_html <- content(anholt_table_res, "text")
Anholt_data <- read_html(anholt_html) %>%
  html_table() %>%
  .[[1]]  # Tag første tabel


# Aarhus
initial_res_aarhus <- GET(paste0(base_url, "/Luftdata/Presentation/table/Aarhus/AARH3"))
initial_html_aarhus <- read_html(initial_res_aarhus)

aarhus_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Aarhus/AARH3"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded"
  ),
  body = list("__RequestVerificationToken" = token),
  encode = "form"
)

aarhus_html <- content(aarhus_table_res, "text")
aarhus_data <- read_html(aarhus_html) %>%
  html_table() %>%
  .[[1]]

# Risø
initial_res_risoe <- GET(paste0(base_url, "/Luftdata/Presentation/table/Rural/RISOE"))
initial_html_risoe <- read_html(initial_res_risoe)

risoe_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Rural/RISOE"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded"
  ),
  body = list("__RequestVerificationToken" = token),
  encode = "form"
)

risoe_html <- content(risoe_table_res, "text")

risoe_data <- read_html(risoe_html) %>%
  html_table() %>%
  .[[1]]



# dagen efter

Hcab_data_2 <- read_html(table_html) %>%
  html_table() %>%
  .[[1]]  

anholt_data_2 <- read_html(anholt_html) %>%
  html_table() %>%
  .[[1]]

aarhus_data_2 <- read_html(aarhus_html) %>%
  html_table() %>%
  .[[1]]

risoe_data_2 <- read_html(risoe_html) %>%
  html_table() %>%
  .[[1]]

nyhcab <- Hcab_data_2[c(1:21),]
nyanholt <- anholt_data_2[c(1:20),]
nyaarhus <- aarhus_data_2[c(1:20),]
nyrisoe <- risoe_data_2[c(1:20),]

Hcab_data <- rbind(nyhcab,Hcab_data)
Anholt_data <- rbind(nyanholt,Anholt_data)
aarhus_data <- rbind(nyaarhus,aarhus_data)
risoe_data <- rbind(nyrisoe,risoe_data)


