library(rvest)
library(dplyr)
library(stringr)
library(httr)

# START
essential_cookies <- c(
  `aws-waf-token` = 'a395d947-fddc-4598-8b82-0ea45a222045:CgoAo6VLmXMrAAAA:DQPlacMrEJtUbWkHBZFCm0jZDoLSi2C0xHtQf27cwOJ4taJL4FCeWvLwzFdiD9OCXh0ZJD/2SFg3Dj10QLaGJJN7ZyLoPDo48wqbf0YWlWEGcQUSyPo7VDQ2GdZ2P3e5EUn/UQmuvjJf9mFp2IA5ddrkTJm16m4LJKhKMLdUQpqJAClrxa6Bm+W0MbpKul4nbz8=',
  `bbsession` = 'id=8024d4ed-5151-424f-922b-1d2cf530f5cb',
  `bbtracker` = 'id=08ec1200-3486-43db-b6f1-d7ebfe74bfea'
)

# Sammensæt headers med kun de vigtigste cookies
header <- add_headers(
  "Host" = "www.bilbasen.dk",
  "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36",
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language" = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
  "Accept-Encoding" = "gzip, deflate, br",
  "Connection" = "keep-alive"
)

#
headers <- add_headers(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
)

starturl <- "https://www.bilbasen.dk/brugt/bil/audi?fuel=3&includeengroscvr=true&includeleasing=false&sortby=date&sortorder=desc"
rawres <- GET(url=starturl, header)
rawres$status_code
rawcontent <- httr::content(rawres,as="text")
page <- read_html(rawcontent)
carlist <- page %>% html_elements("article")

# tag-liste
ptag=".Listing_price__6B3kE"
proptag=".Listing_properties___ptWv"
mmtag="[class^='Listing_makeModel']"
dettag="[class^='Listing_details']"
dettagitem="[class^='ListingDetails_listItem']"
desctag="[class^='Listing_description']"
loctag="[class^='Listing_location']"

# til dataframe
carheader <- c("price","property","model","detailitems","description","location","link","carid","scrapedate")
df <- as.data.frame(matrix(data=NA,nrow=0,ncol = 9))
colnames(df) <- carheader

# til en side
for (car in carlist) {
  tryCatch({
    price <- car %>% html_element(ptag) %>% html_text()
    props <- car %>% html_element(proptag) %>% html_text()
    makemodel <- car %>% html_element(mmtag) %>% html_text()
    details1 <- car %>% html_element(dettag) %>% html_text()
    details <- car %>% html_elements(dettagitem) %>% html_text() %>% paste0(collapse = "_")
    description <- car %>% html_elements(desctag) %>% html_text()
    location <- car %>% html_elements(loctag) %>% html_text()
    link <- car %>% html_element("a") %>% html_attr("href") 
    carid <- link %>% str_extract("[0-9]{7}")
    tmpdf <- data.frame(price,details,makemodel,props,description,location,link,carid,Sys.time())
    df <- rbind(df,tmpdf)
  },
  error = function(cond) {
    print(makemodel)
  }
  )
}

# Df for alle sider

carheader <- c("price","property","model","detailitems","description","location","link","carid","scrapedate")
adf <- as.data.frame(matrix(data=NA,nrow=0,ncol = 9))
colnames(adf) <- carheader


for (i in 1:37) {
  url <- paste0("https://www.bilbasen.dk/brugt/bil/audi?fuel=3&includeengroscvr=true&includeleasing=false&page=",i)
  rwres <- GET(url=url, header)
  rwcontent <- httr::content(rwres,as="text")
  pge <- read_html(rwcontent)
  cars <- pge %>% html_elements("article")
  Sys.sleep(runif(1, 0.2, 0.7))
  for (car in cars) {
    tryCatch({
      price <- car %>% html_element(ptag) %>% html_text()
      props <- car %>% html_element(proptag) %>% html_text()
      makemodel <- car %>% html_element(mmtag) %>% html_text()
      details1 <- car %>% html_element(dettag) %>% html_text()
      details <- car %>% html_elements(dettagitem) %>% html_text() %>% paste0(collapse = "_")
      description <- car %>% html_elements(desctag) %>% html_text()
      location <- car %>% html_elements(loctag) %>% html_text()
      link <- car %>% html_element("a") %>% html_attr("href") 
      carid <- link %>% str_extract("[0-9]{7}")
      tmpdf <- data.frame(price,details,makemodel,props,description,location,link,carid,Sys.time())
      adf <- rbind(adf,tmpdf)
    },
    error = function(cond) {
      print(makemodel)
    }
    )
  }
}

# hente data til forhandler
links <- adf$link
forheader <- c("forhandler", "id", "adresse", "cvr","carid", "scrape_date")
fordf <- as.data.frame(matrix(data=NA,nrow=0,ncol = 6))
colnames(fordf)=forheader

# taglist
fortag <- "[class^='bas-MuiTypography-root bas-MuiTypography-h3']"
idtag <- "a.bas-MuiTypography-root.bas-MuiLink-root:contains('Se forhandlerens')"
alttag <- ".bas-MuiTypography-root.bas-MuiLink-root.bas-MuiLink-underlineNone.bas-MuiSellerInfoComponent-address.bas-MuiTypography-colorPrimary"
cvrtag <- ".bas-MuiSellerInfoComponent-cvr"

# er ik sikkert den virker
for (i in links) {
  
  tryCatch({
    rlres <- GET(url=i, header)
    Sys.sleep(runif(1, 0.2, 0.7))
    
    if(status_code(rlres) == 200) {
      pgl <- read_html(httr::content(rlres, "text"))
      
      forhandler <- pgl %>% html_element(fortag) %>% html_text(trim=TRUE)
      
      id_element <- pgl %>% html_element(idtag)
      id <- if(!is.null(id_element)) {
        href <- html_attr(id_element, "href")
        str_extract(href, "[0-9]{4}")
      } else NA
      
      link <- pgl %>% html_element("a") %>% html_attr("href")
      carid <- link %>% str_extract("[0-9]{7}")
      
      adresse <- pgl %>% html_element(alttag) %>% html_text(trim=TRUE)
      cvr <- pgl %>% html_element(cvrtag) %>% html_text(trim=TRUE)
      cvrnr <- str_extract(cvr, "[0-9]{8}")
      
      if(!is.na(forhandler) && !is.na(id) && !is.na(adresse) && !is.na(cvrnr) && !is.na(carid)) {
        fdf <- data.frame(
          forhandler = forhandler,
          id = id,
          adresse = adresse,
          cvr = cvrnr,
          carid = carid,
          scrape_date = Sys.time(),  # Rettet syntaks her
          stringsAsFactors = FALSE
        )
        fordf <- rbind(fordf, fdf)
        print(paste("Behandlet link:", i))
      }
    }
  }, error = function(e) {
    print(paste("Fejl ved behandling af link:", i))
    print(e)
  })
}



# den her virker
linkss <- df$link
forheader <- c("forhandler", "id", "adresse", "cvr","carid", "scrape_date")
rdf <- as.data.frame(matrix(data=NA,nrow=0,ncol = 6))
colnames(rdf)=forheader

for (i in linkss) {
  tryCatch({
    print(paste("Behandler link:", i))  # Debug print
    
    rlres <- GET(url=i, header)
    Sys.sleep(runif(1, 0.2, 0.7))
    
    if(status_code(rlres) == 200) {
      pgl <- read_html(httr::content(rlres, "text"))
      
      # Sikker element extraction med NULL check
      for_element <- pgl %>% html_element(fortag)
      forhandler <- if(!is.null(for_element)) html_text(for_element, trim=TRUE) else NA
      
      id_element <- pgl %>% html_element(idtag)
      id <- if(!is.null(id_element)) {
        href <- html_attr(id_element, "href")
        str_extract(href, "[0-9]{4}")
      } else NA
      
      # Hent carid direkte fra URL'en
      carid <- str_extract(i, "[0-9]{7}")
      
      adr_element <- pgl %>% html_element(alttag)
      adresse <- if(!is.null(adr_element)) html_text(adr_element, trim=TRUE) else NA
      
      cvr_element <- pgl %>% html_element(cvrtag)
      cvr <- if(!is.null(cvr_element)) html_text(cvr_element, trim=TRUE) else NA
      cvrnr <- if(!is.na(cvr)) str_extract(cvr, "[0-9]{8}") else NA
      
      if(!is.na(forhandler) && !is.na(id) && !is.na(adresse) && !is.na(cvrnr) && !is.na(carid)) {
        fdf <- data.frame(
          forhandler = forhandler,
          id = id,
          adresse = adresse,
          cvr = cvrnr,
          carid = carid,
          scrape_date = Sys.time(),
          stringsAsFactors = FALSE
        )
        rdf <- rbind(rdf, fdf)
        print(paste("Succesfuldt behandlet:", i))
      } else {
        print(paste("Manglende data for link:", i))
        print(paste("forhandler:", !is.na(forhandler)))
        print(paste("id:", !is.na(id)))
        print(paste("adresse:", !is.na(adresse)))
        print(paste("cvr:", !is.na(cvrnr)))
        print(paste("carid:", !is.na(carid)))
      }
    } else {
      print(paste("Fejl status code:", status_code(rlres), "for link:", i))
    }
  }, error = function(e) {
    print(paste("Fejl ved behandling af link:", i))
    print(e)
  })
}


# merge dataframe
merged_df <- merge(adf, fordf, by = "carid")

# 1.2 rense data

# merge details column
merged_df <- merged_df[,-5]
merged_df$details <- gsub("_"," ",merged_df$details)

# merge decription column

# fjerne alle emoji
emoji_pattern <- "[\U0001F300-\U0001F9FF]|[\\x{2600}-\\x{26FF}]"
merged_df$description <- merged_df$description %>%
  str_replace_all(emoji_pattern, "") %>%  
  str_replace_all("[^\x01-\x7FæøåÆØÅ]", "")

# fjerne special tegn
special_chars <- '[\\*\\/\\-\\_\\;\\:\\!\\@\\#\\$\\%\\^\\&\\+\\=\\"]'
merged_df$description <- merged_df$description %>% 
  str_replace_all(special_chars,"")

# fjerne multiple mellemrum
merged_df$description <- merged_df$description %>% 
  str_squish()

# fjerne .,.
merged_df$description <- merged_df$description %>%
  str_replace_all("^[\\s\\.\\,]+", "")

# erstatte newline
merged_df$description <- merged_df$description %>%
  str_replace_all("\\n", ".")

# pris til numeric
merged_df$price <- gsub("[kr.]", "", merged_df$price)  # Fjerner kr og punktum
merged_df$price <- gsub("\\s", "", merged_df$price)    # Fjerner mellemrum
merged_df$price <- gsub("[^0-9]", "", merged_df$price) # Beholder kun tal
merged_df$price <- as.numeric(merged_df$price)

class(merged_df$price)

merged_df <- merged_df[,-8]


d_pattern <- '(\\d{1,2}/\\d{4}).(\\S+).*(\\d{3})'
details <- str_match(merged_df$details,d_pattern)
d_kol <- c("details", "reg_date", "driven", "range")
colnames(details) <- d_kol

m_pattern <- "(Audi)\\s+([A-Za-z0-9\\-\\s]+)\\s+(\\d)"

makemodel <- str_match(merged_df$makemodel,m_pattern)
m_kol <- c("makemodel", "make", "model", "doors")
colnames(makemodel) <- m_kol

l_pattern <- "(\\w+(?: \\w+)*),*(.*)"
location <- str_match(merged_df$location,l_pattern)
l_kol <- c("location", "city", "area")
colnames(location) <- l_kol

a_pattern <- "^(.+?)\\s+(\\d+(?:\\s*[A-Za-z])?(?:-\\d+(?:\\s*[A-Za-z])?)?),?\\s+(\\d{4})\\s+(.+)$"
adress <- str_match(merged_df$adresse,a_pattern)

a_kol <- c("adress", "for_street", "for_street_nr", "for_postcode", "for_city")
colnames(adress) <- a_kol

# Indsæt i merged_df
merged_df_final <- cbind(merged_df[, 1:2], 
                         details[, c("reg_date", "driven", "range")], 
                         makemodel[, c("make", "model", "doors")],
                         merged_df[,5],
                         location[, c("city", "area")],
                         merged_df[,7:9],
                         adress[, c("for_street", "for_street_nr", "for_postcode", "for_city")],
                         merged_df[, 11:12])

merged_df_final$driven <- as.numeric(merged_df_final$driven)
merged_df_final$range <- as.numeric(merged_df_final$range)
merged_df_final$doors <- as.numeric(merged_df_final$doors)

merged_df_final <- na.omit(merged_df_final)

# 1.3

sim_df <- merged_df_final[,-9]
testmdf <- merge(df,rdf, by="carid")
testmdf <- testmdf[-c(3:12),-c(5,6,9)]

# merge
testmdf$price <- gsub("[kr.]", "", testmdf$price)  # Fjerner kr og punktum
testmdf$price <- gsub("\\s", "", testmdf$price)    # Fjerner mellemrum
testmdf$price <- gsub("[^0-9]", "", testmdf$price) # Beholder kun tal
testmdf$price <- as.numeric(testmdf$price)
testmdf$details <- gsub("_"," ",testmdf$details)

d_pattern <- '(\\d{1,2}/\\d{4}).(\\S+).*(\\d{3})'
details <- str_match(testmdf$details, d_pattern)
d_kol <- c("details", "reg_date", "driven", "range")
colnames(details) <- d_kol

m_pattern <- "(Audi)\\s+([A-Za-z0-9\\-\\s]+)\\s+(\\d)"
makemodel <- str_match(testmdf$makemodel, m_pattern)
m_kol <- c("makemodel", "make", "model", "doors")
colnames(makemodel) <- m_kol

l_pattern <- "(\\w+(?: \\w+)*),*(.*)"
location <- str_match(testmdf$location, l_pattern)
l_kol <- c("location", "city", "area")
colnames(location) <- l_kol

a_pattern <- "^(.+)\\s+(\\d+),?\\s+(\\d{4})\\s+(.+)$"
adress <- str_match(testmdf$adresse, a_pattern)
a_kol <- c("adress", "for_street", "for_street_nr", "for_postcode", "for_city")
colnames(adress) <- a_kol

# Indsæt i testmdf
testmdf_final <- cbind(testmdf[, 1:2], 
                       details[, c("reg_date", "driven", "range")], 
                       makemodel[, c("make", "model", "doors")],
                       location[, c("city", "area")],
                       testmdf[,6:8],
                       adress[, c("for_street", "for_street_nr", "for_postcode", "for_city")],
                       testmdf[, 10:11])

testmdf_final$driven <- as.numeric(testmdf_final$driven)
testmdf_final$range <- as.numeric(testmdf_final$range)
testmdf_final$doors <- as.numeric(testmdf_final$doors)

#
names(testmdf_final)[names(testmdf_final) == "range"] <- "mrange"

testmdf_final <- bil_11_21[1:2,]
testmdf_final <- testmdf_final[,-9]

sim_df <- rbind(sim_df,testmdf_final)
sim_df$scrape_date <- gsub("2024-11-20", "2024-11-21", sim_df$scrape_date)
sim_df$price[688] <- "427900"
sim_df$price[689] <- "389500"
sim_df$price[690] <- "409800"

sim_df <- sim_df[-c(345,397,434,239,194),]

names(merged_df_final)[names(merged_df_final) == "range"] <- "mrange"


# 1.4

# header
tysk_header <- add_headers(
  `referer` = "https://www.12gebrauchtwagen.de/",
  `sec-ch-ua` = '"Chromium";v="130", "Google Chrome";v="130", "Not?A_Brand";v="99"',
  `sec-ch-ua-mobile` = "?0",
  `sec-ch-ua-platform` = '"macOS"',
  `user-agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
)

# test på en side
turl <- "https://www.12gebrauchtwagen.de/kraftstoff/elektro/audi?page=2"
rtres <- GET(url=turl, tysk_header)
rtcontent <- httr::content(rtres,as="text")
pgt <- read_html(rtcontent)
tcars <- pgt %>% html_elements("div.row.search-results")

# taglist
# For model navn (finder links med bil-titler)
model_tag <- "div.font-bold a.provider-link"
price_tag <- "div.purchase-price"
km_tag <- "div.column.medium-4.text-md.mt-half.mileage"
year_tag <- "div.column.medium-4.text-md.mt-half.reg_year"
address_tag <- "a[href*='google.de/maps']"
link_tag <- "a.provider-link.click-out"  # Selector for bil-links

# Udtræk data
models <- pgt %>% html_elements(model_tag) %>% html_text()
prices <- pgt %>% html_elements(price_tag) %>% html_text()
kilometers <- pgt %>% html_elements(km_tag) %>% html_text() 
kilometers <- kilometers[seq(1, length(kilometers), 2)]
years <- pgt %>% html_elements(year_tag) %>% html_text()
years <- years[seq(1, length(years), 2)]
addresses <- pgt %>% html_elements(address_tag) %>% html_text()
addresses <- addresses[seq(1, length(addresses), 2)]
tlinks <- pgt %>% html_elements(link_tag) %>% html_attr("href")
tlinks <- tlinks[seq(1, length(tlinks), 3)]

# test
tysk_testdf <- data.frame(
  model = models,
  price = prices,
  kilometers = kilometers,
  year = years,
  address = addresses,
  link = paste0("https://www.12gebrauchtwagen.de", tlinks)
)

# træk 708 biler fra hjemside
tcarheader <- c("model","price","kilometer","year","location","link","scrapedate")
tysk_df <- as.data.frame(matrix(data=NA,nrow=0,ncol = 7))
colnames(tysk_df) <- tcarheader

for (i in 1:40) {
  tturl <- paste0("https://www.12gebrauchtwagen.de/kraftstoff/elektro/audi?page=",i)
  rttres <- GET(url=tturl, tysk_header)
  rttcontent <- httr::content(rttres,as="text")
  pgtt <- read_html(rttcontent)
  ttcar <- pgtt %>% html_elements("div.row.search-results")
  Sys.sleep(runif(1, 0.2, 0.7))
  for (car in ttcar) {
    tryCatch({
      modelst <- car %>% html_elements(model_tag) %>% html_text()
      pricest <- car %>% html_elements(price_tag) %>% html_text()
      kilometerst <- car %>% html_elements(km_tag) %>% html_text() 
      kilometerst <- kilometerst[seq(1, length(kilometerst), 2)]
      yearst <- car %>% html_elements(year_tag) %>% html_text()
      yearst <- yearst[seq(1, length(yearst), 2)]
      addressest <- car %>% html_elements(address_tag) %>% html_text()
      addressest <- addressest[seq(1, length(addressest), 2)]
      tlinkst <- car %>% html_elements(link_tag) %>% html_attr("href")
      tlinkst <- tlinkst[seq(1, length(tlinkst), 3)]
      ttmpdf <- data.frame(modelst,pricest,kilometerst,yearst,addressest,tlinkst,Sys.time())
      tysk_df <- rbind(tysk_df,ttmpdf)
    },
    error = function(cond) {
      print(models)
    }
    )
  }
}

# træk audi q4
# dansk
q4df_dansk <- merged_df[str_detect(merged_df$makemodel, "Q4"), ]
q4df_dansk$kilometer <- str_extract(q4df_dansk$details, "\\d+\\.?\\d*(?=\\s*km)")
q4df_dansk$date <- str_extract(q4df_dansk$details, "\\d{1,2}/\\d{4}")
q4df_dansk <- q4df_dansk[-c(163:416),-c(1,3,5:13)]
rownames(q4df_dansk) <- 1:nrow(q4df_dansk)

q4df_dansk$price <- gsub("[kr.]", "", q4df_dansk$price)  
q4df_dansk$price <- q4df_dansk$price %>% str_squish()  
q4df_dansk$price <- as.numeric(q4df_dansk$price)

q4df_dansk$kilometer <- gsub("\\.", "", q4df_dansk$kilometer)  
q4df_dansk$kilometer <- q4df_dansk$kilometer %>% str_squish()  
q4df_dansk$kilometer <- as.numeric(q4df_dansk$kilometer)

q4df_dansk$pris_km <- q4df_dansk$price/q4df_dansk$kilometer


# tysk
q4df_tysk <- tysk_df[str_detect(tysk_df$modelst, "Q4"), ]
q4df_tysk <- q4df_tysk[,-c(5:7)]
rownames(q4df_tysk) <- 1:nrow(q4df_tysk)

q4df_tysk$pricest <- gsub("[€.]", "", q4df_tysk$pricest)  # Fjerner € og punktummer
q4df_tysk$pricest <- gsub("\\s", "", q4df_tysk$pricest)   # Fjerner mellemrum
q4df_tysk$pricest <- as.numeric(q4df_tysk$pricest)
q4df_tysk$pricest <- q4df_tysk$pricest * 7.46 # Konverter til DKK (gang med 7.46)


q4df_tysk$kilometerst <- gsub("[km.]", "", q4df_tysk$kilometerst)
q4df_tysk$kilometerst <- q4df_tysk$kilometerst %>% str_squish()
q4df_tysk$kilometerst <- as.numeric(q4df_tysk$kilometerst)

q4df_tysk$pris_km <- q4df_tysk$pricest/q4df_tysk$kilometerst


# samled dataframe
q4df_samled <- cbind(q4df_dansk,q4df_tysk)

# stikprøve til q4 e-tron 40
e40_dansk <- q4df_dansk[c(1:7,13,14,17,18,19,20,23,27),]
e40_tysk <- q4df_tysk[c(3,8,18,19,27,35,43,47,56,71,73,76,78,89,91),]

e40_samled <- cbind(e40_dansk,e40_tysk)

# undewrsøgelse af priser
mean(e40_samled$price)
mean(e40_samled$pricest)
mean(e40_samled$kilometer)
mean(e40_samled$kilometerst)

# indlæse nyest data

merged_df_final_2 <- readRDS("~/Documents/da-1s/da1-projekt/Merged_df_final.rds")

bil_11_21 <- anti_join(merged_df_final_2,merged_df_final, by = "carid")

nyebiler <- merged_df_final %>%
  mutate(solgt = ifelse(carid %in% Retailers$carid, 0, 1))

nyebiler <- nyebiler[,-9]

merged_df_final <- merged_df_final %>%
  mutate(solgt = ifelse(carid %in% sim_df$carid, 0, 1))

nyebiler$solgt[c(345,397,434,239,194)] <- 1