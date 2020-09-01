# MSME - Construction

url <- "http://www.msmemart.com/msme/listings/company-list/construction/8/230/Supplier"

library(RSelenium)
library(stringr)
library(rvest)
library(tidyverse)

# On checking the site, the info about last page is visible; we have 57 pages here

c <- 1:57
html_store <- paste("html", c, sep = '_')
page_store <- paste("page", c, sep = '_')

# fireup selenium
rD <- rsDriver(browser="firefox", port=4445L, verbose=F)
remDr <- rD[["client"]]

# navigate
remDr$navigate("http://www.msmemart.com/msme/listings/company-list/construction/8/230/Supplier")

for(i in c){
  remDr$findElements("link text", as.character(i))[[1]]$clickElement()
  assign(page_store[i],read_html(assign(html_store[i], remDr$getPageSource()[[1]])))
  Sys.sleep(30)
}

# seems read_html hasnt worked previously

for(i in c){
  assign(page_store[i], read_html(eval(as.symbol(html_store[i]))))
} # eval + as.symbol saving life; get could also have been used

# to store html converted to text for the specified node, define
to_text_sorted <- paste("text_sorted",c,sep = '_')

# refer exp1 for basic structure
for(i in c){
  assign(to_text_sorted[i], 
         unlist(get(page_store[i]) %>% 
           html_node(".upl_list") %>%
           html_text() %>%
           str_trim() %>% 
           str_split('\n'))
  )
}

combined <- vector()
for(i in 2:57){
  combined <- c(combined, get(to_text_sorted[i]))
}

combined_new <- str_squish(combined)
write.csv(combined_new, file = 'construction_combined.csv', row.names = FALSE)

d <- c("", "Send Message","Send Enquiry","Send Quotation")

for(i in 1:length(combined_new)) {
  if(combined_new[i] %in% d){
    combined_new[i] = NA
  }
}

combined_new_1 <- na.omit(combined_new)
write.csv(combined_new_1, file = 'construction_combined_1.csv', row.names = FALSE)

dd <- c("URL:")
for(i in 1:length(combined_new_1)){
  if(combined_new_1[i] %in% dd){
    combined_new_1[i] = NA
    combined_new_1[i+1] = NA
  }
}

combined_new_2 <- na.omit(combined_new_1)
write.csv(combined_new_2, file = 'construction_combined_2.csv', row.names = FALSE)

address_filter <- combined_new_2[str_match(combined_new_2, 'Address:') == 'Address:']

construction_index <- is.na(address_filter)

for(i in 1:length(construction_index)){
  if(construction_index[i] == FALSE){
    construction_index[i] = 'address'
    construction_index[i+1] = 'contact'
    construction_index[i+2] = 'name'
  }
}

write.csv(construction_index, file = 'construction_index.csv', row.names = FALSE)

construction_names <- c("Om Gurudev Construction", 
                        na.omit(combined_new_2[construction_index == 'name']))

construction_contact <- combined_new_2[construction_index == 'contact']

construction_address <- combined_new_2[construction_index == 'address']

write.csv(cbind(construction_names, construction_contact, construction_address), 
          file = "raw.csv", row.names = FALSE)
