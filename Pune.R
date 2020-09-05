# Pune

library(magick)
library(tidyverse)
library(tesseract)
library(rvest)
library(stringr)

# Step 1 - Visit the required webpage, scroll down to load complete data
# Step 2 - Take screenshot and save the webpage as a .txt file
# Step 3 - Split the ss into 25 images

contractors <- c("carp", "cico","elect","floor","marb","paint","plumb","shut","steel","weld")
subfolder <- c("c","cc","e","f","m","pa","pl","sh","st","w")

# Step 4 - Crop the images

image_manipulate <- function(subfolder, k){
  for(i in 1:k){
    im <- magick::image_read(path = paste("Pune/", subfolder, "/row-", i, "-col-1.png", sep = ""))
    first_crop <- magick::image_crop(im, -720)
    second_crop <- magick::image_crop(first_crop, 200)
    magick::image_write(second_crop, path = paste("Pune/", subfolder, "/", subfolder, "_", i, ".png", sep = ""))
  } 
}

for(b in subfolder[1:5]){
  image_manipulate(subfolder = b, k = 25)
}


for(b in subfolder[6:10]){
  image_manipulate(subfolder = b, k = 25)
}

# Step 5 - Phone number extraction using OCR

for (j in subfolder) {
  base <- vector()
  for (i in 1:25){
    eng <- tesseract("eng")
    path <- paste("Pune/", j, "/", j, "_", i, ".png", sep = "")
    #print(path)
    t <- tesseract::ocr_data(path, engine = eng)
    ct <- t[["word"]]
    t_filter <- ct[!is.na(str_extract(ct,"-"))]
    t_matrix <- str_split(t_filter, "-", simplify = TRUE)
    
    if(ncol(t_matrix) == 3){
      t_df <- as.data.frame(t_matrix)
      t_mutate <- t_df %>%
        mutate(V4 = paste(t_df[,2],t_df[,3],sep=""))
      n <- t_mutate[,4]
      p <- str_extract(n,"[0-9]+")
      p1 <- p[!is.na(p)]
      #print(p1)
      base <- c(base, p1)
      #base <- unique(base)
    } else{
      t_df <- as.data.frame(t_matrix)
      n <- t_df[,2]
      p <- str_extract(n,"[0-9]+")
      p1 <- p[!is.na(p)]
      #print(p1)
      base <- c(base, p1)
      #base <- unique(base)
    }
  }
  #print(base)
  phone <- paste("phone_", j, sep = "")
  assign(phone,base)
}


# Step 6 - Extract names, address, tags from .txt files

for(i in 1:10){
  assign(contractors[i], read_html(paste("Pune/", subfolder[i], "/", subfolder[i], ".txt", sep = "")))
}

L <- list(carp, cico, elect, floor, marb, paint, plumb, shut, steel, weld)

for (j in 1:10){
  for(i in L[j]){
    names <- paste("names_", subfolder[j] , sep = "")
    assign(names, i %>%
             html_nodes(".lng_cont_name") %>% 
             html_text() %>%
             as.data.frame()
    )
    
    address <- paste("address_", subfolder[j], sep = "")
    assign(address, i %>%
             html_nodes(".cont_sw_addr") %>%
             html_text() %>%
             str_squish() %>%
             as.data.frame()
    )
    
    profile <- paste("profile_", subfolder[j], sep = "")
    assign(profile, i %>%
             html_nodes(".adinfoex") %>%
             html_text() %>%
             str_squish() %>%
             as.data.frame()
    )
  }
}


# Step 7 - Check the outputs of Step 5,6 and make changes if necessary

# Checking c

names_c <- as.data.frame(names_c[-c(37, 43, 44, 56, 77),])
address_c <- as.data.frame(address_c[-c(37, 43, 44, 56, 77),])
profile_c <- as.data.frame(profile_c[-c(37, 43, 44, 56, 77),])

# Checking cc

phone_cc <- phone_cc[-5]

# Checking e

phone_e <- phone_e[-11]

# Checking f

# looks fine

# Checking m

names_m <- as.data.frame(names_m[-78,])
address_m <- as.data.frame(address_m[-78, ])
profile_m <- as.data.frame(profile_m[-78,])

# Checking pa

phone_pa <- phone_pa[-30]

# Checking pl

phone_pl <-  phone_pl[-23]

# Checking sh

phone_sh <-  phone_sh[-19]

# Checking st

phone_st <- phone_st[-1]

# Checking w

# seems fine


# Step 8 - Create .csv files

long_list <- list(list(names_c, phone_c, address_c, profile_c), 
                  list(names_cc, phone_cc, address_cc, profile_cc),
                  list(names_e, phone_e, address_e, profile_e), 
                  list(names_f, phone_f, address_f, profile_f),
                  list(names_m, phone_m, address_m, profile_m), 
                  list(names_pa, phone_pa, address_pa, profile_pa),
                  list(names_pl, phone_pl, address_pl, profile_pl), 
                  list(names_sh, phone_sh, address_sh, profile_sh),
                  list(names_st, phone_st, address_st, profile_st), 
                  list(names_w, phone_w, address_w, profile_w))

full_names <- c("Carpentry", "Civil Construction", "Electrical", "Flooring", "Marble", "Painting",
                "Plumbing", "Shuttering", "Steelwork", "Welding")

for(i in 1:10){
  x <- cbind(long_list[[i]][[1]], long_list[[i]][[2]], long_list[[i]][[3]], long_list[[i]][[4]])
  names(x) <- c("Name","Phone","Address","Tags") 
  #print(x)
  write.csv(x, paste("Pune/", "Pune_", full_names[i], ".csv", sep = ""))
}
