library(dplyr)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(readr)

#due to 02.04.2021

##loop for brand's sites

#list of brands to scrap
brands<-c("dacia", "skoda", "audi", "bmw", "alfa-romeo", "bentley", "chevrolet", "chrysler", "citroen", "ferrari", "fiat", "ford", "honda", "hyundai", 
          "jaguar", "jeep", "volkswagen", "kia", "lamborghini", "land-rover", "lexus", "nissan", "maserati", "mazda", "mercedes", "mini", "mitsubishi", "opel","peugeot", "porsche", "renault", "seat", "smart", "subaru", "suzuki", "tesla", "toyota", "volvo")

#to recognize all models of each brand and to obtain their subpages
for (br in brands){
  rm(set)
  rm(set2)  
  www <- paste0(br,".autobazar.eu/hladaj/?damaged=N&checkcountry1=on")%>%paste0("https://",.)
  w <- read_html(www) 
  
  #making URL to scrap each model of every brand = filtered to include only undamaged cars and only offers from Slovakia
  a <- w %>%html_nodes(".item a:nth-child(1)")%>%html_attr("href")%>%str_remove("//")%>%paste0("https://",.)%>%paste0("/hladaj/?damaged=N&checkcountry1=on&page=")
  
  
  set2 <-tibble()
  
  ##read number of subpages of each model 
  
  for (u in a){ 
    url_n_pages <- paste0(u,"1")
    n_pages_read <- read_html(url_n_pages)
    n_pages <- html_nodes(n_pages_read , ".pager-btns a")%>%html_attr("href")
    n_pages <- length(n_pages) - 2
   
   #treatment for pages with only 1 subpages
   
   if (n_pages < 0){ 
      n_pages <- 1
    } else if (n_pages > 5) { 
      n_pages <- html_node(n_pages_read, ".pager-btns a:nth-child(8)")%>%html_text(trim=TRUE)
    }  else{
      n_pages 
    }
    
    ##loop for scraping content
    
    for (i in 1:n_pages){ 
      
      url <- paste0(u, i)
      web <- read_html(url)
      
      
      #super node = to save all selected data as a list and so treat NAs if present
      
      super_node <- ".listitem-description"
      super_node_read <- html_nodes(web, super_node)
      
      #nodes = specification of all concrete variables to obtain from super node
      
      price <- 'strong'
      name <- 'h2'
      year <- ".listitem-info-year"
      region <- ".label-region"
      diesel <- ".listitem-info-fuel"
      transmission <- ".listitem-info-trans"
      km <- ".listitem-info-km"
      kw <- ".listitem-info-kw"
      type <- ".label:nth-child(2)"
      items <- ".listitem-tags"
      
      # extract the output for each as cleaned text:
      
      price_data <- html_node(super_node_read, price) %>%
        html_text(trim=T) 
      
      name_data <- html_node(super_node_read, name) %>%
        html_text(trim=T)
      
      year_data <- html_node(super_node_read, year) %>%
        html_text(trim=T)
      
      region_data <- html_node(super_node_read, region) %>%
        html_text(trim=T)
      
      diesel_data <- html_node(super_node_read, diesel) %>%
        html_text(trim=T)
      
      transmission_data <- html_node(super_node_read, transmission) %>% html_text(trim=T)
      
      km_data <- html_node(super_node_read, km) %>%
        html_text(trim=T)
      
      kw_data <- html_node(super_node_read, kw) %>%
        html_text(trim=T)
      
      type_data <- html_node(super_node_read, type) %>%
        html_text(trim=T)
      
      items_data <- html_node(super_node_read, items) %>%
        html_text(trim=T)
      
      model<-u%>%str_remove("https://")%>%str_split("autobazar", simplify=T)
      model<-str_remove(model[,1], "\\.$")
      
      brand <- br
      
      #making final data frame for each brand
      
      set <- tibble(brand, model, name_data, price_data, year_data, region_data, diesel_data, transmission_data, km_data, kw_data, type_data, items_data)
      set2 <- rbind(set2, set)
      
      
    }}
    
  #to name dataset for brands
  
  assign(paste(br, "set", sep='_'), set2)
  print(paste(br, "done"))
}

#joining data of each brand into one common dataset

autobazar_all <- rbind(fiat_set, skoda_set, audi_set, bmw_set, volkswagen_set, `alfa-romeo_set`, bentley_set, chevrolet_set, chrysler_set, citroen_set, 
                       dacia_set, ferrari_set, ford_set, honda_set, hyundai_set, jaguar_set, jeep_set, kia_set, lamborghini_set, mazda_set, mercedes_set, mini_set, mitsubishi_set, opel_set,
                       porsche_set, renault_set, seat_set, smart_set, subaru_set, suzuki_set, tesla_set, toyota_set, volvo_set, `land-rover_set`, nissan_set, peugeot_set, maserati_set, lexus_set)

#saving the whole dataset, backup
write_csv(autobazar_all, "autobazar1.csv")
