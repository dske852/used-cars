#separating joined strings in items column

items <- autobazar_all$items_data
items_separate <- str_split(items, "kraj", simplify = T)

items_separate2 <- str_split(items_separate[,2], "[ABCDEFGHIJKLMNOPRSZYVQW4]", simplify = T)

#replacing strings that were damaged by separating and are needed for further processing

items_separate2[,2] <- items_separate2[,2] %>%
str_replace_all(c("^atchback$" = "Hatchback", "^abrio$" = "Cabrio", "^pv$" = "Mpv", "^ombi$" = "Combi", "^né$"= "Iné",
                    "^edan$"= "Sedan", "^odávka$" = "Dodávka", "^an$" = "Van", "^kriňa$" = "Skriňa", "^raziarenske$"= "Mraziarenske",
                    "^us$" = "Bus", "^alník$"="Valník", "^hladiarenske$"= "Chladiarenske", "^abína$" = "Kabína", "^imuzína$" = "Limuzína",
                    "^iftback$"= "Liftback", "^oadster$" = "Roadster", "^odvozok$" = "Podvozok", "^ick up$" = "Pick up", "^oupé$" = "Coupé" ))

items_separate2[,3]<- str_replace(items_separate2[,3], "x", "4x4" )

#joining corrected columns to the main dataset

autobazar_all <-cbind(autobazar_all, items_separate2[,2])
autobazar_all <-cbind(autobazar_all, items_separate2[,3])

autobazar_all <- autobazar_all[-12]

#to clean regions in wrong category "type", and replace it by the real type (type = car's body)

regions <- unique(autobazar_all$region_data)


for (i in 1:nrow(autobazar_all)){
  if(autobazar_all[i,11] %in% regions){
    autobazar_all[i,11] <-  autobazar_all[i,12]
  } else{
    next
  }
  
}


#numeric variables editing

autobazar_all$price_data <- str_remove(autobazar_all$price_data, "€")
autobazar_all$price_data <- str_remove_all(autobazar_all$price_data, " ")
autobazar_all$price_data <- as.numeric(autobazar_all$price_data)

autobazar_all$kw_data <- str_remove(autobazar_all$kw_data, "kw")
autobazar_all$kw_data <-  str_remove(autobazar_all$kw_data, " ")
autobazar_all$kw_data <- as.numeric(autobazar_all$kw_data)

autobazar_all$km_data <- str_remove(autobazar_all$km_data, "km")
autobazar_all$km_data <-  str_remove(autobazar_all$km_data, " ")
autobazar_all$km_data <- as.numeric(autobazar_all$km_data) 

#adding column quattro(4x4) as factor variable(0-1)

autobazar_all$quattro<-ifelse(autobazar_all$`items_separate2[, 3]` == "4x4", 1, 0)

#grouping uncommon car's body types into more common ones

autobazar_all$type_data<-str_replace_all(autobazar_all$type_data, c("^Valník$" = "Iné", "^Skriňa$" = "Dodávka", "^Mraziarenske$" = "Dodávka", "^Chladiarenske$" = "Dodávka"))

#replacing unknown category "x" by "Combi", only a few variables, inspected by a view -> all Combis in reality

autobazar_all$type_data<-str_replace_all(autobazar_all$type_data, c("^x$" = "Combi"))

#deleting variables that should not be included in the car's dataset (motorbikes...)

autobazar_all<-filter(autobazar_all, !model=="honda-africa" | !type_data == "Kabína" | !type_data == "Podvozok")

#editing names of brands and models into prettier and more correct way

autobazar_all$model <- str_replace_all(autobazar_all$model, "-", " ")
autobazar_all$brand <- str_replace_all(autobazar_all$brand, "-", " ")
autobazar_all$brand <- str_to_title(autobazar_all$brand)
autobazar_all$model <- str_to_title(autobazar_all$model)

#OUTLIERS(price)

autobazar_all%>%group_by(brand)%>%ggplot(.,aes(x=brand, y=price_data))+geom_boxplot()+coord_flip()
#treatment = rather NA than to delete
autobazar_all$price_data <- ifelse(autobazar_all$price_data > 750000, NA, autobazar_all$price_data)

#checking mileage outliers

max(autobazar_all$km_data, na.rm=T)
autobazar_all%>%ggplot(aes(brand, kw_data))+geom_point()+coord_flip()

#checking power outliers

max(autobazar_all$kw_data, na.rm=T)
autobazar_all%>%ggplot(aes(brand, km_data))+geom_point()+coord_flip()

#treatment for wrong power data = rather NA 

autobazar_all$kw_data <- ifelse(autobazar_all$kw_data > 900, NA, autobazar_all$kw_data)
max(autobazar_all$kw_data, na.rm=T)

#checking of unique values = if some categories wrong

unique(autobazar_all$type_data)
unique(autobazar_all$diesel_data)
unique(autobazar_all$transmission_data)
unique(autobazar_all$quattro)
unique(autobazar_all$region_data)
unique(autobazar_all$year_data)
unique(autobazar_all$brand)

#replacing wrong or missing categories as NA

autobazar_all$region_data <- ifelse(autobazar_all$region_data == "", NA, autobazar_all$region_data)
autobazar_all$type_data <- ifelse(autobazar_all$type_data == "" | autobazar_all$type_data == "Automat", NA, autobazar_all$type_data)
autobazar_all$diesel_data <- ifelse(autobazar_all$diesel_data == "" , NA, autobazar_all$diesel_data)

#deleting not needed cols anymore, making final dataset

autobazar <- autobazar_all[,c(1:11,14)]

#checking proportions of missing data in variables

visdat::vis_miss(autobazar)


#writing csv for shiny app

write_csv(autobazar, "autobazar_shiny_new.csv")
