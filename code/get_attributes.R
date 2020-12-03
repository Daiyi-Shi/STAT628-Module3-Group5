# load package
if (!require("data.table")) {
  install.packages("data.table")
  stopifnot(require("data.table"))
}

if (!require("stringr")) {
  install.packages("stringr")
  stopifnot(require("stringr"))
}

# read data
fastfood0<-as.data.frame(fread("../data/fastfood_embedded.csv"))
fastfood0$name<-str_remove(fastfood0$name, "[^\u0001-\u007F]")  # remove non English characters
colnames(fastfood0)[colnames(fastfood0)=="average_stars"]<-"user_average_stars"
brand_average_stars<-tapply(fastfood0$stars_x, fastfood0$business_id, mean) # shiny, boxplot
restaurant_average_stars<-tapply(fastfood0$stars_x, fastfood0$business_id, mean)
restaurant_review_number<-table(fastfood0$business_id) # shiny

# attributes extraction
fastfood_attributes<-unique(fastfood0[, c("business_id", "name", "address", "city", "state", "postal_code", "attributes", "stars_y")])
attribute_counts<-table(unlist(str_extract_all(fastfood_attributes$attributes, "(?<=')[A-Z][^']+(?=':)")))
all_attributes<-names(attribute_counts[attribute_counts>100][c(5, 6, 12, 13, 15, 19)])

restaurant_attributes<-as.data.frame(matrix(NA, nrow=nrow(fastfood_attributes), ncol=length(all_attributes)))
colnames(restaurant_attributes)<-all_attributes

for (cur_attr in all_attributes){
  restaurant_attributes[, cur_attr]<-unlist(str_extract(fastfood_attributes$attributes, paste0("(?<=", cur_attr, "': \"{0,2}u{0,1}')[a-zA-Z0-9]+")))
  print(cur_attr)
}

restaurant_attributes<-cbind(fastfood_attributes[, c("business_id", "name", "address", "city", "state", "postal_code", "stars_y")], restaurant_attributes)
write.csv(restaurant_attributes, "../data/fastfood_attributes.csv", row.names = F)


