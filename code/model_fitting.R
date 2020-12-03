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
fastfood0<-fastfood0[fastfood0$words!="",]
fastfood0$name<-str_remove(fastfood0$name, "[^\u0001-\u007F]")  # remove non English characters
colnames(fastfood0)[colnames(fastfood0)=="average_stars"]<-"user_average_stars"
brand_average_stars<-tapply(fastfood0$stars_x, fastfood0$business_id, mean) # shiny, boxplot
restaurant_average_stars<-tapply(fastfood0$stars_x, fastfood0$business_id, mean)
restaurant_review_number<-table(fastfood0$business_id) # shiny

# descriptive plots
par(mfrow=c(1,2))
barplot(table(fastfood0$stars_x), names.arg=1:5, cex.lab=1.3, cex.main=1.3, cex.axis=1.2,
        ylim=c(0,10000),
        xlab="Score",
        ylab="Count",
        main="The count of reviews at each score")

review_split<-str_split(fastfood0$text, " ")
review_length<-sapply(review_split, length)
review_length_new<-as.integer(review_length/40)
review_length_new[review_length_new>10]<-10
review_length_score<-tapply(fastfood0$stars_x, review_length_new, mean)
plot(40*0:10, review_length_score, type="b", lwd=2, cex.lab=1.3, cex.main=1.3, cex.axis=1.2, 
     xlab="Number of words in the review", 
     ylab="Average score",
     main="Relationship between review length and score")
par(mfrow=c(1,1))

get_word_freq_plot<-function(word){
  word_detect<-str_detect(fastfood0$text, paste0(" ", word, " "))
  word_freq<-tapply(word_detect, fastfood0$stars_x, mean)
  word_restaurant_freq<-tapply(word_detect, fastfood0$business_id, mean)
  barplot(word_freq, names.arg=1:5, cex.lab=1.5, cex.main=1.5, cex.axis=1.2, 
          xlab=paste0("Average score = ", 
                      round(mean(fastfood0$stars_x[word_detect]),2)),
          ylab="Frequency",
          main=paste0("Frequency of word ", word, " at different score"))
}

par(mfrow=c(1,3))
get_word_freq_plot("great")
get_word_freq_plot("the")
get_word_freq_plot("awful")
par(mfrow=c(1,1))

# linear regression by using word embedding matrix
fastfood<-fastfood0[, c(3, 16:(ncol(fastfood0)))]
fastfood_model<-lm(stars_x~., data=fastfood)
fastfood_model_summary<-summary(fastfood_model)$coefficients[-1,]
p_fdr<-p.adjust(fastfood_model_summary[,4], method = "fdr", n = length(fastfood_model_summary[,4]))
sig_list<-rownames(fastfood_model_summary[p_fdr<0.10, ])
coef_list<-fastfood_model_summary[, 1]
sign_list<-sign(coef_list)

# significant words and classification
safety_list<-c("grub", "fly", "hair", "undercooked", "raw", "fresh", "stale", "original")
taste_list<-c("delicious", "yum", "yummy", "tasty", "delish", "nom", "authentic", "juicy",
              "bland", "mediocre", "plain", "average", "tasteless", "dry")
clean_list<-c("clean", "dirty", "mess", "messy", "filthy")
service_list<-c("service", "friendly", "employee", "manager", "busy", "helpful", "polite", "patient",
                "management", "courteous", "rude", "unfriendly", "dumb", "hire", "understaffed", "disorganized")
speed_list<-c("quick", "quickly", "hour", "minute", "fast", "slow", "wait", "soon", "efficient", "speedy")
price_list<-c("expensive", "worth", "affordable", "overprice", "upcharge", "charge", "overcharge", "consistent", "skimpy", "skimp")

all_list<-list(safety_list, taste_list, clean_list, 
               service_list, speed_list, price_list)


all_restaurant<-unique(fastfood0$business_id)
all_rates<-matrix(NA, nrow=length(all_restaurant), ncol=length(all_list))
all_judge<-matrix(NA, nrow=length(all_restaurant), ncol=length(all_list))
all_number<-rep(NA, length(all_restaurant))
all_stars<-rep(NA, length(all_restaurant))
for (cur_num in 1:length(all_restaurant)){
  df<-fastfood0[fastfood0$business_id==all_restaurant[cur_num],]
  all_rates[cur_num, ]<-sapply(all_list, function(x){sum(t(as.matrix(df[, x])) * coef_list[x])/nrow(df)})
  all_judge[cur_num, ]<-sapply(all_list, function(x){sum(abs(as.matrix(df[, x])))!=0})
  all_number[cur_num]<-nrow(df)
  all_stars[cur_num]<-mean(df$stars_x)
  if(cur_num %% 10==0){print(cur_num)}
}



all_percentile<-data.frame(matrix(NA, nrow=length(all_restaurant), ncol=length(all_list)))
colnames(all_percentile)<-c("safety", "taste", "clean", 
                            "service", "speed", "price")
for (cur_col in 1:length(all_list)){
  all_percentile[as.logical(all_judge[, cur_col]), cur_col]<-rank(all_rates[as.logical(all_judge[, cur_col]), cur_col])/sum(all_judge[, cur_col])
}


restaurant_average_stars<-rep(NA, length(all_restaurant))
for (num in 1:length(all_restaurant)){
  restaurant_average_stars[num]<-mean(fastfood0$stars_x[fastfood0$business_id==all_restaurant[num]])
}

corr_estimate<-c()
corr_pval<-c()
for (cur_num in 1:ncol(all_percentile)){
  cur_percentile<-all_percentile[, cur_num]
  corr_test<-cor.test(cur_percentile[!is.na(cur_percentile)], restaurant_average_stars[!is.na(cur_percentile)])
  corr_estimate<-c(corr_estimate, corr_test$estimate)
  corr_pval<-c(corr_pval, corr_test$p.value)
}

corr_estimate
corr_pval

write.csv(cbind(all_restaurant, all_percentile), "../data/restaurant_score.csv", row.names=F)

restaurant_percentile<-cbind(stars=restaurant_average_stars[all_restaurant], all_percentile)
rownames(restaurant_percentile)<-1:nrow(restaurant_percentile)
restaurant_percentile_model<-lm(stars~., restaurant_percentile)
summary(restaurant_percentile_model)
car::vif(restaurant_percentile_model)
par(mfrow=c(1,4))
plot(restaurant_percentile_model, cex.main = 2, cex.lab=1.4)
par(mfrow=c(1,1))


restaurant<-as.data.frame(cbind(all_number, all_stars))
write.csv(cbind(all_restaurant, restaurant), "../data/restaurant_info.csv", row.names=F)
