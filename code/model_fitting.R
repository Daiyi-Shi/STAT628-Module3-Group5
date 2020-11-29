# load package
if (!require("data.table")) {
  install.packages("data.table")
  stopifnot(require("data.table"))
}

# read data
fastfood0<-as.data.frame(fread("../data/fastfood_embedded.csv"))

# linear regression with scaled stars
fastfood0$star_scaled=fastfood0$stars_x-fastfood0$average_stars
fastfood<-fastfood0[, c(16:ncol(fastfood0))]
fastfood_model<-lm(star_scaled~., data=fastfood)
fastfood_model_summary<-summary(fastfood_model)$coefficients[-1,]
p_fdr<-p.adjust(fastfood_model_summary[,4], method = "fdr", n = length(fastfood_model_summary[,4]))
sig_list<-rownames(fastfood_model_summary[p_fdr<0.10, ])

# linear regression with unscaled stars
fastfood2<-fastfood0[, c(3, 16:(ncol(fastfood0)-1))]
fastfood_model2<-lm(stars_x~., data=fastfood2)
fastfood_model_summary2<-summary(fastfood_model2)$coefficients[-1,]
p_fdr2<-p.adjust(fastfood_model_summary2[,4], method = "fdr", n = length(fastfood_model_summary2[,4]))
sig_list2<-rownames(fastfood_model_summary2[p_fdr2<0.10, ])

# significant & meaningful words (using unscaled stars):
# environment: clean dirty accommodate mess messy filthy sit wifi
# quality: fresh cold hot original stale scratch grub fly hair health smell undercooked raw skimpy skimp
# taste: delicious yum yummy tasty bland mediocre plain average juicy authentic delish creative consistent tasteless gross nom
# type: whip brisket pizza goat(cheese) sweet olive bison crisp vanilla salmon
# service: service friendly employee manager busy helpful polite patient management courteous rude unfriendly behavior dumb
# speed: quick quickly hour minute fast slow wait soon efficient speedy
# price: expensive worth affordable overprice upcharge charge overcharge
# others: drive deliver curb university rain
# (FDR < 0.10)
