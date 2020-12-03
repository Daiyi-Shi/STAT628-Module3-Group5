data = read.csv('../fastfood_attributes.csv')
sub_data = data[, -c(1, 2)]

fill_na = function(column, value){
  sub_data[is.na(sub_data[column]),column] = value
  # sub_data[sub_data[column] == 'None',column] = value
  return(sub_data)
}

sub_data[sub_data=='None'] = NA

sub_data = fill_na('WiFi', 'no')
sub_data = fill_na('RestaurantsPriceRange2', '1')
sub_data = fill_na('RestaurantsAttire', 'casual')
sub_data = fill_na('NoiseLevel', 'average')
sub_data = fill_na('Alcohol', 'none')
# sub_data[sub_data['Alcohol'] == 'None', 'Alcohol'] = 'none'

sub_data[is.na(sub_data)] = 'False'

sub_data[sub_data['NoiseLevel'] == 'very', 'NoiseLevel'] = 'loud'
sub_data[sub_data['NoiseLevel'] == 'quiet', 'NoiseLevel'] = 'average'

sub_data[sub_data['Alcohol'] == 'beer', 'Alcohol'] = 'full'

sub_data[sub_data['RestaurantsAttire'] == 'normal', 'RestaurantsAttire'] = 'casual'
sub_data[sub_data['RestaurantsAttire'] == 'dressy', 'RestaurantsAttire'] = 'formal'

sub_data[sub_data['WiFi'] == 'paid', 'WiFi'] = 'free'

sub_data[sub_data['RestaurantsPriceRange2'] == 2, 'RestaurantsPriceRange2'] = 1
sub_data[sub_data['RestaurantsPriceRange2'] == 3, 'RestaurantsPriceRange2'] = 4


sub_data = droplevels(sub_data)
# model = lm(average_stars~., data=sub_data)
model = lm(star_y~., data=sub_data)
summary(model)

fastfood_model_summary<-summary(model)$coefficients[-1,]
p_fdr<-p.adjust(fastfood_model_summary[,4], method = "fdr", n = length(fastfood_model_summary[,4]))
sig_list<-rownames(fastfood_model_summary[p_fdr<0.10, ])
sig_list

sig_col_list = c('BikeParking', 'Caters', 'OutdoorSeating', 'RestaurantsDelivery', 'RestaurantsTableService', 'WheelchairAccessible')
for(sig in sig_col_list){
  filter_data = data[!is.na(data[sig]),]
  print(paste(sig, t.test(filter_data[filter_data[sig] == 'True', 'star_y'], 
         filter_data[filter_data[sig] == 'False', 'star_y'])$p.value))
}

filter_data = data[!is.na(data$NoiseLevel),]
t.test(filter_data[(filter_data$NoiseLevel == 'loud' | filter_data$NoiseLevel == 'very'), 'star_y'], 
       filter_data[(filter_data$NoiseLevel == 'average' | filter_data$NoiseLevel == 'quiet'), 'star_y'])

sig_col_list = c('WheelchairAccessible')

filter_data = sub_data[!is.na(data['WheelchairAccessible']),]
filter_model = lm(star_y~., data=filter_data)
summary(filter_model)

