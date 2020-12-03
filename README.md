# STAT628-Module3-Group5
 
## Member

Daiyi Shi (dshi42@wisc.edu)
Zijun Feng (zfeng66@wisc.edu)
Hanlin Tang (htang79@wisc.edu)

## Project Introduction
Our analysis focuses on the restaurants in PA, OH, WI, IL that are categorized as **fast food**. Among these restaurants, our specific goals are to find out which advantages and disadvantages they have from the reviews and their related star ratings by customers on Yelp and then give them corresponding suggestions of improving their star ratings.

## Conclusion

In conclusion, through the business data about fast food, we find out a bunch of meaningful words which significantly related with review stars, then divide them into 6 categories, each corresponding to an business aspect. Then, we set up an evaluation method for these aspects based on the count of words. Business owners can check the suggestion we provide in our [Shiny app](https://zijunfeng.shinyapps.io/yelp/) by selecting the brand name and address. If the restaurant performs worse than average in any aspect, then the owner should improve it to get better review star ratings from customers. We also give advice based on restaurant attributes, which are also useful.

## Repository Contents:
-	a `data` folder containing the cleaned data sets. `fastfood.csv` is the main data set.
-	a `code` folder containing all the code for analysis. The pipeline is `data_clean.py`, `word_embedding.py`, `model_fitting.R`, `get_attributes.R` and `attribute_regression.R`.
-	an `image` folder containing any figures/images/tables produced in analysis. The discriptions of figures in this folder can be found in correspoinding code comments.
-   a two-page executive **summary** file `summary.pdf`.

## Shiny App:
Link: <https://zijunfeng.shinyapps.io/yelp/>