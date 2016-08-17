library(dplyr)
library(sqldf)
library(scales)
setwd("C:/Users/tknutson001/Desktop/Data Science/github/project3")

m = read.csv('michelin_review_data_all.csv', header = TRUE, stringsAsFactors = FALSE)
m$review_date = as.Date(m$review_date, '%m/%d/%Y')

nm = read.csv('non_michelin_review_data_all.csv', header = TRUE, stringsAsFactors = FALSE)
nm$review_date = as.Date(nm$review_date, '%m/%d/%Y')

# need to add additional 'michelin year' column to classify reviews to particular rolling year

m$michelin_year <- ifelse(as.numeric(format(m$review_date, '%m')) < 10, 
                          as.numeric(format(m$review_date, '%Y')),
                          as.numeric(format(m$review_date, '%Y')) + 1)

nm$michelin_year <- ifelse(as.numeric(format(nm$review_date, '%m')) < 10, 
                          as.numeric(format(nm$review_date, '%Y')),
                          as.numeric(format(nm$review_date, '%Y')) + 1)


# import michelin ratings, filter out restaurants where no yelp page

s = read.csv('michelin_stars.csv', header = TRUE, stringsAsFactors = FALSE)
s = s[s$flag == 'Include',]

s$event_flag2 = s$event_flag

# add higher level grouping to event
s$event_flag2 = ifelse(s$event_flag %in% c("Gained 1 Star(s)", "Gained 2 Star(s)"), "Gained Star",
  ifelse(s$event_flag == "Lost 1 Star(s)", "Lost Star", "None"))


# create summary table by restaurant, by year aggregation

# first put annual figures on for each restaurant by year (don't calculate cumulative yet)

# unique(nm$restaurant_name)


t = sqldf('
select
m.michelin_year,
m.restaurant_name,
s.michelin_stars,
s.event_flag,
s.event_flag2,
NULL as reviews_cum,
NULL as one_star_cum,
NULL as two_star_cum,
NULL as three_star_cum,
NULL as four_star_cum,
NULL as five_star_cum,
NULL as five_proportion_cum,
NULL as positive_cum,
NULL as negative_cum,
NULL as pos_proportion_cum,
NULL as elite_cum,
NULL as elite_proportion_cum,
NULL as elite_pos_cum,
NULL as elite_neg_cum,
NULL as elite_pos_proportion_cum,
NULL as elite_neg_proportion_cum,
count(*) as reviews_ann,
sum(case when m.review_rating = 1 then 1.0 else 0 end) as one_star_ann,
sum(case when m.review_rating = 2 then 1.0 else 0 end) as two_star_ann,
sum(case when m.review_rating = 3 then 1.0 else 0 end) as three_star_ann,
sum(case when m.review_rating = 4 then 1.0 else 0 end) as four_star_ann,
sum(case when m.review_rating = 5 then 1.0 else 0 end) as five_star_ann,
cast(sum(case when m.review_rating = 5 then 1.0 else 0 end) / cast(count(*) as float) as float) as five_proportion_ann,
sum(case when m.review_rating in (4,5) then 1.0 else 0 end) as positive_ann,
sum(case when m.review_rating in (1,2) then 1.0 else 0 end) as negative_ann,
cast(sum(case when m.review_rating in (4,5) then 1.0 else 0 end) / cast(count(*) as float) as float) as pos_proportion_ann,
sum(case when m.is_elite = "Yes" then 1.0 else 0 end) as elite_ann,
cast(sum(case when m.is_elite = "Yes" then 1.0 else 0 end) / cast(count(*) as float) as float) as elite_proportion_ann,
sum(case when (m.review_rating in (4,5) and m.is_elite = "Yes") then 1.0 else 0 end) as elite_pos_ann,
sum(case when (m.review_rating in (1,2) and m.is_elite = "Yes") then 1.0 else 0 end) as elite_neg_ann,
cast(sum(case when (m.review_rating in (4,5) and m.is_elite = "Yes") then 1.0 else 0 end) / cast(count(*) as float) as float) as elite_pos_proportion_ann,
cast(sum(case when (m.review_rating in (1,2) and m.is_elite = "Yes") then 1.0 else 0 end) / cast(count(*) as float) as float) as elite_neg_proportion_ann,
NULL as pred_stars,
NULL as moe,
NULL as imputed_yelp_rating,
NULL as vorb
from m
inner join s on m.michelin_year = s.michelin_year and m.restaurant_name = s.restaurant_name
group by m.michelin_year, m.restaurant_name, s.michelin_stars, s.event_flag, s.event_flag2
      ;')
unique(m[m$restaurant_name == "Masas", "michelin_year"])



t2 = sqldf('
select
t.michelin_year,
t.restaurant_name,
t.michelin_stars,
t.event_flag,
t.event_flag2,
sum(t2.reviews_ann) as reviews_cum,
sum(t2.one_star_ann) as one_star_cum,
sum(t2.two_star_ann) as two_star_cum,
sum(t2.three_star_ann) as three_star_cum,
sum(t2.four_star_ann) as four_star_cum,
sum(t2.five_star_ann) as five_star_cum,
cast(cast(sum(t2.five_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) as five_proportion_cum,
sum(t2.positive_ann) as positive_cum,
sum(t2.negative_ann) as negative_cum,
cast(cast(sum(t2.positive_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) as pos_proportion_cum,
sum(t2.elite_ann) as elite_cum,
cast(cast(sum(t2.elite_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) as elite_proportion_cum,
sum(t2.elite_pos_ann) as elite_pos_cum,
sum(t2.elite_neg_ann) as elite_neg_cum,
cast(cast(sum(t2.elite_pos_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) as elite_pos_proportion_cum,
cast(cast(sum(t2.elite_neg_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) as elite_neg_proportion_cum,
t.reviews_ann,
t.one_star_ann,
t.two_star_ann,
t.three_star_ann,
t.four_star_ann,
t.five_star_ann,
t.five_proportion_ann,
t.positive_ann,
t.negative_ann,
t.pos_proportion_ann,
t.elite_ann,
t.elite_proportion_ann,
t.elite_pos_ann,
t.elite_neg_ann,
t.elite_pos_proportion_ann,
t.elite_neg_proportion_ann,
NULL as pred_stars,
cast(0.264/(sqrt(cast(sum(t2.reviews_ann) as float))+0.083) as float) as moe,
(cast(cast(sum(t2.one_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) * 1 +
cast(cast(sum(t2.two_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) * 2 + 
cast(cast(sum(t2.three_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) * 3 + 
cast(cast(sum(t2.four_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) * 4 + 
cast(cast(sum(t2.five_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) * 5)  as imputed_yelp_rating,
NULL as vorb
from t
inner join t t2 on t.restaurant_name = t2.restaurant_name and t.michelin_year >= t2.michelin_year
group by 
  t.michelin_year,
  t.restaurant_name,
  t.michelin_stars,
  t.event_flag,
  t.event_flag2,
  t.reviews_ann,
  t.one_star_ann,
  t.two_star_ann,
  t.three_star_ann,
  t.four_star_ann,
  t.five_star_ann,
  t.five_proportion_ann,
  t.positive_ann,
  t.negative_ann,
  t.pos_proportion_ann,
  t.elite_ann,
  t.elite_proportion_ann,
  t.elite_pos_ann,
  t.elite_neg_ann,
  t.elite_pos_proportion_ann,
  t.elite_neg_proportion_ann
;')

t3 = sqldf('
select
 t2.michelin_year,
 t2.restaurant_name,
 t2.michelin_stars,
 t2.event_flag,
 t2.event_flag2,
 t2.reviews_cum,
 t2.one_star_cum,
 t2.two_star_cum,
 t2.three_star_cum,
 t2.four_star_cum,
 t2.five_star_cum,
 t2.five_proportion_cum,
 t2.positive_cum,
 t2.negative_cum,
 t2.pos_proportion_cum,
 t2.elite_cum,
 t2.elite_proportion_cum,
 t2.elite_pos_cum,
 t2.elite_neg_cum,
 t2.elite_pos_proportion_cum,
 t2.elite_neg_proportion_cum,
 t2.reviews_ann,
 t2.one_star_ann,
 t2.two_star_ann,
 t2.three_star_ann,
 t2.four_star_ann,
 t2.five_star_ann,
 t2.five_proportion_ann,
 t2.positive_ann,
 t2.negative_ann,
 t2.pos_proportion_ann,
 t2.elite_ann,
 t2.elite_proportion_ann,
 t2.elite_pos_ann,
 t2.elite_neg_ann,
 t2.elite_pos_proportion_ann,
 t2.elite_neg_proportion_ann,
 2.784+(t2.imputed_yelp_rating+t2.five_proportion_cum)*0.176+LOG(t2.reviews_cum)*-0.328+LOG(t2.reviews_cum)*(t2.imputed_yelp_rating+t2.five_proportion_cum)*0.093 as pred_stars,
 t2.moe,
 t2.imputed_yelp_rating,
 .232*(max(1,LOG(t2.reviews_cum))*
((2.784+(t2.imputed_yelp_rating+t2.five_proportion_cum)*0.176+LOG(t2.reviews_cum)*-0.328+LOG(t2.reviews_cum)*(t2.imputed_yelp_rating+t2.five_proportion_cum)*0.093)-3.3)/t2.moe) as vorb
 from t2
 group by 
  t2.michelin_year,
  t2.restaurant_name,
  t2.michelin_stars,
  t2.event_flag,
  t2.event_flag2,
  t2.reviews_cum,
  t2.one_star_cum,
  t2.two_star_cum,
  t2.three_star_cum,
  t2.four_star_cum,
  t2.five_star_cum,
  t2.five_proportion_cum,
  t2.positive_cum,
  t2.negative_cum,
  t2.pos_proportion_cum,
  t2.elite_cum,
  t2.elite_proportion_cum,
  t2.elite_pos_cum,
  t2.elite_neg_cum,
  t2.elite_pos_proportion_cum,
  t2.elite_neg_proportion_cum,
  t2.reviews_ann,
  t2.one_star_ann,
  t2.two_star_ann,
  t2.three_star_ann,
  t2.four_star_ann,
  t2.five_star_ann,
  t2.five_proportion_ann,
  t2.positive_ann,
  t2.negative_ann,
  t2.pos_proportion_ann,
  t2.elite_ann,
  t2.elite_proportion_ann,
  t2.elite_pos_ann,
  t2.elite_neg_ann,
  t2.elite_pos_proportion_ann,
  t2.elite_neg_proportion_ann,
  t2.moe,
  t2.imputed_yelp_rating
 ;')


dif = sqldf('
select
  t3.michelin_year,
  t3.restaurant_name,
  t3.michelin_stars,
  t3.event_flag,
  t3.event_flag2,
  cast(cast(t3.reviews_cum as float) - cast(t3prior.reviews_cum as float) as float) as reviews_cum,
  cast(cast(t3.one_star_cum as float) - cast(t3prior.one_star_cum as float) as float) as one_star_cum,
  cast(cast(t3.two_star_cum as float) - cast(t3prior.two_star_cum as float) as float) as two_star_cum,
  cast(cast(t3.three_star_cum as float) - cast(t3prior.three_star_cum as float) as float) as three_star_cum,
  cast(cast(t3.four_star_cum as float) - cast(t3prior.four_star_cum as float) as float) as four_star_cum,
  cast(cast(t3.five_star_cum as float) - cast(t3prior.five_star_cum as float) as float) as five_star_cum,
  cast(cast(t3.five_proportion_cum as float) - cast(t3prior.five_proportion_cum as float) as float) as five_proportion_cum,
  cast(cast(t3.positive_cum as float) - cast(t3prior.positive_cum as float) as float) as positive_cum,
  cast(cast(t3.negative_cum as float) - cast(t3prior.negative_cum as float) as float) as negative_cum,
  cast(cast(t3.pos_proportion_cum as float) - cast(t3prior.pos_proportion_cum as float) as float) as pos_proportion_cum,
  cast(cast(t3.elite_cum as float) - cast(t3prior.elite_cum as float) as float) as elite_cum,
  cast(cast(t3.elite_proportion_cum as float) - cast(t3prior.elite_proportion_cum as float) as float) as elite_proportion_cum,
  cast(cast(t3.elite_pos_cum as float) - cast(t3prior.elite_pos_cum as float) as float) as elite_pos_cum,
  cast(cast(t3.elite_neg_cum as float) - cast(t3prior.elite_neg_cum as float) as float) as elite_neg_cum,
  cast(cast(t3.elite_pos_proportion_cum as float) - cast(t3prior.elite_pos_proportion_cum as float) as float) as elite_pos_proportion_cum,
  cast(cast(t3.elite_neg_proportion_cum as float) - cast(t3prior.elite_neg_proportion_cum as float) as float) as elite_neg_proportion_cum,
  cast(cast(t3.reviews_ann as float) - cast(t3prior.reviews_ann as float) as float) as reviews_ann,
  cast(cast(t3.one_star_ann as float) - cast(t3prior.one_star_ann as float) as float) as one_star_ann,
  cast(cast(t3.two_star_ann as float) - cast(t3prior.two_star_ann as float) as float) as two_star_ann,
  cast(cast(t3.three_star_ann as float) - cast(t3prior.three_star_ann as float) as float) as three_star_ann,
  cast(cast(t3.four_star_ann as float) - cast(t3prior.four_star_ann as float) as float) as four_star_ann,
  cast(cast(t3.five_star_ann as float) - cast(t3prior.five_star_ann as float) as float) as five_star_ann,
  cast(cast(t3.five_proportion_ann as float) - cast(t3prior.five_proportion_ann as float) as float) as five_proportion_ann,
  cast(cast(t3.positive_ann as float) - cast(t3prior.positive_ann as float) as float) as positive_ann,
  cast(cast(t3.negative_ann as float) - cast(t3prior.negative_ann as float) as float) as negative_ann,
  cast(cast(t3.pos_proportion_ann as float) - cast(t3prior.pos_proportion_ann as float) as float) as pos_proportion_ann,
  cast(cast(t3.elite_ann as float) - cast(t3prior.elite_ann as float) as float) as elite_ann,
  cast(cast(t3.elite_proportion_ann as float) - cast(t3prior.elite_proportion_ann as float) as float) as elite_proportion_ann,
  cast(cast(t3.elite_pos_ann as float) - cast(t3prior.elite_pos_ann as float) as float) as elite_pos_ann,
  cast(cast(t3.elite_neg_ann as float) - cast(t3prior.elite_neg_ann as float) as float) as elite_neg_ann,
  cast(cast(t3.elite_pos_proportion_ann as float) - cast(t3prior.elite_pos_proportion_ann as float) as float) as elite_pos_proportion_ann,
  cast(cast(t3.elite_neg_proportion_ann as float) - cast(t3prior.elite_neg_proportion_ann as float) as float) as elite_neg_proportion_ann,
  cast(cast(t3.pred_stars as float) - cast(t3prior.pred_stars as float) as float) as pred_stars,
  cast(cast(t3.moe as float) - cast(t3prior.moe as float) as float) as moe,
  cast(cast(t3.imputed_yelp_rating as float) - cast(t3prior.imputed_yelp_rating as float) as float) as imputed_yelp_rating,
  cast(cast(t3.vorb as float) - cast(t3prior.vorb as float) as float) as vorb,
  
  (cast(cast(t3.reviews_cum as float) - cast(t3prior.reviews_cum as float) as float))/ cast(t3prior.reviews_cum as float) as reviews_cum_chg,
  (cast(cast(t3.one_star_cum as float) - cast(t3prior.one_star_cum as float) as float)) / cast(t3prior.one_star_cum as float) as one_star_cum_chg,
  (cast(cast(t3.two_star_cum as float) - cast(t3prior.two_star_cum as float) as float)) / cast(t3prior.two_star_cum as float) as two_star_cum_chg,
  (cast(cast(t3.three_star_cum as float) - cast(t3prior.three_star_cum as float) as float)) / cast(t3prior.three_star_cum as float) as three_star_cum_chg,
  (cast(cast(t3.four_star_cum as float) - cast(t3prior.four_star_cum as float) as float)) / cast(t3prior.four_star_cum as float) as four_star_cum_chg,
  (cast(cast(t3.five_star_cum as float) - cast(t3prior.five_star_cum as float) as float)) / cast(t3prior.five_star_cum as float) as five_star_cum_chg,
  (cast(cast(t3.five_proportion_cum as float) - cast(t3prior.five_proportion_cum as float) as float)) / cast(t3prior.five_proportion_cum as float) as five_proportion_cum_chg,
  (cast(cast(t3.positive_cum as float) - cast(t3prior.positive_cum as float) as float)) / cast(t3prior.positive_cum as float) as positive_cum_chg,
  (cast(cast(t3.negative_cum as float) - cast(t3prior.negative_cum as float) as float)) / cast(t3prior.negative_cum as float) as negative_cum_chg,
  (cast(cast(t3.pos_proportion_cum as float) - cast(t3prior.pos_proportion_cum as float) as float)) / cast(t3prior.pos_proportion_cum as float) as pos_proportion_cum_chg,
  (cast(cast(t3.elite_cum as float) - cast(t3prior.elite_cum as float) as float)) / cast(t3prior.elite_cum as float) as elite_cum_chg,
  (cast(cast(t3.elite_proportion_cum as float) - cast(t3prior.elite_proportion_cum as float) as float)) / cast(t3prior.elite_proportion_cum as float) as elite_proportion_cum_chg,
  (cast(cast(t3.elite_pos_cum as float) - cast(t3prior.elite_pos_cum as float) as float)) / cast(t3prior.elite_pos_cum as float) as elite_pos_cum_chg,
  (cast(cast(t3.elite_neg_cum as float) - cast(t3prior.elite_neg_cum as float) as float)) / cast(t3prior.elite_neg_cum as float) as elite_neg_cum_chg,
  (cast(cast(t3.elite_pos_proportion_cum as float) - cast(t3prior.elite_pos_proportion_cum as float) as float)) / cast(t3prior.elite_pos_proportion_cum as float) as elite_pos_proportion_cum_chg,
  (cast(cast(t3.elite_neg_proportion_cum as float) - cast(t3prior.elite_neg_proportion_cum as float) as float)) / cast(t3prior.elite_neg_proportion_cum as float) as elite_neg_proportion_cum_chg,
  (cast(cast(t3.reviews_ann as float) - cast(t3prior.reviews_ann as float) as float)) / cast(t3prior.reviews_ann as float) as reviews_ann_chg,
  (cast(cast(t3.one_star_ann as float) - cast(t3prior.one_star_ann as float) as float)) / cast(t3prior.one_star_ann as float) as one_star_ann_chg,
  (cast(cast(t3.two_star_ann as float) - cast(t3prior.two_star_ann as float) as float)) / cast(t3prior.two_star_ann as float) as two_star_ann_chg,
  (cast(cast(t3.three_star_ann as float) - cast(t3prior.three_star_ann as float) as float)) / cast(t3prior.three_star_ann as float) as three_star_ann_chg,
  (cast(cast(t3.four_star_ann as float) - cast(t3prior.four_star_ann as float) as float)) / cast(t3prior.four_star_ann as float) as four_star_ann_chg,
  (cast(cast(t3.five_star_ann as float) - cast(t3prior.five_star_ann as float) as float)) / cast(t3prior.five_star_ann as float) as five_star_ann_chg,
  (cast(cast(t3.five_proportion_ann as float) - cast(t3prior.five_proportion_ann as float) as float)) / cast(t3prior.five_proportion_ann as float) as five_proportion_ann_chg,
  (cast(cast(t3.positive_ann as float) - cast(t3prior.positive_ann as float) as float)) / cast(t3prior.positive_ann as float) as positive_ann_chg,
  (cast(cast(t3.negative_ann as float) - cast(t3prior.negative_ann as float) as float)) / cast(t3prior.negative_ann as float) as negative_ann_chg,
  (cast(cast(t3.pos_proportion_ann as float) - cast(t3prior.pos_proportion_ann as float) as float)) / cast(t3prior.pos_proportion_ann as float) as pos_proportion_ann_chg,
  (cast(cast(t3.elite_ann as float) - cast(t3prior.elite_ann as float) as float)) / cast(t3prior.elite_ann as float) as elite_ann_chg,
  (cast(cast(t3.elite_proportion_ann as float) - cast(t3prior.elite_proportion_ann as float) as float)) / cast(t3prior.elite_proportion_ann as float) as elite_proportion_ann_chg,
  (cast(cast(t3.elite_pos_ann as float) - cast(t3prior.elite_pos_ann as float) as float)) / cast(t3prior.elite_pos_ann as float) as elite_pos_ann_chg,
  (cast(cast(t3.elite_neg_ann as float) - cast(t3prior.elite_neg_ann as float) as float)) / cast(t3prior.elite_neg_ann as float) as elite_neg_ann_chg,
  (cast(cast(t3.elite_pos_proportion_ann as float) - cast(t3prior.elite_pos_proportion_ann as float) as float)) / cast(t3prior.elite_pos_proportion_ann as float) as elite_pos_proportion_ann_chg,
  (cast(cast(t3.elite_neg_proportion_ann as float) - cast(t3prior.elite_neg_proportion_ann as float) as float)) / cast(t3prior.elite_neg_proportion_ann as float) as elite_neg_proportion_ann_chg,
  (cast(cast(t3.pred_stars as float) - cast(t3prior.pred_stars as float) as float)) / cast(t3prior.pred_stars as float) as pred_stars_chg,
  (cast(cast(t3.moe as float) - cast(t3prior.moe as float) as float)) / cast(t3prior.moe as float) as moe_chg,
  (cast(cast(t3.imputed_yelp_rating as float) - cast(t3prior.imputed_yelp_rating as float) as float)) / cast(t3prior.imputed_yelp_rating as float) as imputed_yelp_rating_chg,
  (cast(cast(t3.vorb as float) - cast(t3prior.vorb as float) as float)) / cast(t3prior.vorb as float) as vorb_chg
from t3 
inner join t3 t3prior on t3.restaurant_name = t3prior.restaurant_name and (t3.michelin_year - 1) = t3prior.michelin_year
group by 
  t3.michelin_year,
  t3.restaurant_name,
  t3.michelin_stars,
  t3.event_flag,
  t3.event_flag2
;')

# need to change the class of numerics that show up as characters

char_to_num <- c("reviews_cum", "one_star_cum", "two_star_cum", "three_star_cum", "four_star_cum", "five_star_cum", 
                 "five_proportion_cum", "positive_cum", "negative_cum", "pos_proportion_cum", "elite_cum", "elite_proportion_cum",
                 "elite_pos_cum", "elite_neg_cum", "elite_pos_proportion_cum", "elite_neg_proportion_cum", "pred_stars",
                 "imputed_yelp_rating", "vorb")

char_to_num_dif <- c(char_to_num, "elite_neg_cum_chg", "elite_neg_proportion_cum_chg", "elite_neg_ann_chg", "elite_neg_proportion_ann_chg")

t3[,char_to_num] = sapply(t3[,char_to_num], as.numeric)
dif[,char_to_num_dif] = sapply(dif[,char_to_num_dif], as.numeric)

# write.csv(t3, "test_join_summary.csv", row.names = TRUE)
# write.csv(dif, 'change_from_prior_year.csv', row.names =  TRUE)


# now repeat for non-michelin restaurants

t_nm = sqldf('
select
m.michelin_year,
m.restaurant_name,
NULL as michelin_stars,
NULL as event_flag,
NULL as event_flag2,
NULL as reviews_cum,
NULL as one_star_cum,
NULL as two_star_cum,
NULL as three_star_cum,
NULL as four_star_cum,
NULL as five_star_cum,
NULL as five_proportion_cum,
NULL as positive_cum,
NULL as negative_cum,
NULL as pos_proportion_cum,
NULL as elite_cum,
NULL as elite_proportion_cum,
NULL as elite_pos_cum,
NULL as elite_neg_cum,
NULL as elite_pos_proportion_cum,
NULL as elite_neg_proportion_cum,
count(*) as reviews_ann,
sum(case when m.review_rating = 1 then 1.0 else 0 end) as one_star_ann,
sum(case when m.review_rating = 2 then 1.0 else 0 end) as two_star_ann,
sum(case when m.review_rating = 3 then 1.0 else 0 end) as three_star_ann,
sum(case when m.review_rating = 4 then 1.0 else 0 end) as four_star_ann,
sum(case when m.review_rating = 5 then 1.0 else 0 end) as five_star_ann,
cast(sum(case when m.review_rating = 5 then 1.0 else 0 end) / cast(count(*) as float) as float) as five_proportion_ann,
sum(case when m.review_rating in (4,5) then 1.0 else 0 end) as positive_ann,
sum(case when m.review_rating in (1,2) then 1.0 else 0 end) as negative_ann,
cast(sum(case when m.review_rating in (4,5) then 1.0 else 0 end) / cast(count(*) as float) as float) as pos_proportion_ann,
sum(case when m.is_elite = "Yes" then 1.0 else 0 end) as elite_ann,
cast(sum(case when m.is_elite = "Yes" then 1.0 else 0 end) / cast(count(*) as float) as float) as elite_proportion_ann,
sum(case when (m.review_rating in (4,5) and m.is_elite = "Yes") then 1.0 else 0 end) as elite_pos_ann,
sum(case when (m.review_rating in (1,2) and m.is_elite = "Yes") then 1.0 else 0 end) as elite_neg_ann,
cast(sum(case when (m.review_rating in (4,5) and m.is_elite = "Yes") then 1.0 else 0 end) / cast(count(*) as float) as float) as elite_pos_proportion_ann,
cast(sum(case when (m.review_rating in (1,2) and m.is_elite = "Yes") then 1.0 else 0 end) / cast(count(*) as float) as float) as elite_neg_proportion_ann,
NULL as pred_stars,
NULL as moe,
NULL as imputed_yelp_rating,
NULL as vorb
from nm m
group by m.michelin_year, m.restaurant_name
;')



t2_nm = sqldf('
select
t.michelin_year,
t.restaurant_name,
t.michelin_stars,
t.event_flag,
t.event_flag2,
sum(t2.reviews_ann) as reviews_cum,
sum(t2.one_star_ann) as one_star_cum,
sum(t2.two_star_ann) as two_star_cum,
sum(t2.three_star_ann) as three_star_cum,
sum(t2.four_star_ann) as four_star_cum,
sum(t2.five_star_ann) as five_star_cum,
cast(cast(sum(t2.five_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) as five_proportion_cum,
sum(t2.positive_ann) as positive_cum,
sum(t2.negative_ann) as negative_cum,
cast(cast(sum(t2.positive_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) as pos_proportion_cum,
sum(t2.elite_ann) as elite_cum,
cast(cast(sum(t2.elite_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) as elite_proportion_cum,
sum(t2.elite_pos_ann) as elite_pos_cum,
sum(t2.elite_neg_ann) as elite_neg_cum,
cast(cast(sum(t2.elite_pos_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) as elite_pos_proportion_cum,
cast(cast(sum(t2.elite_neg_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) as elite_neg_proportion_cum,
t.reviews_ann,
t.one_star_ann,
t.two_star_ann,
t.three_star_ann,
t.four_star_ann,
t.five_star_ann,
t.five_proportion_ann,
t.positive_ann,
t.negative_ann,
t.pos_proportion_ann,
t.elite_ann,
t.elite_proportion_ann,
t.elite_pos_ann,
t.elite_neg_ann,
t.elite_pos_proportion_ann,
t.elite_neg_proportion_ann,
NULL as pred_stars,
cast(0.264/(sqrt(cast(sum(t2.reviews_ann) as float))+0.083) as float) as moe,
(cast(cast(sum(t2.one_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) * 1 +
cast(cast(sum(t2.two_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) * 2 + 
cast(cast(sum(t2.three_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) * 3 + 
cast(cast(sum(t2.four_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) * 4 + 
cast(cast(sum(t2.five_star_ann) as float) / cast(sum(t2.reviews_ann) as float) as float) * 5)  as imputed_yelp_rating,
NULL as vorb
from t_nm t
inner join t_nm t2 on t.restaurant_name = t2.restaurant_name and t.michelin_year >= t2.michelin_year
group by 
t.michelin_year,
t.restaurant_name,
t.michelin_stars,
t.event_flag,
t.event_flag2,
t.reviews_ann,
t.one_star_ann,
t.two_star_ann,
t.three_star_ann,
t.four_star_ann,
t.five_star_ann,
t.five_proportion_ann,
t.positive_ann,
t.negative_ann,
t.pos_proportion_ann,
t.elite_ann,
t.elite_proportion_ann,
t.elite_pos_ann,
t.elite_neg_ann,
t.elite_pos_proportion_ann,
t.elite_neg_proportion_ann
;')

t3_nm = sqldf('
select
t2.michelin_year,
t2.restaurant_name,
t2.michelin_stars,
t2.event_flag,
t2.event_flag2,
t2.reviews_cum,
t2.one_star_cum,
t2.two_star_cum,
t2.three_star_cum,
t2.four_star_cum,
t2.five_star_cum,
t2.five_proportion_cum,
t2.positive_cum,
t2.negative_cum,
t2.pos_proportion_cum,
t2.elite_cum,
t2.elite_proportion_cum,
t2.elite_pos_cum,
t2.elite_neg_cum,
t2.elite_pos_proportion_cum,
t2.elite_neg_proportion_cum,
t2.reviews_ann,
t2.one_star_ann,
t2.two_star_ann,
t2.three_star_ann,
t2.four_star_ann,
t2.five_star_ann,
t2.five_proportion_ann,
t2.positive_ann,
t2.negative_ann,
t2.pos_proportion_ann,
t2.elite_ann,
t2.elite_proportion_ann,
t2.elite_pos_ann,
t2.elite_neg_ann,
t2.elite_pos_proportion_ann,
t2.elite_neg_proportion_ann,
2.784+(t2.imputed_yelp_rating+t2.five_proportion_cum)*0.176+LOG(t2.reviews_cum)*-0.328+LOG(t2.reviews_cum)*(t2.imputed_yelp_rating+t2.five_proportion_cum)*0.093 as pred_stars,
t2.moe,
t2.imputed_yelp_rating,
.232*(max(1,LOG(t2.reviews_cum))*
((2.784+(t2.imputed_yelp_rating+t2.five_proportion_cum)*0.176+LOG(t2.reviews_cum)*-0.328+LOG(t2.reviews_cum)*(t2.imputed_yelp_rating+t2.five_proportion_cum)*0.093)-3.3)/t2.moe) as vorb
from t2_nm t2
group by 
t2.michelin_year,
t2.restaurant_name,
t2.michelin_stars,
t2.event_flag,
t2.event_flag2,
t2.reviews_cum,
t2.one_star_cum,
t2.two_star_cum,
t2.three_star_cum,
t2.four_star_cum,
t2.five_star_cum,
t2.five_proportion_cum,
t2.positive_cum,
t2.negative_cum,
t2.pos_proportion_cum,
t2.elite_cum,
t2.elite_proportion_cum,
t2.elite_pos_cum,
t2.elite_neg_cum,
t2.elite_pos_proportion_cum,
t2.elite_neg_proportion_cum,
t2.reviews_ann,
t2.one_star_ann,
t2.two_star_ann,
t2.three_star_ann,
t2.four_star_ann,
t2.five_star_ann,
t2.five_proportion_ann,
t2.positive_ann,
t2.negative_ann,
t2.pos_proportion_ann,
t2.elite_ann,
t2.elite_proportion_ann,
t2.elite_pos_ann,
t2.elite_neg_ann,
t2.elite_pos_proportion_ann,
t2.elite_neg_proportion_ann,
t2.moe,
t2.imputed_yelp_rating
;')


dif_nm = sqldf('
select
t3.michelin_year,
t3.restaurant_name,
t3.michelin_stars,
t3.event_flag,
t3.event_flag2,
cast(cast(t3.reviews_cum as float) - cast(t3prior.reviews_cum as float) as float) as reviews_cum,
cast(cast(t3.one_star_cum as float) - cast(t3prior.one_star_cum as float) as float) as one_star_cum,
cast(cast(t3.two_star_cum as float) - cast(t3prior.two_star_cum as float) as float) as two_star_cum,
cast(cast(t3.three_star_cum as float) - cast(t3prior.three_star_cum as float) as float) as three_star_cum,
cast(cast(t3.four_star_cum as float) - cast(t3prior.four_star_cum as float) as float) as four_star_cum,
cast(cast(t3.five_star_cum as float) - cast(t3prior.five_star_cum as float) as float) as five_star_cum,
cast(cast(t3.five_proportion_cum as float) - cast(t3prior.five_proportion_cum as float) as float) as five_proportion_cum,
cast(cast(t3.positive_cum as float) - cast(t3prior.positive_cum as float) as float) as positive_cum,
cast(cast(t3.negative_cum as float) - cast(t3prior.negative_cum as float) as float) as negative_cum,
cast(cast(t3.pos_proportion_cum as float) - cast(t3prior.pos_proportion_cum as float) as float) as pos_proportion_cum,
cast(cast(t3.elite_cum as float) - cast(t3prior.elite_cum as float) as float) as elite_cum,
cast(cast(t3.elite_proportion_cum as float) - cast(t3prior.elite_proportion_cum as float) as float) as elite_proportion_cum,
cast(cast(t3.elite_pos_cum as float) - cast(t3prior.elite_pos_cum as float) as float) as elite_pos_cum,
cast(cast(t3.elite_neg_cum as float) - cast(t3prior.elite_neg_cum as float) as float) as elite_neg_cum,
cast(cast(t3.elite_pos_proportion_cum as float) - cast(t3prior.elite_pos_proportion_cum as float) as float) as elite_pos_proportion_cum,
cast(cast(t3.elite_neg_proportion_cum as float) - cast(t3prior.elite_neg_proportion_cum as float) as float) as elite_neg_proportion_cum,
cast(cast(t3.reviews_ann as float) - cast(t3prior.reviews_ann as float) as float) as reviews_ann,
cast(cast(t3.one_star_ann as float) - cast(t3prior.one_star_ann as float) as float) as one_star_ann,
cast(cast(t3.two_star_ann as float) - cast(t3prior.two_star_ann as float) as float) as two_star_ann,
cast(cast(t3.three_star_ann as float) - cast(t3prior.three_star_ann as float) as float) as three_star_ann,
cast(cast(t3.four_star_ann as float) - cast(t3prior.four_star_ann as float) as float) as four_star_ann,
cast(cast(t3.five_star_ann as float) - cast(t3prior.five_star_ann as float) as float) as five_star_ann,
cast(cast(t3.five_proportion_ann as float) - cast(t3prior.five_proportion_ann as float) as float) as five_proportion_ann,
cast(cast(t3.positive_ann as float) - cast(t3prior.positive_ann as float) as float) as positive_ann,
cast(cast(t3.negative_ann as float) - cast(t3prior.negative_ann as float) as float) as negative_ann,
cast(cast(t3.pos_proportion_ann as float) - cast(t3prior.pos_proportion_ann as float) as float) as pos_proportion_ann,
cast(cast(t3.elite_ann as float) - cast(t3prior.elite_ann as float) as float) as elite_ann,
cast(cast(t3.elite_proportion_ann as float) - cast(t3prior.elite_proportion_ann as float) as float) as elite_proportion_ann,
cast(cast(t3.elite_pos_ann as float) - cast(t3prior.elite_pos_ann as float) as float) as elite_pos_ann,
cast(cast(t3.elite_neg_ann as float) - cast(t3prior.elite_neg_ann as float) as float) as elite_neg_ann,
cast(cast(t3.elite_pos_proportion_ann as float) - cast(t3prior.elite_pos_proportion_ann as float) as float) as elite_pos_proportion_ann,
cast(cast(t3.elite_neg_proportion_ann as float) - cast(t3prior.elite_neg_proportion_ann as float) as float) as elite_neg_proportion_ann,
cast(cast(t3.pred_stars as float) - cast(t3prior.pred_stars as float) as float) as pred_stars,
cast(cast(t3.moe as float) - cast(t3prior.moe as float) as float) as moe,
cast(cast(t3.imputed_yelp_rating as float) - cast(t3prior.imputed_yelp_rating as float) as float) as imputed_yelp_rating,
cast(cast(t3.vorb as float) - cast(t3prior.vorb as float) as float) as vorb,

(cast(cast(t3.reviews_cum as float) - cast(t3prior.reviews_cum as float) as float))/ cast(t3prior.reviews_cum as float) as reviews_cum_chg,
(cast(cast(t3.one_star_cum as float) - cast(t3prior.one_star_cum as float) as float)) / cast(t3prior.one_star_cum as float) as one_star_cum_chg,
(cast(cast(t3.two_star_cum as float) - cast(t3prior.two_star_cum as float) as float)) / cast(t3prior.two_star_cum as float) as two_star_cum_chg,
(cast(cast(t3.three_star_cum as float) - cast(t3prior.three_star_cum as float) as float)) / cast(t3prior.three_star_cum as float) as three_star_cum_chg,
(cast(cast(t3.four_star_cum as float) - cast(t3prior.four_star_cum as float) as float)) / cast(t3prior.four_star_cum as float) as four_star_cum_chg,
(cast(cast(t3.five_star_cum as float) - cast(t3prior.five_star_cum as float) as float)) / cast(t3prior.five_star_cum as float) as five_star_cum_chg,
(cast(cast(t3.five_proportion_cum as float) - cast(t3prior.five_proportion_cum as float) as float)) / cast(t3prior.five_proportion_cum as float) as five_proportion_cum_chg,
(cast(cast(t3.positive_cum as float) - cast(t3prior.positive_cum as float) as float)) / cast(t3prior.positive_cum as float) as positive_cum_chg,
(cast(cast(t3.negative_cum as float) - cast(t3prior.negative_cum as float) as float)) / cast(t3prior.negative_cum as float) as negative_cum_chg,
(cast(cast(t3.pos_proportion_cum as float) - cast(t3prior.pos_proportion_cum as float) as float)) / cast(t3prior.pos_proportion_cum as float) as pos_proportion_cum_chg,
(cast(cast(t3.elite_cum as float) - cast(t3prior.elite_cum as float) as float)) / cast(t3prior.elite_cum as float) as elite_cum_chg,
(cast(cast(t3.elite_proportion_cum as float) - cast(t3prior.elite_proportion_cum as float) as float)) / cast(t3prior.elite_proportion_cum as float) as elite_proportion_cum_chg,
(cast(cast(t3.elite_pos_cum as float) - cast(t3prior.elite_pos_cum as float) as float)) / cast(t3prior.elite_pos_cum as float) as elite_pos_cum_chg,
(cast(cast(t3.elite_neg_cum as float) - cast(t3prior.elite_neg_cum as float) as float)) / cast(t3prior.elite_neg_cum as float) as elite_neg_cum_chg,
(cast(cast(t3.elite_pos_proportion_cum as float) - cast(t3prior.elite_pos_proportion_cum as float) as float)) / cast(t3prior.elite_pos_proportion_cum as float) as elite_pos_proportion_cum_chg,
(cast(cast(t3.elite_neg_proportion_cum as float) - cast(t3prior.elite_neg_proportion_cum as float) as float)) / cast(t3prior.elite_neg_proportion_cum as float) as elite_neg_proportion_cum_chg,
(cast(cast(t3.reviews_ann as float) - cast(t3prior.reviews_ann as float) as float)) / cast(t3prior.reviews_ann as float) as reviews_ann_chg,
(cast(cast(t3.one_star_ann as float) - cast(t3prior.one_star_ann as float) as float)) / cast(t3prior.one_star_ann as float) as one_star_ann_chg,
(cast(cast(t3.two_star_ann as float) - cast(t3prior.two_star_ann as float) as float)) / cast(t3prior.two_star_ann as float) as two_star_ann_chg,
(cast(cast(t3.three_star_ann as float) - cast(t3prior.three_star_ann as float) as float)) / cast(t3prior.three_star_ann as float) as three_star_ann_chg,
(cast(cast(t3.four_star_ann as float) - cast(t3prior.four_star_ann as float) as float)) / cast(t3prior.four_star_ann as float) as four_star_ann_chg,
(cast(cast(t3.five_star_ann as float) - cast(t3prior.five_star_ann as float) as float)) / cast(t3prior.five_star_ann as float) as five_star_ann_chg,
(cast(cast(t3.five_proportion_ann as float) - cast(t3prior.five_proportion_ann as float) as float)) / cast(t3prior.five_proportion_ann as float) as five_proportion_ann_chg,
(cast(cast(t3.positive_ann as float) - cast(t3prior.positive_ann as float) as float)) / cast(t3prior.positive_ann as float) as positive_ann_chg,
(cast(cast(t3.negative_ann as float) - cast(t3prior.negative_ann as float) as float)) / cast(t3prior.negative_ann as float) as negative_ann_chg,
(cast(cast(t3.pos_proportion_ann as float) - cast(t3prior.pos_proportion_ann as float) as float)) / cast(t3prior.pos_proportion_ann as float) as pos_proportion_ann_chg,
(cast(cast(t3.elite_ann as float) - cast(t3prior.elite_ann as float) as float)) / cast(t3prior.elite_ann as float) as elite_ann_chg,
(cast(cast(t3.elite_proportion_ann as float) - cast(t3prior.elite_proportion_ann as float) as float)) / cast(t3prior.elite_proportion_ann as float) as elite_proportion_ann_chg,
(cast(cast(t3.elite_pos_ann as float) - cast(t3prior.elite_pos_ann as float) as float)) / cast(t3prior.elite_pos_ann as float) as elite_pos_ann_chg,
(cast(cast(t3.elite_neg_ann as float) - cast(t3prior.elite_neg_ann as float) as float)) / cast(t3prior.elite_neg_ann as float) as elite_neg_ann_chg,
(cast(cast(t3.elite_pos_proportion_ann as float) - cast(t3prior.elite_pos_proportion_ann as float) as float)) / cast(t3prior.elite_pos_proportion_ann as float) as elite_pos_proportion_ann_chg,
(cast(cast(t3.elite_neg_proportion_ann as float) - cast(t3prior.elite_neg_proportion_ann as float) as float)) / cast(t3prior.elite_neg_proportion_ann as float) as elite_neg_proportion_ann_chg,
(cast(cast(t3.pred_stars as float) - cast(t3prior.pred_stars as float) as float)) / cast(t3prior.pred_stars as float) as pred_stars_chg,
(cast(cast(t3.moe as float) - cast(t3prior.moe as float) as float)) / cast(t3prior.moe as float) as moe_chg,
(cast(cast(t3.imputed_yelp_rating as float) - cast(t3prior.imputed_yelp_rating as float) as float)) / cast(t3prior.imputed_yelp_rating as float) as imputed_yelp_rating_chg,
(cast(cast(t3.vorb as float) - cast(t3prior.vorb as float) as float)) / cast(t3prior.vorb as float) as vorb_chg
from t3_nm t3
inner join t3_nm t3prior on t3.restaurant_name = t3prior.restaurant_name and (t3.michelin_year - 1) = t3prior.michelin_year
group by 
t3.michelin_year,
t3.restaurant_name,
t3.michelin_stars,
t3.event_flag,
t3.event_flag2
;')

# need to change the class of numerics that show up as characters

t3_nm[,char_to_num] = sapply(t3_nm[,char_to_num], as.numeric)
dif_nm[,char_to_num_dif] = sapply(dif_nm[,char_to_num_dif], as.numeric)


# write.csv(t3_nm, "test_join_summary_nm.csv", row.names = TRUE)
# write.csv(dif_nm, 'change_from_prior_year_nm.csv', row.names =  TRUE)


# create grouping table by michelin rating to establish baseline
#t3[t3$michelin_year == 2016 & t3$michelin_stars == 0 & t3$reviews_cum == 1581, c("reviews_cum", "restaurant_name")]
  m_baseline_cur <- 
  t3 %>% 
  group_by(michelin_stars) %>% 
  filter(michelin_year == 2016) %>% 
  summarise(avg_pred_yelp_rating = mean(pred_stars, na.rm = TRUE), avg_yelp_rating = mean(imputed_yelp_rating, na.rm = TRUE), avg_total_reviews = mean(reviews_cum, na.rm = TRUE), avg_elite_reviews = mean(elite_cum, na.rm = TRUE), avg_vorb = mean(vorb, na.rm = TRUE), count = sum(n()), avg_five_star_count = mean(five_star_cum, na.rm = TRUE), min_total_reviews = min(reviews_cum, na.rm = TRUE), max_total_reviews = max(reviews_cum, na.rm = TRUE))
unique(t3[t3$michelin_year == 2016,"restaurant_name"])
unique(t3[t3$restaurant_name == "Masas","michelin_year"])
unique(m[m$restaurant_name == "Masas","michelin_year"])
unique(s[s$restaurant_name == "Masas","michelin_year"])

#write.csv(unique(t3[c("restaurant_name", "michelin_year")]), "prob_w_2016.csv", row.names =  TRUE)
# write.csv(m_baseline_cur, "2016_baseline_michelin.csv", row.names =  TRUE)

m_baseline_all <- 
  t3 %>% 
  group_by(michelin_stars) %>% 
  #filter(michelin_year == 2016) %>% 
  summarise(avg_pred_yelp_rating = mean(pred_stars, na.rm = TRUE), avg_yelp_rating = mean(imputed_yelp_rating, na.rm = TRUE), avg_total_reviews = mean(reviews_cum, na.rm = TRUE), avg_elite_reviews = mean(elite_cum, na.rm = TRUE), avg_vorb = mean(vorb, na.rm = TRUE), count = sum(n()))

# m_baseline_all

# create grouping table for restaurants with no michelin stars since 2010 with many reviews / high yelp rating

nm_baseline_cur <-
  t3_nm %>% 
  filter(michelin_year == 2016) %>% 
  summarise(avg_pred_yelp_rating = mean(pred_stars, na.rm = TRUE), avg_yelp_rating = mean(imputed_yelp_rating, na.rm = TRUE), avg_total_reviews = mean(reviews_cum, na.rm = TRUE), avg_elite_reviews = mean(elite_cum, na.rm = TRUE), avg_vorb = mean(vorb, na.rm = TRUE))

# nm_baseline_cur

# create grouping table for restaurants with at least 1 star since 2010 grouped by event type, net changes and % changes for metrics

m_dif_event <-
  dif %>%
  group_by(event_flag2) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

# write.csv(m_dif_event, 'michelin_dif_event.csv', row.names = TRUE)
# write.csv(m_baseline_cur, 'michelin_2016_baseline.csv', row.names = TRUE)

nm_names = names(t3_nm)
nm_dif_names = names(dif_nm)

# nm_names
# nm_dif_names


nm_2016 <- sqldf('
select
b.restaurant_name,
b.reviews_cum,
b.five_proportion_cum,
b.elite_cum,
b.pred_stars,
d.elite_ann_chg
from t3_nm b
inner join dif_nm d on b.restaurant_name = d.restaurant_name and b.michelin_year = d.michelin_year
where b.michelin_year = 2016
')

# nm_2016

# how many reviews did first timers in 2016 have (in 2015)?

first_time_names <- t3[t3$michelin_year == 2016 & t3$event_flag2 == "Gained Star" & t3$michelin_stars == 1,c("restaurant_name")]
first_time_2016 <- t3[t3$michelin_year == 2015 & t3$restaurant_name %in% first_time_names, c("restaurant_name", "reviews_cum", "pred_stars", "elite_cum")]

# summary(first_time_2016)
# first_time_2016

