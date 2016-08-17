library(ggplot2)

# So we will check significance of reviews_cum, five_star_ann_chg, elite_ann_chg

names(m_dif_event)
cols <- names(m_dif_event)[c(1,5,6,11,12,38,63,68,74)]

# removing unneeded columns

dif_subset <- dif[,cols]

# translating event flag to 1, 0, -1

dif_subset$event_flag = ifelse(
  dif_subset$event_flag == "None", 0,
  ifelse(
    dif_subset$event_flag == "Gained 1 Star(s)", 1,
    ifelse(
      dif_subset$event_flag == "Lost 1 Star(s)", -1, 2
    )))

dif_subset$event_flag2 = ifelse(dif_subset$event_flag %in% c(1,2), 1, dif_subset$event_flag)

# let's compare 2 sample t test with gained star events vs no change events (sample size is small for lost events)


t.gain.elite_ann_chg <- dif_subset[dif_subset$event_flag2 == 1,"elite_ann_chg"]
t.none.elite_ann_chg <- dif_subset[dif_subset$event_flag2 == 0,"elite_ann_chg"]

t.test(t.gain.elite_ann_chg, t.none.elite_ann_chg, alternative = "greater")

Welch Two Sample t-test

data:  t.gain.elite_ann_chg and t.none.elite_ann_chg
t = 2.6138, df = 48.33, p-value = 0.005952
alternative hypothesis: true difference in means is greater than 0
95 percent confidence interval:
  0.3561711       Inf
sample estimates:
  mean of x mean of y 
1.3172140 0.3234178 
              


dif_nm_subset <- dif_nm[dif_nm$michelin_year == 2016,c(2,68)]
dif_nm_subset

next_star <- sqldf('
select
t3_nm.restaurant_name,
d.elite_ann_chg,
t3_nm.pred_stars,
t3_nm.reviews_cum
from t3_nm
inner join dif_nm_subset d on t3_nm.restaurant_name = d.restaurant_name
where t3_nm.michelin_year = 2016
      ;')

t3_nm[t3_nm$restaurant_name == "Trestle",]

#write.csv(next_star,"next_michelin_star.csv", row.names = TRUE)

g <- ggplot(dif_subset, aes(x = as.factor(event_flag2), y = elite_ann_chg))#, fill = as.factor(event_flag2)))
g + geom_boxplot(fill=c('#8F8F8F', '#A5CCED', '#066BB0'))+
  labs(title="Annual % Change in Elite Reviews",x="Michelin Change Event", y = "% Change in Elite Reviews") +
  theme_minimal() +
  scale_x_discrete(labels = c("Lost Star(s)", "No Change", "Gained Star(s)")) +
  scale_y_continuous(labels = percent, limits = c(-0.3,5))+
  geom_text(data = boxgroup, aes(label = sprintf("%1.0f%%", 100*avg_elite), y = avg_elite))
ggsave("BoxPlot.wmf", height = 3.83, width = 10.8)
 

# boxgroup <- dif_subset %>% group_by(event_flag2) %>% summarise(avg_elite = mean(elite_ann_chg, na.rm = TRUE))
# boxgroup
# write.csv(dif_subset, "dif_subset_boxplot.csv", row.names = TRUE)
