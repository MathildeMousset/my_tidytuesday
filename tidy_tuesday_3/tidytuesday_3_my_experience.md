---
title: "Tutorial"
author: "Mathilde Mousset"
date: "23 janvier 2019"
output: 
  html_document: 
    keep_md: yes
    theme: cosmo
    toc: yes
  prettydoc::html_pretty:
    theme: cayman
    highlight: github
editor_options: 
  chunk_output_type: console
---

# Some thoughs about the #Tidytuesday challenge

This week, I decided to try the [#Tidytuesday](https://github.com/rfordatascience/tidytuesday)twitter challenge. Every week, Thomas Mock provides a mostly clean dataset (or set of dataset) for the community to analyse. Past dataset have covered a variety of subjects, from  New York restaurant grades to tweets from the #rstat hashtag, and they provide excellent opportunity for everybody to try their hands on new datasets.  

I come from an experimental background in plant biology. My typical workflow is to think about some interesting questions that I want to answer, design an experiment, perform it, and analyse the data. I usually have in mind the statistical model I want to use to analyse the data. It does not matter if reality checks in and that model is no longer adequate, but it helps design good experiments, and avoid the pitfall of "oh, we did not think this really through". So I rarely come across a dataset where I am not **deeply** familiar with all columns (because I spend hours measuring the plants, in a design that I setup myself). 

Recently, I became interested in analysing other's data. Data that seems to be everywhere on the internet. So many great questions to ask. However, I quickly realized that they are many challenges associated with this kind of data: they come in shapes that are not necessarily tidy, they often involve lot's of text, and there is often an overload of information. All of this makes it hard to find relevant questions and address them. 

The #Tidytuesday challenge provides me with training opportunities :   

- Unknown datasets of diverse shapes and focus: this leads me to **learn** new tools, get **fluent** at using the ones I seldom use, get **intuitive** with the ones I already use often.  

- A dataset every week: for me the aim is not to perform a detailed statistical analysis, but to get better at quickly discovering a new dataset: optimal cleaning and reshaping, plotting, finding the big meaningful messages (or some it them). Learning not to get lost in the data, not to obsess with going into details for all piece of information. Regular dataset force me to get working on new things.  

- Learn from the community: seeing a lot of people sharing their ideas, figures, code is an incredible opportunity to learn, beside the motivation boost.  

# Space launches

[This week's dataset](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-01-15) was data about space launches across the world. There was one table about agencies responsible for the launches, and a table about the launches. This data was associated with an [article from The Economist](https://www.economist.com/graphic-detail/2018/10/18/the-space-race-is-dominated-by-new-contenders), with a truly beautiful infographics.

I watched part of [Dave Robinson's](https://t.co/5rBN2FPeB1) video on [how it tackled this](https://www.youtube.com/watch?v=ZyPrP_Yo1BA). To be honest, I have not yet finished watching it, because I was so earnest to try some of the data cleaning tricks he showed that I began coding after half of it. But I will watch the rest, to see what else he did.


New tools I wanted to learn:  
- the `countrycode` package, to go from country codes to full names.  
- Some functions from the `forcats` package. I have use this package before, but I had never used the `fct_collapse` and `fct_lump` functions.  

Readme: *https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-01-15/readme.md*




# Preparing the data

I focused mostly on the `launches` table. I was already tidy, so I did not perform great reshaping. However, I wanted to twee some columns a bit.

First, I used the `lubridate` package to get the date into a nice, tidy date format. In the end, I don't think I used that column, but anyway, I need to get fluent with dates in R. Here, the `ymd()` function take the `launch_date` and turns it into a year-month-day format.


```r
# Get better date format in lauches
launches <- launches %>% 
  mutate(launch_date = ymd(launch_date))
```

Now, I tried to improve the country names. Some of the code are familiar, some are not, I had to do a little search.  


```
##  [1] "BR"     "CN"     "CYM"    "F"      "I"      "I-ELDO" "I-ESA" 
##  [8] "IL"     "IN"     "IR"     "J"      "KP"     "KR"     "RU"    
## [15] "SU"     "UK"     "US"
```

I use the `countrycode` package to obtain the full names of the countries. Since the `countrycode()` function takes ISO codes, I manually changed the provided code to ISO code when necessary. I also collapsed the Soviet Union and current Russia into a Russia state.

The `fct_collapse` function allow us to collapse the levels of a factor into a new level. Here I collapsed SU and RU into RU for example, and changed the name of some factors.


```r
launches$state_code_clean <- fct_collapse(launches$state_code,
    "RU" = c("SU", "RU"),
    "FR" = c("F", "I-ELDO", "I-ESA"),
    "JP" = "J",
    "IT" = "I",
    "KY" = "CYM",
    "GB" = "UK") 
```

Now that I made sure that all my country codes follow the ISO2c norm, I can use the `countrycode` function to obtain the full country names. 


```r
launches$state_name <- countrycode(launches$state_code_clean,
                                   "iso2c", 
                                   "country.name")

launches$state_name %>% unique() %>% kable()
```



|x              |
|:--------------|
|United States  |
|France         |
|Brazil         |
|China          |
|Italy          |
|Russia         |
|Iran           |
|Israel         |
|Japan          |
|India          |
|South Korea    |
|North Korea    |
|United Kingdom |
|Cayman Islands |

Sweet. 

Now, that we have some human-readable names, let's have a look at how many launches each country performed.


```r
launches %>% 
  count(state_name, sort = TRUE) %>% kable()
```



state_name           n
---------------  -----
Russia            3178
United States     1716
France             307
China              302
Japan              115
India               65
Israel              10
Italy                9
Iran                 8
North Korea          5
Cayman Islands       4
South Korea          3
Brazil               2
United Kingdom       2

We can see that there is a big drop between India and Israel. I will follow Dave Robinson lead here and create a variable with the six first countries, and pool the rest in "Others". To do this, I use the `fct_lump` function.


```r
launches <- launches %>%  
  mutate(state_name_short = fct_lump(state_name, 6)) %>%
  replace_na(list(state_name_short = "Other")) 
```

Personally, I also like the idea of pooling countries by geographical area. It does not change much for the state and Russia, but I think it helps having a better picture about Europe and Asia.

I also change the levels of the `category` column to make them more understandable.


```r
launches <- launches %>% 
  mutate(state_bloc = fct_collapse(state_code_clean,
                                   "URSS"   = "RU",
                                   "Europe" = c("FR", "IT", "GB", "KY"),
                                   "North_america"     = "US",
                                   "Asia"   = c("CN", "JP", "IN", "KR", "KP"),
                                   "South_america" = "BR",
                                   "Middle_east" = c("IR", "IL")))  %>% 
  mutate(category = fct_recode(category,
                                Success = "O",
                                Failure = "F"))
```


# Lauches per bloc


```r
launches %>% 
  count(state_bloc, sort = TRUE) %>% 
  ggplot(aes(x = state_bloc, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of lauches per state blocs",
       x = "",
       y = "Number of lauches") +
   theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
  my_theme
```

![](tidytuesday_3_my_experience_files/figure-html/unnamed-chunk-8-1.png)<!-- -->



Let's have a look at the launches across time per country.  


```r
launches %>% 
  count(launch_year, state_name, state_bloc) %>% 
  ggplot(aes(x = launch_year, y = n)) +
  geom_line(size = 2) +
  labs(title = "Number of lauches per state",
       x = "",
       y = "Number of lauches") +
  facet_wrap(~state_bloc, scales = "free") +
  facet_wrap(~state_name) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
   scale_x_continuous("", breaks = seq(1960, 2020, 10)) +
  my_theme
```

![](tidytuesday_3_my_experience_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


It is not bad, but I will use the shortened list of country as Dave Robinson did to make comparisons between countries easier.



```r
launches %>% 
  count(state_name_short, launch_year) %>%
  mutate(state_name_short = fct_reorder(state_name_short, -n, sum)) %>% 
  ggplot(aes(x = launch_year, y = n, 
             colour = state_name_short)) +
  geom_line(size = 1.5) +
  labs(title = "Number of lauches per state (short list)",
       x = "Year of launch",
       y = "Number of lauches",
       colour = "State") +
  theme_minimal() +
   theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
   scale_x_continuous("", breaks = seq(1960, 2020, 10)) +
  my_theme
```

![](tidytuesday_3_my_experience_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Lets have a look with the geographical blocs.


```r
launches %>% 
  count(state_bloc, launch_year) %>%
  mutate(state_bloc = fct_reorder(state_bloc, -n, sum)) %>% 
  ggplot(aes(x = launch_year, y = n, 
             colour = state_bloc)) +
  geom_line(size = 1.5) +
  labs(title = "Number of lauches per state bloc",
       subtitle = "US and USSR dominated the early race, Asia is booming now",
       x = "Year of launch",
       y = "Number of lauches",
       colour = "State bloc") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
   scale_x_continuous("", breaks = seq(1960, 2020, 10)) +
  my_theme
```

![](tidytuesday_3_my_experience_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


I like how it is easier to see what's happening in Asia.


The United states began the space race, closely followed by Russia, which dominated the number of launches until the late nineties. After that, Russia and the US were stable and equivalent. In the early 2000, China, followed by India increased their shares of the launches.


# US vs Russia

Since for a long time the race was dominated by the US and Russia, let's have a nice comparison graph between the two.


```r
launches %>% 
  filter(state_name %in% c("United States", "Russia")) %>%
   ggplot(aes(x = launch_year, fill = state_name)) +
  geom_area(stat = "bin", 
            position = "identity",
            bins = 30, alpha = 0.6) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(title = "Space race: US vs Russia",
       subtitle = "Numer of launches per year in Russia and the states",
       x = "Year of launches ",
       y = "Number of launches",
       fill = "") +
   scale_x_continuous("", breaks = seq(1960, 2020, 10)) +
  my_theme
```

![](tidytuesday_3_my_experience_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


# Success vs Failure

We have the information on whether the launch was a success or a failure. let's have a look at how the number of failures evolved though time.


```r
launches %>% 
  count(category, launch_year) %>% 
  ggplot(aes(x = launch_year, y = n,
             fill = category)) +
  geom_bar(stat = "identity", 
            #position = "identity",
            ) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(title = "Number of successfull and failed launches accross time",
       subtitle = "It seems the number of failed launches decreases with time",
       x = "Year of launches ",
       y = "Number of launches",
       fill = "Launch outcome") +
  scale_x_continuous("", breaks = seq(1960, 2020, 10)) +
  my_theme
```

![](tidytuesday_3_my_experience_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Sweet. Let's have a look by country, focusing on the countries that send most launches.


```r
launches %>% 
  filter(state_name %in% c("China", "France", "Russia", "United States")) %>% 
  count(category, launch_year, state_name_short) %>% 
  ggplot(aes(x = launch_year, y = n,
             fill = category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~state_name_short, nrow = 4) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(title = "Number of successfull and failed launches accross time",
       subtitle = "It seems the number of failed launches decreases with time",
       x = "Year of launches ",
       y = "Number of launches",
       fill = "Launch outcome") +
  scale_x_continuous("", breaks = seq(1960, 2020, 10)) +
  my_theme
```

![](tidytuesday_3_my_experience_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

It's nice, but I would have to have a look at the proportion of failed launches.


```r
launches %>% 
  filter(state_name %in% c("China", "France", "Russia", "United States")) %>% 
  group_by(state_name, launch_year) %>% 
  add_tally() %>% 
  add_count(category) %>% 
  distinct(state_bloc, launch_year, category, n, nn) %>%  
  mutate(proportion = nn / n) %>% 
  ggplot(aes(x = launch_year, y = proportion*100, 
             fill = category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~state_name, nrow = 4) +
  labs(title = "Space mastery increases with time",
       subtitle = "Countries experienced many failures in their early years. Nowadays, lauches are mostly successfull",
       x = "Year of launch",
       y = "Percentage of lauches",
       fill = "Launch outcome") +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_minimal() +
  theme_minimal() +
   theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
  scale_x_continuous("", breaks = seq(1960, 2020, 10)) +
  my_theme
```

![](tidytuesday_3_my_experience_files/figure-html/unnamed-chunk-15-1.png)<!-- -->



# Private vs public lauches

Now I wanted to see whether the launches were mostly made by states or by private companies, and how this varied with time.


```r
launches %>% 
  filter(state_bloc != "South_america" & state_bloc != "Middle_east") %>% 
  ggplot(aes(x = launch_year, fill = agency_type) ) +
  geom_histogram(bins = 62) + 
  facet_wrap(vars(state_bloc), nrow = 4) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
    scale_fill_brewer(palette = "Set1") +
  labs(title = "Who accounts for space launches in different blocs?",
       subtitle = "While in Europe and in the states the market has mostly been privatised, state accounts for most lauches in Asia and Russia",
       x = "Year of launch",
       y = "Number of launches",
       fill = "Type of agency") +
   scale_x_continuous("", breaks = seq(1960, 2020, 10)) +
  my_theme
```

![](tidytuesday_3_my_experience_files/figure-html/unnamed-chunk-16-1.png)<!-- -->



```r
launches %>% 
  filter(state_bloc != "South_america" & state_bloc != "Middle_east") %>% 
  select(state_bloc, launch_year, agency_type) %>% 
  group_by(state_bloc, launch_year) %>% 
  add_tally() %>% 
  add_count(agency_type) %>% 
  distinct(state_bloc, launch_year, agency_type, n, nn) %>%  
  mutate(proportion = nn / n) %>% 
  ggplot(aes(x = launch_year, y = proportion*100, 
             fill = agency_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~state_bloc, nrow = 4) +
  labs(title = "Number of lauches per state bloc",
       subtitle = "US and USSR dominated the early race, Asia is booming now",
       x = "Year of launch",
       y = "Percentage of lauches",
       fill = "Type of agency") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
   theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
   scale_x_continuous("", breaks = seq(1960, 2020, 10)) +
  my_theme
```

![](tidytuesday_3_my_experience_files/figure-html/unnamed-chunk-17-1.png)<!-- -->





