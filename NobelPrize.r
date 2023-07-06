install.packages("lubridate")
library(tidyverse)
library(lubridate)

nobel <- read_csv('NobelData.csv')
head(nobel)

nobel %>% 
    count()
nobel %>%
    group_by(sex) %>%
    count()
nobel %>%
    group_by(birth_country) %>%
    count() %>%
    arrange(desc(n)) %>%
    head()

correct_value <- nobel %>%
                 group_by(birth_country) %>%
                 count()  %>% 
                 arrange(desc(n))  %>% 
                 head(20)

prop_usa_winners <- nobel %>% 
                    mutate(usa_born_winner = birth_country == "United States of America",
                    decade = floor(year / 10) * 10 ) %>% 
                    group_by(decade) %>%
                    summarize(proportion = mean(usa_born_winner, na.rm = TRUE))
prop_usa_winners

correct_prop_usa_winners <- nobel %>% 
                    mutate(usa_born_winner = birth_country == "United States of America",
                    decade = floor(year / 10) * 10 ) %>% 
                    group_by(decade) %>%
                    summarize(proportion = mean(usa_born_winner, na.rm = TRUE))

# options(repr.plot.width=7, repr.plot.height=4)

ggplot(prop_usa_winners,
      aes(x = decade, y = proportion)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(label = scales::percent,
                         limits = 0:1,
                         expand = c(0,0))

prop_female_winners <- nobel %>%
                       mutate(female_winner = sex == "Female",
                       decade = floor(year / 10) * 10 ) %>%
                       group_by(decade, category) %>%
                       summarize(proportion = mean(female_winner, na.rm = TRUE))

ggplot(prop_female_winners,
       aes(x = decade, y = proportion, color = category)) +
       geom_line() +
       geom_point() +
       scale_y_continuous(label = scales::percent,
                          limits = 0:1,
                          expand = c(0,0))

nobel %>%
    filter(sex == "Female") %>%
    top_n(1, desc(year))

nobel %>%
    group_by(full_name) %>%
    count() %>%
    filter(n > 1)

nobel_age <- nobel %>%
             mutate(age = year - year(birth_date))

ggplot(nobel_age, 
       aes(year, age)) +
       geom_point() + 
       geom_smooth()

ggplot(nobel_age, 
       aes(year, age)) +
       geom_point() + 
       geom_smooth(se = FALSE) +
       facet_wrap(~category)


nobel_age %>% top_n(1, age)

nobel_age %>% top_n(1, desc(age))

youngest_winner <- "Malala Yousafzai"