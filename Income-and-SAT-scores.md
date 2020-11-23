Effects of Income on Average SAT score
================

**This is an exploration of how average household income influences
university entrance exam scores. Income data was sourced from the IPUMS
NHGIS database while exam score data was sourced from the GOSA database
(Georgia office of student achievement).**

``` r
library(knitr)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------------ tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts --------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(sf)
```

    ## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1

``` r
library(spatstat)
```

    ## Loading required package: spatstat.data

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## Loading required package: rpart

    ## Registered S3 method overwritten by 'spatstat':
    ##   method     from
    ##   print.boxx cli

    ## 
    ## spatstat 1.64-1       (nickname: 'Help you I can, yes!') 
    ## For an introduction to spatstat, type 'beginner'

    ## 
    ## Note: spatstat version 1.64-1 is out of date by more than 6 months; we recommend upgrading to the latest version.

``` r
library(tmap)
library(car)
```

    ## Warning: package 'car' was built under R version 4.0.3

    ## Loading required package: carData

    ## Warning: package 'carData' was built under R version 4.0.3

    ## 
    ## Attaching package: 'car'

    ## The following objects are masked from 'package:spatstat':
    ## 
    ##     bc, ellipse

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

Import income data and sort into two different categories.

``` r
income_data<-read.csv("data/complete analysis.csv")

#Household income < 25k/year
low_income<-income_data%>%
  select(School_DSTRCT_CD, SDUNI, SDUNIA, NAME_E,
         AJY9E001, AJY9E002, AJY9E003, AJY9E004, AJY9E005)%>%
  mutate(Count_less30k = AJY9E002 + AJY9E003 + AJY9E004 + AJY9E005)%>%
  mutate(pct_low_inc = (AJY9E002 + AJY9E003 + AJY9E004 + AJY9E005)/AJY9E001)
kable(head(low_income))
```

| School\_DSTRCT\_CD | SDUNI                           | SDUNIA | NAME\_E                                  | AJY9E001 | AJY9E002 | AJY9E003 | AJY9E004 | AJY9E005 | Count\_less30k | pct\_low\_inc |
| -----------------: | :------------------------------ | -----: | :--------------------------------------- | -------: | -------: | -------: | -------: | -------: | -------------: | ------------: |
|                601 | Appling County School District  |     60 | Appling County School District, Georgia  |     6682 |      612 |      528 |      430 |      517 |           2087 |     0.3123316 |
|                602 | Atkinson County School District |     90 | Atkinson County School District, Georgia |     2785 |      240 |      265 |      174 |      259 |            938 |     0.3368043 |
|                603 | Bacon County School District    |    150 | Bacon County School District, Georgia    |     3966 |      410 |      356 |      338 |      251 |           1355 |     0.3416541 |
|                604 | Baker County School District    |    180 | Baker County School District, Georgia    |     1385 |      240 |       83 |       58 |       69 |            450 |     0.3249097 |
|                605 | Baldwin County School District  |    210 | Baldwin County School District, Georgia  |    16122 |     2357 |     1192 |     1365 |      895 |           5809 |     0.3603151 |
|                606 | Banks County School District    |    240 | Banks County School District, Georgia    |     6603 |      548 |      382 |      459 |      351 |           1740 |     0.2635166 |

``` r
#Household income > 100k/year
high_income<-income_data%>%
  select(School_DSTRCT_CD, SDUNI, SDUNIA, NAME_E,
         AJY9E001, AJY9E014, AJY9E015, AJY9E016, AJY9E017)%>%
  mutate(Count_more100k = AJY9E014 + AJY9E015 + AJY9E016 + AJY9E017)%>%
  mutate(pct_high_inc = (AJY9E014 + AJY9E015 + AJY9E016 + AJY9E017)/AJY9E001)
kable(head(high_income))
```

| School\_DSTRCT\_CD | SDUNI                           | SDUNIA | NAME\_E                                  | AJY9E001 | AJY9E014 | AJY9E015 | AJY9E016 | AJY9E017 | Count\_more100k | pct\_high\_inc |
| -----------------: | :------------------------------ | -----: | :--------------------------------------- | -------: | -------: | -------: | -------: | -------: | --------------: | -------------: |
|                601 | Appling County School District  |     60 | Appling County School District, Georgia  |     6682 |      497 |      165 |      169 |       76 |             907 |      0.1357378 |
|                602 | Atkinson County School District |     90 | Atkinson County School District, Georgia |     2785 |       92 |       40 |       62 |       55 |             249 |      0.0894075 |
|                603 | Bacon County School District    |    150 | Bacon County School District, Georgia    |     3966 |      231 |      122 |       25 |       42 |             420 |      0.1059002 |
|                604 | Baker County School District    |    180 | Baker County School District, Georgia    |     1385 |       57 |       29 |       72 |        9 |             167 |      0.1205776 |
|                605 | Baldwin County School District  |    210 | Baldwin County School District, Georgia  |    16122 |     1085 |      323 |      425 |      269 |            2102 |      0.1303808 |
|                606 | Banks County School District    |    240 | Banks County School District, Georgia    |     6603 |      365 |      221 |      145 |       65 |             796 |      0.1205513 |

Import SAT scores and filter so that only records for combined scores
remain.

``` r
raw_SAT_scores<-read.csv("data/All SAT scores GA 2011-2019.csv")%>%
  group_by(LONG_SCHOOL_YEAR, SCHOOL_DISTRCT_CD)%>%
  filter(TEST_CMPNT_TYP_CD == "Combined" | TEST_CMPNT_TYP_CD == "Combined Test Score")%>%
  filter(LONG_SCHOOL_YEAR == "2014-15" | LONG_SCHOOL_YEAR == "2015-16" | LONG_SCHOOL_YEAR == "2016-17" | LONG_SCHOOL_YEAR == "2017-18" | LONG_SCHOOL_YEAR == "2018-19")
kable(head(raw_SAT_scores))
```

| LONG\_SCHOOL\_YEAR | SCHOOL\_DISTRCT\_CD | SCHOOL\_DSTRCT\_NM | INSTN\_NUMBER | INSTN\_NAME                 | TEST\_CMPNT\_TYP\_CD | NATIONAL\_NUM\_TESTED\_CNT | STATE\_NUM\_TESTED\_CNT | DSTRCT\_NUM\_TESTED\_CNT | INSTN\_NUM\_TESTED\_CNT | STATE\_AVG\_SCORE\_VAL | DSTRCT\_AVG\_SCORE\_VAL | INSTN\_AVG\_SCORE\_VAL |
| :----------------- | :------------------ | :----------------- | :------------ | :-------------------------- | :------------------- | :------------------------- | :---------------------- | :----------------------- | :---------------------- | :--------------------- | :---------------------- | :--------------------- |
| 2014-15            | 601                 | Appling County     | 103           | Appling County High School  | Combined             | 1332096                    | 84860                   | 150                      | 150                     | 1352                   | 1304                    | 1304                   |
| 2015-16            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  |                            | 28212.33333             | 34                       | 34                      | 1084                   | 1055                    | 1055                   |
| 2016-17            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  | 1715481                    | 62394.33333             | 94.33333333              | 94.33333333             | 1053                   | 1031                    | 1031                   |
| 2017-18            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  | 2136539                    | 65795                   | 102                      | 102                     | 1052                   | 1020                    | 1020                   |
| 2018-19            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  | 2220087                    | 49345                   | 78                       | 78                      | 1027                   | 1002                    | 1002                   |
| 2014-15            | 602                 | Atkinson County    | 103           | Atkinson County High School | Combined             | 1332096                    | 84860                   | 50                       | 50                      | 1352                   | 1211                    | 1211                   |

``` r
new_scale_scores<-raw_SAT_scores%>%
  filter(LONG_SCHOOL_YEAR == "2015-16" | LONG_SCHOOL_YEAR == "2016-17" | LONG_SCHOOL_YEAR == "2017-18" | LONG_SCHOOL_YEAR == "2018-19")%>%
  group_by(LONG_SCHOOL_YEAR, SCHOOL_DISTRCT_CD, INSTN_NUMBER)%>%
  mutate(scale = "New Scale")
kable(head(new_scale_scores))
```

| LONG\_SCHOOL\_YEAR | SCHOOL\_DISTRCT\_CD | SCHOOL\_DSTRCT\_NM | INSTN\_NUMBER | INSTN\_NAME                 | TEST\_CMPNT\_TYP\_CD | NATIONAL\_NUM\_TESTED\_CNT | STATE\_NUM\_TESTED\_CNT | DSTRCT\_NUM\_TESTED\_CNT | INSTN\_NUM\_TESTED\_CNT | STATE\_AVG\_SCORE\_VAL | DSTRCT\_AVG\_SCORE\_VAL | INSTN\_AVG\_SCORE\_VAL | scale     |
| :----------------- | :------------------ | :----------------- | :------------ | :-------------------------- | :------------------- | :------------------------- | :---------------------- | :----------------------- | :---------------------- | :--------------------- | :---------------------- | :--------------------- | :-------- |
| 2015-16            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  |                            | 28212.33333             | 34                       | 34                      | 1084                   | 1055                    | 1055                   | New Scale |
| 2016-17            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  | 1715481                    | 62394.33333             | 94.33333333              | 94.33333333             | 1053                   | 1031                    | 1031                   | New Scale |
| 2017-18            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  | 2136539                    | 65795                   | 102                      | 102                     | 1052                   | 1020                    | 1020                   | New Scale |
| 2018-19            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  | 2220087                    | 49345                   | 78                       | 78                      | 1027                   | 1002                    | 1002                   | New Scale |
| 2015-16            | 602                 | Atkinson County    | 103           | Atkinson County High School | Combined Test Score  |                            | 28212.33333             | 11                       | 11                      | 1084                   | 991                     | 991                    | New Scale |
| 2016-17            | 602                 | Atkinson County    | 103           | Atkinson County High School | Combined Test Score  | 1715481                    | 62394.33333             | 41                       | 41                      | 1053                   | 1020                    | 1020                   | New Scale |

``` r
SAT_scores<-raw_SAT_scores%>%
  left_join(new_scale_scores)%>%
  replace_na(list(scale = "Old Scale"))
```

    ## Joining, by = c("LONG_SCHOOL_YEAR", "SCHOOL_DISTRCT_CD", "SCHOOL_DSTRCT_NM", "INSTN_NUMBER", "INSTN_NAME", "TEST_CMPNT_TYP_CD", "NATIONAL_NUM_TESTED_CNT", "STATE_NUM_TESTED_CNT", "DSTRCT_NUM_TESTED_CNT", "INSTN_NUM_TESTED_CNT", "STATE_AVG_SCORE_VAL", "DSTRCT_AVG_SCORE_VAL", "INSTN_AVG_SCORE_VAL")

``` r
kable(head(SAT_scores))
```

| LONG\_SCHOOL\_YEAR | SCHOOL\_DISTRCT\_CD | SCHOOL\_DSTRCT\_NM | INSTN\_NUMBER | INSTN\_NAME                 | TEST\_CMPNT\_TYP\_CD | NATIONAL\_NUM\_TESTED\_CNT | STATE\_NUM\_TESTED\_CNT | DSTRCT\_NUM\_TESTED\_CNT | INSTN\_NUM\_TESTED\_CNT | STATE\_AVG\_SCORE\_VAL | DSTRCT\_AVG\_SCORE\_VAL | INSTN\_AVG\_SCORE\_VAL | scale     |
| :----------------- | :------------------ | :----------------- | :------------ | :-------------------------- | :------------------- | :------------------------- | :---------------------- | :----------------------- | :---------------------- | :--------------------- | :---------------------- | :--------------------- | :-------- |
| 2014-15            | 601                 | Appling County     | 103           | Appling County High School  | Combined             | 1332096                    | 84860                   | 150                      | 150                     | 1352                   | 1304                    | 1304                   | Old Scale |
| 2015-16            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  |                            | 28212.33333             | 34                       | 34                      | 1084                   | 1055                    | 1055                   | New Scale |
| 2016-17            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  | 1715481                    | 62394.33333             | 94.33333333              | 94.33333333             | 1053                   | 1031                    | 1031                   | New Scale |
| 2017-18            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  | 2136539                    | 65795                   | 102                      | 102                     | 1052                   | 1020                    | 1020                   | New Scale |
| 2018-19            | 601                 | Appling County     | 103           | Appling County High School  | Combined Test Score  | 2220087                    | 49345                   | 78                       | 78                      | 1027                   | 1002                    | 1002                   | New Scale |
| 2014-15            | 602                 | Atkinson County    | 103           | Atkinson County High School | Combined             | 1332096                    | 84860                   | 50                       | 50                      | 1352                   | 1211                    | 1211                   | Old Scale |

Now convert scores on the old scale. The scale changed from 2400 to 1600
in the spring 2016. Create a conversion calculator.

``` r
conversion_scale<-read.csv("data/Conversion chart new to old SAT dash separat.csv")
kable(head(conversion_scale))
```

| ï..New\_SAT | Old\_Max |
| ----------: | -------: |
|        1600 |     2400 |
|        1590 |     2380 |
|        1580 |     2350 |
|        1570 |     2330 |
|        1560 |     2310 |
|        1550 |     2290 |

``` r
new_score_calc<-function(old_score){
  max_range<-conversion_scale %>%
    filter(Old_Max>old_score)%>%
    filter(Old_Max == min(Old_Max))
data.frame(score=old_score,new_score=max_range[,1])
}
test<-new_score_calc(1374)

#Conversion of institutional scores
inst_scores<-SAT_scores%>%
  filter(LONG_SCHOOL_YEAR == "2014-15")%>%
  mutate(score = as.numeric(INSTN_AVG_SCORE_VAL))%>%
  filter(score > 0)

score_transform<-map_df(inst_scores$score, new_score_calc)

scores<-as.numeric(score_transform$INSTN_AVG_SCORE_VAL)

final_converted_scores<-inst_scores%>%
  left_join(score_transform)
```

    ## Joining, by = "score"

``` r
all_old_scores_final_instn<-final_converted_scores%>%
  full_join(new_scale_scores)
```

    ## Joining, by = c("LONG_SCHOOL_YEAR", "SCHOOL_DISTRCT_CD", "SCHOOL_DSTRCT_NM", "INSTN_NUMBER", "INSTN_NAME", "TEST_CMPNT_TYP_CD", "NATIONAL_NUM_TESTED_CNT", "STATE_NUM_TESTED_CNT", "DSTRCT_NUM_TESTED_CNT", "INSTN_NUM_TESTED_CNT", "STATE_AVG_SCORE_VAL", "DSTRCT_AVG_SCORE_VAL", "INSTN_AVG_SCORE_VAL", "scale")

``` r
kable(head(all_old_scores_final_instn))
```

| LONG\_SCHOOL\_YEAR | SCHOOL\_DISTRCT\_CD | SCHOOL\_DSTRCT\_NM | INSTN\_NUMBER | INSTN\_NAME                 | TEST\_CMPNT\_TYP\_CD | NATIONAL\_NUM\_TESTED\_CNT | STATE\_NUM\_TESTED\_CNT | DSTRCT\_NUM\_TESTED\_CNT | INSTN\_NUM\_TESTED\_CNT | STATE\_AVG\_SCORE\_VAL | DSTRCT\_AVG\_SCORE\_VAL | INSTN\_AVG\_SCORE\_VAL | scale     | score | new\_score |
| :----------------- | :------------------ | :----------------- | :------------ | :-------------------------- | :------------------- | :------------------------- | :---------------------- | :----------------------- | :---------------------- | :--------------------- | :---------------------- | :--------------------- | :-------- | ----: | ---------: |
| 2014-15            | 601                 | Appling County     | 103           | Appling County High School  | Combined             | 1332096                    | 84860                   | 150                      | 150                     | 1352                   | 1304                    | 1304                   | Old Scale |  1304 |        970 |
| 2014-15            | 601                 | Appling County     | 103           | Appling County High School  | Combined             | 1332096                    | 84860                   | 150                      | 150                     | 1352                   | 1304                    | 1304                   | Old Scale |  1304 |        970 |
| 2014-15            | 602                 | Atkinson County    | 103           | Atkinson County High School | Combined             | 1332096                    | 84860                   | 50                       | 50                      | 1352                   | 1211                    | 1211                   | Old Scale |  1211 |        910 |
| 2014-15            | 602                 | Atkinson County    | 103           | Atkinson County High School | Combined             | 1332096                    | 84860                   | 50                       | 50                      | 1352                   | 1211                    | 1211                   | Old Scale |  1211 |        910 |
| 2014-15            | 602                 | Atkinson County    | 103           | Atkinson County High School | Combined             | 1332096                    | 84860                   | 50                       | 50                      | 1352                   | 1211                    | 1211                   | Old Scale |  1211 |        910 |
| 2014-15            | 602                 | Atkinson County    | 103           | Atkinson County High School | Combined             | 1332096                    | 84860                   | 50                       | 50                      | 1352                   | 1211                    | 1211                   | Old Scale |  1211 |        910 |

``` r
#Conversion of district scores
DSTRCT_scores<-SAT_scores%>%
  filter(LONG_SCHOOL_YEAR == "2014-15")%>%
  mutate(score = as.numeric(DSTRCT_AVG_SCORE_VAL))%>%
  filter(score > 0)

score_transform2<-map_df(DSTRCT_scores$score, new_score_calc)

scores2<-as.numeric(score_transform2$DSTRCT_AVG_SCORE_VAL)

final_converted_scores2<-DSTRCT_scores%>%
  left_join(score_transform2)
```

    ## Joining, by = "score"

``` r
all_old_scores_final_DSTRCT<-final_converted_scores2%>%
  full_join(new_scale_scores)
```

    ## Joining, by = c("LONG_SCHOOL_YEAR", "SCHOOL_DISTRCT_CD", "SCHOOL_DSTRCT_NM", "INSTN_NUMBER", "INSTN_NAME", "TEST_CMPNT_TYP_CD", "NATIONAL_NUM_TESTED_CNT", "STATE_NUM_TESTED_CNT", "DSTRCT_NUM_TESTED_CNT", "INSTN_NUM_TESTED_CNT", "STATE_AVG_SCORE_VAL", "DSTRCT_AVG_SCORE_VAL", "INSTN_AVG_SCORE_VAL", "scale")

``` r
kable(head(all_old_scores_final_DSTRCT))
```

| LONG\_SCHOOL\_YEAR | SCHOOL\_DISTRCT\_CD | SCHOOL\_DSTRCT\_NM     | INSTN\_NUMBER | INSTN\_NAME                                       | TEST\_CMPNT\_TYP\_CD | NATIONAL\_NUM\_TESTED\_CNT | STATE\_NUM\_TESTED\_CNT | DSTRCT\_NUM\_TESTED\_CNT | INSTN\_NUM\_TESTED\_CNT | STATE\_AVG\_SCORE\_VAL | DSTRCT\_AVG\_SCORE\_VAL | INSTN\_AVG\_SCORE\_VAL | scale     | score | new\_score |
| :----------------- | :------------------ | :--------------------- | :------------ | :------------------------------------------------ | :------------------- | :------------------------- | :---------------------- | :----------------------- | :---------------------- | :--------------------- | :---------------------- | :--------------------- | :-------- | ----: | ---------: |
| 2014-15            | 601                 | Appling County         | 103           | Appling County High School                        | Combined             | 1332096                    | 84860                   | 150                      | 150                     | 1352                   | 1304                    | 1304                   | Old Scale |  1304 |        970 |
| 2014-15            | 602                 | Atkinson County        | 103           | Atkinson County High School                       | Combined             | 1332096                    | 84860                   | 50                       | 50                      | 1352                   | 1211                    | 1211                   | Old Scale |  1211 |        910 |
| 2014-15            | 602                 | Atkinson County        | 103           | Atkinson County High School                       | Combined             | 1332096                    | 84860                   | 50                       | 50                      | 1352                   | 1211                    | 1211                   | Old Scale |  1211 |        910 |
| 2014-15            | 602                 | Atkinson County        | 103           | Atkinson County High School                       | Combined             | 1332096                    | 84860                   | 50                       | 50                      | 1352                   | 1211                    | 1211                   | Old Scale |  1211 |        910 |
| 2014-15            | 602                 | Atkinson County        | 103           | Atkinson County High School                       | Combined             | 1332096                    | 84860                   | 50                       | 50                      | 1352                   | 1211                    | 1211                   | Old Scale |  1211 |        910 |
| 2014-15            | 761                 | Atlanta Public Schools | 113           | Booker T. Washington - Early College Small School | Combined             | 1332096                    | 84860                   | 1547                     | 9                       | 1352                   | 1273                    |                        | Old Scale |  1273 |        950 |

Get the average of all years SAT scores for each school district. Join
the income data to the SAT scores according to SCHOOL\_DISTRCT\_CD

``` r
aggregate_scores_dist<-all_old_scores_final_DSTRCT%>%
  full_join(new_scale_scores)%>%
  group_by(LONG_SCHOOL_YEAR ,SCHOOL_DISTRCT_CD, DSTRCT_AVG_SCORE_VAL, scale, score, new_score)%>%
  summarise(District_avg_score = mean(new_score))%>%
  ungroup()
```

    ## Joining, by = c("LONG_SCHOOL_YEAR", "SCHOOL_DISTRCT_CD", "SCHOOL_DSTRCT_NM", "INSTN_NUMBER", "INSTN_NAME", "TEST_CMPNT_TYP_CD", "NATIONAL_NUM_TESTED_CNT", "STATE_NUM_TESTED_CNT", "DSTRCT_NUM_TESTED_CNT", "INSTN_NUM_TESTED_CNT", "STATE_AVG_SCORE_VAL", "DSTRCT_AVG_SCORE_VAL", "INSTN_AVG_SCORE_VAL", "scale")

    ## `summarise()` regrouping output by 'LONG_SCHOOL_YEAR', 'SCHOOL_DISTRCT_CD', 'DSTRCT_AVG_SCORE_VAL', 'scale', 'score' (override with `.groups` argument)

``` r
kable(head(aggregate_scores_dist))
```

| LONG\_SCHOOL\_YEAR | SCHOOL\_DISTRCT\_CD | DSTRCT\_AVG\_SCORE\_VAL | scale     | score | new\_score | District\_avg\_score |
| :----------------- | :------------------ | :---------------------- | :-------- | ----: | ---------: | -------------------: |
| 2014-15            | 601                 | 1304                    | Old Scale |  1304 |        970 |                  970 |
| 2014-15            | 602                 | 1211                    | Old Scale |  1211 |        910 |                  910 |
| 2014-15            | 603                 | 1218                    | Old Scale |  1218 |        910 |                  910 |
| 2014-15            | 605                 | 1248                    | Old Scale |  1248 |        930 |                  930 |
| 2014-15            | 606                 | 1371                    | Old Scale |  1371 |       1020 |                 1020 |
| 2014-15            | 607                 | 1316                    | Old Scale |  1316 |        980 |                  980 |
