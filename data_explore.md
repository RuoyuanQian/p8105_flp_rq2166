try
================
Ruoyuan Qian
2019/11/11

``` r
data_2019 = read_csv(".\\data\\2019.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   `Country (region)` = col_character(),
    ##   Ladder = col_double(),
    ##   `SD of Ladder` = col_double(),
    ##   `Positive affect` = col_double(),
    ##   `Negative affect` = col_double(),
    ##   `Social support` = col_double(),
    ##   Freedom = col_double(),
    ##   Corruption = col_double(),
    ##   Generosity = col_double(),
    ##   `Log of GDP
    ## per capita` = col_double(),
    ##   `Healthy life
    ## expectancy` = col_double()
    ## )

``` r
data_2015 = read_csv(".\\data\\2015.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Country = col_character(),
    ##   Region = col_character(),
    ##   `Happiness Rank` = col_double(),
    ##   `Happiness Score` = col_double(),
    ##   `Standard Error` = col_double(),
    ##   `Economy (GDP per Capita)` = col_double(),
    ##   Family = col_double(),
    ##   `Health (Life Expectancy)` = col_double(),
    ##   Freedom = col_double(),
    ##   `Trust (Government Corruption)` = col_double(),
    ##   Generosity = col_double(),
    ##   `Dystopia Residual` = col_double()
    ## )

``` r
data_2016 = read_csv(".\\data\\2016.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Country = col_character(),
    ##   Region = col_character(),
    ##   `Happiness Rank` = col_double(),
    ##   `Happiness Score` = col_double(),
    ##   `Lower Confidence Interval` = col_double(),
    ##   `Upper Confidence Interval` = col_double(),
    ##   `Economy (GDP per Capita)` = col_double(),
    ##   Family = col_double(),
    ##   `Health (Life Expectancy)` = col_double(),
    ##   Freedom = col_double(),
    ##   `Trust (Government Corruption)` = col_double(),
    ##   Generosity = col_double(),
    ##   `Dystopia Residual` = col_double()
    ## )

``` r
data_2017 = read_csv(".\\data\\2017.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Country = col_character(),
    ##   Happiness.Rank = col_double(),
    ##   Happiness.Score = col_double(),
    ##   Whisker.high = col_double(),
    ##   Whisker.low = col_double(),
    ##   Economy..GDP.per.Capita. = col_double(),
    ##   Family = col_double(),
    ##   Health..Life.Expectancy. = col_double(),
    ##   Freedom = col_double(),
    ##   Generosity = col_double(),
    ##   Trust..Government.Corruption. = col_double(),
    ##   Dystopia.Residual = col_double()
    ## )

``` r
data_2018 = read_csv(".\\data\\2018.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Rank = col_double(),
    ##   Country = col_character(),
    ##   Score = col_double(),
    ##   GDP_Per_Capita = col_double(),
    ##   Social_Support = col_double(),
    ##   Healthy_Life_Expectancy = col_double(),
    ##   Freedom_To_Make_Life_Choices = col_double(),
    ##   Generosity = col_double(),
    ##   Perceptions_Of_Corruption = col_character(),
    ##   Residual = col_character()
    ## )

``` r
data_all = read_excel(".\\data\\WHR.xls")
data_concap = read_csv(".\\data\\concap.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   CountryName = col_character(),
    ##   CapitalName = col_character(),
    ##   CapitalLatitude = col_double(),
    ##   CapitalLongitude = col_double(),
    ##   CountryCode = col_character(),
    ##   ContinentName = col_character()
    ## )

``` r
data_all = 
 data_all %>% 
 janitor::clean_names() 
```

    ## Warning in FUN(X[[i]], ...): strings not representable in native encoding
    ## will be translated to UTF-8

``` r
data_concap = 
  data_concap %>% 
 janitor::clean_names() 

sum_na = function(x){
  sum = sum(is.na(x))
  sum
}

inter = 
data_all %>% 
  select(country_name:delivery_quality) 

# 每个变量删失个数
map(inter,sum_na) %>% 
  as.data.frame()  
```

    ##   country_name year life_ladder log_gdp_per_capita social_support
    ## 1            0    0           0                 28             13
    ##   healthy_life_expectancy_at_birth freedom_to_make_life_choices generosity
    ## 1                               28                           29         82
    ##   perceptions_of_corruption positive_affect negative_affect
    ## 1                        96              19              13
    ##   confidence_in_national_government democratic_quality delivery_quality
    ## 1                               174                146              145

``` r
#选择删失在30以下的变量
inter_r = 
  inter %>% 
  select(country_name:freedom_to_make_life_choices,positive_affect,negative_affect)
```

``` r
# 有删失的行的数据集
 list <-which(rowSums(is.na(inter_r)) > 0) # hafu数据集中有缺失值的行。
  data_all_na <- inter_r[list,]

  # 最终数据集：1.无删失 2.年份2011-2018
 final_data = 
 inter_r %>% 
  drop_na() %>% 
    filter(year %in% c(2011:2018))
  
 # 最终国家数量
   country_number =
 inter_r %>% 
  drop_na() %>% 
    filter(year %in% c(2011:2018)) %>% 
    select(country_name) %>% 
    distinct() %>% 
    pull(country_name) %>% 
    length()
   
# 最终国家名字  
country_name = 
   inter_r %>% 
  drop_na() %>% 
    filter(year %in% c(2011:2018)) %>% 
    select(country_name) %>% 
    distinct()

# 与地图完全匹配
joint_concap = left_join(country_name,data_concap,by="country_name")

#变量个数
length(final_data)
```

    ## [1] 9
