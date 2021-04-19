
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crosstabby

<!-- badges: start -->

<!-- badges: end -->

Crosstabby is a package built to streamline the pre-processing and
analysis of survey data.

## Installation

You can install the released version of crosstabby from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("crosstabby")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("smchoong/crosstabby")
```

## Crosstabby example: extended crosstab functionality

Let’s use crosstabby’s main function, **`big_tabby()`**, to generate a
simple crosstab and then a more extensive crosstab.

Here’s an example of how to pull a simple crosstab:

``` r
library(crosstabby)
row<-c("pres_approval")
col<-c("gender")
data<-nationscape_2020_excerpt

crosstabby::big_tabby(data, row, col, percent = TRUE)
#> # A tibble: 5 x 5
#> # Groups:   Question [1]
#>   Question      Response            Total Female Male 
#>   <chr>         <chr>               <chr> <chr>  <chr>
#> 1 pres_approval Not sure            4%    4%     3%   
#> 2 pres_approval Somewhat approve    20%   18%    21%  
#> 3 pres_approval Somewhat disapprove 12%   13%    12%  
#> 4 pres_approval Strongly approve    23%   18%    29%  
#> 5 pres_approval Strongly disapprove 41%   47%    35%
```

Now, let’s pull a more complex crosstab that uses weights:

``` r
row<-c("pres_approval", "vote_2016", "right_track", "economy_better", "vote_2020")
col<-c("pid3", "ideo5", "household_gun_owner", "gender", "census_region", 
       "race_ethnicity", "household_income", "education")
data<-nationscape_2020_excerpt

crosstabby::big_tabby(data, row, col, wts = "weight", percent = TRUE) %>%
  head()
#> Variable input(s) HOUSEHOLD_INCOME exceeds the recommended number of response levels (<=20).
#> Would you like to remove the problematic variables and proceed with the operation? (Y/N)
#> # A tibble: 6 x 49
#> # Groups:   Question [2]
#>   Question   Response     Total Democrat Independent Republican `Something else`
#>   <chr>      <chr>        <chr> <chr>    <chr>       <chr>      <chr>           
#> 1 pres_appr… Not sure     5%    2%       7%          1%         23%             
#> 2 pres_appr… Somewhat ap… 20%   8%       22%         32%        19%             
#> 3 pres_appr… Somewhat di… 13%   13%      16%         9%         12%             
#> 4 pres_appr… Strongly ap… 21%   5%       13%         50%        7%              
#> 5 pres_appr… Strongly di… 42%   72%      42%         7%         39%             
#> 6 vote_2016  Did not vot… 22%   15%      32%         15%        40%             
#> # … with 42 more variables: Conservative <chr>, Liberal <chr>, Moderate <chr>,
#> #   Not Sure <chr>, Very Conservative <chr>, Very Liberal <chr>,
#> #   I don't, but a member of my household owns a gun <chr>,
#> #   No one in my household owns a gun <chr>, Not sure <chr>,
#> #   Yes, I personally own a gun <chr>, Female <chr>, Male <chr>, Midwest <chr>,
#> #   Northeast <chr>, South <chr>, West <chr>,
#> #   American Indian or Alaska Native <chr>, Asian (Asian Indian) <chr>,
#> #   Asian (Chinese) <chr>, Asian (Filipino) <chr>, Asian (Japanese) <chr>,
#> #   Asian (Korean) <chr>, Asian (Other) <chr>, Asian (Vietnamese) <chr>,
#> #   Black, or African American <chr>, Pacific Islander (Guamanian) <chr>,
#> #   Pacific Islander (Native Hawaiian) <chr>, Pacific Islander (Other) <chr>,
#> #   Pacific Islander (Samoan) <chr>, Some other race <chr>, White <chr>,
#> #   3rd Grade or less <chr>, Associate Degree <chr>,
#> #   College Degree (such as B.A., B.S.) <chr>,
#> #   Completed some college, but no degree <chr>,
#> #   Completed some graduate, but no degree <chr>,
#> #   Completed some high school <chr>, Doctorate degree <chr>,
#> #   High school graduate <chr>, Masters degree <chr>,
#> #   Middle School - Grades 4 - 8 <chr>,
#> #   Other post high school vocational training <chr>
```

So here we have a larger scale crosstab with row variables broken down
by a number of different demographic variables. We don’t yet have column
spanner labels added in to make the crosstab more readable but this is
the minimum viable **`big_tabby()`** product.

You’ll notice that the **`big_tabby()`** function removed the
“household\_income” variable because it exceeded the recommended
number of response levels (less than or equal to 20 levels). To get
around this QC within **`big_tabby()`**, we’ll want to either recode the
income variable into a grouped variable with fewer levels or simply
remove it from our desired column variables before running the crosstab.
