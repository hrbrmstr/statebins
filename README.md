statesquares is ...

The following functions are implemented:

-   `statebins` -

### News

-   Version `1.0.0` released

### Installation

``` {.r}
devtools::install_github("hrbrmstr/statebins")
```

### Usage

``` {.r}
library(statebins)
```

    ## Loading required package: ggplot2
    ## Loading required package: grid
    ## Loading required package: scales
    ## Loading required package: gridExtra

``` {.r}
# current verison
packageVersion("statebins")
```

    ## [1] '1.0'

``` {.r}
dat <- read.csv("http://www.washingtonpost.com/wp-srv/special/business/states-most-threatened-by-trade/states.csv?cache=1", stringsAsFactors=FALSE)

gg <- statebins(dat, "state", "avgshare94_00", breaks=4, 
                labels=c("0-1", "1-2", "2-3", "3-4"),
                legend_title="State Groups",
                brewer_pal="Blues", text_color="black", 
                plot_title="1994-2000", title_position="bottom")

gg
```

![plot of chunk unnamed-chunk-3](./_README_files/figure-markdown_github/unnamed-chunk-31.png)

``` {.r}
gg2 <- statebins_continuous(dat, "state", "avgshare01_07",
                legend_title="States", legend_position="none",
                brewer_pal="OrRd", text_color="black", 
                plot_title="1994-2000", title_position="bottom")

gg2
```

![plot of chunk unnamed-chunk-3](./_README_files/figure-markdown_github/unnamed-chunk-32.png)

### Test Results

``` {.r}
library(statebins)
library(testthat)

date()
```

    ## [1] "Mon Aug 25 22:25:49 2014"

``` {.r}
test_dir("tests/")
```

    ## basic functionality :
