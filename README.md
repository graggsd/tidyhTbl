
<!-- README.md is generated from README.Rmd. Please edit that file -->
Development of this package has been abandoned
==============================================

The main function in this package has now been incorporated into the official version of the [htmlTable](https://cran.r-project.org/web/packages/htmlTable/index.html) package, and development of this package will not longer be supported.

In the `htmlTable` package, the `htmlTable_td` function has been renamed to `tidyHtmlTable` with additional changes made to parameter names.

See `?tidyHtmlTable` for further details.

``` r

# For the dev version of htmlTable
install.packages("devtools")
devtools::install_github("gforge/tidyhTbl")

# For the lastest CRAN release
install.packages("htmlTable")
```

[![Build Status](https://travis-ci.org/graggsd/tidyhTbl.svg?branch=master)](https://travis-ci.org/graggsd/tidyhTbl)

tidyhTbl
========

The goal of tidyhTbl is to wrap the `htmlTable` function from the [htmlTable](https://cran.r-project.org/web/packages/htmlTable/index.html) package for use with tidy data.

Installation
------------

You can install tidyhTbl from github with:

``` r
install.packages("devtools")
devtools::install_github("graggsd/tidyhTbl")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(magrittr)
library(tidyr)
library(dplyr)
library(tidyhTbl)
library(tibble)
mtcars %>%
    rownames_to_column %>%
    select(rowname, cyl, gear, hp, mpg, qsec) %>%
    gather(per_metric, value, hp, mpg, qsec) %>%
    group_by(cyl, gear, per_metric) %>%
    summarise(Mean = round(mean(value), 1),
              SD = round(sd(value), 1),
              Min = round(min(value), 1),
              Max = round(max(value), 1)) %>%
     gather(summary_stat, value, Mean, SD, Min, Max) %>%
     ungroup %>%
     mutate(gear = paste(gear, "Gears"),
            cyl = paste(cyl, "Cylinders")) %>%
     htmlTable_td(header_td = "gear",
                  cgroup1_td = "cyl",
                  cell_value = "value",
                  rnames_td = "summary_stat",
                  rgroup_td = "per_metric")
```

<!--html_preserve-->
<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<th style="border-top: 2px solid grey;">
</th>
<th colspan="3" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
4 Cylinders
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="3" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
6 Cylinders
</th>
<th style="border-top: 2px solid grey;; border-bottom: hidden;">
 
</th>
<th colspan="2" style="font-weight: 900; border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
8 Cylinders
</th>
</tr>
<tr>
<th style="border-bottom: 1px solid grey;">
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
1
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
2
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
3
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
4
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
5
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
6
</th>
<th style="border-bottom: 1px solid grey;" colspan="1">
 
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
7
</th>
<th style="border-bottom: 1px solid grey; text-align: center;">
8
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">
1
</td>
<td style="text-align: center;">
97
</td>
<td style="text-align: center;">
109
</td>
<td style="text-align: center;">
113
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
110
</td>
<td style="text-align: center;">
123
</td>
<td style="text-align: center;">
175
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
245
</td>
<td style="text-align: center;">
335
</td>
</tr>
<tr>
<td style="text-align: left;">
2
</td>
<td style="text-align: center;">
97
</td>
<td style="text-align: center;">
76
</td>
<td style="text-align: center;">
102
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
107.5
</td>
<td style="text-align: center;">
116.5
</td>
<td style="text-align: center;">
175
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
194.2
</td>
<td style="text-align: center;">
299.5
</td>
</tr>
<tr>
<td style="text-align: left;">
3
</td>
<td style="text-align: center;">
97
</td>
<td style="text-align: center;">
52
</td>
<td style="text-align: center;">
91
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
105
</td>
<td style="text-align: center;">
110
</td>
<td style="text-align: center;">
175
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
150
</td>
<td style="text-align: center;">
264
</td>
</tr>
<tr>
<td style="text-align: left;">
4
</td>
<td style="text-align: center;">
NaN
</td>
<td style="text-align: center;">
20.1
</td>
<td style="text-align: center;">
15.6
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
3.5
</td>
<td style="text-align: center;">
7.5
</td>
<td style="text-align: center;">
NaN
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
33.4
</td>
<td style="text-align: center;">
50.2
</td>
</tr>
<tr>
<td style="text-align: left;">
5
</td>
<td style="text-align: center;">
21.5
</td>
<td style="text-align: center;">
33.9
</td>
<td style="text-align: center;">
30.4
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
21.4
</td>
<td style="text-align: center;">
21
</td>
<td style="text-align: center;">
19.7
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
19.2
</td>
<td style="text-align: center;">
15.8
</td>
</tr>
<tr>
<td style="text-align: left;">
6
</td>
<td style="text-align: center;">
21.5
</td>
<td style="text-align: center;">
26.9
</td>
<td style="text-align: center;">
28.2
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
19.8
</td>
<td style="text-align: center;">
19.8
</td>
<td style="text-align: center;">
19.7
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
15.1
</td>
<td style="text-align: center;">
15.4
</td>
</tr>
<tr>
<td style="text-align: left;">
7
</td>
<td style="text-align: center;">
21.5
</td>
<td style="text-align: center;">
21.4
</td>
<td style="text-align: center;">
26
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
18.1
</td>
<td style="text-align: center;">
17.8
</td>
<td style="text-align: center;">
19.7
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
10.4
</td>
<td style="text-align: center;">
15
</td>
</tr>
<tr>
<td style="text-align: left;">
8
</td>
<td style="text-align: center;">
NaN
</td>
<td style="text-align: center;">
4.8
</td>
<td style="text-align: center;">
3.1
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
2.3
</td>
<td style="text-align: center;">
1.6
</td>
<td style="text-align: center;">
NaN
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
2.8
</td>
<td style="text-align: center;">
0.6
</td>
</tr>
<tr>
<td style="text-align: left;">
9
</td>
<td style="text-align: center;">
20
</td>
<td style="text-align: center;">
22.9
</td>
<td style="text-align: center;">
16.9
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
20.2
</td>
<td style="text-align: center;">
18.9
</td>
<td style="text-align: center;">
15.5
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
18
</td>
<td style="text-align: center;">
14.6
</td>
</tr>
<tr>
<td style="text-align: left;">
10
</td>
<td style="text-align: center;">
20
</td>
<td style="text-align: center;">
19.6
</td>
<td style="text-align: center;">
16.8
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
19.8
</td>
<td style="text-align: center;">
17.7
</td>
<td style="text-align: center;">
15.5
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
17.1
</td>
<td style="text-align: center;">
14.6
</td>
</tr>
<tr>
<td style="text-align: left;">
11
</td>
<td style="text-align: center;">
20
</td>
<td style="text-align: center;">
18.5
</td>
<td style="text-align: center;">
16.7
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
19.4
</td>
<td style="text-align: center;">
16.5
</td>
<td style="text-align: center;">
15.5
</td>
<td style colspan="1">
 
</td>
<td style="text-align: center;">
15.4
</td>
<td style="text-align: center;">
14.5
</td>
</tr>
<tr>
<td style="border-bottom: 2px solid grey; text-align: left;">
12
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
NaN
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
1.5
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
0.1
</td>
<td style="border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
0.6
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
1.1
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
NaN
</td>
<td style="border-bottom: 2px solid grey;" colspan="1">
 
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
0.8
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
0.1
</td>
</tr>
</tbody>
</table>
<!--/html_preserve-->
