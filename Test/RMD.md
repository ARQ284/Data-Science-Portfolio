Excel Merging
================

Two workbooks with 3 sheets in each. Same format of (SALARY,DEPT,EMP)

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)


dir_path <- "Test/"         # target directory where the xlsx files are located. 
re_file <- "^Book[0-9]+\\.xlsx"    # match files with "Book" in name followed by a number

read_sheets <- function(dir_path, file){
  xlsx_file <- paste0(dir_path, file)
  xlsx_file %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel, path = xlsx_file, .id = 'sheet_name') %>% 
    mutate(file_name = file) %>% 
    select(file_name, sheet_name, everything())
}

df <- list.files(dir_path, re_file) %>% 
  map_df(~ read_sheets(dir_path, .))
```

``` r
df %>% as_tibble() %>% sample_n(10)
```

    ## # A tibble: 10 × 5
    ##    file_name  sheet_name SALARY DEPT  EMP  
    ##    <chr>      <chr>       <dbl> <chr> <chr>
    ##  1 Book2.xlsx Sheet2     0.463  A     B    
    ##  2 Book2.xlsx Sheet2     0.197  A     B    
    ##  3 Book1.xlsx Sheet2     0.463  A     B    
    ##  4 Book2.xlsx Sheet3     0.391  A     B    
    ##  5 Book2.xlsx Sheet1     0.215  A     B    
    ##  6 Book2.xlsx Sheet2     0.927  A     B    
    ##  7 Book1.xlsx Sheet3     0.0583 A     B    
    ##  8 Book1.xlsx Sheet1     0.403  A     B    
    ##  9 Book2.xlsx Sheet3     0.0962 A     B    
    ## 10 Book1.xlsx Sheet2     0.250  A     B
