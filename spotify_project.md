final_proj
================

``` r
# large random dataset of 3000+ songs, found this on reddit.."3000 songs everyone should hear" https://open.spotify.com/playlist/5kLB4N6DX3DffIoqVsxPS4?si=wPYdUDfHSLO800trQbpo3Q&nd=1

big_set <- read_csv("/Users/jimmyq/Desktop/senior fall/econ370/q_data/random_big.csv")
```

    ## New names:
    ## * `` -> ...1

``` r
# /Users/jimmyq/Desktop/senior fall/econ370/q_data/top10s.csv --> smaller subset of top10s


bs_edited <- big_set %>%
  rename(index = ...1, bpm = tempo, dB = loudness, acoustic = acous, song_name = name, spotify_id = id) %>%
  mutate(index = index + 1) %>% # not totally sure this column is 100% reliable i need to check
  rename_with( ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) # next mutate to scale

bs_edited <- bs_edited %>% # scaling the numbers so they're easier to work with
  mutate(dance = dance*100) %>%
  mutate(energy = energy*100) %>%
  mutate(speech = speech*100) %>%
  mutate(mode = mode*100) %>%
  mutate(acoustic = round(acoustic*100, 2)) %>%
  mutate(instrum = round(instrum*100, 2)) %>%
  mutate(live = live*100) %>%
  mutate(valence = valence*100) %>%
  mutate(duration_m = `duration_(ms)`/60000) %>%
  mutate(dance_sq = dance*dance) %>%
  mutate(energy_sq = energy*energy) %>%
  mutate(live_sq = live*live) %>%
  mutate(ln_valence = log(valence)) %>%
  mutate(bpm_sq = bpm*bpm) %>%
  mutate(db_sq = db*db) %>%
  mutate(positive = ifelse(valence > 50, 1, 0)) %>%
  select(-c(song_name, artist, album, id_check, spotify_id, index, key)) # dropping key for now, just because it is difficult to deal with

# summary stats
str(bs_edited)
```

    ## tibble [3,006 × 20] (S3: tbl_df/tbl/data.frame)
    ##  $ dance        : num [1:3006] 27 52.8 34.3 57.4 38.8 57.2 63.9 48.4 83.3 50.2 ...
    ##  $ mode         : num [1:3006] 100 0 0 100 100 100 100 0 0 100 ...
    ##  $ energy       : num [1:3006] 95.4 68.7 82.8 73.4 33.8 78.6 54.8 64.1 80.6 89 ...
    ##  $ db           : num [1:3006] -2.86 -5.95 -6.22 -5.64 -10.05 ...
    ##  $ speech       : num [1:3006] 16.1 5.32 6.46 5.91 3.29 6.42 3.94 30 11.3 3.64 ...
    ##  $ acoustic     : num [1:3006] 0.56 14.4 6.26 16.8 65.2 51.9 29 11.7 23.1 0.01 ...
    ##  $ instrum      : num [1:3006] 0 0 0.21 0 0 0.09 0.01 0 0.94 1.17 ...
    ##  $ live         : num [1:3006] 31.2 25.1 21.6 7.94 24.8 11 36.6 32.4 6.81 21.7 ...
    ##  $ valence      : num [1:3006] 43.3 42.3 35.2 25.2 47.8 95.2 88.7 68.8 64 25.9 ...
    ##  $ bpm          : num [1:3006] 168 143 78 164 178 ...
    ##  $ duration_(ms): num [1:3006] 179267 230227 170080 197927 303373 ...
    ##  $ popularity   : num [1:3006] 0 53 53 0 80 0 0 0 0 82 ...
    ##  $ duration_m   : num [1:3006] 2.99 3.84 2.83 3.3 5.06 ...
    ##  $ dance_sq     : num [1:3006] 729 2788 1176 3295 1505 ...
    ##  $ energy_sq    : num [1:3006] 9101 4720 6856 5388 1142 ...
    ##  $ live_sq      : num [1:3006] 973 630 467 63 615 ...
    ##  $ ln_valence   : num [1:3006] 3.77 3.74 3.56 3.23 3.87 ...
    ##  $ bpm_sq       : num [1:3006] 28162 20336 6088 26933 31600 ...
    ##  $ db_sq        : num [1:3006] 8.2 35.5 38.7 31.8 101.1 ...
    ##  $ positive     : num [1:3006] 0 0 0 0 0 1 1 1 1 0 ...

``` r
summary(bs_edited)
```

    ##      dance            mode            energy             db         
    ##  Min.   : 0.00   Min.   :  0.00   Min.   : 0.527   Min.   :-37.264  
    ##  1st Qu.:43.80   1st Qu.:  0.00   1st Qu.:37.500   1st Qu.:-12.353  
    ##  Median :55.10   Median :100.00   Median :57.200   Median : -9.498  
    ##  Mean   :55.01   Mean   : 71.36   Mean   :55.578   Mean   :-10.145  
    ##  3rd Qu.:66.40   3rd Qu.:100.00   3rd Qu.:75.675   3rd Qu.: -7.165  
    ##  Max.   :98.10   Max.   :100.00   Max.   :99.200   Max.   : -0.158  
    ##      speech          acoustic        instrum           live           valence     
    ##  Min.   : 0.000   Min.   : 0.00   Min.   : 0.00   Min.   : 1.880   Min.   : 0.00  
    ##  1st Qu.: 3.292   1st Qu.: 6.67   1st Qu.: 0.00   1st Qu.: 9.312   1st Qu.:36.80  
    ##  Median : 4.185   Median :28.25   Median : 0.04   Median :13.100   Median :57.10  
    ##  Mean   : 6.658   Mean   :37.42   Mean   :10.85   Mean   :19.766   Mean   :56.16  
    ##  3rd Qu.: 6.460   3rd Qu.:65.90   3rd Qu.: 2.43   3rd Qu.:25.400   3rd Qu.:77.08  
    ##  Max.   :92.700   Max.   :99.60   Max.   :98.30   Max.   :98.500   Max.   :98.90  
    ##       bpm         duration_(ms)       popularity      duration_m         dance_sq   
    ##  Min.   :  0.00   Min.   :  25987   Min.   : 0.00   Min.   : 0.4331   Min.   :   0  
    ##  1st Qu.: 97.44   1st Qu.: 174153   1st Qu.:13.00   1st Qu.: 2.9026   1st Qu.:1918  
    ##  Median :118.73   Median : 212674   Median :38.00   Median : 3.5446   Median :3036  
    ##  Mean   :119.42   Mean   : 227930   Mean   :36.05   Mean   : 3.7988   Mean   :3289  
    ##  3rd Qu.:136.65   3rd Qu.: 262357   3rd Qu.:56.00   3rd Qu.: 4.3726   3rd Qu.:4409  
    ##  Max.   :209.24   Max.   :1360027   Max.   :85.00   Max.   :22.6671   Max.   :9624  
    ##    energy_sq           live_sq           ln_valence        bpm_sq     
    ##  Min.   :   0.278   Min.   :   3.534   Min.   : -Inf   Min.   :    0  
    ##  1st Qu.:1406.250   1st Qu.:  86.723   1st Qu.:3.605   1st Qu.: 9495  
    ##  Median :3271.840   Median : 171.610   Median :4.045   Median :14098  
    ##  Mean   :3679.335   Mean   : 665.012   Mean   : -Inf   Mean   :15083  
    ##  3rd Qu.:5726.708   3rd Qu.: 645.160   3rd Qu.:4.345   3rd Qu.:18674  
    ##  Max.   :9840.640   Max.   :9702.250   Max.   :4.594   Max.   :43782  
    ##      db_sq             positive     
    ##  Min.   :   0.025   Min.   :0.0000  
    ##  1st Qu.:  51.337   1st Qu.:0.0000  
    ##  Median :  90.203   Median :1.0000  
    ##  Mean   : 121.500   Mean   :0.5941  
    ##  3rd Qu.: 152.603   3rd Qu.:1.0000  
    ##  Max.   :1388.606   Max.   :1.0000

``` r
big_set %>% # highest to lowest valence
  arrange(desc(valence))
```

    ## # A tibble: 3,006 × 19
    ##     ...1 dance  mode energy   key loudness speech  acous instrum   live valence tempo
    ##    <dbl> <dbl> <dbl>  <dbl> <dbl>    <dbl>  <dbl>  <dbl>   <dbl>  <dbl>   <dbl> <dbl>
    ##  1  1188 0.812     1  0.644     0   -10.8  0.0493 0.0877 7.85e-1 0.0563   0.989 140. 
    ##  2  2066 0.638     1  0.884     4    -4.76 0.0322 0.652  9.47e-1 0.345    0.988 146. 
    ##  3  2166 0.764     1  0.777     2    -9.56 0.0429 0.863  1.67e-2 0.0284   0.987 125. 
    ##  4   346 0.551     1  0.686     4   -12.0  0.0536 0.346  0       0.33     0.982  71.0
    ##  5   704 0.697     1  0.809     9    -8.20 0.0302 0.114  5.21e-4 0.183    0.98  126. 
    ##  6  2620 0.809     1  0.518    10    -8.72 0.0581 0.959  7.92e-1 0.0863   0.98  115. 
    ##  7  2327 0.819     1  0.637     2    -8.56 0.0465 0.132  5.71e-1 0.182    0.978 123. 
    ##  8  1641 0.834     0  0.737     8    -7.51 0.0332 0.144  9.34e-1 0.153    0.977 108. 
    ##  9   531 0.818     1  0.728    10    -8.33 0.0314 0.232  6.42e-2 0.251    0.975 113. 
    ## 10  1057 0.63      1  0.863     4   -12.1  0.0459 0.433  8.9 e-1 0.0616   0.975 133. 
    ## # … with 2,996 more rows, and 7 more variables: duration_(ms) <dbl>, id_check <chr>,
    ## #   popularity <dbl>, name <chr>, artist <chr>, album <chr>, id <chr>

``` r
rolling_stone <- read_csv("/Users/jimmyq/Desktop/senior fall/econ370/q_data/rollingstone2.csv")
```

    ## New names:
    ## * `` -> ...1

``` r
# from webscrape of the rolling stone top 500 songs playlist as of sept 2021

rs_edited <- rolling_stone %>%
  rename(song_rank = ...1, bpm = tempo, dB = loudness, acoustic = acous, song_name = name, spotify_id = id) %>%
  mutate(song_rank = song_rank + 1) %>% # not totally sure this column is 100% reliable i need to check 
  rename_with( ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) # next mutate to scale 

rs_edited <- rs_edited %>% # scaling the numbers so they're easier to work with 
  mutate(dance = dance*100) %>%
  mutate(energy = energy*100) %>%
  mutate(speech = speech*100) %>%
  mutate(acoustic = round(acoustic*100, 2)) %>%
  mutate(instrum = round(instrum*100, 2)) %>%
  mutate(live = live*100) %>%
  mutate(mode = mode*100) %>%
  mutate(valence = valence*100) %>%
  mutate(duration_m = `duration_(ms)`/60000) %>%  # creating minute values instead 
  select(-c(song_name, artist, album, id_check, key)) # dropping key for now, just because I think its complicated to deal with 

# need to run some interaction effects
rs_extra <- rs_edited %>% # have to run another summary table 
  mutate(dance_sq = dance*dance) %>%
  mutate(energy_sq = energy*energy) %>%
  mutate(live_sq = live*live) %>%
  mutate(ln_valence = log(valence)) %>%
  mutate(bpm_sq = bpm*bpm) %>%
  mutate(db_sq = db*db) %>%
  mutate(positive = ifelse(valence > 50, 1, 0))  

# summary stats
str(rs_edited)
```

    ## tibble [500 × 15] (S3: tbl_df/tbl/data.frame)
    ##  $ song_rank    : num [1:500] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dance        : num [1:500] 80.5 79.7 21.2 48.2 50.2 28.3 39 79.7 82.3 72.7 ...
    ##  $ mode         : num [1:500] 100 100 100 100 100 0 100 100 100 0 ...
    ##  $ energy       : num [1:500] 55.8 58.2 38.3 72.1 91.2 71.6 50.2 75 33.8 97.4 ...
    ##  $ db           : num [1:500] -5.23 -12.97 -10.07 -6.84 -4.56 ...
    ##  $ speech       : num [1:500] 4.1 25.5 3.47 3.21 5.64 9.86 17.8 24.7 4.13 6.64 ...
    ##  $ acoustic     : num [1:500] 16.4 0.48 72.3 73.1 0 44.7 33.6 53.3 13.9 10.3 ...
    ##  $ instrum      : num [1:500] 0 0 0 0 0.02 0 0.01 10.8 0.13 0.05 ...
    ##  $ live         : num [1:500] 5.46 51.7 29.9 18.9 10.6 39.9 7.13 9.5 50 17.4 ...
    ##  $ valence      : num [1:500] 96.5 41.5 45.2 55.7 72 82.8 28.9 74 78.5 96.5 ...
    ##  $ bpm          : num [1:500] 115 106 173.8 95.3 116.8 ...
    ##  $ duration_(ms): num [1:500] 147600 282640 191160 369600 301920 ...
    ##  $ popularity   : num [1:500] 74 58 68 72 78 71 70 70 68 81 ...
    ##  $ spotify_id   : chr [1:500] "7s25THrKz86DM225dOYwnr" "1yo16b3u0lptm6Cs7lx4AD" "0KOE1hat4SIer491XKk4Pa" "3AhXZa8sUQht0UEdBJgpGc" ...
    ##  $ duration_m   : num [1:500] 2.46 4.71 3.19 6.16 5.03 ...

``` r
summary(rs_edited)
```

    ##    song_rank         dance            mode           energy            db         
    ##  Min.   :  1.0   Min.   :13.80   Min.   :  0.0   Min.   : 4.00   Min.   :-22.627  
    ##  1st Qu.:125.8   1st Qu.:47.08   1st Qu.:  0.0   1st Qu.:44.02   1st Qu.:-11.194  
    ##  Median :250.5   Median :59.80   Median :100.0   Median :65.75   Median : -8.694  
    ##  Mean   :250.5   Mean   :59.22   Mean   : 73.6   Mean   :61.83   Mean   : -9.078  
    ##  3rd Qu.:375.2   3rd Qu.:71.75   3rd Qu.:100.0   3rd Qu.:79.70   3rd Qu.: -6.369  
    ##  Max.   :500.0   Max.   :94.20   Max.   :100.0   Max.   :98.90   Max.   : -1.609  
    ##      speech          acoustic        instrum            live           valence     
    ##  Min.   : 2.430   Min.   : 0.00   Min.   : 0.000   Min.   : 2.600   Min.   : 6.00  
    ##  1st Qu.: 3.380   1st Qu.: 3.20   1st Qu.: 0.000   1st Qu.: 8.545   1st Qu.:41.00  
    ##  Median : 4.340   Median :18.70   Median : 0.010   Median :11.700   Median :61.95  
    ##  Mean   : 7.611   Mean   :28.77   Mean   : 4.494   Mean   :17.884   Mean   :59.57  
    ##  3rd Qu.: 8.117   3rd Qu.:49.23   3rd Qu.: 0.270   3rd Qu.:21.725   3rd Qu.:79.40  
    ##  Max.   :42.700   Max.   :98.90   Max.   :94.900   Max.   :97.900   Max.   :98.50  
    ##       bpm         duration_(ms)      popularity     spotify_id       
    ##  Min.   : 61.34   Min.   :113000   Min.   : 6.00   Length:500        
    ##  1st Qu.:101.96   1st Qu.:187817   1st Qu.:57.00   Class :character  
    ##  Median :118.32   Median :235106   Median :66.00   Mode  :character  
    ##  Mean   :121.50   Mean   :253397   Mean   :63.57                     
    ##  3rd Qu.:136.01   3rd Qu.:294724   3rd Qu.:74.00                     
    ##  Max.   :208.28   Max.   :720186   Max.   :86.00                     
    ##    duration_m    
    ##  Min.   : 1.883  
    ##  1st Qu.: 3.130  
    ##  Median : 3.918  
    ##  Mean   : 4.223  
    ##  3rd Qu.: 4.912  
    ##  Max.   :12.003

``` r
str(rs_extra)
```

    ## tibble [500 × 22] (S3: tbl_df/tbl/data.frame)
    ##  $ song_rank    : num [1:500] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dance        : num [1:500] 80.5 79.7 21.2 48.2 50.2 28.3 39 79.7 82.3 72.7 ...
    ##  $ mode         : num [1:500] 100 100 100 100 100 0 100 100 100 0 ...
    ##  $ energy       : num [1:500] 55.8 58.2 38.3 72.1 91.2 71.6 50.2 75 33.8 97.4 ...
    ##  $ db           : num [1:500] -5.23 -12.97 -10.07 -6.84 -4.56 ...
    ##  $ speech       : num [1:500] 4.1 25.5 3.47 3.21 5.64 9.86 17.8 24.7 4.13 6.64 ...
    ##  $ acoustic     : num [1:500] 16.4 0.48 72.3 73.1 0 44.7 33.6 53.3 13.9 10.3 ...
    ##  $ instrum      : num [1:500] 0 0 0 0 0.02 0 0.01 10.8 0.13 0.05 ...
    ##  $ live         : num [1:500] 5.46 51.7 29.9 18.9 10.6 39.9 7.13 9.5 50 17.4 ...
    ##  $ valence      : num [1:500] 96.5 41.5 45.2 55.7 72 82.8 28.9 74 78.5 96.5 ...
    ##  $ bpm          : num [1:500] 115 106 173.8 95.3 116.8 ...
    ##  $ duration_(ms): num [1:500] 147600 282640 191160 369600 301920 ...
    ##  $ popularity   : num [1:500] 74 58 68 72 78 71 70 70 68 81 ...
    ##  $ spotify_id   : chr [1:500] "7s25THrKz86DM225dOYwnr" "1yo16b3u0lptm6Cs7lx4AD" "0KOE1hat4SIer491XKk4Pa" "3AhXZa8sUQht0UEdBJgpGc" ...
    ##  $ duration_m   : num [1:500] 2.46 4.71 3.19 6.16 5.03 ...
    ##  $ dance_sq     : num [1:500] 6480 6352 449 2323 2520 ...
    ##  $ energy_sq    : num [1:500] 3114 3387 1467 5198 8317 ...
    ##  $ live_sq      : num [1:500] 29.8 2672.9 894 357.2 112.4 ...
    ##  $ ln_valence   : num [1:500] 4.57 3.73 3.81 4.02 4.28 ...
    ##  $ bpm_sq       : num [1:500] 13214 11230 30203 9075 13633 ...
    ##  $ db_sq        : num [1:500] 27.3 168.2 101.4 46.8 20.8 ...
    ##  $ positive     : num [1:500] 1 0 0 1 1 1 0 1 1 1 ...

``` r
summary(rs_extra)
```

    ##    song_rank         dance            mode           energy            db         
    ##  Min.   :  1.0   Min.   :13.80   Min.   :  0.0   Min.   : 4.00   Min.   :-22.627  
    ##  1st Qu.:125.8   1st Qu.:47.08   1st Qu.:  0.0   1st Qu.:44.02   1st Qu.:-11.194  
    ##  Median :250.5   Median :59.80   Median :100.0   Median :65.75   Median : -8.694  
    ##  Mean   :250.5   Mean   :59.22   Mean   : 73.6   Mean   :61.83   Mean   : -9.078  
    ##  3rd Qu.:375.2   3rd Qu.:71.75   3rd Qu.:100.0   3rd Qu.:79.70   3rd Qu.: -6.369  
    ##  Max.   :500.0   Max.   :94.20   Max.   :100.0   Max.   :98.90   Max.   : -1.609  
    ##      speech          acoustic        instrum            live           valence     
    ##  Min.   : 2.430   Min.   : 0.00   Min.   : 0.000   Min.   : 2.600   Min.   : 6.00  
    ##  1st Qu.: 3.380   1st Qu.: 3.20   1st Qu.: 0.000   1st Qu.: 8.545   1st Qu.:41.00  
    ##  Median : 4.340   Median :18.70   Median : 0.010   Median :11.700   Median :61.95  
    ##  Mean   : 7.611   Mean   :28.77   Mean   : 4.494   Mean   :17.884   Mean   :59.57  
    ##  3rd Qu.: 8.117   3rd Qu.:49.23   3rd Qu.: 0.270   3rd Qu.:21.725   3rd Qu.:79.40  
    ##  Max.   :42.700   Max.   :98.90   Max.   :94.900   Max.   :97.900   Max.   :98.50  
    ##       bpm         duration_(ms)      popularity     spotify_id       
    ##  Min.   : 61.34   Min.   :113000   Min.   : 6.00   Length:500        
    ##  1st Qu.:101.96   1st Qu.:187817   1st Qu.:57.00   Class :character  
    ##  Median :118.32   Median :235106   Median :66.00   Mode  :character  
    ##  Mean   :121.50   Mean   :253397   Mean   :63.57                     
    ##  3rd Qu.:136.01   3rd Qu.:294724   3rd Qu.:74.00                     
    ##  Max.   :208.28   Max.   :720186   Max.   :86.00                     
    ##    duration_m        dance_sq        energy_sq       live_sq          ln_valence   
    ##  Min.   : 1.883   Min.   : 190.4   Min.   :  16   Min.   :   6.76   Min.   :1.792  
    ##  1st Qu.: 3.130   1st Qu.:2216.1   1st Qu.:1938   1st Qu.:  73.02   1st Qu.:3.714  
    ##  Median : 3.918   Median :3576.0   Median :4323   Median : 136.89   Median :4.126  
    ##  Mean   : 4.223   Mean   :3790.3   Mean   :4319   Mean   : 563.29   Mean   :3.976  
    ##  3rd Qu.: 4.912   3rd Qu.:5148.1   3rd Qu.:6352   3rd Qu.: 472.02   3rd Qu.:4.374  
    ##  Max.   :12.003   Max.   :8873.6   Max.   :9781   Max.   :9584.41   Max.   :4.590  
    ##      bpm_sq          db_sq            positive   
    ##  Min.   : 3763   Min.   :  2.589   Min.   :0.00  
    ##  1st Qu.:10395   1st Qu.: 40.563   1st Qu.:0.00  
    ##  Median :13999   Median : 75.594   Median :1.00  
    ##  Mean   :15520   Mean   : 95.462   Mean   :0.66  
    ##  3rd Qu.:18499   3rd Qu.:125.311   3rd Qu.:1.00  
    ##  Max.   :43381   Max.   :511.981   Max.   :1.00

``` r
# this dataset is for out of model testing, going to use the rolling stone set to establish the features i want to use in my initial regression, once i find a group of features that explain the valence of a song i will then run with this set to see if those features are good predictors for regular data 

out_of_sample <- read_csv("/Users/jimmyq/Desktop/senior fall/econ370/q_data/spotifyoutofsamplebig.csv")
```

    ## New names:
    ## * `` -> ...1

``` r
# /Users/jimmyq/Desktop/senior fall/econ370/q_data/spotifyoutofsample.csv --> smaller subset of the top 10 including "happy"


oos_edited <- out_of_sample %>%
  rename(index = ...1, bpm = tempo, dB = loudness, acoustic = acous, song_name = name, spotify_id = id) %>%
  mutate(index = index + 1) %>% # not totally sure this column is 100% reliable i need to check 
  rename_with( ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) # next mutate to scale 

oos_edited <- oos_edited %>% # scaling the numbers so they're easier to work with 
  mutate(dance = dance*100) %>%
  mutate(energy = energy*100) %>%
  mutate(speech = speech*100) %>%
  mutate(mode = mode*100) %>%
  mutate(acoustic = round(acoustic*100, 2)) %>%
  mutate(instrum = round(instrum*100, 2)) %>%
  mutate(live = live*100) %>%
  mutate(valence = valence*100) %>%
  mutate(duration_m = `duration_(ms)`/60000) %>%
  mutate(dance_sq = dance*dance) %>%
  mutate(energy_sq = energy*energy) %>%
  mutate(live_sq = live*live) %>%
  mutate(ln_valence = log(valence)) %>%
  mutate(bpm_sq = bpm*bpm) %>%
  mutate(db_sq = db*db) %>%
  mutate(positive = ifelse(valence > 50, 1, 0)) %>%
  select(-c(song_name, artist, album, id_check, spotify_id, index, key)) # dropping key for now, just because it is difficult to deal with 

# summary stats
str(oos_edited)
```

    ## tibble [110 × 20] (S3: tbl_df/tbl/data.frame)
    ##  $ dance        : num [1:110] 38.8 55.9 39.6 57.8 52.6 32 58.3 62.2 58.9 62 ...
    ##  $ mode         : num [1:110] 100 100 0 100 100 100 100 0 100 100 ...
    ##  $ energy       : num [1:110] 33.8 86.8 47.3 65.5 77.5 91.7 41.4 80.7 71.5 57.3 ...
    ##  $ db           : num [1:110] -10.05 -5.28 -7.67 -7.93 -6.05 ...
    ##  $ speech       : num [1:110] 3.29 17 3.45 2.77 3.52 7.71 2.89 40.8 4.01 4.23 ...
    ##  $ acoustic     : num [1:110] 65.2 4.75 32.6 12.1 70.7 7.17 13 10.3 8.84 27.1 ...
    ##  $ instrum      : num [1:110] 0 0.02 0.01 0 0 0 0.06 0 0.1 0 ...
    ##  $ live         : num [1:110] 24.8 77.6 10.9 13.7 21.7 16.9 6.4 30.6 5.59 6.07 ...
    ##  $ valence      : num [1:110] 47.8 60.9 38.5 89.8 96.2 71.5 53.2 89.2 80.6 89.7 ...
    ##  $ bpm          : num [1:110] 177.8 156.3 133.4 101.9 80.1 ...
    ##  $ duration_(ms): num [1:110] 303373 209413 219027 234840 167373 ...
    ##  $ popularity   : num [1:110] 80 81 70 66 73 67 54 76 58 79 ...
    ##  $ duration_m   : num [1:110] 5.06 3.49 3.65 3.91 2.79 ...
    ##  $ dance_sq     : num [1:110] 1505 3125 1568 3341 2767 ...
    ##  $ energy_sq    : num [1:110] 1142 7534 2237 4290 6006 ...
    ##  $ live_sq      : num [1:110] 615 6022 119 188 471 ...
    ##  $ ln_valence   : num [1:110] 3.87 4.11 3.65 4.5 4.57 ...
    ##  $ bpm_sq       : num [1:110] 31600 24428 17809 10381 6417 ...
    ##  $ db_sq        : num [1:110] 101.1 27.8 58.8 63 36.6 ...
    ##  $ positive     : num [1:110] 0 1 0 1 1 1 1 1 1 1 ...

``` r
summary(oos_edited)
```

    ##      dance            mode            energy            db              speech      
    ##  Min.   :27.60   Min.   :  0.00   Min.   :33.80   Min.   :-16.409   Min.   : 2.270  
    ##  1st Qu.:53.65   1st Qu.: 25.00   1st Qu.:65.67   1st Qu.: -9.050   1st Qu.: 3.373  
    ##  Median :62.70   Median :100.00   Median :78.85   Median : -6.971   Median : 4.255  
    ##  Mean   :62.24   Mean   : 74.55   Mean   :75.62   Mean   : -7.497   Mean   : 5.986  
    ##  3rd Qu.:70.83   3rd Qu.:100.00   3rd Qu.:88.45   3rd Qu.: -5.643   3rd Qu.: 6.410  
    ##  Max.   :91.60   Max.   :100.00   Max.   :97.40   Max.   : -2.261   Max.   :40.800  
    ##     acoustic        instrum             live           valence           bpm        
    ##  Min.   : 0.01   Min.   : 0.0000   Min.   : 3.440   Min.   :17.30   Min.   : 71.52  
    ##  1st Qu.: 2.36   1st Qu.: 0.0000   1st Qu.: 7.445   1st Qu.:59.10   1st Qu.:101.97  
    ##  Median :10.02   Median : 0.0050   Median :13.150   Median :76.60   Median :121.12  
    ##  Mean   :16.42   Mean   : 1.9727   Mean   :19.143   Mean   :71.75   Mean   :123.94  
    ##  3rd Qu.:26.07   3rd Qu.: 0.1575   3rd Qu.:22.250   3rd Qu.:86.38   3rd Qu.:136.85  
    ##  Max.   :70.70   Max.   :88.2000   Max.   :95.000   Max.   :98.20   Max.   :213.77  
    ##  duration_(ms)      popularity      duration_m       dance_sq        energy_sq   
    ##  Min.   :139413   Min.   : 0.00   Min.   :2.324   Min.   : 761.8   Min.   :1142  
    ##  1st Qu.:211850   1st Qu.:57.00   1st Qu.:3.531   1st Qu.:2878.3   1st Qu.:4313  
    ##  Median :232894   Median :67.00   Median :3.882   Median :3931.4   Median :6218  
    ##  Mean   :237204   Mean   :61.45   Mean   :3.953   Mean   :4019.3   Mean   :5943  
    ##  3rd Qu.:256933   3rd Qu.:75.00   3rd Qu.:4.282   3rd Qu.:5016.2   3rd Qu.:7823  
    ##  Max.   :510933   Max.   :83.00   Max.   :8.516   Max.   :8390.6   Max.   :9487  
    ##     live_sq          ln_valence        bpm_sq          db_sq        
    ##  Min.   :  11.83   Min.   :2.851   Min.   : 5114   Min.   :  5.112  
    ##  1st Qu.:  55.43   1st Qu.:4.079   1st Qu.:10399   1st Qu.: 31.846  
    ##  Median : 172.93   Median :4.339   Median :14669   Median : 48.595  
    ##  Mean   : 678.12   Mean   :4.232   Mean   :16084   Mean   : 63.526  
    ##  3rd Qu.: 495.07   3rd Qu.:4.459   3rd Qu.:18727   3rd Qu.: 81.918  
    ##  Max.   :9025.00   Max.   :4.587   Max.   :45699   Max.   :269.255  
    ##     positive     
    ##  Min.   :0.0000  
    ##  1st Qu.:1.0000  
    ##  Median :1.0000  
    ##  Mean   :0.8455  
    ##  3rd Qu.:1.0000  
    ##  Max.   :1.0000

    ##            dance energy speech acoustic instrum  live valence duration_m song_rank
    ## dance       1.00   0.06   0.28    -0.15    0.01 -0.14    0.42      -0.02      0.08
    ## energy      0.06   1.00   0.22    -0.62    0.13  0.08    0.35       0.12      0.08
    ## speech      0.28   0.22   1.00    -0.22   -0.08  0.01    0.06       0.07      0.00
    ## acoustic   -0.15  -0.62  -0.22     1.00   -0.13  0.02   -0.15      -0.21     -0.10
    ## instrum     0.01   0.13  -0.08    -0.13    1.00 -0.04    0.09       0.11     -0.04
    ## live       -0.14   0.08   0.01     0.02   -0.04  1.00   -0.05       0.00      0.00
    ## valence     0.42   0.35   0.06    -0.15    0.09 -0.05    1.00      -0.16      0.05
    ## duration_m -0.02   0.12   0.07    -0.21    0.11  0.00   -0.16       1.00      0.02
    ## song_rank   0.08   0.08   0.00    -0.10   -0.04  0.00    0.05       0.02      1.00
    ## db          0.10   0.74   0.21    -0.42   -0.01 -0.01    0.16       0.02      0.05
    ## bpm        -0.27   0.12  -0.01    -0.03    0.04  0.06    0.11      -0.10     -0.02
    ## popularity  0.02   0.00   0.05    -0.06   -0.07 -0.08   -0.04      -0.04     -0.08
    ##               db   bpm popularity
    ## dance       0.10 -0.27       0.02
    ## energy      0.74  0.12       0.00
    ## speech      0.21 -0.01       0.05
    ## acoustic   -0.42 -0.03      -0.06
    ## instrum    -0.01  0.04      -0.07
    ## live       -0.01  0.06      -0.08
    ## valence     0.16  0.11      -0.04
    ## duration_m  0.02 -0.10      -0.04
    ## song_rank   0.05 -0.02      -0.08
    ## db          1.00  0.03       0.11
    ## bpm         0.03  1.00      -0.04
    ## popularity  0.11 -0.04       1.00

![](spotify_project_files/figure-gfm/plots%20&%20explatory-1.png)<!-- -->

    ##            dance energy speech acoustic instrum  live valence duration_m    db   bpm
    ## dance       1.00  -0.09   0.06    -0.08    0.04 -0.11    0.29       0.06 -0.07 -0.24
    ## energy     -0.09   1.00   0.15    -0.17    0.17  0.21    0.21      -0.08  0.66  0.16
    ## speech      0.06   0.15   1.00     0.03    0.08  0.05    0.06      -0.12  0.11  0.32
    ## acoustic   -0.08  -0.17   0.03     1.00    0.01  0.15   -0.02      -0.08 -0.13  0.09
    ## instrum     0.04   0.17   0.08     0.01    1.00 -0.06   -0.22      -0.05  0.02  0.12
    ## live       -0.11   0.21   0.05     0.15   -0.06  1.00    0.09      -0.01  0.27  0.00
    ## valence     0.29   0.21   0.06    -0.02   -0.22  0.09    1.00      -0.05  0.01 -0.05
    ## duration_m  0.06  -0.08  -0.12    -0.08   -0.05 -0.01   -0.05       1.00 -0.19 -0.01
    ## db         -0.07   0.66   0.11    -0.13    0.02  0.27    0.01      -0.19  1.00  0.10
    ## bpm        -0.24   0.16   0.32     0.09    0.12  0.00   -0.05      -0.01  0.10  1.00
    ## popularity  0.02  -0.10  -0.07     0.06   -0.14 -0.02   -0.06      -0.04 -0.12 -0.09
    ##            popularity
    ## dance            0.02
    ## energy          -0.10
    ## speech          -0.07
    ## acoustic         0.06
    ## instrum         -0.14
    ## live            -0.02
    ## valence         -0.06
    ## duration_m      -0.04
    ## db              -0.12
    ## bpm             -0.09
    ## popularity       1.00

![](spotify_project_files/figure-gfm/plots%20&%20explatory-2.png)<!-- -->![](spotify_project_files/figure-gfm/plots%20&%20explatory-3.png)<!-- -->![](spotify_project_files/figure-gfm/plots%20&%20explatory-4.png)<!-- -->

    ## [1] -0.3126999

    ##   [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ##  [14] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ##  [27] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ##  [40] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ##  [53] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ##  [66] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ##  [79] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ##  [92] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [105] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [118] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [131] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [144] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [157] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE
    ## [170] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [183] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [196] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [209] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [222] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [235] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [248] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [261] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [274] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [287] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [300] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [313] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [326] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [339] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [352] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [365] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [378] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [391] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [404] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [417] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [430] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [443] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [456] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [469] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [482] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [495] FALSE FALSE FALSE FALSE FALSE FALSE

``` r
# summary tables creating using vtable library, should generate laTex code in the output. Then use that laTex code in overleaf project document to output the pdfs 

# summary table, for rs
# st(rs_extra,
#    out = "latex",
#    file = "rollingStone_summary.tex",
#    summ = c("min(x)", "mean(x)", "max(x)"),
#    summ.names = c("Minimum", "Mean", "Maximum"),
#    title = "Rolling Stone 500 Greatest Songs Summary Statistics"
#    )
# 
# # summary measures for oos 
# st(oos_edited,
#    out = "latex",
#    file = "OutOfSample_summary.tex",
#    summ = c("min(x)", "mean(x)", "max(x)"),
#    summ.names = c("Minimum", "Mean", "Maximum"),
#    title = "Out of Sample for Testing Summary Statistics"
#    )

# rolling stone
# stargazer(reg1, tobit1, reg2, tobit2, logit1, title = "Preliminary Results", align = TRUE,
#           dep.var.labels = c("Valence", "Positive or Not"),
#           omit.stat = c("LL", "ser", "F"), no.space = TRUE)

# out of sample test: from jacob jolij 
# stargazer(reg2, tobit3, title = "Out of Sample Results", align = TRUE,
#          dep.var.labels = c("Valence"), omit.stat = c("LL", "ser", "F"), no.space = TRUE)
```

``` r
# OLS on rolling stone
reg1 <- lm(valence ~ dance + energy + db + speech +
            acoustic + live + popularity + 
            bpm + duration_m, data = rs_edited)
summary(reg1)
```

    ## 
    ## Call:
    ## lm(formula = valence ~ dance + energy + db + speech + acoustic + 
    ##     live + popularity + bpm + duration_m, data = rs_edited)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -60.102 -12.921   1.615  13.688  41.730 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -46.00298   10.62206  -4.331 1.80e-05 ***
    ## dance         0.69923    0.05395  12.961  < 2e-16 ***
    ## energy        0.68077    0.06579  10.347  < 2e-16 ***
    ## db           -1.83596    0.35088  -5.232 2.49e-07 ***
    ## speech       -0.35784    0.11706  -3.057  0.00236 ** 
    ## acoustic      0.12111    0.03791   3.195  0.00149 ** 
    ## live         -0.06723    0.05392  -1.247  0.21304    
    ## popularity   -0.01016    0.06150  -0.165  0.86890    
    ## bpm           0.14085    0.03168   4.446 1.08e-05 ***
    ## duration_m   -2.51526    0.55168  -4.559 6.49e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.36 on 490 degrees of freedom
    ## Multiple R-squared:  0.4058, Adjusted R-squared:  0.3949 
    ## F-statistic: 37.18 on 9 and 490 DF,  p-value: < 2.2e-16

``` r
# tobit on rolling stone to find optimal mix
tobit1 <- tobit(valence ~ dance + dance_sq + energy + energy_sq +
                  db + db_sq + speech + acoustic + live + popularity + bpm +
                  bpm_sq + duration_m, left = 0, right = 100, data = rs_extra)
summary(tobit1) 
```

    ## 
    ## Call:
    ## tobit(formula = valence ~ dance + dance_sq + energy + energy_sq + 
    ##     db + db_sq + speech + acoustic + live + popularity + bpm + 
    ##     bpm_sq + duration_m, left = 0, right = 100, data = rs_extra)
    ## 
    ## Observations:
    ##          Total  Left-censored     Uncensored Right-censored 
    ##            500              0            500              0 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.119e+01  1.852e+01  -1.144 0.252456    
    ## dance        8.535e-01  2.977e-01   2.867 0.004143 ** 
    ## dance_sq    -1.187e-03  2.533e-03  -0.469 0.639208    
    ## energy       1.398e+00  2.442e-01   5.723 1.05e-08 ***
    ## energy_sq   -5.726e-03  1.923e-03  -2.977 0.002910 ** 
    ## db          -2.031e+00  1.061e+00  -1.914 0.055565 .  
    ## db_sq        1.108e-03  4.959e-02   0.022 0.982166    
    ## speech      -4.585e-01  1.199e-01  -3.824 0.000131 ***
    ## acoustic     1.327e-01  3.756e-02   3.533 0.000410 ***
    ## live        -6.706e-02  5.229e-02  -1.282 0.199734    
    ## popularity  -3.890e-02  5.985e-02  -0.650 0.515745    
    ## bpm         -6.492e-01  2.266e-01  -2.865 0.004164 ** 
    ## bpm_sq       3.043e-03  8.741e-04   3.481 0.000500 ***
    ## duration_m  -2.486e+00  5.396e-01  -4.607 4.08e-06 ***
    ## Log(scale)   2.875e+00  3.162e-02  90.914  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Scale: 17.72 
    ## 
    ## Gaussian distribution
    ## Number of Newton-Raphson Iterations: 5 
    ## Log-likelihood: -2147 on 15 Df
    ## Wald-statistic: 384.7 on 13 Df, p-value: < 2.22e-16

``` r
# logit on rolling stone
logit1 <- glm(positive ~ dance + energy + db + 
            speech + acoustic + live + popularity + bpm +
            duration_m, data = rs_extra, family = binomial(link = "logit"))
summary(logit1)
```

    ## 
    ## Call:
    ## glm(formula = positive ~ dance + energy + db + speech + acoustic + 
    ##     live + popularity + bpm + duration_m, family = binomial(link = "logit"), 
    ##     data = rs_extra)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7702  -0.7093   0.4066   0.7105   2.4126  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -10.131461   1.632394  -6.207 5.42e-10 ***
    ## dance         0.078826   0.009035   8.724  < 2e-16 ***
    ## energy        0.071097   0.010409   6.831 8.46e-12 ***
    ## db           -0.183501   0.051808  -3.542 0.000397 ***
    ## speech       -0.021690   0.016010  -1.355 0.175479    
    ## acoustic      0.013521   0.005334   2.535 0.011257 *  
    ## live         -0.010481   0.007501  -1.397 0.162326    
    ## popularity   -0.009279   0.008822  -1.052 0.292938    
    ## bpm           0.010765   0.004318   2.493 0.012677 *  
    ## duration_m   -0.103327   0.079653  -1.297 0.194558    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 641.04  on 499  degrees of freedom
    ## Residual deviance: 464.68  on 490  degrees of freedom
    ## AIC: 484.68
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
# tobit on out of sample set
tobit2 <- tobit(valence ~ dance + energy + db + speech + 
                  acoustic + bpm + duration_m, left = 0, right = 100, data = oos_edited)
summary(tobit2) 
```

    ## 
    ## Call:
    ## tobit(formula = valence ~ dance + energy + db + speech + acoustic + 
    ##     bpm + duration_m, left = 0, right = 100, data = oos_edited)
    ## 
    ## Observations:
    ##          Total  Left-censored     Uncensored Right-censored 
    ##            110              0            110              0 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  1.53700   20.47724   0.075 0.940168    
    ## dance        0.47275    0.13687   3.454 0.000552 ***
    ## energy       0.49172    0.14327   3.432 0.000599 ***
    ## db          -1.63102    0.79000  -2.065 0.038963 *  
    ## speech       0.02328    0.32782   0.071 0.943390    
    ## acoustic     0.04076    0.09275   0.439 0.660318    
    ## bpm         -0.01569    0.06483  -0.242 0.808798    
    ## duration_m  -1.89370    2.01433  -0.940 0.347159    
    ## Log(scale)   2.80220    0.06742  41.563  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Scale: 16.48 
    ## 
    ## Gaussian distribution
    ## Number of Newton-Raphson Iterations: 4 
    ## Log-likelihood: -464.3 on 9 Df
    ## Wald-statistic: 23.95 on 7 Df, p-value: 0.0011617

``` r
# OLS on out of sample set
reg2 <- lm(valence ~ dance + energy + db + speech +
            acoustic + bpm + duration_m, data = oos_edited)
summary(reg2)
```

    ## 
    ## Call:
    ## lm(formula = valence ~ dance + energy + db + speech + acoustic + 
    ##     bpm + duration_m, data = oos_edited)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -44.292  -9.916   3.964  12.917  28.687 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  1.53700   21.26511   0.072  0.94252   
    ## dance        0.47275    0.14213   3.326  0.00122 **
    ## energy       0.49172    0.14879   3.305  0.00131 **
    ## db          -1.63102    0.82040  -1.988  0.04948 * 
    ## speech       0.02328    0.34043   0.068  0.94562   
    ## acoustic     0.04076    0.09632   0.423  0.67305   
    ## bpm         -0.01569    0.06732  -0.233  0.81622   
    ## duration_m  -1.89370    2.09183  -0.905  0.36745   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.11 on 102 degrees of freedom
    ## Multiple R-squared:  0.1788, Adjusted R-squared:  0.1225 
    ## F-statistic: 3.173 on 7 and 102 DF,  p-value: 0.004453

``` r
# # OLS on big set, lets see where this goes 
reg3 <- lm(valence ~ dance + energy + db + speech +
            acoustic + live + popularity +
            bpm + duration_m, data = bs_edited)
summary(reg3)
```

    ## 
    ## Call:
    ## lm(formula = valence ~ dance + energy + db + speech + acoustic + 
    ##     live + popularity + bpm + duration_m, data = bs_edited)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -79.144 -13.527   1.228  14.183  72.533 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -24.749272   3.739232  -6.619 4.27e-11 ***
    ## dance         0.731068   0.023245  31.450  < 2e-16 ***
    ## energy        0.503075   0.027455  18.324  < 2e-16 ***
    ## db           -0.830916   0.129463  -6.418 1.60e-10 ***
    ## speech       -0.185453   0.051764  -3.583 0.000345 ***
    ## acoustic      0.056166   0.015222   3.690 0.000228 ***
    ## live          0.020480   0.022083   0.927 0.353784    
    ## popularity    0.002556   0.014556   0.176 0.860631    
    ## bpm           0.105188   0.012632   8.327  < 2e-16 ***
    ## duration_m   -2.533288   0.240402 -10.538  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.3 on 2996 degrees of freedom
    ## Multiple R-squared:  0.4053, Adjusted R-squared:  0.4035 
    ## F-statistic: 226.9 on 9 and 2996 DF,  p-value: < 2.2e-16

``` r
# tobit on big set, gonna find this optimal mix
tobit3 <- tobit(valence ~ dance + dance_sq + energy + energy_sq +
                  db + db_sq + speech + acoustic + live + popularity + bpm +
                  bpm_sq + duration_m, left = 0, right = 100, data = bs_edited)
summary(tobit3)
```

    ## 
    ## Call:
    ## tobit(formula = valence ~ dance + dance_sq + energy + energy_sq + 
    ##     db + db_sq + speech + acoustic + live + popularity + bpm + 
    ##     bpm_sq + duration_m, left = 0, right = 100, data = bs_edited)
    ## 
    ## Observations:
    ##          Total  Left-censored     Uncensored Right-censored 
    ##           3006              1           3005              0 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -3.382e+01  6.468e+00  -5.229 1.71e-07 ***
    ## dance        1.042e+00  1.173e-01   8.884  < 2e-16 ***
    ## dance_sq    -2.820e-03  1.047e-03  -2.693 0.007087 ** 
    ## energy       1.083e+00  8.229e-02  13.157  < 2e-16 ***
    ## energy_sq   -4.614e-03  6.791e-04  -6.794 1.09e-11 ***
    ## db          -2.639e+00  3.289e-01  -8.024 1.02e-15 ***
    ## db_sq       -5.632e-02  1.181e-02  -4.768 1.86e-06 ***
    ## speech      -1.724e-01  5.168e-02  -3.336 0.000850 ***
    ## acoustic     7.995e-02  1.525e-02   5.243 1.58e-07 ***
    ## live         1.903e-02  2.152e-02   0.884 0.376510    
    ## popularity   3.060e-03  1.422e-02   0.215 0.829611    
    ## bpm         -3.376e-01  8.689e-02  -3.885 0.000102 ***
    ## bpm_sq       1.775e-03  3.457e-04   5.135 2.83e-07 ***
    ## duration_m  -2.627e+00  2.357e-01 -11.145  < 2e-16 ***
    ## Log(scale)   2.932e+00  1.290e-02 227.300  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Scale: 18.77 
    ## 
    ## Gaussian distribution
    ## Number of Newton-Raphson Iterations: 5 
    ## Log-likelihood: -1.308e+04 on 15 Df
    ## Wald-statistic:  2324 on 13 Df, p-value: < 2.22e-16

``` r
# logit on big boy set, this might be interesting
logit2 <- glm(positive ~ dance + energy + db +
            speech + acoustic + live + popularity + bpm +
            duration_m, data = bs_edited, family = binomial(link = "logit"))
summary(logit2)
```

    ## 
    ## Call:
    ## glm(formula = positive ~ dance + energy + db + speech + acoustic + 
    ##     live + popularity + bpm + duration_m, family = binomial(link = "logit"), 
    ##     data = bs_edited)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7145  -0.8556   0.4499   0.8116   3.2417  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -6.8531931  0.5049966 -13.571  < 2e-16 ***
    ## dance        0.0670033  0.0033828  19.807  < 2e-16 ***
    ## energy       0.0448150  0.0036330  12.336  < 2e-16 ***
    ## db          -0.0883920  0.0163954  -5.391 7.00e-08 ***
    ## speech      -0.0105315  0.0062946  -1.673   0.0943 .  
    ## acoustic     0.0036158  0.0019064   1.897   0.0579 .  
    ## live         0.0015720  0.0027603   0.570   0.5690    
    ## popularity  -0.0004266  0.0018385  -0.232   0.8165    
    ## bpm          0.0088469  0.0015611   5.667 1.45e-08 ***
    ## duration_m  -0.2283693  0.0330278  -6.914 4.70e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 4060.0  on 3005  degrees of freedom
    ## Residual deviance: 3108.7  on 2996  degrees of freedom
    ## AIC: 3128.7
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# function to manually calculate the r squared of each regression
r2 <- function(x, y) 
{
  return (sum((fitted(x) - mean(y$valence))^2)/((sum((fitted(x) - y$valence)^2) + sum((fitted(x) - mean(y$valence))^2))))
}

# function to manually calculate the adj r squared of each regression
adj_r2 <- function(x, y) 
{
   return (1 - ((1 - r2(x, y))*(nobs(x) -1)/(nobs(x) - length(x$coefficients) - 1)))
}

# reports a list of the r2
mod_r2 <- list(r2(reg1, rs_edited),
               r2(tobit2, rs_extra),
               r2(logit1, rs_extra),
               r2(tobit3, oos_edited),
               r2(reg2, oos_edited))
```

    ## Warning in fitted(x) - y$valence: longer object length is not a multiple of shorter
    ## object length

    ## Warning in fitted(x) - y$valence: longer object length is not a multiple of shorter
    ## object length

``` r
# reports a list of the r2 
adj_r2_list <- list(adj_r2(reg1, rs_edited),
                 adj_r2(tobit2, rs_extra),
                 adj_r2(logit1, rs_extra),               
                 adj_r2(tobit3, oos_edited),
                 adj_r2(reg2, oos_edited))
```

    ## Warning in fitted(x) - y$valence: longer object length is not a multiple of shorter
    ## object length

    ## Warning in fitted(x) - y$valence: longer object length is not a multiple of shorter
    ## object length

``` r
# predict and fit 
pred <- predict(reg3, oos_edited)
pred_oos <- bind_cols(oos_edited, pred)
```

    ## New names:
    ## * NA -> ...21

``` r
pred_oos %>%
  select(c(...21, valence))
```

    ## # A tibble: 110 × 2
    ##    ...21 valence
    ##    <dbl>   <dbl>
    ##  1  38.6    47.8
    ##  2  70.7    60.9
    ##  3  40.8    38.5
    ##  4  58.5    89.8
    ##  5  63.0    96.2
    ##  6  61.3    71.5
    ##  7  50.0    53.2
    ##  8  69.8    89.2
    ##  9  63.5    80.6
    ## 10  59.2    89.7
    ## # … with 100 more rows
