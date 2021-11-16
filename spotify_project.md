final_proj
================

``` r
#Sys.setenv(SPOTIFY_CLIENT_ID = '24105134c057498aa68b53aaba318ef5') # this is just developer 
#Sys.setenv(SPOTIFY_CLIENT_SECRET = '4220a8ab4b224fde8cf50182b6171f34') # may change with every session

#access_token <- get_spotify_access_token() # access token from developer dashboard 

#my_plists <- get_user_playlists('6iiwl0m7m0jgvcu7khnis8o6q', offset = 0) # user id

#tracks <- get_playlist_tracks(my_plists)
#features <- get_track_audio_features(tracks)
```

This first dataset…

``` r
# test set, using this to play around with what variables to use

# top10 <- read_csv("/Users/jimmyq/Desktop/senior fall/econ370/q_data/top10s.csv")
# https://www.kaggle.com/leonardopena/top-spotify-songs-from-20102019-by-year : datasource 

# basic cleaning 
# top10edited <- top10 %>%
  #drop_na() %>% # drop missing values (should be none)
  #select(-c(...1)) %>% # drop index column
  #rename(energy = nrgy, dance = dnce, valence = val) %>%
  #rename_with( ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) # lowercase all columns + replace space with "_"  

# summary statistics of top10 dataset 
#str(top10edited)
#summary(top10edited)
```

This second dataset…

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
  mutate(mode = mode*100) %>%
  mutate(acoustic = round(acoustic*100, 2)) %>%
  mutate(instrum = round(instrum*100, 2)) %>%
  mutate(live = live*100) %>%
  mutate(valence = valence*100) %>%
  mutate(duration_s = `duration_(ms)`/1000) %>%
  select(-c(song_name, artist, album, id_check))

# need to run some interaction effects
# rs_interaction <- rs_edited %>%
#   mutate(dance_sq = dance*dance) %>%
#   mutate(energy_sq = energy*energy) %>%
#   mutate(live_sq = live*live) %>%
#   mutate(ln_valence = log(valence))

# summary stats
str(rs_edited)
```

    ## tibble [500 × 16] (S3: tbl_df/tbl/data.frame)
    ##  $ song_rank    : num [1:500] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dance        : num [1:500] 80.5 79.7 21.2 48.2 50.2 28.3 39 79.7 82.3 72.7 ...
    ##  $ mode         : num [1:500] 100 100 100 100 100 0 100 100 100 0 ...
    ##  $ energy       : num [1:500] 55.8 58.2 38.3 72.1 91.2 71.6 50.2 75 33.8 97.4 ...
    ##  $ key          : num [1:500] 0 2 10 0 1 1 10 0 0 4 ...
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
    ##  $ duration_s   : num [1:500] 148 283 191 370 302 ...

``` r
summary(rs_edited)
```

    ##    song_rank         dance            mode           energy           key        
    ##  Min.   :  1.0   Min.   :13.80   Min.   :  0.0   Min.   : 4.00   Min.   : 0.000  
    ##  1st Qu.:125.8   1st Qu.:47.08   1st Qu.:  0.0   1st Qu.:44.02   1st Qu.: 2.000  
    ##  Median :250.5   Median :59.80   Median :100.0   Median :65.75   Median : 5.000  
    ##  Mean   :250.5   Mean   :59.22   Mean   : 73.6   Mean   :61.83   Mean   : 4.824  
    ##  3rd Qu.:375.2   3rd Qu.:71.75   3rd Qu.:100.0   3rd Qu.:79.70   3rd Qu.: 8.000  
    ##  Max.   :500.0   Max.   :94.20   Max.   :100.0   Max.   :98.90   Max.   :11.000  
    ##        db              speech          acoustic        instrum      
    ##  Min.   :-22.627   Min.   : 2.430   Min.   : 0.00   Min.   : 0.000  
    ##  1st Qu.:-11.194   1st Qu.: 3.380   1st Qu.: 3.20   1st Qu.: 0.000  
    ##  Median : -8.694   Median : 4.340   Median :18.70   Median : 0.010  
    ##  Mean   : -9.078   Mean   : 7.611   Mean   :28.77   Mean   : 4.494  
    ##  3rd Qu.: -6.369   3rd Qu.: 8.117   3rd Qu.:49.23   3rd Qu.: 0.270  
    ##  Max.   : -1.609   Max.   :42.700   Max.   :98.90   Max.   :94.900  
    ##       live           valence           bpm         duration_(ms)      popularity   
    ##  Min.   : 2.600   Min.   : 6.00   Min.   : 61.34   Min.   :113000   Min.   : 6.00  
    ##  1st Qu.: 8.545   1st Qu.:41.00   1st Qu.:101.96   1st Qu.:187817   1st Qu.:57.00  
    ##  Median :11.700   Median :61.95   Median :118.32   Median :235106   Median :66.00  
    ##  Mean   :17.884   Mean   :59.57   Mean   :121.50   Mean   :253397   Mean   :63.57  
    ##  3rd Qu.:21.725   3rd Qu.:79.40   3rd Qu.:136.01   3rd Qu.:294724   3rd Qu.:74.00  
    ##  Max.   :97.900   Max.   :98.50   Max.   :208.28   Max.   :720186   Max.   :86.00  
    ##   spotify_id          duration_s   
    ##  Length:500         Min.   :113.0  
    ##  Class :character   1st Qu.:187.8  
    ##  Mode  :character   Median :235.1  
    ##                     Mean   :253.4  
    ##                     3rd Qu.:294.7  
    ##                     Max.   :720.2

(…1, renamed to song_rank) index should correspond with the rank of the
song in the dataset this can be checked by looking at the rankings on rs

``` r
# this dataset is for out of model testing, going to use the rolling stone set to establish the features i want to use in my initial regression, once i find a group of features that explain the valence of a song i will then run with this set to see if those features are good predictors for regular data 

out_of_sample <- read_csv("/Users/jimmyq/Desktop/senior fall/econ370/q_data/spotifyoutofsample.csv")
```

    ## New names:
    ## * `` -> ...1

``` r
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
  mutate(duration_s = `duration_(ms)`/1000) %>%
  select(-c(song_name, artist, album, id_check, index))

# summary stats
str(oos_edited)
```

    ## tibble [11 × 15] (S3: tbl_df/tbl/data.frame)
    ##  $ dance        : num [1:11] 56.1 54.3 37.1 70.1 81.7 50.2 68.6 53.2 77.7 59.6 ...
    ##  $ mode         : num [1:11] 100 100 100 100 0 100 100 100 100 100 ...
    ##  $ energy       : num [1:11] 75.7 87 42.4 94.4 59.9 77.2 58.6 88.7 72.5 86.9 ...
    ##  $ key          : num [1:11] 5 9 10 4 0 0 6 0 0 10 ...
    ##  $ db           : num [1:11] -10.06 -6.51 -9.5 -2.99 -9.25 ...
    ##  $ speech       : num [1:11] 19.2 4.28 4.11 4.55 3.28 3.22 3.56 3.35 5 3.7 ...
    ##  $ acoustic     : num [1:11] 5.63 35.8 33.2 7.55 13.2 59.5 15.3 7.78 1.31 1.16 ...
    ##  $ instrum      : num [1:11] 0.13 0.09 0 0 0.03 0 0.03 0.02 0.8 17.3 ...
    ##  $ live         : num [1:11] 55.8 79.2 13.1 60.1 8.73 19.9 35.3 29.4 24.6 6.78 ...
    ##  $ valence      : num [1:11] 64 75.4 32.5 79.2 54.8 93.5 78 79.5 52.9 94.4 ...
    ##  $ bpm          : num [1:11] 156 101 152 129 109 ...
    ##  $ duration_(ms): num [1:11] 209560 230400 215827 197707 245640 ...
    ##  $ popularity   : num [1:11] 58 81 48 78 76 58 55 83 69 77 ...
    ##  $ spotify_id   : chr [1:11] "43DHLzDkncpby82Po5jlOZ" "0GjEhVFGZW8afUYGChu3Rr" "7tf64lNC31lWlTsih0nfZf" "5zA8vzDGqPl2AzZkEYQGKh" ...
    ##  $ duration_s   : num [1:11] 210 230 216 198 246 ...

``` r
summary(oos_edited)
```

    ##      dance            mode            energy           key               db         
    ##  Min.   :37.10   Min.   :  0.00   Min.   :42.40   Min.   : 0.000   Min.   :-12.377  
    ##  1st Qu.:53.75   1st Qu.:100.00   1st Qu.:66.20   1st Qu.: 0.000   1st Qu.: -9.779  
    ##  Median :59.60   Median :100.00   Median :77.20   Median : 5.000   Median : -8.681  
    ##  Mean   :61.21   Mean   : 81.82   Mean   :75.05   Mean   : 4.455   Mean   : -7.814  
    ##  3rd Qu.:69.35   3rd Qu.:100.00   3rd Qu.:86.95   3rd Qu.: 7.500   3rd Qu.: -5.434  
    ##  Max.   :81.70   Max.   :100.00   Max.   :94.40   Max.   :10.000   Max.   : -2.986  
    ##      speech          acoustic        instrum            live          valence     
    ##  Min.   : 3.220   Min.   : 1.16   Min.   : 0.000   Min.   : 6.78   Min.   :32.50  
    ##  1st Qu.: 3.455   1st Qu.: 6.59   1st Qu.: 0.000   1st Qu.:11.09   1st Qu.:59.40  
    ##  Median : 4.110   Median :13.20   Median : 0.030   Median :24.60   Median :78.00  
    ##  Mean   : 6.595   Mean   :18.39   Mean   : 1.673   Mean   :31.09   Mean   :72.76  
    ##  3rd Qu.: 4.775   3rd Qu.:27.55   3rd Qu.: 0.110   3rd Qu.:45.55   3rd Qu.:86.50  
    ##  Max.   :19.200   Max.   :59.50   Max.   :17.300   Max.   :79.20   Max.   :96.20  
    ##       bpm         duration_(ms)      popularity     spotify_id       
    ##  Min.   : 79.46   Min.   :165613   Min.   :48.00   Length:11         
    ##  1st Qu.:109.39   1st Qu.:203813   1st Qu.:58.00   Class :character  
    ##  Median :120.42   Median :224547   Median :76.00   Mode  :character  
    ##  Mean   :123.25   Mean   :218919   Mean   :69.36                     
    ##  3rd Qu.:140.31   3rd Qu.:235726   3rd Qu.:79.00                     
    ##  Max.   :160.02   Max.   :249293   Max.   :83.00                     
    ##    duration_s   
    ##  Min.   :165.6  
    ##  1st Qu.:203.8  
    ##  Median :224.5  
    ##  Mean   :218.9  
    ##  3rd Qu.:235.7  
    ##  Max.   :249.3

    ##            dance energy speech acoustic instrum  live valence duration_s song_rank
    ## dance       1.00   0.06   0.28    -0.15    0.01 -0.14    0.42      -0.02      0.08
    ## energy      0.06   1.00   0.22    -0.62    0.13  0.08    0.35       0.12      0.08
    ## speech      0.28   0.22   1.00    -0.22   -0.08  0.01    0.06       0.07      0.00
    ## acoustic   -0.15  -0.62  -0.22     1.00   -0.13  0.02   -0.15      -0.21     -0.10
    ## instrum     0.01   0.13  -0.08    -0.13    1.00 -0.04    0.09       0.11     -0.04
    ## live       -0.14   0.08   0.01     0.02   -0.04  1.00   -0.05       0.00      0.00
    ## valence     0.42   0.35   0.06    -0.15    0.09 -0.05    1.00      -0.16      0.05
    ## duration_s -0.02   0.12   0.07    -0.21    0.11  0.00   -0.16       1.00      0.02
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
    ## duration_s  0.02 -0.10      -0.04
    ## song_rank   0.05 -0.02      -0.08
    ## db          1.00  0.03       0.11
    ## bpm         0.03  1.00      -0.04
    ## popularity  0.11 -0.04       1.00

![](spotify_project_files/figure-gfm/plots-1.png)<!-- -->

    ##            dance energy speech acoustic instrum  live valence duration_s    db   bpm
    ## dance       1.00   0.13  -0.02    -0.53   -0.02 -0.06    0.06       0.14 -0.06 -0.13
    ## energy      0.13   1.00   0.13    -0.20    0.25  0.40    0.72       0.00  0.58 -0.20
    ## speech     -0.02   0.13   1.00    -0.16   -0.16  0.08    0.14       0.01  0.07  0.72
    ## acoustic   -0.53  -0.20  -0.16     1.00   -0.33  0.01    0.11      -0.47  0.20 -0.38
    ## instrum    -0.02   0.25  -0.16    -0.33    1.00 -0.33    0.35       0.25 -0.44 -0.18
    ## live       -0.06   0.40   0.08     0.01   -0.33  1.00    0.02      -0.15  0.26 -0.04
    ## valence     0.06   0.72   0.14     0.11    0.35  0.02    1.00      -0.02  0.29 -0.29
    ## duration_s  0.14   0.00   0.01    -0.47    0.25 -0.15   -0.02       1.00 -0.14  0.25
    ## db         -0.06   0.58   0.07     0.20   -0.44  0.26    0.29      -0.14  1.00  0.02
    ## bpm        -0.13  -0.20   0.72    -0.38   -0.18 -0.04   -0.29       0.25  0.02  1.00
    ## popularity  0.40   0.75  -0.02    -0.34    0.20  0.13    0.46       0.50  0.54 -0.15
    ##            popularity
    ## dance            0.40
    ## energy           0.75
    ## speech          -0.02
    ## acoustic        -0.34
    ## instrum          0.20
    ## live             0.13
    ## valence          0.46
    ## duration_s       0.50
    ## db               0.54
    ## bpm             -0.15
    ## popularity       1.00

![](spotify_project_files/figure-gfm/plots-2.png)<!-- -->

``` r
# summary tables creating using vtable library, should generate laTex code in the output. Then use that laTex code in overleaf project document to output the pdfs 

# summary table, for rs
#st(rs_edited,
#   out = "latex",
#   file = "rollingStone_summary.tex",
#   summ = c("min(x)", "mean(x)", "max(x)"),
#   summ.names = c("Minimum", "Mean", "Maximum"),
#   title = "Rolling Stone 500 Greatest Songs Summary Statistics"
#   )

# summary measures for oos 
#st(oos_edited,
#   out = "latex",
#   file = "OutOfSample_summary.tex",
#   summ = c("min(x)", "mean(x)", "max(x)"),
#   summ.names = c("Minimum", "Mean", "Maximum"),
#   title = "Out of Sample for Testing Summary Statistics"
#   )

# st(top10edited,
# out = "latex",
#   file = "top10_summary.tex",
#   summ = c("min(x)", "mean(x)", "max(x)"),
#   summ.names = c("Minimum", "Mean", "Maximum"),
#   title = "Top 10s since 2010-19 Summary Statistics"
#   )
```

``` r
# gonna run a few different regressions to fit the rolling stone list valence
# to my test set 
# prelim investigation 


reg <- lm(valence ~ dance + energy + key + db + 
            speech + acoustic + instrum + live + popularity + bpm +
            duration_s, data = rs_edited)
summary(reg)
```

    ## 
    ## Call:
    ## lm(formula = valence ~ dance + energy + key + db + speech + acoustic + 
    ##     instrum + live + popularity + bpm + duration_s, data = rs_edited)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -60.163 -12.879   1.438  13.509  42.150 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -46.404107  10.728934  -4.325 1.85e-05 ***
    ## dance         0.699791   0.054108  12.933  < 2e-16 ***
    ## energy        0.675704   0.066837  10.110  < 2e-16 ***
    ## key           0.161214   0.236410   0.682  0.49561    
    ## db           -1.814345   0.355569  -5.103 4.81e-07 ***
    ## speech       -0.364623   0.119066  -3.062  0.00232 ** 
    ## acoustic      0.120235   0.038058   3.159  0.00168 ** 
    ## instrum       0.016612   0.057777   0.288  0.77384    
    ## live         -0.064461   0.054177  -1.190  0.23470    
    ## popularity   -0.007760   0.061701  -0.126  0.89997    
    ## bpm           0.140768   0.031752   4.433 1.15e-05 ***
    ## duration_s   -0.042276   0.009244  -4.573 6.09e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.39 on 488 degrees of freedom
    ## Multiple R-squared:  0.4064, Adjusted R-squared:  0.3931 
    ## F-statistic: 30.38 on 11 and 488 DF,  p-value: < 2.2e-16

``` r
# reg2 <- lm(pop ~ bpm + energy + dance + db + live +
#             valence + dur + acous + spch, data = top10edited)
# summary(reg2)

# ordered probit for song_rank in rolling stone set  

# also there has to be some sort of KNN for the valence and test using oos
```

next step is to build the model and then fit using a “valence” song to
test the validity of the model fit
