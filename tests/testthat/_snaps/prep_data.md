# data is prepared correctly

    Code
      .prep_data(jacobson_factor, subject, time, gds)
    Message <rlang_message>
      i Your "pre" was set as pre measurement and and your "post" as post.
      * If that is not correct, please specify the pre measurement with the argument "pre".
    Output
      $original
      # A tibble: 60 x 4
         subject time    das   gds
           <dbl> <fct> <dbl> <dbl>
       1       1 pre    90.5  68  
       2       1 post   97    62.5
       3       2 pre    74    74.5
       4       2 post  124    56  
       5       3 pre    97    58.5
       6       3 post   97.5  58  
       7       4 pre    73.5  73.5
       8       4 post   88    71  
       9       5 pre    61    78.5
      10       5 post   96.5  60.5
      # i 50 more rows
      
      $wide
      # A tibble: 30 x 3
            id   pre  post
         <dbl> <dbl> <dbl>
       1     1  68    62.5
       2     2  74.5  56  
       3     3  58.5  58  
       4     4  73.5  71  
       5     5  78.5  60.5
       6     6  76    77  
       7     7  76.5  58.5
       8     8  63    52  
       9     9  70    65.5
      10    10  75    73  
      # i 20 more rows
      
      $data
      # A tibble: 26 x 4
            id   pre  post change
         <dbl> <dbl> <dbl>  <dbl>
       1     1  68    62.5   -5.5
       2     2  74.5  56    -18.5
       3     3  58.5  58     -0.5
       4     4  73.5  71     -2.5
       5     5  78.5  60.5  -18  
       6     6  76    77      1  
       7     7  76.5  58.5  -18  
       8     8  63    52    -11  
       9     9  70    65.5   -4.5
      10    10  75    73     -2  
      # i 16 more rows
      

