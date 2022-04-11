# Recovered categories are calculated correctly

    Code
      .calc_recovered(ids, cutoff_data, rci_data)
    Output
      # A tibble: 5 x 10
           id rci   cs_indiv clinical_pre functional_post recovered improved unchanged
        <int> <chr> <chr>    <lgl>        <lgl>           <lgl>     <lgl>    <lgl>    
      1     1 Not ~ Not nee~ TRUE         TRUE            TRUE      FALSE    FALSE    
      2     2 Not ~ Not nee~ FALSE        FALSE           FALSE     TRUE     FALSE    
      3     3 Not ~ Not nee~ FALSE        FALSE           FALSE     FALSE    TRUE     
      4     4 Not ~ Not nee~ FALSE        TRUE            FALSE     FALSE    FALSE    
      5     5 Not ~ Not nee~ FALSE        FALSE           FALSE     FALSE    FALSE    
      # ... with 2 more variables: deteriorated <lgl>, harmed <lgl>

---

    Code
      .calc_recovered_ha(ids, cutoff_data, rci_data)
    Output
      # A tibble: 5 x 10
           id cs_indiv rci   clinical_pre functional_post recovered improved unchanged
        <int> <chr>    <chr> <lgl>        <lgl>           <lgl>     <lgl>    <lgl>    
      1     1 Not nee~ Not ~ TRUE         TRUE            TRUE      FALSE    FALSE    
      2     2 Not nee~ Not ~ FALSE        FALSE           FALSE     TRUE     FALSE    
      3     3 Not nee~ Not ~ FALSE        FALSE           FALSE     FALSE    TRUE     
      4     4 Not nee~ Not ~ FALSE        TRUE            FALSE     FALSE    FALSE    
      5     5 Not nee~ Not ~ FALSE        FALSE           FALSE     FALSE    FALSE    
      # ... with 2 more variables: deteriorated <lgl>, harmed <lgl>

