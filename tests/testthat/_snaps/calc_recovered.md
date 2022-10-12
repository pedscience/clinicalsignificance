# Recovered categories are calculated correctly

    Code
      .calc_recovered(ids, cutoff_data, rci_data)
    Output
      # A tibble: 5 x 10
           id rci       cs_in~1 clini~2 funct~3 recov~4 impro~5 uncha~6 deter~7 harmed
        <int> <chr>     <chr>   <lgl>   <lgl>   <lgl>   <lgl>   <lgl>   <lgl>   <lgl> 
      1     1 Not need~ Not ne~ TRUE    TRUE    TRUE    FALSE   FALSE   FALSE   FALSE 
      2     2 Not need~ Not ne~ FALSE   FALSE   FALSE   TRUE    FALSE   FALSE   FALSE 
      3     3 Not need~ Not ne~ FALSE   FALSE   FALSE   FALSE   TRUE    FALSE   FALSE 
      4     4 Not need~ Not ne~ FALSE   TRUE    FALSE   FALSE   FALSE   TRUE    FALSE 
      5     5 Not need~ Not ne~ FALSE   FALSE   FALSE   FALSE   FALSE   FALSE   TRUE  
      # ... with abbreviated variable names 1: cs_indiv, 2: clinical_pre,
      #   3: functional_post, 4: recovered, 5: improved, 6: unchanged,
      #   7: deteriorated

---

    Code
      .calc_recovered_ha(ids, cutoff_data, rci_data)
    Output
      # A tibble: 5 x 10
           id cs_indiv   rci    clini~1 funct~2 recov~3 impro~4 uncha~5 deter~6 harmed
        <int> <chr>      <chr>  <lgl>   <lgl>   <lgl>   <lgl>   <lgl>   <lgl>   <lgl> 
      1     1 Not needed Not n~ TRUE    TRUE    TRUE    FALSE   FALSE   FALSE   FALSE 
      2     2 Not needed Not n~ FALSE   FALSE   FALSE   TRUE    FALSE   FALSE   FALSE 
      3     3 Not needed Not n~ FALSE   FALSE   FALSE   FALSE   TRUE    FALSE   FALSE 
      4     4 Not needed Not n~ FALSE   TRUE    FALSE   FALSE   FALSE   TRUE    FALSE 
      5     5 Not needed Not n~ FALSE   FALSE   FALSE   FALSE   FALSE   TRUE    FALSE 
      # ... with abbreviated variable names 1: clinical_pre, 2: functional_post,
      #   3: recovered, 4: improved, 5: unchanged, 6: deteriorated

