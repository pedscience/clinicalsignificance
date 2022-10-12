# clinical_significance throws expected errors

    You must specify an ID column.

---

    You must specify a column indicating the different measurements.

---

    You must specify an outcome.

---

    argument "reliability" is missing, with no default

---

    Your measurement column contains more than two measurements. 
    Please specify which two measurements should be used with arguments "pre" and "post".

---

    i The NK method requires reliability estimates for pre and post measurements.
    * You can specify the post reliability with the "reliabilit_post" argument. For now, reliability post was set to reliability pre.

---

    To calculate cutoff "c", summary statistics for the functional population must be defined.

---

    i You selected cutoff type "a" and provided summary statistics for the functional population. This information will be dicarded.
    * If you wand to incorporate data from the functional population for the cutoff, choose type = "b" or "c"

---

    i You specified a reliability estimate for the post measurement but did not choose the NK method.
    * Your post measurement reliability estimate will be ignored.

# clinical_significance output is correct

    Code
      clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.8,
        pre = "pre", method = "JT")
    Output
      Clinical Significance Results (JT)
      
      Category     |  n | Percent
      ---------------------------
      Recovered    | 10 |   0.385
      Improved     |  4 |   0.154
      Unchanged    | 12 |   0.462
      Deteriorated |  0 |   0.000
      Harmed       |  0 |   0.000

---

    Code
      clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.8,
        pre = "pre", method = "GLN")
    Output
      Clinical Significance Results (GLN)
      
      Category     |  n | Percent
      ---------------------------
      Recovered    | 12 |   0.462
      Improved     |  4 |   0.154
      Unchanged    | 10 |   0.385
      Deteriorated |  0 |   0.000
      Harmed       |  0 |   0.000

---

    Code
      clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.8,
        pre = "pre", method = "HLL")
    Output
      Clinical Significance Results (HLL)
      
      Category     |  n | Percent
      ---------------------------
      Recovered    |  7 |   0.269
      Improved     |  0 |   0.000
      Unchanged    | 11 |   0.423
      Deteriorated |  8 |   0.308
      Harmed       |  0 |   0.000

---

    Code
      clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.8,
        pre = "pre", method = "EN")
    Output
      Clinical Significance Results (EN)
      
      Category     |  n | Percent
      ---------------------------
      Recovered    | 12 |   0.462
      Improved     |  5 |   0.192
      Unchanged    |  9 |   0.346
      Deteriorated |  0 |   0.000
      Harmed       |  0 |   0.000

---

    Code
      clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.8,
        reliability_post = 0.8, pre = "pre", method = "NK")
    Output
      Clinical Significance Results (NK)
      
      Category     |  n | Percent
      ---------------------------
      Recovered    | 12 |   0.462
      Improved     |  4 |   0.154
      Unchanged    | 10 |   0.385
      Deteriorated |  0 |   0.000
      Harmed       |  0 |   0.000

---

    Code
      clinical_significance(jacobson_1989, subject, time, gds, reliability = 0.8,
        pre = "pre", method = "HA")
    Output
      Clinical Significance Results (HA Individual Level)
      
      Category     |  n | Percent
      ---------------------------
      Recovered    |  7 |   0.269
      Improved     | 10 |   0.385
      Unchanged    |  9 |   0.346
      Deteriorated |  0 |   0.000
      Harmed       |  0 |   0.000
      
      Clinical Significance Results (HA Group Level)
      
      Category   | Percent
      --------------------
      Changed    |   0.900
      Functional |   0.448

---

    Code
      clinical_significance(anxiety, subject, measurement, anxiety, method = "HLM")
    Output
      Clinical Significance Results (HLM)
      
      Category     |  n | Percent
      ---------------------------
      Recovered    | 21 |   0.193
      Improved     |  0 |   0.000
      Unchanged    | 64 |   0.587
      Deteriorated | 23 |   0.211
      Harmed       |  1 |   0.009

---

    Code
      clinical_significance(claus_2020, id, time, bdi, group = treatment,
        reliability = 0.8, pre = 1, post = 4)
    Output
      Clinical Significance Results (JT)
      
      Group | Category     |  n | Percent
      -----------------------------------
      TAU   | Recovered    |  3 |   0.158
      TAU   | Improved     |  2 |   0.105
      TAU   | Unchanged    | 14 |   0.737
      TAU   | Deteriorated |  0 |   0.000
      TAU   | Harmed       |  0 |   0.000
      PA    | Recovered    |  7 |   0.333
      PA    | Improved     |  6 |   0.286
      PA    | Unchanged    |  8 |   0.381
      PA    | Deteriorated |  0 |   0.000
      PA    | Harmed       |  0 |   0.000

# summary is correct

    Code
      summary(clinical_significance(claus_2020, id, time, bdi, reliability = 0.8,
        pre = 1, post = 4, method = "JT"))
    Output
      
      Clinical Significance Results
      
      There were 43 participants in the whole dataset of which 40 (93%)
      could be included in the analysis.
      
      The JT method for calculating cutoffs and reliable change was chosen
      and the outcome variable was "bdi".
      
      The cutoff type was "a" with a value of 19.16 based on the following
      population characteristics (with lower values representing a
      beneficial outcome):
      
      Population Characteristics
      
      M Clinical | SD Clinical | M Functional | SD Functional
      -------------------------------------------------------
      35.48      | 8.16        | ---          | ---          
      
      
      The instrument's reliability was set to 0.8 
      
      Individual Level Results
      
      Category     |  n | Percent
      ---------------------------
      Recovered    | 10 |   0.250
      Improved     |  8 |   0.200
      Unchanged    | 22 |   0.550
      Deteriorated |  0 |   0.000
      Harmed       |  0 |   0.000

---

    Code
      summary(clinical_significance(claus_2020, id, time, bdi, reliability = 0.8,
        pre = 1, post = 4, m_functional = 8, sd_functional = 8, type = "c", method = "JT"))
    Output
      
      Clinical Significance Results
      
      There were 43 participants in the whole dataset of which 40 (93%)
      could be included in the analysis.
      
      The JT method for calculating cutoffs and reliable change was chosen
      and the outcome variable was "bdi".
      
      The cutoff type was "c" with a value of 21.6 based on the following
      population characteristics (with lower values representing a
      beneficial outcome):
      
      Population Characteristics
      
      M Clinical | SD Clinical | M Functional | SD Functional
      -------------------------------------------------------
      35.48      | 8.16        | 8            | 8            
      
      
      The instrument's reliability was set to 0.8 
      
      Individual Level Results
      
      Category     |  n | Percent
      ---------------------------
      Recovered    | 10 |   0.250
      Improved     |  8 |   0.200
      Unchanged    | 22 |   0.550
      Deteriorated |  0 |   0.000
      Harmed       |  0 |   0.000

---

    Code
      summary(clinical_significance(claus_2020, id, time, bdi, pre = 1, post = 4,
        reliability = 0.8, group = treatment))
    Output
      
      Clinical Significance Results
      
      There were 43 participants in the whole dataset of which 40 (93%)
      could be included in the analysis.
      
      The JT method for calculating cutoffs and reliable change was chosen
      and the outcome variable was "bdi".
      
      The cutoff type was "a" with a value of 19.16 based on the following
      population characteristics (with lower values representing a
      beneficial outcome):
      
      Population Characteristics
      
      M Clinical | SD Clinical | M Functional | SD Functional
      -------------------------------------------------------
      35.48      | 8.16        | ---          | ---          
      
      
      The instrument's reliability was set to 0.8 
      
      Individual Level Results
      
      Group | Category     |  n | Percent
      -----------------------------------
      TAU   | Recovered    |  3 |   0.158
      TAU   | Improved     |  2 |   0.105
      TAU   | Unchanged    | 14 |   0.737
      TAU   | Deteriorated |  0 |   0.000
      TAU   | Harmed       |  0 |   0.000
      PA    | Recovered    |  7 |   0.333
      PA    | Improved     |  6 |   0.286
      PA    | Unchanged    |  8 |   0.381
      PA    | Deteriorated |  0 |   0.000
      PA    | Harmed       |  0 |   0.000

---

    Code
      summary(clinical_significance(claus_2020, id, time, bdi, reliability = 0.8,
        pre = 1, post = 4, m_functional = 8, sd_functional = 8, type = "c", method = "HA"))
    Output
      
      Clinical Significance Results
      
      There were 43 participants in the whole dataset of which 40 (93%)
      could be included in the analysis.
      
      The HA method for calculating cutoffs and reliable change was chosen
      and the outcome variable was "bdi".
      
      The cutoff type was "c_true" with a value of 21.57 based on the
      following population characteristics (with lower values representing
      a beneficial outcome):
      
      Population Characteristics
      
      M Clinical | SD Clinical | M Functional | SD Functional | Reliability Clinical | Reliability Functional
      -------------------------------------------------------------------------------------------------------
      35.48      | 8.16        | 8            | 8             | 0.80                 | 0.79                  
      
      
      The instrument's reliability was set to 0.8 
      
      Individual Level Results
      
      Category     |  n | Percent
      ---------------------------
      Recovered    |  8 |   0.200
      Improved     | 17 |   0.425
      Unchanged    | 15 |   0.375
      Deteriorated |  0 |   0.000
      Harmed       |  0 |   0.000
      
      Group Level Results
      
      Category   | Percent
      --------------------
      Changed    |   0.841
      Functional |   0.374

---

    Code
      summary(clinical_significance(anxiety, subject, measurement, anxiety, method = "HLM"))
    Output
      
      Clinical Significance Results
      
      There were 116 participants in the whole dataset of which 109 (94%)
      could be included in the analysis.
      
      The HLM method for calculating cutoffs and reliable change was chosen
      and the outcome variable was "anxiety".
      
      The cutoff type was "a" with a value of 22.21 based on the following
      population characteristics (with lower values representing a
      beneficial outcome):
      
      Population Characteristics
      
      M Clinical | SD Clinical | M Functional | SD Functional
      -------------------------------------------------------
      36.61      | 7.20        | ---          | ---          
      
      
      The instrument's reliability was not specified.
      
      Individual Level Results
      
      Category     |  n | Percent
      ---------------------------
      Recovered    | 21 |   0.193
      Improved     |  0 |   0.000
      Unchanged    | 64 |   0.587
      Deteriorated | 23 |   0.211
      Harmed       |  1 |   0.009

