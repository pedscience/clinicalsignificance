# Results are correct

    Code
      cs_percentage(claus_2020, id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5)
    Message <cliMessage>
      
      -- Clinical Significance Results --
      
      Percentage-change approach with a 50% decrease in instrument scores indicating
      a clinical significant improvement.
    Output
      
    Message <cliMessage>
      Category     |  n | Percent
      ---------------------------
      Improved     | 16 |    0.40
      Unchanged    | 24 |    0.60
      Deteriorated |  0 |    0.00

---

    Code
      cs_percentage(claus_2020, id, time, hamd, pre = 1, post = 4, pct_improvement = 0.5,
        pct_deterioration = 0.3)
    Message <cliMessage>
      
      -- Clinical Significance Results --
      
      Percentage-change approach with a 50% decrease in instrument scores indicating
      a clinical significant improvement and a 30% increase in instrument scores
      indicating a clinical significant deterioration.
    Output
      
    Message <cliMessage>
      Category     |  n | Percent
      ---------------------------
      Improved     | 16 |    0.40
      Unchanged    | 24 |    0.60
      Deteriorated |  0 |    0.00

