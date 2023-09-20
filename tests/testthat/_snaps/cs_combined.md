# All arguments are set correctly

    Argument `id` is missing with no default. A column containing patient-specific IDs must be supplied.

---

    Argument `time` is missing with no default. A column identifying the individual measurements must be supplied.

---

    Argument `outcome` is missing with no default. A column containing the outcome must be supplied.

---

    `reliability` must be numeric but a `character` was supplied.

---

    `reliability` must be between 0 and 1 but 1.1 was supplied.

---

    `reliability` must be between 0 and 1 but -0.8 was supplied.

# Results are correct

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "JT")
    Message <cliMessage>
      
      -- Clinical Significance Results --
      
      Combined approach using the JT and statistical approach.
    Output
      
    Message <cliMessage>
      Category     |  n | Percent
      ---------------------------
      Recovered    | 26 |    0.65
      Improved     |  3 |    0.07
      Unchanged    | 10 |    0.25
      Deteriorated |  1 |    0.02
      Harmed       |  0 |    0.00

---

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "EN")
    Message <cliMessage>
      
      -- Clinical Significance Results --
      
      Combined approach using the EN and statistical approach.
    Output
      
    Message <cliMessage>
      Category     |  n | Percent
      ---------------------------
      Recovered    | 26 |    0.65
      Improved     |  6 |    0.15
      Unchanged    |  5 |    0.12
      Deteriorated |  3 |    0.07
      Harmed       |  0 |    0.00

---

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "HLL")
    Message <cliMessage>
      
      -- Clinical Significance Results --
      
      Combined approach using the HLL and statistical approach.
    Output
      
    Message <cliMessage>
      Category     |  n | Percent
      ---------------------------
      Recovered    | 12 |    0.30
      Improved     |  0 |    0.00
      Unchanged    | 16 |    0.40
      Deteriorated | 11 |    0.28
      Harmed       |  1 |    0.02

---

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "GLN")
    Message <cliMessage>
      
      -- Clinical Significance Results --
      
      Combined approach using the GLN and statistical approach.
    Output
      
    Message <cliMessage>
      Category     |  n | Percent
      ---------------------------
      Recovered    | 26 |    0.65
      Improved     |  3 |    0.07
      Unchanged    | 10 |    0.25
      Deteriorated |  1 |    0.02
      Harmed       |  0 |    0.00

---

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", reliability_post = 0.5,
        rci_method = "NK")
    Message <cliMessage>
      
      -- Clinical Significance Results --
      
      Combined approach using the NK and statistical approach.
    Output
      
    Message <cliMessage>
      Category     |  n | Percent
      ---------------------------
      Recovered    | 26 |    0.65
      Improved     |  2 |    0.05
      Unchanged    | 11 |    0.28
      Deteriorated |  1 |    0.02
      Harmed       |  0 |    0.00

---

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "HA")
    Message <cliMessage>
      
      -- Clinical Significance Results --
      
      Combined approach using the HA and statistical approach.
    Output
      
    Message <cliMessage>
      Individual Level Summary
      Category     |  n | Percent
      ---------------------------
      Recovered    | 23 |    0.57
      Improved     |  9 |    0.22
      Unchanged    |  7 |    0.17
      Deteriorated |  1 |    0.02
      Harmed       |  0 |    0.00
    Output
      
    Message <cliMessage>
      Groupcs Level Summary
      Category   | Percent
      --------------------
      Changed    |    0.90
      Functional |    0.76

---

    Code
      cs_combined(claus_2020, id, time, hamd, m_functional = 8, sd_functional = 8,
        cutoff_type = "c", rci_method = "HLM")
    Message <cliMessage>
      
      -- Clinical Significance Results --
      
      Combined approach using the HLM and statistical approach.
    Output
      
    Message <cliMessage>
      Category     |  n | Percent
      ---------------------------
      Recovered    | 10 |    0.25
      Improved     |  1 |    0.02
      Unchanged    | 20 |    0.50
      Deteriorated |  9 |    0.22
      Harmed       |  0 |    0.00

