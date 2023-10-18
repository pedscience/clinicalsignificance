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
    Message
      
      -- Clinical Significance Results --
      
      Combined approach using the JT and statistical approach.
    Output
      
    Message
      Category     |  n | Percent
      ---------------------------
      Recovered    | 26 |  65.00%
      Improved     |  3 |   7.50%
      Unchanged    | 10 |  25.00%
      Deteriorated |  1 |   2.50%
      Harmed       |  0 |   0.00%

---

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "EN")
    Message
      
      -- Clinical Significance Results --
      
      Combined approach using the EN and statistical approach.
    Output
      
    Message
      Category     |  n | Percent
      ---------------------------
      Recovered    | 26 |  65.00%
      Improved     |  6 |  15.00%
      Unchanged    |  5 |  12.50%
      Deteriorated |  3 |   7.50%
      Harmed       |  0 |   0.00%

---

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "HLL")
    Message
      
      -- Clinical Significance Results --
      
      Combined approach using the HLL and statistical approach.
    Output
      
    Message
      Category     |  n | Percent
      ---------------------------
      Recovered    | 12 |  30.00%
      Improved     |  0 |   0.00%
      Unchanged    | 16 |  40.00%
      Deteriorated | 11 |  27.50%
      Harmed       |  1 |   2.50%

---

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "GLN")
    Message
      
      -- Clinical Significance Results --
      
      Combined approach using the GLN and statistical approach.
    Output
      
    Message
      Category     |  n | Percent
      ---------------------------
      Recovered    | 26 |  65.00%
      Improved     |  3 |   7.50%
      Unchanged    | 10 |  25.00%
      Deteriorated |  1 |   2.50%
      Harmed       |  0 |   0.00%

---

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", reliability_post = 0.5,
        rci_method = "NK")
    Message
      
      -- Clinical Significance Results --
      
      Combined approach using the NK and statistical approach.
    Output
      
    Message
      Category     |  n | Percent
      ---------------------------
      Recovered    | 26 |  65.00%
      Improved     |  2 |   5.00%
      Unchanged    | 11 |  27.50%
      Deteriorated |  1 |   2.50%
      Harmed       |  0 |   0.00%

---

    Code
      cs_combined(claus_2020, id, time, hamd, pre = 1, post = 4, reliability = 0.8,
        m_functional = 8, sd_functional = 8, cutoff_type = "c", rci_method = "HA")
    Message
      
      -- Clinical Significance Results --
      
      Combined approach using the HA and statistical approach.
    Output
      
    Message
      Individual Level Summary
      Category     |  n | Percent
      ---------------------------
      Recovered    | 23 |  57.50%
      Improved     |  9 |  22.50%
      Unchanged    |  7 |  17.50%
      Deteriorated |  1 |   2.50%
      Harmed       |  0 |   0.00%
    Output
      
    Message
      Groupcs Level Summary
      Category   | Percent
      --------------------
      Changed    |    0.90
      Functional |    0.76

---

    Code
      cs_combined(claus_2020, id, time, hamd, m_functional = 8, sd_functional = 8,
        cutoff_type = "c", rci_method = "HLM")
    Message
      
      -- Clinical Significance Results --
      
      Combined approach using the HLM and statistical approach.
    Output
      
    Message
      Category     |  n | Percent
      ---------------------------
      Recovered    | 10 |  25.00%
      Improved     |  1 |   2.50%
      Unchanged    | 20 |  50.00%
      Deteriorated |  9 |  22.50%
      Harmed       |  0 |   0.00%

