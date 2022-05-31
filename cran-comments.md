## Test environments
- R-hub macos-highsierra-release-cran (r-release)
- R-hub windows-x86_64-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Benedikt Claus <b.claus@pedscience.de>'
  
  New submission

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
    
* As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

0 errors ✔ | 0 warnings ✔ | 2 notes ✖
