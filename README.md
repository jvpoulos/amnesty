# amnesty
Code and data for the paper "Amnesty Policy and Elite Persistence in the Postbellum South: Evidence from a Regression Discontinuity Design"


Prerequsites
------

* **R** >= 3.5.0 (tested on 4.0.2)

* package-list.R # required **R** packages

* requires X11 and png graphics capabilities. Open **R** and type `capabilities()` to check. 

Instructions
------

1. Make directory for plots: `mkdir data/plots`

2. `chmod +x code/main.sh` and run from the command line. `code/main.R` prepares data and outputs RD plots, RD estimates, and descriptive plots. By default, it uses trained models for record linkage. 

3. The file `code/power-indiv.R` performs individual-level power analysis. Defaults to run. 