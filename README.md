# trustworthy_titanic
titanic kernels with outputs and output processing

### Description of files:

 - Contains ready-to-run Titanic kernels from Kaggle, all converted to `.R` from `.Rmd`/`.irnb`/`.ipynb` formats, each folder contains input files.
 - `titanicRunner.R` - R script that extracts functions and packages of the kernels (in `.json` format - already extracted) and runs them to extract outputs (if any) and runtimes.
 - `getResults.ipynb` - Python notebook to analyze the ouputs of `titaniocRunner.R`
 - `417NamesAccuracies.csv` - csv files with directory names (directory names are unique, script names are not!) and accuracies from all the outputs
 - `timeDirs.csv` - table with directory names and run times
 - `cheater.csv` - fine-tuned output gives ~99% accuracy (need to fix it to 100%)
 - `dirsTimesAccuracies.csv` - concatenation of 2 previous files
