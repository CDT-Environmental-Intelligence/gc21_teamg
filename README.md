# Grand Challenge 2021: Team G

## Project structure
```
data/
|-- FAOSTAT_yield.csv                         Grape yield data
|-- [index]_annual_mean_[method].csv          Mean annual index values
|-- [index]_annual_variance_[method].csv      Annual variance of index values 
|-- [index].csv                               Monthly index values

notebooks/
|-- annualise_[index].ipynb                   Python pipeline to convert index data from monthly to annual
|-- linear_regression_plots.ipynb             Python pipeline for linear regression

plots/
|-- [country]_[index]_lagplot.png             Distributed lag plots of each country and index

r-scripts/
|-- linear_annual_models.R                    R script for linear regression coefficient calculations
|-- distributed_lag.R                         R script for distributed lag model plots

environment.yaml                              Conda environment manifest
renv.lock                                     renv environment manifest
GC_TeamG_TipplingPoints*                      Poster in PDF/PNG format
```

`[index]`   - Climate oscillation index acronym <br>
`[method]`  - Method of annualising (how a year is defined) <br>
`[country]` - Two letter abbreviation for wine producing country <br>

## Software environment (Python + R)

### Python (Conda)
[(Mini)conda](https://docs.conda.io/en/latest/miniconda.html) was used to manage the software environment for Python.

Follow these steps to recreate the environment to run the Notebooks in this project:

1. Clone this repository:
```bash
$ git clone https://github.com/CDT-Environmental-Intelligence/gc21_teamg.git
```

2. `cd` into the project folder:
```bash
$ cd gc21_teamg
```

3. Create the conda environment from the `environment.yaml` file:
```bash
$ conda env create --file environment.yaml
```

4. Activate Conda environment:
```bash
$ conda activate gc21_teamg
```

5. Start Jupyter Lab:
```bash
$ jupyter lab .
```

### R (renv)
[`renv`](https://rstudio.github.io/renv/index.html) was used to manage the software environment for R.

Follow these steps to recreate the environment to run the `r-scripts` in this project:

1. Launch R in a terminal (or an R console in RStudio and open the folder as a project):
```bash
$ cd gc21_teamg
$ R
```

2. Install `renv` (if not already available):
```R
install.packages("renv")
```

3. Restore the `renv` from the lockfile:
```R
renv::restore()
```

# Poster
<image src="poster/GC_TeamG_TipplingPoints_poster.png"/>
