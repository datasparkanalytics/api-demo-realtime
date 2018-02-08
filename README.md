# Real Time Footfall Demo Dashboard

*Author: Aloysius Lim, DataSpark*

This dashboard demonstrates DataSpark's Real Time Footfall API. Built on [Shiny](https://shiny.rstudio.com/), it can be run on any local installation of R, or deployed on [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/)

## Set Up

### Install Dependencies

This project uses [Packrat](https://rstudio.github.io/packrat/) to manage package dependencies. To install the dependencies, do one of the following:

#### Using R Command Line

In the project directory, run R in interactive mode, and the initialisation script should run automatically:

```
$ R
R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin15.6.0 (64-bit)

...

Packrat is not installed in the local library -- attempting to bootstrap an installation...
> No source tarball of packrat available locally
> Using user-library packrat (0.4.8.1) to bootstrap this project
Installing BH (1.65.0-1) ... 
	OK (downloaded binary)
Installing R6 (2.2.2) ... 
	OK (downloaded binary)
...
```

#### Using RStudio

Open the project in RStudio, and RStudio should automatically trigger the packrat initialisation script to install all required packages.

### Enter API Credentials

Copy the `api-credentials.json` file:

```bash
cp api-credentials.template.json api-credentials.json
```

Edit the file to insert your API key and secret:

```json
{
  "key": "YOUR API CONSUMER KEY",
  "secret": "YOUR API CONSUMER SECRET"
}
```

## Run

### Using R Command Line

Run this command in R:

```r
shiny::runApp()
```

### Using RStudio

Open `server.R` and click the "Run App" button in the code editor.
