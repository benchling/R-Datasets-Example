# Sample Code - Working with Datasets Using R
A repository containing the sample scripts described in the [Working with Datasets Using R](link) developer guide.

Provided for educational purposes only; not for production use.

## Configuration and Setup
1. Create a [Benchling App](https://docs.benchling.com/docs/getting-started-benchling-apps)
2. Create a [Benchling Analysis](https://help.benchling.com/hc/en-us/articles/15298157390861-Creating-an-analysis)
3. Configure the sample script(s) with your tenant-specific variables

## Usage
* `get_dataframe.R` pulls the input dataset from a Benchling analysis and converts it into an R dataframe.
* `results_analysis.R` performs an IC50 calculation on a dataframe, creates a plot, and imports both
as outputs to a Benchling analysis.
* `Mortality IC50.csv` contains basic sample data of the form required by `results_analysis.R`

It's recommended to start by working with `get_dataframe.R` and `results_analysis.R` in order to familiarize
yourself with the principles involved. For best results, these should be run sequentially, since the latter expects
a dataframe to be imported. Once you're confident using the scripts and with the configuration in Benchling, the
techniques involved can be applied to more robust applications using tools like Rshiny.
