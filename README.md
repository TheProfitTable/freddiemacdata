# freddiemacdata
**transformation of the freddie mac data to The Profit Table's standard data dictionary**

#### Background

As part of a larger effort to increase transparency, Freddie Mac is making available loan-level credit performance data on a portion of fully amortizing fixed-rate mortgages that the company purchased or guaranteed from 1999 to 2016.

You can get more information around the data set, as well as the user manual and actual download [here](http://www.freddiemac.com/research/datasets/sf_loanlevel_dataset.html). 

#### Scope

The scope of this repo is to convert the Freddie Mac data to the standard format used by The Profit Table for loan analysis in all of our packages. Please see [this repo](https://github.com/TheProfitTable/masterlibrary) for the data dictionary. You can also acces our style guide and on boarding information there. 

The main output of the script is a data frame (df) used for analysis using the [loanportr](https://github.com/TheProfitTable/loanportr) package. We are currently reading in the Freddie Mac sample data from 2012 onwards as this results in a data frame of an easily manageable size. 

#### Improvements and Work To Be Done

* Fix data issue whereby loan performance history stops prematurely (no more rows for that contract past a certain date). See [Issue #8](https://github.com/TheProfitTable/freddiemacdata/issues/8) for more details.   
* Create a very small sample set to accompany the [loanportr](https://github.com/TheProfitTable/loanportr) package as lazy data.  

NOTE: issue for the above still to be logged. 
