## Test environments
* local x86_64-w64-mingw32 install, R 4.0.3
* windows-latest (on github actions), R 4.0.3
* macOS-latest (on github actions), R 4.0.3
* ubuntu-20.04 (devel and release) (on github actions), R 4.0.3


## R CMD check results
There were no ERRORs or WARNINGs. 


## This is a resubmission. In this version I have:

* Checked for misspelling at description file to solve this NOTE:

 Possibly mis-spelled words in DESCRIPTION:
    RESTful (3:14, 8:15)

Now the title is: ''a R Wrapper for the Brazilian House of Representatives Open Data 'RESTful' API''
And the description is: The goal of 'camaradeputadosapi' is to ease access and provide a 
    clean interface in R for querying the Brazilian House of Representatives 
    open data 'RESTful' API.

* Checked for the "Server error: (504) Gateway Timeout" at "deputados_despesas(id = 74173, 2013)"

It looks like this is a server-side error at the Brazilian House of Representatives servers and do not occur often.
I ran multiple tests locally, on github actions and with `check_rhub()` and everything is working fine.
