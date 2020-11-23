# GA_script

Hi everyone! I created a Google Analytics data extractor that goes forward than the traditional plugins you can find on Google Spreadsheets. I use R and Excel in order to be able to run the entire thing with all its properties.

I got a combination of 3 R files plus 2 Excel/csv files. Basically:
  1) GA.R
  2) URL.Breaker.R
  3) URL.Deduplicator.R
  4) Credential.csv
  5) Extraction.csv
  
The file to run is the GA.R. The other 2 R files are complementary to it and the 2 csv files are for the setup.
 
Things it can do:
 1) On the Extraction.csv file, third column we have "time". Here, you can either specify "month" (which means the data will be breaked into the respective months within the time range specified), "no" (which means it won't break the data within the time range specified), and a cardinal number (which will break the data into groups of days within the specified time range. For example, if you put "1", it will be daily data, and if you put "7", it will be weekly data, starting on the weekday of the first day of the time range specified)
 2) On line 19 of the GA.R file you can put 'ub <- "yes"' or  'ub <- "no"', which will either call the URL.breaker.R file code in or not. If you include it than it will do organize the data in a different way which is quite handy for data analysis. Note that parameteres will be removed. This code is nice in case you want to have a better grasp of how users are moving inside your website.
 3) On line 21 of the GA.R file you can put 'ub <- "yes"' or  'ub <- "no"', which will either call the URL.Deduplicator.R file code in or not. By pulling this script you are grouping the data.
 
Basically, 2) and 3) are great if you want to go to extra detail by working with Pivot Tables in excel.
  

Good luck and let me know how it works. Improvements are welcomed!
