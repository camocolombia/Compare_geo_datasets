# Compare_geo_datasets
This dataset allows compare different georeferenced coordinates using three criteria (Distance, Uncertainty, ISO2)

Workflow followed to perform the analysis

1.)	Install R, Rstudio, MySQL Workbench, Wamp Server, sublime text or Notepad ++, ArcGIS Desktop.
2.)	Install R libraries geosphere, ggplot2, RMySQL
3.)	Configure the separator list of computer to be “|”
4.)	Extract IRRI FINAL COORDS from the access file using the field SRCNO to join the tables: acc, src, and toUSDA.
5.)	Extract GRIN 2007 from the access file using the field SRCNO to join the tables: src, acc, and geo.
6.)	Extract IRRI IMP DATA in the same way as IRRI FINAL COORDS omitting the georeferenced coordinates. The table toUSDA has the IRRI-standardized geographical information.
7.)	Convert all datasets to excel files and adjust to be used in the Java API. 
8.)	Extract GRIN 2017 geographic passport data through a database administrator and GRIN Global curator tool using accession id and source id. Filter and join the comments field in the source table to get annotations. 
9.)	Georeference using the cwroccurrences Java API.
10.)	Join all datasets in a unique excel file according to the structure given in Appendix I.
11.)	Create flags by reviewing data (IRRI_LOCALITY FLAG, LOCALITY_FLAG, IRRI_BYHAND_FLAG, IRRI_ORI_FLAG)
    a.	IRRI_LOCALITY FLAG and LOCALITY_FLAG are extracted from ADM2 and locality fields
    b.	Extract SOS_FLAG through GRIN Global database using label SOS PROJECT
    c.	Extract GG_COORDS_FLAG using the file used in the georeferencing step. (These are the original coords)
12.)	Convert the Excel file to CSV file (Please check out the output using Notepad++ or Sublime text because GRIN Global has hidden information in some fields and this can affect the R workflow. 
13.)	Run R Scripts in the same order as given in Appendix III.
14.)	Do an Excel file copy and join with the traffic lights file named COORDS_FINAL.csv using a VLOOKUP or just copy and paste. Please check out the file before performing the join. In some cases, the file is altered in the last 10 rows.
15.)	Run the 005_SUMMARY_PLOTS.R to get summary plots.
16.)	Use the final Excel file to organize data to map coordinates.



