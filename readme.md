Fire occurrence dataset downloaded from: https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.4/

Fire severity dataset downloaded from https://osf.io/ke4qj/ and stored in data/data_raw/wildfire-severity_sierra_nevada-ca-usa_ypmc_1984-2017_rasters/

Each geoTIFF represents one fire and the file is named with 4 separate pieces of metadata in the file name. Each piece of metadata is separated by an underscore. The pieces of metadata (in order) are: 1) the fire alarm date in yyyymmdd format, 2) an identifier representing which loop iteration the raster was generated on (should be unique, but might not be in the future; probably not useful to end users), 3) the unique Google Earth Engine-generated ID, and 4) the coordinate reference system (as of 2019-03-16, all rasters are presented in EPSG3310)

Fire severity metadata downloaded from https://osf.io/5na3r/ and stored as data/data_raw/wildfire-severity_sierra_nevada-ca-usa_ypmc_1984-2017_fire-metadata.csv

Original perimeters for fire severity dataset:
http://frap.fire.ca.gov/data/frapgisdata-sw-fireperimeters_download

Metadata details for fire severity dataset:
http://frap.fire.ca.gov/projects/fire_data/fire_perimeters_data_description

Fire Return Interval Depature dataset (for designated yellow pine/mixed-conifer presettlement fire regimes):
https://www.fs.usda.gov/detail/r5/landmanagement/gis/?cid=STELPRDB5327836

The Google Earth Engine code is in its own git repository, but is included in this repository as a *submodule*. You should still be able to clone all the code in this submodule (you'll have to log in with a Google account, but it need not be an account that has access to Earth Engine)-- check out [this link](https://gist.github.com/gitaarik/8735255) for some more info on how you might go about this.