# Andrew Boslett
# Rochester Data Science Consortium
# Email: andrew_boslett@urmc.rochester.edu

# Set options

import arcpy
import os
import csv
import sys

# Set up environments

arcpy.env.overwriteOutput = True

box_dir = 'C:/Users/aboslett/Box'
pers_dir = 'C:/Users/aboslett/Documents'

if not arcpy.Exists(os.path.join(box_dir + '/shale-varying/Scratch/' + 'Spatial_Data.gdb')):
    arcpy.CreateFileGDB_management(out_folder_path = os.path.join(box_dir + '/shale-varying/Scratch/'),
    out_name= 'Spatial_Data',
    out_version="CURRENT")

    print "New geodatabase created"

else:
    print "Geodatabase already exists"

# Project databases

for fff in ['ShalePlays_US_EIA_Sep2019', 'tl_2020_us_county']:
    arcpy.Project_management(
    in_dataset = os.path.join(box_dir + '/shale-varying/Data/GIS/' + fff + '.shp'),
    out_dataset = os.path.join(box_dir + '/shale-varying/Data/GIS/' + fff + '_prj.shp'),
    out_coor_system="PROJCS['USA_Contiguous_Albers_Equal_Area_Conic',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Albers'],PARAMETER['False_Easting',0.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-96.0],PARAMETER['Standard_Parallel_1',29.5],PARAMETER['Standard_Parallel_2',45.5],PARAMETER['Latitude_Of_Origin',37.5],UNIT['Meter',1.0]]",
    preserve_shape="NO_PRESERVE_SHAPE",
    max_deviation="",
    vertical="NO_VERTICAL")

# Create near table

arcpy.GenerateNearTable_analysis(
near_features = os.path.join(box_dir + "/shale-varying/Data/GIS/" + 'ShalePlays_US_EIA_Sep2019_prj.shp'),
in_features = os.path.join(box_dir + "/shale-varying/Data/GIS/" + "tl_2020_us_county_prj.shp"),
out_table = os.path.join(box_dir + '/shale-varying/Scratch/' + 'Spatial_Data.gdb' + '/USCB_County_to_USEIA_Shale_Play'),
search_radius = "50 MILES",
location = "NO_LOCATION",
angle = "NO_ANGLE",
closest = "ALL",
closest_count = "",
method = "PLANAR")

# Export data table to a CSV file in the Box scratch folder

arcpy.TableToTable_conversion(
in_rows = os.path.join(box_dir + '/shale-varying/Scratch/' + 'Spatial_Data.gdb' + '/USCB_County_to_USEIA_Shale_Play'),
out_path= box_dir + '/shale-varying/Scratch/',
out_name= 'USCB_County_to_USEIA_Shale_Play_50Miles' + '.csv')



