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

# Step 3(2) - Create near table

arcpy.GenerateNearTable_analysis(
near_features = os.path.join(box_dir + "/shale-varying/Data/GIS/" + 'ShalePlays_US_EIA_Sep2019.shp'),
in_features = os.path.join(box_dir + "/shale-varying/Data/GIS/" + "tl_2020_us_county.shp"),
out_table = os.path.join(box_dir + '/shale-varying/Scratch/' + 'Spatial_Data.gdb' + '/USCB_County_to_USEIA_Shale_Play'),
search_radius = "50 MILES",
location = "NO_LOCATION",
angle = "NO_ANGLE",
closest = "ALL",
closest_count = "",
method = "PLANAR")

# My attempt at copying over the tables in a .msb from my local drive to the Box Sync folder.

arcpy.TableToTable_conversion(
in_rows = os.path.join(box_dir + '/shale-varying/Scratch/' + 'Spatial_Data.gdb' + '/USCB_County_to_USEIA_Shale_Play'),
out_path= box_dir + '/shale-varying/Scratch/',
out_name= 'USCB_County_to_USEIA_Shale_Play_50Miles' + '.csv')



