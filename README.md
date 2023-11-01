# FlowProcess

An R package for processing flowcam output to be best formatted for Ecotaxa and Morphocluster annotation

# Getting Set-up

First to get set-up correctly, you'll need to export data from FlowCam's Visual Spreadsheet in a way which FlowProcess expects. To do this, you'll want to export images, particle properties, and run metadata. Currently the process is a little sticky when exporting the data. However, it is constructed in a way which best emulates Zooprocess so it can be easily be used in Morphocluster and Ecotaxa smoothly.

What is necessary to use this R package is to put only ***one*** run from visual spreadsheet into a folder. That folder must contain all exported images, the exported data (labeled project_name_data.csv), and the exported data summary (labeled project_name_summary.csv), **where your project_name matches the run name.** This is tedious for export from VisualSpreadsheet but is the best way to get the metadata from the summary file and easiest to use to duplicate removal algorithm. \*\* A best practice would be to just export the data and summary to a folder while you process your runs in real-time\*\*

It is not necessary to use the \_work and \_result directory framework but it should be familiar to anyone who uses zooprocess. Also if you want to keep duplicate removal plots, this framework keeps it organized.

See example directory:

root/

\|--- FlowCam_Project/

\| \|------\_work/

\| \| └--------- Project_Name_Run1/

\| \| \| └--------- Project_Name_Run1_data.csv

\| \| \| └--------- Project_Name_Run1_summary.csv

\| \| \| └--------- Project_Name_Run1_00000.png

\| \| \| └--------- Project_Name_Run1_00001.png

\| \| \| └--------- Project_Name_Run1_00003.png

\| \| \| └--------- ....

\| └--------- Project_Name_Run2/

\| \|-------\_results/

# Establishing your tsv file

The major challenge fo
