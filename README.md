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

The major challenge for formatting the data for ecotaxa input is that you need to convert the features from VisualSpreadsheet into the .tsv format familiar with Ecotaxa. Really this is a quite straightforward process but there can be some
headaches with the format. Thus, the provided code makes it easy, so long as the files are formatted as listed above.

For a single run, you can use `establish_flowprocess(root_dir)` where root_dir is set to the path of your run folder. To process multiple runs at once, you can run `batch_flowprocess(root_dir)` where root_dir is set to the _work folder or the folder where all runs are stored.

## Troubleshooting
There are several possible errors which can come from flowprocess formating. Note that in a batch process, errors are skipped and stored as warnings so make sure to check those warnings.
Typically the most common error is that a data or summary file are not correctly formatted. Go and make sure that the files are correctly named and in the right folders. Once fixed, try `establish_flowprocess()` on that one folder.

Another possibility is that not all images were copied over. Make sure that there is the same number of objects in the folder as were recorded in visual spreadsheet.

# Duplicate Delete
A common issue in using the FlowCam is that objects can become lodged in the field of view, resulting in the same particle being imaged mutliple times. The best solution to avoid this is consistently cleaning and checking the flowcell during imaging, however, sometimes it is unavoidable. Here is an imperfect solution but it can be helpful when duplicates are prevalent.

Duplicates are removed in a two-step process with a conservative mindset. Thus, it won't remove all duplicates, but it will confidently remove a few. The motiviation here is that it can be unsupervised in removal for large projects with many runs, however users should still be wary when viewing vignettes in Ecotaxa (sort by x or y coordinates). The first step is a proximity filter - it will identify areas in the imaging field where the density is much higher than even, indicating that duplicate imaging occurred in this region. Then, DBSCAN is used to identify clusters of objects with similar features. Finally, vignettes from those clusters (minus the first instance) are all removed. 

For a single run, use `duplicate_deleter(root_dir,)` with the root_dir set to the run folder. To run multiple at once, use `batch_dup_deleter(root_dir,..)` set to the _work folder or project folder. 
When batch processing, there may be several warnings related to not enough neighbors or cluster member, this just means that no ROIs were closely related for the duplicate deleter in the second stage to confidently remove them. Additional agruments can be passed through to adjust the sensitivity of each stage (see documentation). 

Finally, by default the argument write_plots is set to TRUE. This will create plots of which ROIs are removed in a _results folder. You can turn this off if you don't want it.

# Exporting to Ecotaxa
Once files are all set in a run-folder with a tsv file, you can upload these to Ecotaxa. The best way to do this is to zip multiple runs in a compressed folder (typically you can zip the whole work directory). Once there, you should be able to upload to the server via Filezilla or upload directly if the file is less than 500mb. See ecotaxa documentation for details.

# Merging Runs for MorphoCluster
Once files are all finalized and formatted, it can be sometimes attractive to merge all the separate runs into a single ecotaxa tsv. This is useful for moving data into MorphoCluster. 
**Feature is not yet available**
