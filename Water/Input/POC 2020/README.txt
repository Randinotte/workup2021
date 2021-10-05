2021 notes: 

-Run 1 failed because I messed up the calibration. No data saved because none was produced. Called next attempt Run 1b.
-Run 1b: Retention times changed (both N and C) during the run, which caused #46 to fall outside the integration window for C. 
	-To Fix: Open the Calibration window and open the cal file for that run (Run 1b).
	-Change the retention window from .300 to .325 (for both left and right). Click Save.
	-Go to the Chromatogram window and open the chromatograms for the full run:
		-Uncheck to turn off Overlay Mode
		-Click Open Chromatogram, and choose the first file ("_001") from that run.
		-Check back on Overlay Mode
		-Open Chromatogram from Sample Table, select that run.
		-Look to see if the value populated for the broken sample. Readjust windows if it's still not populating.
		-Export chromatogram again. I saved it as "Run 1b2" ("b.2" threw a hissy fit) because I wanted to maintain the original export just in case.
		-Open the original chromatogram output from File Explorer, check to see if any other values changed besides the broken row(s). (Just to take note)
	-2021-02-12: Copied JonesLabData 2020/POC 2020 to Google Drive/Randi/Database/compileDataYear/workup2020/Water/Input/POC 2020: 
		-changed "run1b2" to "run1" and deleted intermediaries in this working folder. Originals maintained in JonesLabData 2020/POC 2020

2021-05-01: Can't find script that created the merged POC file so I'm recreating it. The changes noted above aren't made in the folder like I said they were, so maybe
Google Drive didn't sync or I accidentally deleted something? For each of the files in JonesLabData 2020/POC 2020, I used the Windows tool "Spreadhseet Compare" 
to compare run#'s if there was a "#b" to make sure that I was aware of all the changes. In workup2020/Water/Input/POC2020 I kept only the versions that had all the data,
changed their name to just the #, and deleted the other versions: 
	-kept run1b3, deleted 1b and 1b2
	-kept run2b, deleted run2
	-kept run3b, deleted run3
	