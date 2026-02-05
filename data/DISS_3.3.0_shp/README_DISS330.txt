This README file contains information about the downloadable files of the DISS3.3.0, this file ends with an <EOF> tag.
Last updated on 08/03/2022

--- Citation ---
DISS Working Group. (2021). Database of Individual Seismogenic Sources (DISS), version 3.3.0: A compilation of potential sources for earthquakes larger than M 5.5 in Italy and surrounding areas. Istituto Nazionale di Geofisica e Vulcanologia (INGV). https://doi.org/10.13127/diss3.3.0

Except where otherwise noted, the content of this file and the files here described are licensed under a Creative Commons Attribution-NonCommercial 4.0 International (CC BY 4.0) license.

DISS 3.3.0 is made available for desktop PCs in the following file formats:
MapInfo (TAB), MapInfo (mif/mid), ESRI Shapefile (SHP), Google Earth (KMZ).
For each format, there are several suites of files with the following extensions:
1) MapInfo (TAB): .tab, .map, .id, .dat, .ind, and one .wor file.
2) MapInfo (mif/mid): .mif, .mid.
3) ESRI Shapefile (SHP): .shp, .shx, .prj, .dbf, .lyr, and one .mxd file.
4) Google Earth (KMZ): .kmz.
5) Generic Mapping Tools (gmt): ASCII .txt file.

All geographic information is provided in latitude/longitude coordinates, datum WGS84.

For formats 1), 2), 3), and 5) there are the following files:
ISS330 - Individual Seismogenic Sources;
CSSTOP330 - Top traces of Composite Seismogenic Sources;
CSSPLD330 - Dipping planes of Composite Seismogenic Sources;
DSS330 - Debated Seismogenic Sources; 
SUBDCNT330 - Subduction depth contours;
SUBDZON330 - Subduction zone;
For formats 1) and 3) there is also the following file:
DISS330 - Container file with layering information.

For format 4) the kmz file is a container itself with embedded layering information.

For additional information on the database structure not indicated here please refer to
Basili, R., Kastelic, V., Valensise, G., and DISS Working Group 2009 (2009), DISS3 tutorial series: Guidelines for compiling records of the Database of Individual Seismogenic Sources, version 3, Rapporti Tecnici INGV, no. 108, 20 p.,https://diss.ingv.it/images/downloads/rapporti_108.pdf

or otherwise, send an email message to sorgenti.diss@ingv.it.

Parametric attributes for each category of seismogenic sources are as follows:
--- ISS330 ---
IDSource	The DISS identifier assigned to the record (text)
SourceName	Name of the seismogenic source (text)
Length		Fault length measured along strike (km)
Width		Fault width measured along dip (km)
MinDepth	Depth of the fault upper edge from sea level (km)
MaxDepth	Depth of the fault lower edge from sea level (km)
Strike		Fault strike right-hand rule (deg, 0-360 CW from North)
Dip		Fault dip (deg, 0-90 from horizontal) 
Rake		Fault rake (deg, 0-360 CCW from horizontal)
AvgDispl	Average displacement (m)
SRMin		Minimum slip rate value (mm/y)
SRMax		Maximum slip rate value (mm/y)
RecIntMin	Minimum recurrence interval (y)
RecIntMax	Maximum recurrence interval (y)
LatestEq	Date or age of the most recent earthquake associated with the source, if known (text)
MaxMag		Maximum moment magnitude (Mw)
LatUL		Latitude of upper-left corner
LonUL		Longitude of upper-left corner
LatUR		Latitude of upper-right corner
LonUR		Longitude of upper-right corner
LatLR		Latitude of lower-right corner
LonLR		Longitude of lower-right corner
LatLL		Latitude of lower-left corner
LonLL		Longitude of lower-left corner
Created		Date of creation of the record (text)
Updated		Date of the latest update of the record (text)
LengthQ		Qualifier of the length   
WidthQ		Qualifier of the width    
MinDepthQ	Qualifier of the minimum depth 
MaxDepthQ	Qualifier of the maximum depth 
StrikeQ		Qualifier of the strike angle   
DipQ		Qualifier of the dip angle      
RakeQ		Qualifier of the rake angle     
AvgDisplQ	Qualifier of the Average displacement 
SlipRateQ	Qualifier of the slip rate values
RecIntQ		Qualifier of the recurrence interval values  
MaxMagQ		Qualifier of the maximum magnitude  
LocationQ	Qualifier of the fault location
LinkToInfo	WWW link to full information of the record

--- CSSTOP330 - CSSPLN330 ---
IDSource	The DISS identifier assigned to the record (text)
SourceName	Name of the seismogenic source (text)
MinDepth	Depth of the fault upper edge from sea level (km)
MaxDepth	Depth of the fault lower edge from sea level (km)
StrikeMin	Minimum fault strike right-hand rule (deg, 0-360 CW from North)
StrikeMax	Maximum fault strike right-hand rule (deg, 0-360 CW from North)
DipMin		Minimum fault dip (deg, 0-90 from horizontal)
DipMax		Maximum fault dip (deg, 0-90 from horizontal)
RakeMin		Minimum fault rake (deg, 0-360 CCW from horizontal)
RakeMax		Maximum fault rake (deg, 0-360 CCW from horizontal)
SRMin		Minimum slip rate value (mm/y)
SRMax		Maximum slip rate value (mm/y)
MaxMag		Maximum moment magnitude (Mw)
Created		Date of creation of the record (text)
Updated		Date of the latest update of the record (text)
MinDepthQ	Qualifier of the minimum depth 
MaxDepthQ	Qualifier of the maximum depth 
StrikeQ		Qualifier of the strike angle   
DipQ		Qualifier of the dip angle      
RakeQ		Qualifier of the rake angle     
SlipRateQ	Qualifier of the slip rate values
MaxMagQ		Qualifier of the maximum magnitude  
LinkToInfo	WWW link to full information of the record

--- DSS330 ---
IDSource	The DISS identifier assigned to the record (text)
SourceName	Name of the seismogenic source (text)
Created		Date of creation of the record (text)
Updated		Date of the latest update of the record (text)
LinkToInfo	WWW link to full information of the record

--- SUBDCNT330 ---
IDSource	The DISS identifier assigned to the record (text)
Depth		Depth of the contour line (km)
LinkToInfo	WWW link to full information of the record

--- SUBDZON330 ---
IDSource	The DISS identifier assigned to the record (text)
SourceName	Name of the seismogenic source (text)
MinDepth	Depth of the seismogenic interface upper limit from sea level (km)
MaxDepth	Depth of the seismogenic interface lower limit from sea level (km)
DipDir		Average dip direction of the slab (N, NNE, NE, ENE, E, ESE, SE, SSE, S, SSW, SW, WSW, W, WNW, NW, NNW)
ConvAzMin	Minimum direction of plate convergence (deg, 0-180 CW from North)
ConvAzMax	Maximum direction of plate convergence (deg, 0-180 CW from North)
ConvRMin	Minimum convergence rate value (mm/y)
ConvRMax	Maximum convergence rate value (mm/y)
MaxMag		Maximum moment magnitude (Mw)
Created		Date of creation of the record (text)
Updated		Date of the latest update of the record (text)
MinDepthQ	Qualifier of the minimum seismogenic interface depth 
MaxDepthQ	Qualifier of the maximum seismogenic interface depth 
DipDirQ		Qualifier of the dip direction  
ConvAzQ		Qualifier of the direction values of plate convergence  
ConvRQ		Qualifier of the convergence rate values  
MaxMagQ		Qualifier of the maximum magnitude  
LinkToInfo	WWW link to full information of the record

--- Common values for "qualifier" fields in all tables ---
1 = LD for Literature Data;
2 = OD for Original Data;
3 = ER for Empirical Relationship;
4 = AR for Analytical Relationship;
5 = EJ for Expert Judgment.

<EOF>