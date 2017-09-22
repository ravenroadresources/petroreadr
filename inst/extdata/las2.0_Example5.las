~Version Information
VERS. 2.00 :CWLS Log ASCII Standard - Version 2.00
WRAP. NO :One line per depth step
~Well Information
#MNEM.UNIT DATA : DESCRIPTION
#----.---- --------- : -----------------
STRT.na 1 :Start
STOP.na 16 :Stop
STEP.na 1 :Step increment
NULL. -999.250 :Null value
COMP. A CANADA LTD. :Company
WELL. A 10-16-39-3 :Well
FLD. Wildcat :Field
PROV. ALBERTA :Province / County
CTRY. CANADA :State / Country
LOC. 100/10-16-039-03W5/00 :Location
SRVC. Weatherford :Service company
UWI. 100101603903W500 :Unique Well Id
LIC. 0323470 :Licence Number
DATE. 25-Jul-2008 :Logging date
#
~Other
-This example illustrates how date and time information can be expressed as a number
and can be included in the data section of LAS 2.0 files.
-An an index number (INDEX) is used in this example as the primary (index) curve. The
curve "TIME" could have been used as the primary curve if moved to the first curve
position.
#
~Curve Information
INDEX.na :Index (line number)
TIME.SEC :Elapsed time
DATE_1. :Date YYYMMDD (see ISO 8601)
DATE_2. :Ordinal Date YYYYDDD (see ISO 8601)
TIME_1. :Time hhmmss.s (see ISO 8601)
TIME_2. :Time hhmmss (see ISO 8601)
TIME_3. :Time hhmm (see ISO 8601)
TPRE.S :Pretest Time
QDPF.KPAA :Quartzdyne Pressure
BIT.MM :Bit size
#
#1 INDEX TIME DATE_1 DATE_2 TIME_1 TIME_2 TIME_3 TPRE QDPF BIT
~A
1 0 20080725 2008177 153000.0 153000 1530 -999.250 -999.250 251.000
2 0.6 20080725 2008177 153000.6 153001 1530 -999.250 -999.250 251.000
3 1.2 20080725 2008177 153001.2 153001 1530 -999.250 -999.250 251.000
4 1.8 20080725 2008177 153001.8 153002 1530 -999.250 -999.250 251.000
5 2.4 20080725 2008177 153002.4 153002 1530 -999.250 9224.281 251.000
6 3.0 20080725 2008177 153003.0 153003 1530 -999.250 -999.250 251.000
7 3.6 20080725 2008177 153003.6 153004 1530 -999.250 -999.250 251.000
8 4.2 20080725 2008177 153004.2 153004 1530 -999.250 -999.250 251.000
9 4.8 20080725 2008177 153004.8 153005 1530 -999.250 -999.250 251.000
10 5.4 20080725 2008177 153005.4 153005 1530 1.195 9221.396 251.000
11 6.0 20080725 2008177 153006.0 153006 1530 1.794 -999.250 251.000
12 6.6 20080725 2008177 153006.6 153007 1530 2.395 -999.250 251.000
13 7.2 20080725 2008177 153007.2 153007 1530 2.999 -999.250 251.000
14 7.8 20080725 2008177 153007.8 153008 1530 3.5 -999.250 251.000
15 8.4 20080725 2008177 153008.4 153008 1530 4.156 9075.793 251.000
16 9.0 20080725 2008177 153009.0 153009 1530 4.802 -999.250 251.000
