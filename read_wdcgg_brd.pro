;+
; NAME:
;    read_wdcgg_brd
;
; PURPOSE:
;     Reads data from WDCGG files at /nas/input/WDCGG...
;
; CALLING SEQUENCE:
;     read_wdcgg_brd,file=file,contri=contri,lat=lat,lon=lon,cal=cal,gvtimes=gvtimes,$
;                     values=values,flag=flag,ndata=ndata,characteristics=characteristics,$
;                     filter=filter,statfilt=statfilt
;
; KEYWORD INPUTS:
;     file       (STRING) : path of WDCGG file
;     contri     (STRING) : acronym of contributing network
;     /filter             : set keyword to additionally filter all stations listed in
;                           statfilt for non-background values
;     statfilt            : list of stations to which more stringent filtering is applied
;     
; OUTPUTS:
;     lat           (FLOAT) : station latitude
;     lon           (FLOAT) : station longitude
;     gvtimes DblArr(ndata) : date/times of measurements in fractional days since 1.1.0001 00:00
;     ch4     FltArr(ndata) : The CH4 measurements
;     flag    StrArr(ndata) : Flag for each measurement
;     ndata       (INTEGER) : Number of data points
;     characteristics       : strarr(5) with some site characteristics
; 
; LIMITATIONS:
;
; MODIFICATION HISTORY:
; Tina Schnadt,    13 Apr 2011: first implementation
; Dominik Brunner, 27 Jun 2011: Adapted from Tina's read_wdcgg_data_single of 13/04/2011
; Dominik Brunner, 05 Jan 2017: Incorporated some extensions by Florian Arfeuille for
;                               additional filtering for non-background data,
;                               improved file doc header, small changes to parameter passing
;-
PRO read_wdcgg_brd,file=file,contri=contri,lat=lat,lon=lon,cal=cal,gvtimes=gvtimes,$
                   values=values,flag=flag,ndata=ndata,characteristics=characteristics,$
                   filter=filter,statfilt=statfilt
  
  datei = file_search(file,count=count)
  IF count EQ 0 THEN BEGIN
     print,'could not find file ',datei
     RETURN
  ENDIF

  station = STRMID(file_basename(datei),0,3)

  ; intercalibration factors for CH4 to NOAA04 scale, continuous & flask providers
  CASE contri OF
     'aemet': conv = 1.
     'agage': conv = 1.0003
     'csiro': conv = 1.
     'ec': conv = 1.
     'enea': conv = 1.
     'empa': conv = 1.
     'ipen': conv = 1.
     'jma': conv = 1.
;     'kma': conv = 1.013        ; Anmyeon-do has KRISS scale, scale up by a factor of 1.3%
     'kma': conv = 1.0          ; Anmyeon-do now has NOAA04 scale
     'gerc': conv = 1.          ; Gosan has NOAA04 scale
     'lsce': conv = 1.0124
     'mgo': conv = 1.
     'mpij': conv = 1.
     'mri': conv = 0.9973
     'nier': conv = 1.
     'nies': conv = 0.9973
     'niwa': conv = 1.
     'noaa': conv = 1.
     'rivm': conv = 0.9973
     'rse': conv = 1.
     'saws': conv = 1.
     'ubag': conv = 1.
     'cmdl': conv = 1.0124
     else: conv = 1.
  ENDCASE

  ; USA stands for USA standard scale (NIST)
  calstandard = ['NOAA04','NOAA-04','WMO','NOAA','KRISS',   'USA Standard','Tohoku University',$
                 'NOAA/CMDL','NIES','NONE','indirect', 'MRI', 'NOAA2004', '1999~2006:', 'NOAA X2004A']
  convfactor  = [      1.,       1.,   1.,1.0124,  1.013,  0.9973, 1.0003,     1.0124,0.9973,    1., $
                       1.,0.9973,    1.,   1.013, 1.]
  
  provider    = ['csiro','ec','jma','mgo','noaa','cmdl','rse','ubag','lsce']
  ; AEMET, EMPA, ENEA, IPEN, ISAC, KMA,
  ; LSCE, NIER, NIWA (baseline/no-baseline provided only), 
  ; RIVM, SAWS do not provide flags, not considered in provider
  nprov          = n_elements(provider)

  ; prepare to remove data that are flagged
  remove         = StrArr(nprov,15)
  nrem           = IntArr(nprov)
  remove[0,0:12] = ['A..','B..','C..','D..','E..','N..','*..','H..','I..','J..','.M.','.O.','NF.'] ; CSIRO
  remove[1,0:0]  = ['0']                                                     ; EC
  remove[2,0:1]  = ['2','9']                                                 ; JMA
  ; MGO available flags: ['.X','.Z','.N','SX','SZ','SN','MX','MZ','MN','+X','S.X']
  remove[3,0:7]  = ['.X','.N','SX','SN','MX','MN','+X','S.X']                ; MGO
  remove[4,0:9]  = ['*..','N..','A..','A.>','F..','+..','.X.','N.I','N.P','.XP'] ; NOAA/GMD
  remove[5,0:9]  = ['*..','N..','A..','A.>','F..','+..','.X.','N.I','N.P','.XP'] ; NOAA/CMDL (older NOAA data) 
  remove[6,0:0]  = ['2']                                                     ; RSE
  remove[7,0:2]  = ['3','6','9']                                             ; UBA
  remove[8,0:3]  = ['N..','+..','III','..S']                                 ; LSCE 
  nrem           = [13,1,2,8,10,10,1,3,4]
    
  ;; additional filtering for non-background values for subset of sites
  IF keyword_set(filter) AND n_elements(statfilt) NE 0 THEN BEGIN
     ind = WHERE(station eq statfilt,cfilt)
     IF cfilt eq 1L THEN BEGIN
        fname = file_basename(file)
        print, 'Filtering data for station ', STRMID(fname,0,3)
        ;; prepare to remove flagged data
        remove          = StrArr(nprov,17)
        nrem            = IntArr(nprov)
        remove[0,0:16]  = ['A..','B..','C..','D..','E..','N..','*..','H..','I..','J..','.M.',$
                           '.O.','NF.','.F.','.G.','.K.','.L.']                               ; CSIRO
        remove[1,0:0]   = ['0']                                                               ; EC
        remove[2,0:7]   = ['0','1','2','3','4','5','6','9']                                   ; JMA
        ;; MGO available flags: ['.X','.Z','.N','SX','SZ','SN','MX','MZ','MN','+X','S.X']
        remove[3,0:8]   = ['.X','.N','SX','SN','MX','MN','+X','S.X','.Z.']                    ; MGO
        remove[4,0:10]  = ['*..','N..','A..','A.>','F..','+..','.X.','N.I','N.P','.XP','.C.'] ; NOAA/GMD
        remove[5,0:10]  = ['*..','N..','A..','A.>','F..','+..','.X.','N.I','N.P','.XP','.C.'] ; NOAA/CMDL (older NOAA data) 
        remove[6,0:0]   = ['2']                                                               ; RSE
        remove[7,0:2]   = ['3','6','9']                                                       ; UBA
        remove[8,0:3]   = ['N..','+..','III','..S']                                           ; LSCE 
        nrem            = [17,1,8,9,11,11,1,3,4]
        print, 'filtering out non-bg values for station ', STRMID(fname,0,3)
     ENDIF
  ENDIF ELSE cfilt=0

  lat     = 0.
  lon     = 0.
  cal     = ''
  characteristics    = StrArr(5)
  characteristics[*] = 'xxx'

  sline = ''

  ;******************************************
  ;*** NOW READ FILE
  ;******************************************

  ;******************************************
  ;*** 1. READ HEADER
  ;******************************************

  nlines = FILE_LINES(datei[0])  ; number of lines in file i
  openr,lun,datei,/get_lun

  ;; first check number of comment lines
  smatch = ''
  nheader = 0L
  WHILE smatch NE 'DATE  TIME' AND NOT eof(lun) DO BEGIN
     readf,lun,sline
     smatch = STRMID(sline,6,10)
     nheader = nheader + 1L
  ENDWHILE
  IF eof(lun) THEN nheader = 32

  point_lun,lun,0               ; reset to file start

  ndata  = long(nlines)-nheader ; number of data lines in file i
  ;; output arrays
  gvtimes = DblArr(ndata)
  values  = FltArr(ndata) & flag = StrArr(ndata) & test = FltArr(ndata)

  readf,lun,sline                            ; first line C1
  param = (strsplit((strsplit(sline,':',/extract))[1],' ',/extract))[0]


  readf,lun,sline                            ; second line C2
  result = STRSPLIT(sline,/EXTRACT)          ; from second line extract filename
  characteristics[0] = STRMID(result[3],0,3) ; filename => characteristics[0], station acronym
  FOR k=2,8 DO readf,lun,sline               ; read lines C3 to C9
  result = STRSPLIT(sline,/EXTRACT)          ; from 9th line extract whether platform is stationary of mobile
  characteristics[1] = result[8]             ; extract type of platform => characteristics[1]
  
  IF result[8] ne 'stationary' AND result[8] NE 'vertical' THEN BEGIN ; read data only from stationary platforms
     print,'sorry, station is not a stationary platform: ',result[8]
     free_lun,lun
     RETURN
  ENDIF

  FOR k=9,11 DO readf,lun,sline ; read lines C10 to C12
  result         = STRSPLIT(sline,/EXTRACT)  
  lat            = result[2]    ; extract latitude information => lat
  readf,lun,sline               ; read line C13
  result         = STRSPLIT(sline,/EXTRACT)
  lon            = result[2]       ; extract longitude information => lon
  FOR k=13,18 DO readf,lun,sline   ; read lines C14 to C19
  result         = STRSPLIT(sline,/EXTRACT)
  characteristics[2] = STRMID(result[3],0,4) ; extract start year of measurements
  readf,lun,sline                            ; read line C20
  result         = STRSPLIT(sline,/EXTRACT)
  characteristics[3] = result[3]   ; extract whether measurements are 'event' or 'hourly'
  FOR k=20,22 DO readf,lun,sline   ; read lines C21 to C23
  result       = STRSPLIT(sline,/EXTRACT)
  characteristics[4] = result[3] ; extract whether measurements are 'flask' or 'continuous'

  IF result[3] NE 'flask' AND result[3] NE 'continuous' THEN BEGIN
     print,'sorry, station does not provide continuous or flask measurements'
     free_lun,lun
     RETURN
  ENDIF
  
  readf,lun,sline               ; line C24, time zone, offset from UTC
  result  = STRSPLIT(sline,':',/EXTRACT)
  IF STRMID(result[0],4,9) NE 'TIME ZONE' THEN stop
  utcoffset = 0D                ; default is UTC
  pos = strpos(result[1],'UTC')
  IF pos NE -1 THEN BEGIN
     utcoffset = double(STRMID(result[1],pos+3,10))
  ENDIF
  ;print,'UTC offset = ',utcoffset

  readf,lun,sline               ; line C25, calibration standard
  result  = STRSPLIT(sline,/EXTRACT)
  IF n_elements(result) LT 4 THEN BEGIN
     IF station EQ 'tkb' THEN cal='MRI' ELSE stop
  ENDIF ELSE IF n_elements(result) GT 5 THEN BEGIN
     cal     = result[3]+' '+result[4] ; check calibration standard
  ENDIF ELSE BEGIN
     cal     = result[3]                ; check calibration standard
  ENDELSE
  FOR k=25,nheader-1 DO readf,lun,sline ; overread rest of header lines
  
  ;; *******************************************
  ;; * 2. READ DATA
  ;; *******************************************
  FOR k=0L,ndata-1L DO BEGIN
     readf,lun,sline
     entry = STRSPLIT(sline,/EXTRACT)
     yyyy  = STRMID(entry[0],0,4)
     mm    = STRMID(entry[0],5,2)
     dd    = STRMID(entry[0],8,2)
     hh    = STRMID(entry[1],0,2)
     minu  = STRMID(entry[1],3,2)
     gvtimes[k] = dtg2gvtime(yyyy+mm+dd+hh+minu)-utcoffset/24D
     values[k]  = float(entry[4])
     flag[k]   = entry[7]
     IF values[k] lt 0. THEN values[k] = !VALUES.F_NAN
     IF keyword_set(filter) THEN IF characteristics[0] eq 'brw' and flag[k] eq '.C.' THEN $
        values[k] = !VALUES.F_NAN        

     ;;*****************************************
     ;;* intercalibrate data to NOAA04 standard
     ;;*****************************************
     IF param EQ 'CH4' THEN BEGIN
        indc      = WHERE(cal eq calstandard,cc)
        IF conv ne convfactor[indc[0]] THEN stop
        IF finite(values[k]) eq 1 THEN values[k] = values[k]*convfactor[indc[0]]
     ENDIF

     ;;********************************************* 
     ;;* remove flagged data from those
     ;;* providers who indicate flags in their data
     ;;*********************************************
     indp = WHERE(contri eq provider,cp)
     test[k] = values[k]
     IF cp eq 1 THEN BEGIN
        indrem = WHERE(flag[k] eq remove[indp,*],crem)
        IF crem gt 0L THEN values[k] = !VALUES.F_NAN
     ENDIF                
     
  ENDFOR                        ; end loop over data

  free_lun,lun

  ;; reduce data array to valid data
  index = WHERE(finite(values),cnt)
  IF cnt GT 0 THEN BEGIN
     values = values[index]
     gvtimes = gvtimes[index]
     flag = flag[index]
  ENDIF

  IF param NE 'CH4' THEN return

END
