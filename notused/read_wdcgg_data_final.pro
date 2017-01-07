;+
; NAME:
;    read_wdcgg_data
;
; PURPOSE:
;     Reads data from WDCGG files at /nas/input/WDCGG...
;     and processes these to compute the global, midlatitude, and 
;     tropical methane evolution, and growth rates     
;
; CALLING SEQUENCE:
;     read_wdcgg_data,dir=dir,type=type,latitudes,longitudes,calibration,$
;                     jahr=jahr,month=month,ch4=ch4,flag=flag,nmeas=nmeas,nf=nf,$
;                     characteristics=characteristics,station=station
;
; ARGUMENTS:
; 
; LIMITATIONS:
;
; MODIFICATION HISTORY:
; Tina on 13/04/2011

PRO read_wdcgg_data_final,dir=dir,type=type,latitudes,longitudes,calibration,$
                    jahr=jahr,month=month,day=day,ch4=ch4,flag=flag,nmeas=nmeas,nf=nf,$
                    characteristics=characteristics,contributor=contributor,station=station


  print, 'Read WDCGG data of type: ', type

  ; USA stands for USA standard scale (NIST)
  calstandard = ['NOAA04','NOAA-04','WMO','NOAA','KRISS',   'USA','AGAGE','NOAA/CMDL','NIES','NONE',$
                 'indirect']
  convfactor  = [      1.,       1.,   1.,1.0124,     1.,  0.9973, 1.0003,     1.0124,0.9973,    1., 1.]

  provider    = ['CSIRO',      'EC','JMA', 'MGO','NOAA/GMD','RSE',  'UBA','LSCE']
  ; AEMET, EMPA, ENEA, IPEN, ISAC, KMA,
  ; LSCE, NIER, NIWA (baseline/no-baseline provided only), 
  ; RIVM, SAWS
  ; do not provide flags, not considered in provider
  nprov          = n_elements(provider)

  ; prepare to remove data that are flagged
  remove         = StrArr(nprov,15)
  nrem           = IntArr(nprov)
  remove[0,0:12] = ['A..','B..','C..','D..','E..','N..','*..','H..','I..','J..','.M.','.O.','NF.'] ; CSIRO
  remove[1,0:0]  = ['0']                                                     ; EC
  remove[2,0:1]  = ['0','9']                                                 ; JMA
  remove[3,0:10] = ['.X','.Z','.N','SX','SZ','SN','MX','MZ','MN','+X','S.X'] ; MGO
  remove[4,0:1]  = ['*..','N..']                                             ; NOAA/GMD
  remove[5,0:0]  = ['2']                                                     ; RSE
  remove[6,0:1]  = ['6','9']                                                 ; UBA
  nrem           = [13,1,2,11,2,1,2]
  
  IF type eq 'event'  THEN files  = FINDFILE(dir+'/*.fl.*.ev.dat')
  IF type eq 'hourly' THEN BEGIN
    files1 = FINDFILE(dir+'/*.cn.ch4.*.hr*.dat')
    ;files2 = FINDFILE(dir+'/*.cn.ch4.200201.hr*.dat')
    ;files3 = FINDFILE(dir+'/*.cn.ch4.filtered.hr*.dat')
                                ;IF strlen(files2) gt 0 THEN files  =
                                ;[files1,files2] ELSE files = [files1]
    files = files1
  ENDIF

  nf              = n_elements(files)
  latitudes       = FltArr(nf)
  longitudes      = FltArr(nf)
  calibration     = StrArr(nf)
  contributor     = StrArr(nf)
  nmeas           = IntArr(nf)
  characteristics = StrArr(nf,5)
  characteristics[*,*] = 'blaue Ente'

  nmax          = 10000
  jahr          = IntArr(nf,nmax) & month = IntArr(nf,nmax)  & day = IntArr(nf,nmax)
  ch4           = FltArr(nf,nmax) & flag = StrArr(nf,nmax)
  station       = StrArr(nf)

  latitudes[*]  = !VALUES.F_NAN
  longitudes[*] = !VALUES.F_NAN 

  sline = ''
  savelon = 0.
  savelat = 0.
  FOR i=0,nf-1 DO BEGIN    

    nlines = FILE_LINES(files[i])  ; number of lines in file i
    ndata  = nlines-32             ; number of data lines in file i
    output = StrArr(10,ndata) 
    ;output = StrArr(nf,ndata) ;flo
    openr,lun,files[i],/get_lun
    print, 'Read: ', files[i]
    FOR k=0,1 DO readf,lun,sline    
    result = STRSPLIT(sline,/EXTRACT)
    characteristics[i,0] = result[3] ; filename
    FOR k=2,8 DO readf,lun,sline
    result = STRSPLIT(sline,/EXTRACT)
    characteristics[i,1] = result[8]
    IF result[8] eq 'stationary' THEN BEGIN
      ; continue looping to read station coordinates
      FOR k=9,10 DO readf,lun,sline
      result         = STRSPLIT(sline,/EXTRACT)
      contributor[i] = result[2]
      readf,lun,sline
      result         = STRSPLIT(sline,/EXTRACT)
      savelat        = result[2]
      readf,lun,sline
      result         = STRSPLIT(sline,/EXTRACT)
      savelon        = result[2]
      ; continue looping to determine the period
      ;whether station is event or hourly sampling
      FOR k=13,18 DO readf,lun,sline
      result         = STRSPLIT(sline,/EXTRACT)
      characteristics[i,2] = STRMID(result[3],0,4)
      ;IF type eq 'event' THEN $
      ;IF fix(STRMID(result[3],0,4)) le 2008 and fix(STRMID(result[4],0,4)) ge 2004 THEN $
      ;goto,ctn ELSE goto, dtrd
      ;next line: is station event or hourly sampling?
ctn:  readf,lun,sline
      result         = STRSPLIT(sline,/EXTRACT)
      characteristics[i,3] = result[3]
      IF result[3] eq 'event' or result[3] eq 'hourly' THEN BEGIN
        FOR k=20,22 DO readf,lun,sline
        result       = STRSPLIT(sline,/EXTRACT)
        characteristics[i,4] = result[3]
        IF result[3] eq 'flask' or result[3] eq 'continuous' THEN BEGIN
          longitudes[i] = savelon
          latitudes[i]  = savelat

          FOR k=23,24 DO readf,lun,sline
          ; check calibration standard
          result     = STRSPLIT(sline,/EXTRACT)

;          IF station[i] ne 'tkb236' THEN calibration[i] = result[3]
;          ELSE IF station[i] eq 'tkb236' THEN calibration[i] =
;          calibration[0] ; MODIFIED IN TKB FILE; check if ok
          calibration[i] = result[3]
          nmeas[i]       = ndata
          station[i]     = STRMID(characteristics[i,0],0,6)
          IF ndata eq 0 THEN stop 
 
;;FLO
;readf,lun,sline

FOR k=25,31 DO readf,lun,sline

result     = STRSPLIT(sline,/EXTRACT)   
IF result[1] eq '#' THEN BEGIN $          
;IF station[i] eq 'sgp436' THEN BEGIN $
FOR k=32,40 DO readf,lun,sline
   ndata  = nlines-41             ; number of data lines in file i    
ENDIF

IF station[i] eq 'sdz240' THEN BEGIN $
FOR k=32,44 DO readf,lun,sline
   ndata  = nlines-54          
ENDIF

IF station[i] eq 'sgp436' THEN BEGIN $
FOR k=32,56 DO readf,lun,sline
   ndata  = nlines-66          
ENDIF

IF station[i] eq 'wlg236' THEN BEGIN $
FOR k=32,45 DO readf,lun,sline
   ndata  = nlines-55          
ENDIF



;IF type eq 'hourly' THEN BEGIN
;IF station[i] eq 'brw471' THEN BEGIN $
;          readf,lun,sline
;          readf,lun,sline
;   readf,lun,sline
;readf,lun,sline
;readf,lun,sline
;readf,lun,sline
;readf,lun,sline
;readf,lun,sline
;readf,lun,sline
;   ndata  = nlines-41             ; number of data lines in file i    
;ENDIF

;IF station[i] eq 'mlo519' THEN BEGIN $
;          readf,lun,sline
;          readf,lun,sline
;   readf,lun,sline
;readf,lun,sline
;readf,lun,sline
;readf,lun,sline
;readf,lun,sline
;readf,lun,sline
;readf,lun,sline
;   ndata  = nlines-41             ; number of data lines in file i    
;ENDIF
;     ENDIF




          ; *******************************************
          ; * READ DATA
          ; *******************************************
          FOR k=0,ndata-1 DO BEGIN
            readf,lun,sline
            output[*,k] = STRSPLIT(sline,/EXTRACT)
            jahr[i,k]   = fix(STRMID(output[0,k],0,4))
            month[i,k]  = fix(STRMID(output[0,k],5,2))
            day[i,k]    = fix(STRMID(output[0,k],8,2))
            ch4[i,k]    = float(output[4,k])
            flag[i,k]   = output[7,k]
            IF ch4[i,k] lt 0. THEN ch4[i,k] = !VALUES.F_NAN
            indc        = WHERE(calibration[i] eq calstandard,cc)
            IF cc le 0L THEN stop ELSE IF finite(ch4[i,k]) eq 1 THEN $
            ch4[i,k] = ch4[i,k]*convfactor[indc[0]]

            ; remove flagged data from those
            ; providers who indicate flags in their data
            indp = WHERE(contributor[i] eq provider,cp)
            IF cp eq 1 THEN BEGIN
              indrem = WHERE(flag[i,k] eq remove[indp,*],crem)
              IF crem gt 0L THEN ch4[i,k] = !VALUES.F_NAN
            ENDIF

         ENDFOR
        ENDIF
      ENDIF
    ENDIF
dtrd: free_lun,lun
ENDFOR

END
