;+
; NAME:
;
;  READ_MODEL_DATA_MONTH
;
; PURPOSE:
;
;  Read FLEXPART receptor output in year month yyyymm
;  at the stations stats and times dtg
;
; CATEGORY:
;
;  URMEL, CH4 inversion
;
; CALLING SEQUENCE:
;
;  read_model_data_month,sim=sim,yyyymm=yyyymm,stats=stats,$
;            dtg=dtg,mch4=mch4,mall=mall,oldest=oldest
;
; INPUTS:
;
;  sim           (structure)    : the model simulation information structure
;                                (see inv_run.pro for details)
;  yyyymm        (string)       : the year and month for which to get the data
;  stats         (strarr(nobs)) : the nobs receptor point (station) names
;  dtg           (strarr(nobs)) : the corresponding date/times
;
; KEYWORD PARAMETERS:
;
;  /oldest                      : set this key to return only the values for
;                                 the oldest age class instead of all classes
;
; OUTPUTS:
;
;  mch4    (fltarr(nobs,ntrace,nage)) : the model simulated CH4 values at the
;                                       nobs receptor points for the ntrace tracers
;                                       and nage age classes
;  mall    (fltarr(nobs))             : the model simulated total CH4 (sum over all
;                                       ntrace tracers and nage age classes)
;
; COMMON BLOCKS:
;
;  none
;
; SIDE EFFECTS:
;
;  none
;
; RESTRICTIONS:
;
;  none
;
; PROCEDURE:
;
;  none
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
; 
;   (c) Tina Schnadt Proberai
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   March 2012: first implementation
;
;-
PRO read_model_data_month_final,sim=sim,yyyymm=yyyymm,stats=stats,$
                          dtg=dtg,mch4=mch4,mall=mall,oldest=oldest,weekly=weekly

  print, 'Run read_model_data_month for ', yyyymm

  IF n_elements(sim) EQ 0 THEN BEGIN
     sim = {name:'URMEL_SENSC_II',$
          obsdir:'/nas/spc134/URMEL/INVERSION/OBSINPUT/',$
          modeldir:'/nas/spc134/URMEL/FLEXPART80CTP/output/',$
            outdir:'/nas/spc134/URMEL_INVERSION/SENSITIVITIES/',$
          syyyymm:'200001',eyyyymm:'200812',scalef_Q:1,$
          ntrace:11,nage:4}
  ENDIF

  n       = n_elements(stats)   ; number of lines in observational data set (times/stations)

  IF keyword_set(oldest) THEN BEGIN
     offs = (sim.nage-1)*sim.ntrace+1 ; skip first nage-1 age classes + 1 for air trace
     ntot = sim.ntrace                ; only one age class
     mch4      = DblArr(ntot,n)+!values.d_nan
  ENDIF ELSE BEGIN
     offs = 1                         ; skip age tracer
     ntot = sim.ntrace * sim.nage     ; all nage age classes
     mch4      = DblArr(ntot,n)+!values.d_nan
  ENDELSE

  mall      = DblArr(n) + !values.d_nan

  ;******************************************
  ;* determine optimum model output level for 
  ;* comparison with observations
  ;******************************************

;  filein  = '/nas/spc134/URMEL/INVERSION/choice_modellevel_station-2001to2008.dat'

filein  = '/home/arf/pers/IDL/urmel/INVERSION/choice_modellevel_station.dat' ;flo
filein  = '/home/arf/pers/IDL/urmel/INVERSION/choice_modellevel_station_'+sn+'.dat' ; FLO

;filein  = '/home/arf/pers/IDL/urmel/INVERSION/choice_modellevel_station_30.dat' ;flo

  nlines  = FILE_LINES(filein)
  station = STrArr(nlines)
  mlev    = IntArr(nlines)
  line    = ''
  openr,lun1,filein,/get_lun
  FOR i=0,nlines-1 DO BEGIN
    readf,lun1,line
    result = STRSPLIT(line,/EXTRACT)
    station[i] = result[0]
    mlev[i]    = fix(result[1])
  ENDFOR
  free_lun,lun1

  ;determine optimum model output level
  ;for comparison with observations
  mlevel = IntArr(n)
  FOR i=0,n-1 DO BEGIN
    index  = WHERE(stats[i] eq station,cstat)
    IF cstat eq 0 THEN stop
    mlevel[i] = mlev[index]
  ENDFOR

  ; call subroutine to read model data
;  read_receptors_urmel,sim=sim,yyyymm=yyyymm,info=info,data=data
   read_receptors_maiolica_final,sim=sim,yyyymm=yyyymm,info=info,data=data ;FLO 08/05



  ntime = info[0].time  ; total number of receptor output points in yyyymm

  mtime = StrArr(ntime) ; corresponding times in dtg time format
  ;; problem with a few days missing in Feb 2006 due to ECMWF model change
  IF yyyymm eq '200602' THEN BEGIN
    FOR it=0,6 DO mtime[it] = yyyymm+'0'+STRCOMPRESS(string(it+3),/REM)+'0000'
    FOR it=7,ntime-1 DO mtime[it]=yyyymm+STRCOMPRESS(string(it+3),/REM)+'0000' 
  ENDIF ELSE BEGIN
    FOR it=0,8 DO mtime[it] = yyyymm+'0'+STRCOMPRESS(string(it+1),/REM)+'0000'
    FOR it=9,ntime-1 DO mtime[it]=yyyymm+STRCOMPRESS(string(it+1),/REM)+'0000'
  ENDELSE

  recname = strlowcase(STRMID(info.rcptname,0,3))

  FOR k=0,n-1 DO BEGIN                    ; loop over all times/stations

    indn  = WHERE(stats[k] eq recname,cn) ; this search identifies potential
                                          ; numerous altitude output levels of
                                          ; the station in question

    indz  = WHERE(dtg[k] eq mtime,cz)     ; where to place model data in month
    indzm = WHERE(gvtime2dtg(dtg2gvtime(dtg[k])-1) eq mtime,czm)   ; day before
    indzp = WHERE(gvtime2dtg(dtg2gvtime(dtg[k])+1) eq mtime,czp)   ; day after

    IF cz eq 0 THEN stop

    ;*********************************************************************************
    IF cn ge 1L and cz eq 1L THEN BEGIN   ; 1 or more than one station identified + 1 output level
      FOR m=0,ntot-1 DO $
      mch4[m,k] = data[indz].pptv[offs+m,indn[mlevel[k]]]/$
                  data[indz].pptv[0,indn[mlevel[k]]]  
      mall[k] = total(data[indz].pptv[1:sim.nage*sim.ntrace,indn[mlevel[k]]])/$
                      data[indz].pptv[0,indn[mlevel[k]]]

      ; interpolate model data in time if values are Nan (occurs very rarely)
      IF finite(mch4[0,k]) eq 0 or finite(mall[k]) eq 0 THEN BEGIN
        IF czm eq 0 or czp eq 0 THEN stop
        FOR m=0,ntot-1 DO $
        mch4[m,k] = (data[indzm].pptv[offs+m,indn[mlevel[k]]]/$
                     data[indzm].pptv[0,indn[mlevel[k]]]+$
                     data[indzp].pptv[offs+m,indn[mlevel[k]]]/$
                     data[indzp].pptv[0,indn[mlevel[k]]])/2.
        mall[k] = (total(data[indzm].pptv[1:sim.nage*sim.ntrace,indn[mlevel[k]]])/$
                   data[indzm].pptv[0,indn[mlevel[k]]]+$
                   total(data[indzp].pptv[1:sim.nage*sim.ntrace,indn[mlevel[k]]])/$
                   data[indzp].pptv[0,indn[mlevel[k]]])/2.
        IF finite(mch4[0,k]) eq 0 THEN stop
        IF finite(mall[k]) eq 0 THEN stop
      ENDIF

    ENDIF

    ;*********************************************************************************************
    IF cn eq 0L THEN BEGIN                ; no station identified by receptor name, use lats/lons

      ;; two stations with different identifiers in obs data and model output
      IF stats[k] eq 'bgu' THEN BEGIN
        ssearch = 'beg'
        slon    = 3.23
        slat    = 41.97
      ENDIF
      IF stats[k] eq 'lpo' THEN BEGIN
        ssearch = 'ile'
        slon    = -3.58
        slat    = 48.8
      ENDIF

      indn = WHERE(ssearch eq recname,cn) ; the station is found in the observations
      IF cn ge 1L and cz eq 1L THEN BEGIN
        indlon = WHERE(float(string(info.xrcpt,format='(f7.2)')) eq slon,clon)
        indlat = WHERE(float(string(info.yrcpt,format='(f7.2)')) eq slat,clat)
        IF clon gt 0L and clat gt 0L THEN BEGIN
          intersect = setintersection(indlon,indlat) ; intersect of modelled lats and lons
          mname     = info[intersect].rcptname       ; search model station name
          FOR m=0,ntot-1 DO $
            mch4[m,k] = data[indz].pptv[offs+m,intersect]/data[indz].pptv[0,intersect]
            mall[k]   = total(data[indz].pptv[1:sim.nage*sim.ntrace,intersect])/$
                        data[indz].pptv[0,intersect]

          ; interpolate model data in time if
          ; model values are Nan (occurs very rarely)
          IF finite(mch4[0,k]) eq 0 or finite(mall[k]) eq 0 THEN BEGIN
            FOR m=0,ntot-1 DO $
            mch4[m,k] = (data[indzm].pptv[offs+m,intersect]/$
                         data[indzm].pptv[0,intersect]+$
                         data[indzp].pptv[offs+m,intersect]/$
                         data[indzp].pptv[0,intersect])/2.
            mall[k]   = (total(data[indzm].pptv[1:sim.nage*sim.ntrace,intersect])/$
                         data[indzm].pptv[0,intersect]+$
                         total(data[indzp].pptv[1:sim.nage*sim.ntrace,intersect])/$
                         data[indzp].pptv[0,intersect])/2.
            IF finite(mch4[0,k]) eq 0 THEN stop
            IF finite(mall[k]) eq 0 THEN stop
            IF k eq 188 THEN stop
          ENDIF

        ENDIF ELSE BEGIN
          print, 'Station ', stats[k], ' not in model output list! => STOP '
          stop
        ENDELSE
            
      ENDIF
    ENDIF

  ENDFOR     ; end loop over stations

END
