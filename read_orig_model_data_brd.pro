;+
; NAME:
;
;  READ_ORIG_MODEL_DATA_BRD
;
; PURPOSE:
;
;  Read FLEXPART receptor output for a given year at the stations
;  and times provided by the array ch4obs.
;
; CATEGORY:
;
;  URMEL, CH4 inversion
;
; CALLING SEQUENCE:
;
;   read_orig_model_data_brd,sim,yyyy,ch4obs=ch4obs,mch4=ch4stat,trace=ch4trace,$
;                        nmod=numstat,mdtg=dtgstat,oldest=oldest
;
; INPUTS:
;
;  sim           (structure)    : the model simulation information structure
;                                 (see inv_configurations.pro for details)
;  yyyy         (string)       : the year for which to get the data
;
; KEYWORD PARAMETERS:
;
;  ch4obs        structarr(nobs): array of structures containing observation data
;  /oldest                      : set this key to return only the values for
;                                 the oldest age class instead of all classes
;
; OUTPUTS:
;
;  ch4recs : array of structures of form
;             {statname:'',
;              dtg:'',$
;              ch4:0D,$
;              ch4trace:DblArr(sim.ntrace*sim.nage)}
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
;   Dominik Brunner, 09 Jan 2016: first implementation
;       Routines extracted from read_weekly_model_data_all_brd.pro 
;       and simplified.
;   DB 11 Feb 2017: modified to return array of structures with
;       same dimension as observation array ch4obs
;   DB 03 Dec 2017: added option to process DLR model output
;-


;***************************************************************************************
;*  subroutine to read in daily FLEXPART receptor output for a complete year
;***************************************************************************************
PRO read_receptor_output_brd,sim,yyyy,ntime=ntime,time=timecollect,$
                             mch4=mch4collect,mtrace=mtracollect

  ppbfact = 1.e9
  nmonths = 12
  nst     = n_elements(sim.stats)
  ntot    = sim.ntrace*sim.nage
  
  ;; collectors for total CH4 and CH4 per tracer
  nday=366L
  mch4collect = DblArr(nst,nday)+!values.d_nan
  mtracollect = DblArr(ntot,nst,nday)+!values.d_nan
  timecollect = StrArr(nday)
  
  FOR im=0,nmonths-1 DO BEGIN
  
    yyyymm = yyyy+STRING(im+1,format='(i2.2)')
    IF keyword_set(sim.dlr) THEN BEGIN
       read_receptors_dlr_brd,sim,yyyymm,info=info,data=data,dtg=dtg
    ENDIF ELSE BEGIN
       read_receptors_maiolica_brd,sim,yyyymm,info=info,data=data,dtg=dtg
    ENDELSE
    IF n_elements(info) NE nst THEN BEGIN
       print,'not all stations found in receptor output'
       stop
    ENDIF

    ;; convert dtg into day index (=0 for first day of the year)
    tid = floor(dtg2gvtime(dtg)-dtg2gvtime(yyyy+'01010000'))

    timecollect[tid] = dtg
    mch4collect[*,tid] = total(data.ppb[1:ntot,*],1)
    mtracollect[*,*,tid]= data.ppb[1:ntot,*]

 ENDFOR                         ; end loop over months in year yyyy 
    
END


;********************************************************************
;                   main routine
;********************************************************************
PRO read_orig_model_data_brd,sim,yyyy,ch4obs=ch4obs,ch4mod=ch4mod,oldest=oldest
  
  IF n_elements(sim) EQ 0 THEN BEGIN
     print,'parameter sim missing in call'
     RETURN
  ENDIF

  ; read receptor output at the nstat stations for the current year
  read_receptor_output_brd,sim,yyyy,ntime=ntime,time=time,mch4=mch4,mtrace=mtrace

  ;; create output structure similar to ch4obs
  nobs = n_elements(ch4obs)
  ntot = sim.ntrace*sim.nage

  IF keyword_set(oldest) THEN BEGIN
     rec = {dtg:'',name:'',ch4:0D}
  ENDIF ELSE BEGIN
     rec = {dtg:'',name:'',ch4:0D,ch4trace:DblArr(sim.ntrace*sim.nage)}
  ENDELSE
  ch4mod = replicate(rec,nobs)

  ;; check if we have a leap year
  nyyyy = STRING(fix(yyyy)+1,format='(i4)')
  yrdays = round(dtg2gvtime(nyyyy+'0101')-dtg2gvtime(yyyy+'0101'))
  leap = yrdays EQ 366

  ;; convert model output dates into continuous GREDVIS format
  gvt = dtg2gvtime(time)
  
  ;; loop over the observations and average model data to corresponding weeks
  FOR i=0,nobs-1 DO BEGIN
     istat = (WHERE(sim.stats EQ ch4obs[i].name,cnt))[0]
     IF cnt EQ 0 THEN stop      ; station not found, should not happen
     ch4mod[i].dtg = ch4obs[i].dtg
     ch4mod[i].name = ch4obs[i].name

     get_week,ch4obs[i].dtg,leap=leap,wmin=wmin,wmax=wmax,wcenter=wcenter,iscenter=iscenter
     IF NOT iscenter THEN stop
     ;; average model data over the days of the week
     index = WHERE(gvt GE dtg2gvtime(wmin) AND gvt LE dtg2gvtime(wmax),cweek)
     IF cweek gt 0L THEN BEGIN
        ch4mod[i].ch4 = mean(mch4[istat,index],/nan)
        IF NOT keyword_set(oldest) THEN BEGIN
           FOR itra=0,ntot-1 DO ch4mod[i].ch4trace[itra] = mean(mtrace[itra,istat,index],/nan)
        ENDIF
     ENDIF
  ENDFOR
  
END
