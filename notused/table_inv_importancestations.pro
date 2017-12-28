;+
; NAME:
;
;   table_inv_importancestations
;
; PURPOSE:
;
;   Compute 10 most important stations per category:
;   sort temporal mean relative contributions of category to all
;   scanning all station data
;   
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;  table_inv_importancestations
;                            
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; COMMON BLOCKS:
;        none
;
; SIDE EFFECTS:
;        none
;
; RESTRICTIONS:
;
; PROCEDURE:
;
;   plot_inv_modelestimates
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 18 January 2012
;-

;*****************************************************************************
;SUBROUTINES
;*****************************************************************************
PRO read_data,sim,ch4obs=ch4stat,dtgobs=dtgstat,lonobs=lonstat,$
              latobs=latstat,nameobs=namestat,numobs=numstat,type=typestat,nobs=nobs

  nmonths = 12

  nst = n_elements(sim.stats)
  e = 0 
  a0 = ''
  a1 = '' 
  r0 = 0.
  
  dtg  = StrArr(10000)+''
  ch4  = DblArr(10000)+!VALUES.D_NAN
  name = StrArr(10000)+''
  lat  = FltArr(10000)+!VALUES.F_NAN
  lon  = FltArr(10000)+!VALUES.F_NAN
  num  = IntArr(10000)+0
  type = StrArr(10000)+''

  dtgstat  = StrArr(10000,nst)+''
  ch4stat  = DblArr(10000,nst)+!VALUES.D_NAN
  namestat = StrArr(10000,nst)+''
  latstat  = FltArr(10000,nst)+!VALUES.F_NAN
  lonstat  = FltArr(10000,nst)+!VALUES.F_NAN
  numstat  = IntArr(10000,nst)+0
  typestat = StrArr(10000,nst)+''
  nobs     = IntArr(nst)

  FOR im=0,nmonths-1 DO BEGIN
    ;*******************************
    ;* read data
    ;*******************************     
    IF im lt 9 THEN mm = '0'+STRCOMPRESS(string(im+1),/REM) ELSE mm = STRCOMPRESS(string(im+1),/REM)
    IF keyword_set(sim.flask) THEN BEGIN
      infile   = dir+'z_allweekly_flask_'+sim.sn+'_'+syear+mm+'.dat' 
    ENDIF ELSE BEGIN
      infile   = dir+'z_allweekly_'+sim.sn+'_'+syear+mm+'.dat'
    ENDELSE    
    nlines   = FILE_LINES(infile)

    openr,lun,infile,/get_lun
    FOR i=0,nlines-1 DO BEGIN
      readf,lun,a0
      result = STRSPLIT(a0,/EXTRACT)
      dtg[e]  = result[0]
      name[e] = result[1]
      lat[e]  = float(result[2])
      lon[e]  = float(result[3])
      ch4[e]  = float(result[4])
      num[e]  = fix(result[5])
      type[e] = result[6]
      e       += 1
    ENDFOR
    free_lun,lun

  ENDFOR ; end loop over months

  FOR i=0,nst-1 DO BEGIN
    ind = WHERE(stats[i] eq name,cst)
    IF cst gt 0L THEN BEGIN
    
      y     = float(ch4[ind])
      miin  = mean(y)
      ober  = miin + 3.*stddev(y)
      indo  = WHERE(y gt ober,c,complement=include)
      IF c gt 0L THEN BEGIN
        nobs[i] = cst-c
        dtgstat[0:cst-c-1,i]  = dtg[ind[include]]
        latstat[0:cst-c-1,i]  = lat[ind[include]]
        lonstat[0:cst-c-1,i]  = lon[ind[include]]
        ch4stat[0:cst-c-1,i]  = y[include]
        numstat[0:cst-c-1,i]  = num[ind[include]]
        typestat[0:cst-c-1,i] = type[ind[include]]       
      ENDIF ELSE BEGIN   
        dtgstat[0L:cst-1L,i]  = dtg[ind]
        namestat[0L:cst-1L,i] = name[ind]
        latstat[0L:cst-1L,i]  = lat[ind]
        lonstat[0L:cst-1L,i]  = lon[ind]
        ch4stat[0L:cst-1L,i]  = ch4[ind]
        numstat[0L:cst-1L,i]  = num[ind]
        typestat[0L:cst-1L,i] = type[ind]   
        nobs[i]               = cst
      ENDELSE
      
    ENDIF
           
  ENDFOR

END ; of routine read_data per year

;******************************************************************************
;MAIN PROGRAM
;******************************************************************************
PRO table_inv_importancestations,sim,rela=rela

  ;*************
  ; DIRECTORIES
  ;*************
  obsdir   = sim.obsdir
  sensdir  = sim.hdir
  modeldir = sim.modeldir+sim.name+'/'

  ;**********************************
  ; GENERAL PARAMETERS AND VARIABLES
  ;**********************************
  mdays    = [31,28,31,30,31,30,31,31,30,31,30,31]
  nmonths  = 12
  nst      = n_elements(stats)
  eyyyy    = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  syyyy    = fix(strmid(sim.syyyymm,0,4))  
  nyears   = eyyyy-syyyy+1
   
  zmstat_rel  = DblArr(nst,9*366,sim.ntrace)+!VALUES.D_NAN ; modelled apriori data at selected stations
  zmstat_abs  = zmstat_rel

  am    = IntArr(nst)
  em    = IntArr(nst)
  latstat = FltArr(nst) & lonstat = FltArr(nst)  
  FOR ij=0,nyears-1 DO BEGIN

    syear = STRCOMPRESS(string(syyyy+ij),/REM)

    FOR im=0,nmonths-1 DO BEGIN
       
      IF im le 8 THEN mm = '0'+STRCOMPRESS(string(im+1),/REM) ELSE mm = STRCOMPRESS(string(im+1),/REM)
      yyyymm = syear+mm
      
      read_data_single,sim=sim,yyyymm=yyyymm,ch4obs=ch4obs,dtgobs=dtgobs,$
                   lonobs=lonobs,latobs=latobs,nameobs=nameobs,nobs=nobs,weekly=weekly,$
                   stats=stats,flask=flask,nobg=nobg,special=special

      ;************************************************************************
      ; In February 2006, model data are only available from 3 February on, 
      ; because of the change from lower vertical to higher vertical resolution 
      ; ECMWF data. Hence, restrict observational data to the same period.
      ;************************************************************************
      IF yyyymm eq '200602' THEN BEGIN
        startdate = dtg2gvtime('200602030000')
        ind       = WHERE(dtg2gvtime(dtgobs) ge startdate,c)
        IF c gt 0L THEN nc        = c
      ENDIF ELSE BEGIN
         nc       = nobs
      ENDELSE      
      
      ; read weekly model data in correct temporal order
      modfile = '/nas/spc134/URMEL/INVERSION/m_allweekly_'+sim.sn+'_'+sim.name+'_'+yyyymm+'.dat'      
      IF keyword_set(sim.flask) THEN modfile = '/nas/spc134/URMEL/INVERSION/m_allweekly_flask_'+sim.sn+'_'+sim.name+'_'+yyyymm+'.dat'
      IF keyword_set(sim.special) THEN modfile = '/nas/spc134/URMEL/INVERSION/m_allweekly_special_'+sim.sn+'_'+sim.name+'_'+yyyymm+'.dat'
      mch4  = DblArr(sim.ntrace*sim.nage,nc)
      help  = DblArr(sim.ntrace*sim.nage)
      mall  = DblArr(nc)    
      namec = StrArr(nc)
      lon   = FltArr(nc) & lat = FltArr(nc)
      line    = ''
      openr,lun,modfile,/get_lun
      FOR i=0,nc-1 DO BEGIN
        readf,lun,line
        result   = STRSPLIT(line,/EXTRACT)
        namec[i] = result[1]
        lon[i]   = float(result[2])
        lat[i]   = float(result[3])
        mall[i]  = double(result[4])
        readf,lun,help
        mch4[*,i] = help[*]
      ENDFOR
      free_lun,lun
            
      FOR j=0,nst-1 DO BEGIN
        ind = WHERE(namec eq stats[j],ck)
        IF ck gt 0L THEN BEGIN
          latstat[j] = lat[ind[0]]
          lonstat[j] = lon[ind[0]]
          IF lonstat[j] eq 0. THEN stop
          rel        = DblArr(ck,sim.ntrace)  ; relative contribution at stats[j]
          absolut    = rel
          FOR k=0,sim.ntrace-1 DO FOR l=0,ck-1 DO $
          rel[l,k] = (mch4[k,ind[l]]+mch4[11+k,ind[l]]+mch4[22+k,ind[l]])/total(mch4[0:32,ind[l]])
          FOR k=0,sim.ntrace-1 DO FOR l=0,ck-1 DO $
          absolut[l,k] = mch4[k,ind[l]]+mch4[11+k,ind[l]]+mch4[22+k,ind[l]]         
          em[j]         += ck
          FOR k=0,sim.ntrace-1 DO zmstat_rel[j,am[j]:em[j]-1,k] = rel[*,k]
          FOR k=0,sim.ntrace-1 DO zmstat_abs[j,am[j]:em[j]-1,k] = absolut[*,k]         
          am[j]         += ck
        ENDIF
      ENDFOR      
        
    ENDFOR ; end loop over months
  ENDFOR   ; end loop over years   

  ; find 10 most important stations for each category
  important1 = StrArr(10,sim.ntrace) & important2 = important1
  FOR k=0,sim.ntrace-1 DO BEGIN
    help1 = DblArr(nst) & help2 = help1
    FOR j=0,nst-1 DO help1[j] = total(zmstat_rel[j,0:em[j]-1,k])
    FOR j=0,nst-1 DO help2[j] = total(zmstat_abs[j,0:em[j]-1,k])        
    s1    = sort(help1) & s2    = sort(help2)
    szmstat1 = help1(s1) & szmstat2 = help2(s2)
    sstats1  = stats(s1) & sstats2  = stats(s2)
    n1       = n_elements(szmstat1) & n2 = n_elements(szmstat2)
    important1[*,k] = sstats1[n1-10:n1-1]
    important2[*,k] = sstats2[n2-10:n2-1]
  ENDFOR

  cat = ['ANTHA','ANTHB','ANTHC','BBTropics','BBExtra','RICE','WLINUND','WLMINSOIL','PEATNA',$
         'PEATEURA','OTHERNAT']

  ; write out result to file
  outfile = '/home/spc134/IDL/urmel/INVERSION/log/importancestations_percategory_'+sim.name+'_abs.dat'
  IF keyword_set(sim.special) THEN BEGIN
    IF keyword_set(rela) THEN $
    outfile = '/home/spc134/IDL/urmel/INVERSION/log/importancestations_percategory_special_'+sim.name+'_rel.dat' ELSE $
    outfile = '/home/spc134/IDL/urmel/INVERSION/log/importancestations_percategory_special_'+sim.name+'_abs.dat'
    IF keyword_set(sim.flask) THEN $
    outfile = '/home/spc134/IDL/urmel/INVERSION/log/importancestations_percategory_special_flask_'+sim.name+'_abs.dat'    
    
  ENDIF       
  IF keyword_set(sim.flask) THEN $
  outfile = '/home/spc134/IDL/urmel/INVERSION/log/importancestations_percategory_flask_'+sim.name+'_abs.dat' 

  openw,lun,outfile
  FOR k=0,sim.ntrace-1 DO BEGIN
    FOR j=0,9 DO BEGIN
      IF keyword_set(rela) THEN indstat = WHERE(important1[9-j,k] eq stats,cstat) ELSE $
      indstat = WHERE(important2[9-j,k] eq stats,cstat)
      IF cstat gt 0L THEN latout = latstat[indstat]
      IF cstat gt 0L THEN lonout = lonstat[indstat]
      IF keyword_set(rela) THEN $
      printf,lun,cat[k],j+1,important1[9-j,k],lonout,latout,format='(a10,2x,i2,2x,a3,2x,f10.2,2x,f10.2)' $
      ELSE $
      printf,lun,cat[k],j+1,important2[9-j,k],lonout,latout,format='(a10,2x,i2,2x,a3,2x,f10.2,2x,f10.2)'     
    ENDFOR
  ENDFOR
  free_lun,lun

END
