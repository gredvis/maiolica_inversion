;*************************************************************
;READ MONTHLY EMISSIONS OF 3 MONTHS BEFORE ACTUAL MONTH
;*************************************************************
PRO inv_emissions_ratio_DLR_final,sim=sim,stats=stats,flask=flask,nobg=nobg,special=special

  ;special = 1  ;FLO 26/03/2015

  IF n_elements(sim) EQ 0 THEN BEGIN
     sim = {name:'final_sim01',$
          obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
          modeldir:'/nas/arf/output/',$
            outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
             hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
            syyyymm:'199001',eyyyymm:'199112',scaleq:[0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30],$
          ntrace:48,nage:5}
  ENDIF

  IF n_elements(stats) EQ 0 THEN BEGIN
      stats  =   [   'alt',   'brw',   'cdl',   'zgt',   'etl',   'mhd',   'ngl',$
                     'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',   'coi',   'thd',$
                     'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',   'mlo',   'rpb',   'smo',$
                     'cpt',   'cgo',   'zep',   'sum',   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',$
                     'shm',   'oxk',   'lpo',   'esp',   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',$
                     'kzd',   'uum',   'pdm',   'bgu',   'nwr',   'uta',   'azr',   'pta',   'sgp',$
                     'tap',   'wlg',   'lmp',   'bmw',   'bme',   'wkt',   'wis',   'key',   'ask',   'lln',$
                     'kum',   'cri',   'gmi',   'abp',   'chr',   'mkn',   'sey',   'asc',   'cfa',$
                     'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',   'crz',   'mqa',   'tdf',   'psa',$
                     'cya',   'syo',   'hba']                   
  ENDIF                     
  sn = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats' 
  
  ; calculate number of months to simulate
  syyyy = fix(strmid(sim.syyyymm,0,4)) & smm = fix(STRMID(sim.syyyymm,4,2))
  eyyyy = fix(strmid(sim.eyyyymm,0,4)) & emm = fix(STRMID(sim.eyyyymm,4,2))
  n = eyyyy*12L+emm-(syyyy*12L+smm)+1

  nmonths = 3
  fcorr = FltArr(n,sim.ntrace)
  sa      = DblArr(n,sim.ntrace)
  sp      = DblArr(n,sim.ntrace)
  qp      = DblArr(n,sim.ntrace)
  eratio  = DblArr(n,sim.ntrace)
  uncert  = DblArr(n)

  ; read apriori and aposteriori emissions ...
  qunc = 'opt'+STRCOMPRESS(string(total(sim.scaleq)),/REM)
  IF keyword_set(flask) THEN BEGIN
    IF keyword_set(nobg) THEN $
    apostfile = sim.outdir+'inv_output_weekly_flask_nobg_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                sim.eyyyymm+'_'+qunc+'_nov12.txt' $
    ELSE apostfile = sim.outdir+'inv_output_weekly_flask_DLR_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+$
                sim.eyyyymm+'_'+qunc+'_nov12.txt' 
  ENDIF ELSE BEGIN
    apostfile = sim.outdir+'inv_output_weekly_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'  
    IF keyword_set(nobg) THEN $
    apostfile = sim.outdir+'inv_output_weekly_nobg_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'
    IF keyword_set(special) THEN $
    apostfile = sim.outdir+'inv_output_weekly_special_'+sn+'_'+sim.name+'_'+sim.syyyymm+'-'+sim.eyyyymm+'_'+qunc+'_nov12.txt'    
  ENDELSE                
                
  openr,lun,apostfile,/get_lun
  readf,lun,fcorr
  readf,lun,sa
  readf,lun,sp
  readf,lun,qp
  readf,lun,uncert
  free_lun,lun
  
  eratio = sp/sa
    
  seas    = FltArr(n,sim.ntrace)
  x       = 1.+findgen(n)
  
  FOR j=0,11 DO BEGIN
    ind=WHERE( ((x) MOD 12) EQ j)
    FOR l=0,sim.ntrace-1 DO seas[ind,l] = mean(eratio[ind,l],/nan)
  ENDFOR  
  mseas = FltArr(sim.ntrace)
  FOR it=0,sim.ntrace-1 DO mseas[it] = mean(eratio[*,it],/nan)

  cyyyymm = sim.syyyymm
  ncmax   = 1000L
  nc      = IntArr(n)
  mch4    = DblArr(sim.ntrace*sim.nage,ncmax)
  help    = DblArr(sim.ntrace*sim.nage)
  mall    = DblArr(ncmax)      
  
  sum      = DblArr(sim.ntrace,n)
  asum     = DblArr(sim.ntrace,n)  

  FOR i=0,n-1 DO BEGIN  
  
    print, cyyyymm
  
    ; read weekly model data of given year and month
    IF keyword_set(flask) THEN BEGIN
      IF keyword_set(nobg) THEN $
           modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_flask_nobg_'+sn+'_'+sim.name+'_'+cyyyymm+'.dat' $      
      ELSE modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_flask_DLR'+sn+'_'+sim.name+'_'+cyyyymm+'.dat'
    ENDIF ELSE BEGIN
      modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_'+sn+'_'+sim.name+'_'+cyyyymm+'.dat'    
      IF keyword_set(nobg) THEN $
           modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_nobg_'+sn+'_'+sim.name+'_'+cyyyymm+'.dat'
      IF keyword_set(special) THEN $
           modfile = '/nas/arf/INVERSION/FINAL/m_allweekly_special_'+sn+'_'+sim.name+'_'+cyyyymm+'.dat'    
    ENDELSE
    line    = ''
    openr,lun1,modfile,/get_lun
    j       = 0
    WHILE NOT EOF(lun1) DO BEGIN
      readf,lun1,line
      result  = STRSPLIT(line,/EXTRACT)
      mall[j] = double(result[4])
      readf,lun1,help
      mch4[*,j] = help[*]
      j += 1
    ENDWHILE
    free_lun,lun1
    nc[i] = j
    
    IF cyyyymm eq sim.syyyymm THEN BEGIN
      nopt = 1
    ENDIF ELSE IF Long(cyyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
      nopt = 2
    ENDIF ELSE IF Long(cyyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN
       nopt = 3
 ENDIF ELSE nopt = 4
    apriori  = DblArr(sim.ntrace,nc[i])
    modest   = DblArr(sim.ntrace,nc[i])
    ;atotteil = DblArr(sim.ntrace,4)
    atotteil = DblArr(sim.ntrace,5) ;flo
    ;ateil    = DblArr(4,sim.ntrace,nc[i]) 
    ateil    = DblArr(5,sim.ntrace,nc[i]) ;flo
    ;totteil  = DblArr(sim.ntrace,4)  
    totteil  = DblArr(sim.ntrace,5) ;flo  
    ;atotteil = DblArr(sim.ntrace,4)  
    atotteil = DblArr(sim.ntrace,5) ;flo  
    modest[*,*] = 0.1D
    teil1 = DblArr(sim.ntrace,nc[i])
    teil2 = teil1 & teil3 = teil1 & teil4 = teil1 & teil5 = teil1
    BG      = DblArr(nc[i],sim.ntrace)
    BGpost  = DblArr(nc[i],sim.ntrace)    
    FOR k=0,nc[i]-1 DO BEGIN   
    
;      BG[k] = fcorr[i]*total(mch4[33:43,k])          ; loop over model data in month i
FOR it=0,sim.ntrace-1 DO BEGIN
 BG[k,it] = fcorr[i,it]*mch4[it+(sim.nage-1)*sim.ntrace,k]       ; loop over model data in month i  ;FLO
ENDFOR

; FOR l=0,sim.ntrace-1 DO BGpost[k] += mch4[33+l,k]*mseas[l]    
                                ; loop over model data in month i

 FOR l=0,sim.ntrace-1 DO BGpost[k,l] += mch4[192+l,k]*mseas[l]    
                                ; loop over model data in month i

      FOR l=0,sim.ntrace-1 DO BEGIN
        IF cyyyymm eq sim.syyyymm THEN BEGIN
          teil1[l,k]   = mch4[l,k]*eratio[i,l]
          teil2[l,k]   = mch4[sim.ntrace+l,k]
          teil3[l,k]   = mch4[2*sim.ntrace+l,k]
          teil4[l,k]   = mch4[3*sim.ntrace+l,k];*fcorr[i] ;flo
          teil5[l,k]   = mch4[4*sim.ntrace+l,k]*fcorr[i,l] ;flo
          
          ateil[0,l,k] = mch4[l,k]
          ateil[1,l,k] = mch4[sim.ntrace+l,k]
          ateil[2,l,k] = mch4[2*sim.ntrace+l,k]
          ateil[3,l,k] = mch4[3*sim.ntrace+l,k]     
          ateil[4,l,k] = mch4[4*sim.ntrace+l,k]     
        ENDIF ELSE IF Long(cyyyymm) eq (Long(sim.syyyymm)+1L) THEN BEGIN
          teil1[l,k] = mch4[l,k]*eratio[i,l]
          teil2[l,k] = mch4[sim.ntrace+l,k]*eratio[i-1,l]
          teil3[l,k] = mch4[2*sim.ntrace+l,k]
          teil4[l,k] = mch4[3*sim.ntrace+l,k];*fcorr[i] ;flo
          teil5[l,k] = mch4[4*sim.ntrace+l,k]*fcorr[i,l]  ;flo     
          
          ateil[0,l,k] = mch4[l,k]
          ateil[1,l,k] = mch4[sim.ntrace+l,k]
          ateil[2,l,k] = mch4[2*sim.ntrace+l,k]
          ateil[3,l,k] = mch4[3*sim.ntrace+l,k]
          ateil[4,l,k] = mch4[4*sim.ntrace+l,k]
       ENDIF ELSE IF Long(cyyyymm) eq (Long(sim.syyyymm)+2L) THEN BEGIN
          teil1[l,k] = mch4[l,k]*eratio[i,l]
          teil2[l,k] = mch4[sim.ntrace+l,k]*eratio[i-1,l]
          teil3[l,k] = mch4[2*sim.ntrace+l,k]*eratio[i-2,l]
          ;teil4[l,k] = mch4[3*sim.ntrace+l,k]*mseas[l]*BG[k]/BGpost[k]
          teil4[l,k] = mch4[3*sim.ntrace+l,k];*eratio[i-3,l]
          teil5[l,k] = mch4[4*sim.ntrace+l,k]*fcorr[i,l]
          
          ateil[0,l,k] = mch4[l,k]
          ateil[1,l,k] = mch4[sim.ntrace+l,k]
          ateil[2,l,k] = mch4[2*sim.ntrace+l,k]
          ateil[3,l,k] = mch4[3*sim.ntrace+l,k] 
          ateil[4,l,k] = mch4[4*sim.ntrace+l,k]                  
       ;ENDELSE
    ENDIF ELSE BEGIN
          teil1[l,k] = mch4[l,k]*eratio[i,l]
          teil2[l,k] = mch4[sim.ntrace+l,k]*eratio[i-1,l]
          teil3[l,k] = mch4[2*sim.ntrace+l,k]*eratio[i-2,l]
          ;teil4[l,k] = mch4[3*sim.ntrace+l,k]*mseas[l]*BG[k]/BGpost[k]
          teil4[l,k] = mch4[3*sim.ntrace+l,k]*eratio[i-3,l]
          ;teil5[l,k] = mch4[4*sim.ntrace+l,k]*mseas[l]*BG[k]/BGpost[k]
          teil5[l,k] = mch4[4*sim.ntrace+l,k]*fcorr[i,l]

          ateil[0,l,k] = mch4[l,k]
          ateil[1,l,k] = mch4[sim.ntrace+l,k]
          ateil[2,l,k] = mch4[2*sim.ntrace+l,k]
          ateil[3,l,k] = mch4[3*sim.ntrace+l,k]  
          ateil[4,l,k] = mch4[4*sim.ntrace+l,k]            
       ENDELSE
        modest[l,k]  = teil1[l,k]+teil2[l,k]+teil3[l,k]+teil4[l,k]+teil5[l,k]
        apriori[l,k] = ateil[0,l,k]+ateil[1,l,k]+ateil[2,l,k]+ateil[3,l,k]+ateil[4,l,k]
      ENDFOR
    ENDFOR



        
    FOR l=0,sim.ntrace-1 DO BEGIN
      totteil[l,0]  = mean(teil1[l,*]) & totteil[l,1]  = mean(teil2[l,*])
      totteil[l,2]  = mean(teil3[l,*]) & totteil[l,3]  = mean(teil4[l,*])
      totteil[l,4]  = mean(teil5[l,*]) 
      atotteil[l,0]  = mean(ateil[0,l,*]) & atotteil[l,1]  = mean(ateil[1,l,*])
      atotteil[l,2]  = mean(ateil[2,l,*]) & atotteil[l,3]  = mean(ateil[3,l,*])      
      atotteil[l,4]  = mean(ateil[4,l,*]) 
      sum[l,i]  = total(totteil[l,*])
      asum[l,i] = total(atotteil[l,*])
    ENDFOR 
              
    IF keyword_set(flask) THEN BEGIN
      IF keyword_set(nobg) THEN $
      outfile = '/nas/arf/INVERSION/FINAL/m_weekly_apost_categories_flask_nobg_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat' $      
      ELSE outfile = '/nas/arf/INVERSION/FINAL/m_weekly_apost_categories_flask_DLR_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat'
    ENDIF ELSE BEGIN
      outfile = '/nas/arf/INVERSION/FINAL/m_weekly_apost_categories_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat'    
      IF keyword_set(nobg) THEN $
      outfile = '/nas/arf/INVERSION/FINAL/m_weekly_apost_categories_nobg_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat'
      IF keyword_set(special) THEN $
      outfile = '/nas/arf/INVERSION/FINAL/m_weekly_apost_categories_special_'+sn+'_'+sim.name+'_'+qunc+'_'+cyyyymm+'.dat'      
    ENDELSE
    print, outfile
    openw,lun2,outfile,/get_lun
;flo
;    FOR k=0,nc[i]-1 DO printf,lun2,k, modest[0,k],modest[1,k],modest[2,k],modest[3,k],$
;                                      modest[4,k],modest[5,k],modest[6,k],modest[7,k],$
;                                      modest[8,k],modest[9,k],modest[10,k],$
;                                      format='(i4,1x,11(f8.3,1x))'
;flo
;flo
    FOR k=0,nc[i]-1 DO printf,lun2,k, modest[0,k],modest[1,k],modest[2,k],modest[3,k],$
                                      modest[4,k],modest[5,k],modest[6,k],modest[7,k],$
                                      modest[8,k],modest[9,k],modest[10,k],$
                              modest[11,k],modest[12,k],modest[13,k],modest[14,k],$
                                      modest[15,k],modest[16,k],modest[17,k],modest[18,k],$
                                      modest[19,k],modest[20,k],modest[21,k],$
                              modest[22,k],modest[23,k],modest[24,k],modest[25,k],$
                                      modest[26,k],modest[27,k],modest[28,k],modest[29,k],$
                                      modest[30,k],modest[31,k],modest[32,k],$
                              modest[33,k],modest[34,k],modest[35,k],modest[36,k],$
                                      modest[37,k],modest[38,k],modest[39,k],modest[40,k],$
                                      modest[41,k],modest[42,k],modest[43,k],$
                              modest[44,k],modest[45,k],modest[46,k],modest[47,k],$
                                      format='(i4,1x,11(f8.3,1x))'
;flo



;    FOR k=0,nc[i]-1 DO printf,lun2,k, apriori[0,k],apriori[1,k],apriori[2,k],apriori[3,k],$
;                                      apriori[4,k],apriori[5,k],apriori[6,k],apriori[7,k],$
;                                      apriori[8,k],apriori[9,k],apriori[10,k],$
;                                      format='(i4,1x,11(f8.3,1x))'                                      
                                      
    free_lun,lun2                                     

    ; get next month by adding 40 days and rounding off to full months
    cyyyymm = STRMID(gvtime2dtg(dtg2gvtime(cyyyymm+'010000')+40),0,6)

  ENDFOR

END
