;+
; NAME:
;
;   read_weekly_model_data
;
; PURPOSE:
;
;   Read daily model data and compute weekly means. Write out in monthly files.
;
; CATEGORY:
;
;   inverse modelling URMEL CTRL run. Preparation for inversion.
;
; CALLING SEQUENCE:
;
;  read_weekly_model_data
;
; INPUTS:
;
;       from: start year of inversion
;       to: end year of inversion
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
;  weekly model data for years of inversion as monthly files
;             
; COMMON BLOCKS:
;
;        none
;
; SIDE EFFECTS:
;
;        none
;
; RESTRICTIONS:
;
; PROCEDURE:
;
;   read_weekly_model_data
;
;     Read daily model data per month
;     Output weekly means in monthly tables.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; CSP 16 April 2012
;-
;******************************************************
;* SUBROUTINES
;******************************************************
PRO read_data,sim=sim,dir=dir,syear,stats=stats,ch4obs=ch4stat,dtgobs=dtgstat,lonobs=lonstat,$
              latobs=latstat,nameobs=namestat,numobs=numstat,type=typestat,nobs=nobs,flask=flask,nobg=nobg,special=special

  nmonths = 12
  nst     = n_elements(stats)
  sn      = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'      

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
;im=1
    ;*******************************
    ;* read data
    ;*******************************     
    IF im lt 9 THEN mm = '0'+STRCOMPRESS(string(im+1),/REM) ELSE mm = STRCOMPRESS(string(im+1),/REM)
    IF keyword_set(flask) THEN BEGIN
      IF keyword_set(nobg) THEN infile   = dir+'z_allweekly_flask_nobg_'+sn+'_'+syear+mm+'.dat' $
      ELSE infile   = dir+'z_allweekly_flask_'+sn+'_'+syear+mm+'.dat'
    ENDIF ELSE BEGIN
      infile   = dir+'z_allweekly_'+sn+'_'+syear+mm+'.dat'    
      IF keyword_set(nobg)    THEN infile   = dir+'z_allweekly_nobg_'+sn+'_'+syear+mm+'.dat' 
      IF keyword_set(special) THEN infile   = dir+'z_allweekly_special_'+sn+'_'+syear+mm+'.dat' 
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
      dtgstat[0L:cst-1L,i]  = dtg[ind]
      namestat[0L:cst-1L,i] = name[ind]
      latstat[0L:cst-1L,i]  = lat[ind]
      lonstat[0L:cst-1L,i]  = lon[ind]
      ch4stat[0L:cst-1L,i]  = ch4[ind]
      numstat[0L:cst-1L,i]  = num[ind]
      typestat[0L:cst-1L,i] = type[ind]   
      nobs[i]               = cst
    ENDIF
  ENDFOR

END ; of routine read_data per year

;**************************************
;* SUBROUTINE OF read_model_data
;* to read in FLEXPART receptor output
;**************************************
PRO read_receptor_output,sim=sim,yyyy=yyyy,stats=stats,ntime=ntime,time=timecollect,mch4=mch4collect,mtrace=mtracollect

  ppbfact = 1.e9
  nmonths = 12
  nst     = n_elements(stats)
  ntot    = sim.ntrace*sim.nage

sn       = STRCOMPRESS(string(fix(n_elements(stats))),/REM) ;flo
  ;******************************************
  ;* determine optimum model output level for 
  ;* comparison with observations
  ;******************************************
;  filein  = '/nas/spc134/URMEL/INVERSION/choice_modellevel_station-2001to2008.dat'

;  filein  = '/home/arf/pers/IDL/urmel/INVERSION/choice_modellevel_station-1990to1992.dat' ; FLO

filein  = '/home/arf/pers/IDL/urmel/INVERSION/choice_modellevel_station.dat' ; FLO

filein  = '/home/arf/pers/IDL/urmel/INVERSION/choice_modellevel_station_DLR_'+sn+'.dat' ; FLO


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
  
  mlevel = 0
  index  = WHERE(stats eq station,cstat)
  IF cstat eq 0 THEN stop
  mlevel = mlev[index]
  
  mch4collect = DblArr(366,nst)
  mtracollect = DblArr(366,ntot,nst)
  timecollect = StrArr(366)
  
  tcoll = IntArr(nst)+0
  t     = 0

  FOR im=0,nmonths-1 DO BEGIN
;im=1  
    IF im le 8 THEN mm = '0'+STRCOMPRESS(string(im+1),/REM) ELSE mm = STRCOMPRESS(string(im+1),/REM)
    yyyymm = yyyy+mm
    read_receptors_DLR_final,sim=sim,yyyymm=yyyymm,info=info,data=data
    ntime  = info[0].time  ; total number of receptor output points in yyyymm
    time   = StrArr(ntime) ; corresponding times in dtg time format
    ; problem with a few days missing in Feb 2006 due to ECMWF model change
    IF yyyymm eq '200602' THEN BEGIN
      FOR it=0,6 DO time[it] = yyyymm+'0'+STRCOMPRESS(string(it+3),/REM)+'0000'
      FOR it=7,ntime-1 DO time[it]=yyyymm+STRCOMPRESS(string(it+3),/REM)+'0000' 
    ENDIF ELSE BEGIN
      FOR it=0,8 DO time[it] = yyyymm+'0'+STRCOMPRESS(string(it+1),/REM)+'0000'
      FOR it=9,ntime-1 DO time[it]=yyyymm+STRCOMPRESS(string(it+1),/REM)+'0000'
    ENDELSE
    FOR it=0,ntime-1 DO timecollect[t+it] = time[it]
    t += ntime
                          
    recname = strlowcase(STRMID(info.rcptname,0,3))
  
    FOR i=0,nst-1 DO BEGIN
      ind        = WHERE(stats[i] eq recname,cout)            ; this can be more data than desired because
                                                              ; of potential several output levels
      IF cout gt 0L THEN BEGIN
        indstat  = WHERE(stats[i] eq station,cstat)
        mlevel = mlev[indstat]    
        cout1      = n_elements(data.pptv[0,ind[mlevel]])     ; cout1 : consider data at one output level only 
        FOR it=0,cout1-1 DO mch4collect[it+tcoll[i],i] = $
        total(data[it].pptv[1:ntot,ind[mlevel]]);/data[it].pptv[0,ind[mlevel]]
        FOR it=0,cout1-1 DO FOR itra=0,ntot-1 DO mtracollect[it+tcoll[i],itra,i] = $
        data[it].pptv[itra+1,ind[mlevel]];/data[it].pptv[0,ind[mlevel]]
        ntime      = cout1
        tcoll[i]  += cout1
      ENDIF ELSE BEGIN
        IF stats[i] eq 'bgu' THEN BEGIN
          ssearch = 'beg'
          slon    = 3.23
          slat    = 41.97
        ENDIF
        IF stats[i] eq 'lpo' THEN BEGIN
          ssearch = 'ile'
          slon    = -3.58
          slat    = 48.8
        ENDIF

        ind1 = WHERE(ssearch eq recname,cout2) ; the station is found in the observations
        IF cout2 ge 1L THEN BEGIN
          indlon = WHERE(float(string(info.xrcpt,format='(f7.2)')) eq slon,clon)
          indlat = WHERE(float(string(info.yrcpt,format='(f7.2)')) eq slat,clat)
          IF clon gt 0L and clat gt 0L THEN BEGIN
            intersect = setintersection(indlon,indlat) ; intersect of modelled lats and lons
            mname     = info[intersect].rcptname       ; search model station name
            clatlon   = n_elements(data.pptv[0,intersect])     ; cout1 : consider data at one output level only
            FOR it=0,clatlon-1 DO $
           mch4collect[tcoll[i]+it,i] = total(data[it].pptv[1:ntot,intersect]);/data[it].pptv[0,intersect]
            FOR it=0,clatlon-1 DO FOR itra=0,ntot-1 DO $
            mtracollect[tcoll[i]+it,itra,i] = data[it].pptv[itra+1,intersect];/data[it].pptv[0,intersect]           
            tcoll[i] += clatlon
          ENDIF ELSE BEGIN
            print, 'Station ', stats, ' not in model output list! => STOP '
            stop
          ENDELSE
        ENDIF       ; endif station needs to be searched by lat/lon identifyers
      ENDELSE       ; endif station is found in recname or by lat/lon identifyers
    
   ENDFOR
;stop 
         ; end loop over stations stats
;stop

  ENDFOR            ; end loop over months in year syear
  
  mch4collect = mch4collect*1.d9
  mtracollect = mtracollect*1.d9
;stop    
END

;* READ FLEXPART DATA
PRO read_model_data,sim=sim,dir=dir,syear,stats=stats,odtg=odtg,type=type,mch4=ch4stat,trace=ch4trace,$
                    nmod=numstat,mdtg=dtgstat
                                        
  nst     = n_elements(stats)
  nmonths = 12    
  ncats   = 48 ;FLO prev 11
  nlev    = 17
  ppbfact = 1.e9
  days    = ['31','28','31','30','31','30','31','31','30','31','30','31']
; days    =
; ['30','30','30','30','30','30','30','30','30','30','30','30'] ;DLR
; new
;  IF syear eq '1992' or syear eq '1996' THEN days[1] = '29'


  IF keyword_set(oldest) THEN BEGIN
    ntot = sim.ntrace                ; only one age class
    modch4    = DblArr(10000,nst)+!values.d_nan
  ENDIF ELSE BEGIN
    ntot = sim.ntrace * sim.nage     ; all nage age classes
    modch4    = DblArr(10000,nst)+!values.d_nan
  ENDELSE  
  moddtg      = StrArr(10000,nst) + ''

  ; call subroutine to read model data of current year
  yyyy = syear        
  read_receptor_output,sim=sim,yyyy=yyyy,stats=stats,ntime=ntime,time=time,mch4=mch4,mtrace=mtrace
 ; stop
  print, 'read_model_data: after read_receptor_output'   
       
  dtgstat  = StrArr(366,nst)                  
  ch4stat  = DblArr(366,nst)
  ch4trace = DblArr(366,ntot,nst)         
  numstat  = IntArr(nst)         
                                    
  FOR i=0,nst-1 DO BEGIN
    indstat = WHERE(odtg[*,i] ne '',cstat)
    IF cstat gt 0L THEN BEGIN
      odtgstat = StrArr(cstat) & typestat = StrArr(cstat) & mch4stat = DblArr(cstat) & mch4trace = DblArr(ntot,cstat)
      odtgstat[*]  = odtg[indstat,i]
      typestat[*]  = type[indstat,i]
      numstat[i]   = cstat
     ;stop                           
      FOR k=0,cstat-1 DO BEGIN                ; loop over all dates of station stats 
        indz  = WHERE(dtg2hiptime(odtgstat[k]) eq dtg2hiptime(time),cz)  ; where to place model data in month
        dtgstat[k,i] = odtgstat[k]
        ;****************************************************************************
        IF cz eq 1L THEN BEGIN   ; station identified     
          dat = fix(STRMID(odtgstat[k],6,2))
          mon = fix(STRMID(odtgstat[k],4,2))
          mm  = STRMID(odtgstat[k],4,2)        
          CASE fix(days[mon-1]) OF
            31: BEGIN
                wmin = ['01','09','17','25'] & wmax = ['08','16','24','31'] & dates = ['04','12','20','28']
                END
            30: BEGIN
                wmin = ['01','09','17','25'] & wmax = ['08','16','24','30'] & dates = ['04','12','20','27']
                END
            28: BEGIN
                wmin = ['01','08','15','23'] & wmax = ['07','14','22','28'] & dates = ['04','11','18','26']
                END
            29: BEGIN
                wmin = ['01','08','15','23'] & wmax = ['07','14','22','29'] & dates = ['04','11','18','26']
                END
                ELSE: stop
          ENDCASE        
          FOR j=0,3 DO BEGIN
            inds = WHERE(dat ge fix(wmin[j]) and dat le fix(wmax[j]),c)
          ;  IF c eq 1L THEN BEGIN ;commented FLO 199308 issue
            ;   IF inds gt -1L THEN BEGIN          ;commented FLO 199308 issue
              datmwa = yyyy+mm+wmin[j]+'0000' ; beginning date week j in month
              datmwe = yyyy+mm+wmax[j]+'0000' ; end date of week j in month
     ;ind = WHERE(dtg2hiptime(time) ge dtg2hiptime(datmwa) and dtg2hiptime(time) le dtg2hiptime(datmwe),cweek)             
          ; ENDIF 
            ind = WHERE(dtg2hiptime(time) ge dtg2hiptime(datmwa) and dtg2hiptime(time) le dtg2hiptime(datmwe),cweek)
;ELSE BEGIN
  ;cweek=-1 

;ENDELSE

   ;commented FLO 199308 issue

            ;IF c eq -1L THEN cweek=0L 
;stop
;ind = WHERE(dtg2hiptime(time) ge dtg2hiptime(datmwa) and dtg2hiptime(time) le dtg2hiptime(datmwe),cweek)
       ;    IF c eq 1L THEN ind = WHERE(dtg2hiptime(time) ge dtg2hiptime(datmwa) and dtg2hiptime(time) le dtg2hiptime(datmwe),cweek) ELSE cweek=-1
            
            IF cweek gt 0L THEN BEGIN
              mch4stat[k] = mean(mch4[ind,i],/NAN)
              FOR itra=0,ntot-1 DO mch4trace[itra,k] = mean(mtrace[ind,itra,i],/NAN)
            ENDIF ELSE BEGIN
              mch4stat[k] = !VALUES.D_NAN
              FOR itra=0,ntot-1 DO mch4trace[itra,k] = !VALUES.D_NAN
            ENDELSE
         ENDFOR                ;end j
       ENDIF    ; endif cz eq 1                                 
     ENDFOR     ; end loop over all dates of station stats[i]
      ch4stat[0:cstat-1,i] = mch4stat[*]
      FOR itra=0,ntot-1 DO ch4trace[0:cstat-1,itra,i] = mch4trace[itra,*] 
    ENDIF        ; endif station data available in year         
 ENDFOR     ; end loop over stations 

;stop
END
;******************************************************

;******************************************************
;* MAIN PROGRAM
;******************************************************
PRO read_weekly_model_data_all_DLR_final,sim=sim,stats=stats,flask=flask,nobg=nobg,special=special

  IF n_elements(sim) EQ 0 THEN BEGIN
;     sim = {name:'final_sim01',$
;          obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
;          modeldir:'/nas/arf/output/',$
;            outdir:'/nas/arf/INVERSION/SENSITIVITIES/',$
;          syyyymm:'199001',eyyyymm:'199004',ntrace:48,nage:5}

 sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/',$
         syyyymm:'199001',eyyyymm:'199012',scaleq:[0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90,0.90],$
         ntrace:48,nage:5} ;1995 

sim = {name:'final_sim01',$
         obsdir:'/nas/arf/INVERSION/OBSINPUT/FINAL/',$
         modeldir:'/nas/arf/output/',$
         outdir:'/home/arf/pers/IDL/urmel/INVERSION/FINAL/',$
         hdir: '/nas/arf/INVERSION/SENSITIVITIES/FINAL/',$
         syyyymm:'200601',eyyyymm:'201212',scaleq:[0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.20,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.40,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.30,0.4,0.4,0.4,0.4],$
         ntrace:48,nage:5} ;keeppos0 change fcorr all flask not initialised  to 1 and new inv_run sp23 apriori uncert from litterature, taking into account diff regions and also fact that we optimize each month 4 times  NEW DLR

stats  =   [   'alt',   'brw',   'llb',   'cdl',   'zgt',   'etl',   'kmw',   'mhd',   'ngl',$
                   'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',   'coi',   'thd',$
                   'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',   'mlo',   'rpb',   'smo',$
                   'cpt',   'cgo',   'zep',   'sum',   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',$
                   'shm',   'oxk',   'lpo',   'esp',   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',$
                   'kzd',   'uum',   'pdm',   'bgu',   'nwr',   'uta',   'azr',   'pta',   'sgp',$
                   'tap',   'wlg',   'lmp',   'bmw',   'bme',   'wkt',   'wis',   'key',   'ask',   'lln',$
                   'kum',   'cri',   'gmi',   'abp',   'chr',   'bkt',   'mkn',   'sey',   'asc',   'cfa',$
                   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',   'crz',   'mqa',   'tdf',   'psa',$
                   'cya',   'syo',   'hba']
 ;DLR wo smo,ice,sis,amt
    stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'thd',   'wsa',   'cgo',   'izo',$
                   'zep',   'sum',$
                   'ter',   'pal',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                   'hpb',   'hun',   'puy',   'lef',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                   'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                   'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                   'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                   'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']

    nstats = n_elements(stats)
    ufact  = FltArr(nstats)
    ufact[*] = 1.

flask=1
ENDIF

  from = fix(STRMID(sim.syyyymm,0,4))
  to   = fix(STRMID(sim.eyyyymm,0,4))

  basedir  = sim.obsdir
  modeldir = sim.modeldir+sim.name+'/'
  
  sn       = STRCOMPRESS(string(fix(n_elements(stats))),/REM)+'stats'  
  ntot     = sim.ntrace*sim.nage
  
  weekly   = 1
                                                       
  ; number of stations
  nst      = n_elements(stats)
  nmonths  = 12
  mon      = ['01','02','03','04','05','06','07','08','09','10','11','12']
  nyears   = long(to-from+1)
  nall     = nyears*nmonths*31L
  syear    = STRCOMPRESS(string(from+indgen(nyears)),/REM)
  ndata    = IntArr(nmonths,nyears)
  
  ;***************************************
  ;* MAIN LOOP: YEARS
  ;***************************************
  FOR ij=0,nyears-1 DO BEGIN

    print, 'process year ', syear[ij]
    
    ;********************************
    ;* 1. read in observational data 
    ;********************************   
    read_data,sim=sim,dir=basedir,syear[ij],stats=stats,ch4obs=ch4obs,dtgobs=dtgobs,lonobs=lonobs,latobs=latobs,$
              nameobs=nameobs,numobs=numobs,type=type,nobs=nobs,flask=flask,nobg=nobg,special=special
              
    dtgsave = dtgobs
        
    ;********************************
    ;* read in model data from stats
    ;********************************
;stop
    read_model_data,sim=sim,dir=modeldir,syear[ij],stats=stats,odtg=dtgobs,type=type,mch4=mch4,$
                    trace=trace,nmod=nmod,mdtg=mdtg
                                       
    ;************************************************************
    ;* write out data for every month in the order that the 
    ;* data came in
    ;************************************************************
 
   FOR im=0,nmonths-1 DO BEGIN
;im=1    
      IF im le 8 THEN yyyymm = syear[ij]+'0'+STRCOMPRESS(string(im+1),/REM) ELSE $
                       yyyymm = syear[ij]+STRCOMPRESS(string(im+1),/REM)
    
      weekly = 1
        
      read_data_single_final,sim=sim,yyyymm=yyyymm,ch4obs=ch4,dtgobs=dtgobs,$
                       lonobs=lonobs,latobs=latobs,nameobs=nameobs,nobs=nobs,weekly=weekly,$
                       stats=stats,flask=flask,nobg=nobg,special=special
        
       IF syear[ij] eq '2006' and im eq 1 THEN BEGIN
         ind = WHERE(dtg2hiptime(dtgobs[0:nobs-1]) ge dtg2hiptime('200602030000'),c)
         dtgc     = StrArr(c)
         namec    = StrArr(c)
         latc     = FltArr(c) & lonc = FltArr(c)
         dtgc[*]  = dtgobs[ind]
         namec[*] = nameobs[ind]
         latc[*]  = latobs[ind] & lonc[*] = lonobs[ind]
         nc       = c
       ENDIF ELSE BEGIN
         namec    = StrArr(nobs)
         dtgc     = StrArr(nobs)
         latc     = FltArr(nobs) & lonc = FltArr(nobs)
         namec[*] = nameobs[0:nobs-1]
         dtgc[*]  = dtgobs[0:nobs-1]
         latc[*]  = latobs[0:nobs-1] & lonc = lonobs[0:nobs-1]
         nc       = nobs
       ENDELSE
;stop
;TEST 20052016: 
;IF mon[im] eq 2 then stop
;STOP TEST

       IF keyword_set(flask) THEN BEGIN  
         monfile = '/nas/arf/INVERSION/FINAL/m_allweekly_flask_DLR'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'        
         IF keyword_set(nobg) THEN $
         monfile      = '/nas/arf/INVERSION/FINAL/m_allweekly_flask_DLR_nobg_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'          
       ENDIF ELSE BEGIN
         monfile = '/nas/arf/INVERSION/FINAL/m_allweekly_DLR_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'       
         IF keyword_set(nobg) THEN $
         monfile = '/nas/arf/INVERSION/FINAL/m_allweekly_DLR_nobg_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'
         IF keyword_set(special) THEN $
         monfile = '/nas/arf/INVERSION/FINAL/m_allweekly_DLR_special_'+sn+'_'+sim.name+'_'+syear[ij]+mon[im]+'.dat'                         
       ENDELSE
       openw,lun,monfile,/get_lun
       FOR i=0,nc-1 DO BEGIN
         ind1  = WHERE(namec[i] eq stats,c1)
         IF c1 eq 1 THEN BEGIN
           ind2 = WHERE(dtg2hiptime(mdtg[*,ind1]) eq dtg2hiptime(dtgc[i]),c2)
         ;  help = DblArr(44)
           help = DblArr(240) ; ntrace (wo air_tracer)
          ; FOR j=0,43 DO help[j] = trace[ind2,j,ind1] 
           FOR j=0,239 DO help[j] = trace[ind2,j,ind1] ;ntrace-1

           IF c2 eq 1 THEN BEGIN
             printf,lun,dtgc[i],namec[i],latc[i],lonc[i],mch4[ind2,ind1],format='(a12,1x,a3,1x,f8.3,1x,f8.3,1x,f9.4)'
             printf,lun,help
           ENDIF
         ENDIF                 
      ENDFOR
       free_lun,lun
    ENDFOR ;end month
ENDFOR

END

