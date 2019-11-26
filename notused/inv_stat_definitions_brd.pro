;+
; NAME:
;
;   inv_stat_definitions_brd
;
; PURPOSE:
;
;   For a given inversion simulation, define list of stations with hourly (continuous)
;   measurements, with event data (flasks), and AGAGE (frequent event data)
;
; CATEGORY:
;
;   MAIOLICA2, inversion, data pre-processing
;
; CALLING SEQUENCE:
;
;    inv_stat_definitions_brd,sim,scont=scont,ccont=ccont,sflask=sflask,cflask=cflask,sagage=sagage
;
; INPUTS:
;
;    sim        : The simulation structure (see inv_configurations_brd.pro)
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;   scont     StrArr(ncont): List of continuous sites
;   ccont     StrArr(ncont): Corresponding contributing networks
;   sevent    StrArr(nevent): List of event (flask) sites
;   cevent    StrArr(nevent): Corresponding contributing networks
;   sagage    StrArr(nevent): List of AGAGE sites
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
;   (c) Dominik Brunner
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   DB, 06 Jan 2017: first implementation
;
;-

PRO inv_stat_definitions_brd,sim,scont=scont,ccont=ccont,sflask=sflask,cflask=cflask,sagage=sagage
;; generate lists of sites with continuous or event data and list of AGAGE stations

  ;; sites with continuous measurements
  statscn  = [   'alt',   'brw',   'llb',   'cdl',   'zgt',   'etl',   'kmw',   'ngl',   'chm',$
                 'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',$
                 'coi',   'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',$
                 'mlo',   'cpt']
  
  ;; contributing network (relevant for scaling)
  contrcn  = [    'ec',  'noaa',   'ec',    'ec',    'ubag',   'ec',    'rivm', 'ubag',  'ec',$
                  'ubag', 'ec',    'ubag',  'empa',  'ubag',   'rse',   'ec',   'ec',$
                  'nies', 'jma',   'kma',   'mri',   'aemet',  'nies',  'jma',  'jma',$
                  'noaa', 'saws']
  
  IF NOT keyword_set(sim.flask) THEN BEGIN
     ;; reduced set of flask sites with event data excluding those that also have continuous data
     statsev  = [   'zep',   'sum',    'ter',  'pal',   'ice',   'sis',   'cba',   'bal',   'shm',   'oxk',$
                    'lpo',   'esp',    'hpb',  'hun',   'puy',   'lef',   'amt',   'bsc',   'kzd',   'uum',$
                    'pdm',$
                    'bgu',    'nwr',  'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',$
                    'bmw',   'bme',    'wkt',  'wis',   'key',   'ask',   'lln',   'kum',   'cri',$
                    'gmi',   'abp',    'chr',  'bkt',   'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',$
                    'ams',   'maa',    'arh',  'bhd',   'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',$
                    'hba']
     
     contrev  = [  'noaa',  'noaa',    'mgo', 'noaa',  'noaa', 'csiro',  'noaa',  'noaa',  'noaa',  'noaa',$
                   'lsce',    'ec',   'noaa', 'noaa',  'lsce',  'cmdl',  'noaa',  'noaa',  'noaa',  'noaa',$
                   'lsce',$
                   'lsce',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',  'noaa',  'noaa',  'enea',$
                   'noaa',  'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',  'noaa', 'csiro',$
                   'noaa',  'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa', 'csiro',  'noaa',  'noaa',$
                   'lsce', 'csiro',   'niwa', 'niwa',  'noaa', 'csiro',  'noaa',  'noaa', 'csiro',  'noaa',$
                   'noaa']
     
  ENDIF ELSE BEGIN
     ;; flask sites with event data
     statsev  = [  'alt',   'brw',    'mhd',  'mlo',   'rpb',   'smo',   'thd',   'wsa',   'cgo',   'izo',$
                   'zep',   'sum',$
                   'ter',   'pal',    'ice',  'sis',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                   'hpb',   'hun',    'puy',  'lef',   'amt',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                   'nwr',   'uta',    'azr',  'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                   'wkt',   'wis',    'key',  'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                   'mkn',   'sey',    'asc',  'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                   'crz',    'mqa',  'tdf',   'psa',   'cya',   'syo',   'hba']
     
     contrev  = [  'ec',  'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',    'ec',  'noaa',  'noaa',$
                   'noaa',  'noaa',$
                   'mgo',  'noaa',   'noaa','csiro',  'noaa',  'noaa',  'noaa',  'noaa',  'lsce',    'ec',$
                   'noaa',  'noaa',   'lsce', 'cmdl',  'noaa',  'noaa',  'noaa',  'noaa',  'lsce',  'lsce',$
                   'noaa',  'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',  'enea',  'noaa',  'noaa',$
                   'noaa',  'noaa',   'noaa', 'noaa',  'noaa',  'noaa', 'csiro',  'noaa',  'noaa',  'noaa',$
                   'noaa',  'noaa',   'noaa','csiro',  'noaa',  'noaa',  'lsce', 'csiro',  'niwa',  'niwa',$
                   'noaa',  'csiro', 'noaa',  'noaa', 'csiro',  'noaa',  'noaa']            
     
  ENDELSE                  

  statsagage   = [ 'mhd', 'thd', 'rpb','smo','cgo']
  
                                ; Lampedusa lmp: NOAA: 2006-12-10 to 2009-12-31
                                ;                ENEA: 1995-01-06 to 2008-12-26
                                ; => take ENEA until end of 2007 and NOAA for 2008/2009
                                ; => check consistency of data sets

                                ; Tina: took out cmn (Monte Cimone), because it's not
                                ; NOAA scale or anything related, 03.11.2011
                                ; took out gsn (Gosan), because it has
                                ; a considerable number of suspicious
                                ; values between 1000 and 1600 ppbv, 19.03.2012
  
                                ; number of stations
  nev     = n_elements(statsev)  
  ncn     = n_elements(statscn)
  nagage  = n_elements(statsagage)
    
  nflask  = 0
  FOR i=0,nev-1 DO BEGIN
     result = WHERE(statsev[i] eq sim.stats,cev)
     IF cev eq 1L THEN nflask += 1
     IF cev gt 1L THEN stop
  ENDFOR

  IF nflask gt 0L THEN BEGIN
     sflask = StrArr(nflask)
     cflask = StrArr(nflask)
     j = 0
     FOR i=0,nev-1 DO BEGIN
        result = WHERE(statsev[i] eq sim.stats,cev)
        IF cev eq 1L THEN BEGIN
           sflask[j] = statsev[i]
           cflask[j] = contrev[i]
           j += 1
        ENDIF
     ENDFOR
  ENDIF

  IF NOT keyword_set(sim.flask) THEN BEGIN
     ncont   = 0
     FOR i=0,ncn-1 DO BEGIN
        result = WHERE(statscn[i] eq sim.stats,cn)
        IF cn eq 1L THEN ncont  += cn
        IF cn gt 1L THEN stop
     ENDFOR

     nhf     = 0
     FOR i=0,nagage-1 DO BEGIN
        result = WHERE(statsagage[i] eq sim.stats,cagage)
        IF cagage eq 1L THEN nhf += 1
        IF cagage gt 1L THEN stop
     ENDFOR

     IF ncont gt 0L  THEN BEGIN
        scont  = StrArr(ncont)
        ccont  = StrArr(ncont)

        j = 0
        FOR i=0,ncn-1 DO BEGIN
           result = WHERE(statscn[i] eq sim.stats,cn)
           IF cn eq 1L THEN BEGIN
              scont[j] = statscn[i]
              ccont[j] = contrcn[i]
              j += 1
           ENDIF
        ENDFOR
     ENDIF

     IF nhf gt 0L THEN BEGIN
        sagage = StrArr(nhf)

        j = 0
        FOR i=0,nagage-1 DO BEGIN
           result = WHERE(statsagage[i] eq sim.stats,cagage)  
           IF cagage eq 1L THEN BEGIN
              sagage[j] = statsagage[i]
              j += 1
           ENDIF
        ENDFOR
     ENDIF
  ENDIF

END
