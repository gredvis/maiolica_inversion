;+
; NAME:
;
;   station_lists
;
; PURPOSE:
;
;  Create a list of station IDs, names and further information.
;   
; CATEGORY:
;
;   Inverse modelling URMEL CTRL run.
;
; CALLING SEQUENCE:
;
;  stats = station_lists(stat_selection,sdetails=sdetails)
;                            
; INPUTS:
;
;       stat_selection (string): A string defining the station set,
;                                options are
;                                'flask' : NOAA flask sites only
;                                'DLR65' : 65 sites for which DLR provides output
;                                'DLR62' : same as DLR65 but 3 sites with bad data removed
;                                'flask_continuous' : selection of
;                                both flask and continuous sites
;
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       RETURN value: String array of station IDs (lower case)
;       sdetails : Structure array with more station details
;                  (long name, lon, lat, elev of station)
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
;   brd, Empa: 6 Jun 2016
;   brd, Empa: 10 Jun 2016: Station details is now read from an external text file and
;                           returned in a separate structure
;-

FUNCTION station_lists,stat_selection,sdetails=sdetails

  IF n_elements(stat_selection) EQ 0 THEN BEGIN
     message,'parameter stat_selection missing',/continue
     RETURN,-1
  ENDIF

  CASE stat_selection OF

     'flask' : stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'smo',   'thd',   'wsa',   'cgo',   'izo',$
                              'zep',   'sum',$
                              'ter',   'pal',   'ice',   'sis',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                              'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                              'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                              'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                              'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                              'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']
     'DLR65' :   stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'thd',   'wsa',   'cgo',   'izo',$
                                'zep',   'sum',$
                                'ter',   'pal',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                                'hpb',   'hun',   'puy',   'lef',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                                'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                                'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                                'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                                'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']
     'flask_continuous':   $
                       ; stats includes data from 34 continuous stations, until 'cgo'
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

     'DLR62':   $
;DLR wo smo,ice,sis,amt and LPO,BSC,WKT removed
        stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'thd',   'wsa',   'cgo',   'izo',$
                   'zep',   'sum',$
                   'ter',   'pal',   'cba',   'bal',   'shm',   'oxk', 'esp',$
                   'hpb',   'hun',   'puy',   'lef',   'kzd',   'uum',   'pdm',   'bgu',$
                   'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                     'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                   'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                   'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']

     ELSE: BEGIN
        message,'unknown stat_selection '+stat_selection,/continue
        RETURN,-1
     ENDELSE

  ENDCASE

  ;; read station information from text file and assign to station ids
  sdetails = read_statlist(/flex)

  index = LonArr(n_elements(stats))
  FOR i = 0,n_elements(stats)-1 DO BEGIN
     ind = WHERE(sdetails.name EQ stats[i],cnt)
     IF cnt EQ 0 THEN BEGIN
        print,'Error, station ',stats[i],' not found in list'
        stop
     ENDIF
     index[i] = ind[0]
  ENDFOR

  sdetails = sdetails[index]

  RETURN,stats

END
