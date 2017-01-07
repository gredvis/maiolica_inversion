;+
; NAME:
;
;   inv_station_settings_brd
;
; PURPOSE:
;
;   Define list of stations to include in inversion and corresponding uncertainty scaling factors
;
;   The routine is called by inv_configuration.pro which itself is called by run_inversion_final
;
; CATEGORY:
;
;    MAIOLICAII, methane inversion, Florian Arfeuille stuff
;
; CALLING SEQUENCE:
;
;    inv_station_settings_brd,sconfig,stats=stats,ufact=ufact,ok=ok
;
; INPUTS:
;
;   sconfig: the station configuration, must be one of
;            'flask', 'flask_12', 'flask_DLR1', 'flask_DLR2', 'all', 'special'
;            The configuration 'flask_DLR2' is probably the one used for the publication.
;
; OUTPUTS:
;
;   ok : =1 if configuration for given run was found, =0 if not
;   stats: StrArr(nstat) list of stations
;   ufact: FltArr(nstat) uncertainty enhancement factors
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
; MODIFICATION HISTORY:
; 
;   (c) Dominik Brunner
;   Swiss Federal Laboratories for Materials Science and Technology
;   Empa Duebendorf, Switzerland
;
;   DB, 04 Jan 2017:  first implementation
;-

PRO inv_station_settings_brd,sconfig,stats=stats,ufact=ufact,ok=ok

  ok = 0

  CASE sconfig OF
     'flask': BEGIN             ; NOAA flask network with 69 stations only
        stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'smo',   'thd',   'wsa',   'cgo',   'izo',$
                       'zep',   'sum',$
                       'ter',   'pal',   'ice',   'sis',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                       'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                       'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                       'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                       'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                       'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']
        nstats = n_elements(stats)
        ufact  = FltArr(nstats) + 1.
     END
     'flask_12': BEGIN          ; test with only 12 NOAA flask sites
        stats  =   [   'cba',   'shm',   'nwr',   'azr', 'key',   'gmi',   'chr', 'brw', 'mlo','sey','maa', 'asc']
        nstats = n_elements(stats)
        ufact  = FltArr(nstats) + 1.
        ufact[10] = 100.
        ufact[11] = 100.
     END
     'flask_DLR1': BEGIN        ; NOAA flask network but without smo,ice,sis,amt
        stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'thd',   'wsa',   'cgo',   'izo',$
                       'zep',   'sum',$
                       'ter',   'pal',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                       'hpb',   'hun',   'puy',   'lef',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                       'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                       'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                       'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                       'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']
     END
     'flask_DLR2': BEGIN        ; NOAA flask network but without smo,ice,sis,amt and LPO,BSC,WKT removed
                                ; probably the one finally used for publication!!
        stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'thd',   'wsa',   'cgo',   'izo',$
                       'zep',   'sum',$
                       'ter',   'pal',   'cba',   'bal',   'shm',   'oxk', 'esp',$
                       'hpb',   'hun',   'puy',   'lef',   'kzd',   'uum',   'pdm',   'bgu',$
                       'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                       'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                       'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                       'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']
     END

     'all': BEGIN               ; all 91 stations including data from 34 continuous stations, until 'cgo'

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
                   
        enh     = 1.      
        ufact   = [       1.,      1.,     enh,      1.,     enh,     enh,     enh,      1.,      1.,$
                          enh,      1.,     enh,      1.,     enh,     enh,     enh,      1.,     enh,      1.,$
                          1.,     enh,     enh,      1.,     enh,      1.,      1.,     enh,      1.,      1.,$
                          1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,     enh,$
                          1.,     enh,     enh,      1.,      1.,      1.,     enh,     enh,      1.,     enh,$
                          enh,      1.,      1.,      enh,      1.,     enh,      1.,     enh,     enh,$
                          1.,      1.,      1.,      1.,     enh,      1.,      1.,      1.,      1.,     enh,$
                          1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,$
                          1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,$
                          1.,      1.,      1.]      

        nstats = n_elements(stats)
        ufact  = FltArr(nstats) + 1.

        ;; ; Test less weigth antarctica
        ;; ufact[81:90] =100.0
        
        ;; ; TEST less weigth tropics
        ;; ufact[26] =100.0   ;mlo
        ;; ufact[65] =100.0   ;key
        ;; ufact[70] =100.0   ;gmi
        ;; ufact[72] =100.0   ;chr
        ;; ufact[75] =100.0   ;sey
        ;; ufact[76] =100.0   ;asc 
        
        ;; ; TEST less weigth except high lat
        ;;ufact[*] =100.0 ;mlo
        ;;ufact[0]=1.0
        ;;ufact[1]=1.0
        ;;ufact[37]=1.0   
        ;;ufact[39]=1.0   ;shm
        ;;ufact[81:90] =1.0
     END

     'special': BEGIN ; same as 'all' but excluding sites llb, kmw, bkt (total 88 sites)
      stats  =   [   'alt',   'brw',            'cdl',   'zgt',   'etl',            'mhd',   'ngl',$
                     'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',   'coi',   'thd',$
                     'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',   'mlo',   'rpb',   'smo',$
                     'cpt',   'cgo',   'zep',   'sum',   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',$
                     'shm',   'oxk',   'lpo',   'esp',   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',$
                     'kzd',   'uum',   'pdm',   'bgu',   'nwr',   'uta',   'azr',   'pta',   'sgp',$
                     'tap',   'wlg',   'lmp',   'bmw',   'bme',   'wkt',   'wis',   'key',   'ask',   'lln',$
                     'kum',   'cri',   'gmi',   'abp',   'chr',            'mkn',   'sey',   'asc',   'cfa',$
                     'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',   'crz',   'mqa',   'tdf',   'psa',$
                     'cya',   'syo',   'hba']
                   
      enh     = 1.
      ufact   = [       1.,      1.,      1.,     enh,     enh,      1.,      1.,$
                        enh,      1.,     enh,      1.,     enh,     enh,     enh,      1.,     enh,      1.,$
                        1.,     enh,     enh,      1.,     enh,      1.,      1.,     enh,      1.,      1.,$
                        1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,     enh,$
                        1.,     enh,     enh,      1.,      1.,      1.,     enh,     enh,      1.,     enh,$
                        enh,      1.,      1.,      enh,      1.,     enh,      1.,     enh,     enh,$
                        1.,      1.,      1.,      1.,     enh,      1.,      1.,      1.,      1.,     enh,$
                        1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,$
                        1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,      1.,$
                        1.,      1.,      1.]                                            
     END
     ELSE: BEGIN
        print,'invalid station configuration sconfig'
        RETURN
     END
  ENDCASE
  ok = 1

END
