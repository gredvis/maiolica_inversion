;+
; NAME:
;
;   inv_station_settings
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
;    inv_station_settings,sconfig,stats=stats,ufact=ufact,ok=ok
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

PRO inv_station_settings,sconfig,stats=stats,ufact=ufact,ok=ok

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
     'flask_DLR1': BEGIN        ; 69 stations, NOAA flask network but without smo, ice, sis, amt
        stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'thd',   'wsa',   'cgo',   'izo',$
                       'zep',   'sum',$
                       'ter',   'pal',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                       'hpb',   'hun',   'puy',   'lef',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                       'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                       'wkt',   'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                       'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                       'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']
        nstats = n_elements(stats)
        ufact  = FltArr(nstats) + 1.
     END
     'flask_DLR2': BEGIN        ; 62 stats: NOAA flask network but without smo (Samoa),ice (Storhofdi Iceland),
                                ; sis (Shettland Lerwick),amt (Argyle), lpo (Ile Grande),
                                ; bsc (black sea), wkt (Moody) removed, probably the one used for publication!!
        stats  =   [   'alt',   'brw',   'mhd',   'mlo',   'rpb',   'thd',   'wsa',   'cgo',   'izo',$
                       'zep',   'sum',$
                       'ter',   'pal',   'cba',   'bal',   'shm',   'oxk', 'esp',$
                       'hpb',   'hun',   'puy',   'lef',   'kzd',   'uum',   'pdm',   'bgu',$
                       'nwr',   'uta',   'azr',   'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                       'wis',   'key',   'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                       'mkn',   'sey',   'asc',   'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                       'crz',   'mqa',   'tdf',   'psa',   'cya',   'syo',   'hba']
        nstats = n_elements(stats)
        ufact  = FltArr(nstats) + 1.
     END
     'all': BEGIN               ; all 93 stations including data from 34 continuous stations, until 'cgo'
                                ; Note: station tdf (Tierra del Fuego) is now ush (Ushuaia)
        stats  =   [   'alt',   'brw',   'llb',   'cdl',   'zgt',   'etl',   'kmw',   'mhd',   'ngl',$
                       'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',   'coi',   'thd',$
                       'ryo',   'amy',   'tkb',   'izo',   'hat',   'mnm',   'yon',   'mlo',   'rpb',   'smo',$
                       'cpt',   'cgo',   'zep',   'sum',   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',$
                       'shm',   'oxk',   'lpo',   'esp',   'hpb',   'hun',   'puy',   'lef',   'amt',   'bsc',$
                       'kzd',   'uum',   'pdm',   'bgu',   'nwr',   'uta',   'azr',   'pta',   'sgp',$
                       'tap',   'wlg',   'lmp',   'bmw',   'bme',   'wkt',   'wis',   'key',   'ask',   'lln',$
                       'kum',   'cri',   'gmi',   'abp',   'chr',   'bkt',   'mkn',   'sey',   'asc',   'cfa',$
                       'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',   'crz',   'mqa',   'tdf',   'psa',$
                       'cya',   'syo',   'hba',   'chm',   'goz']
                   
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
                          1.,      1.,      1.,      1.,      1.]
     END
     'brd': BEGIN               
        ;; same as 'all' but excluding
        ;; Kollumerwaard (kmw): large local influence that cannot be reproduced
        ;; Black Sea (bsc): large local influence that cannot be reproduced
        ;; Tsukuba (tkb): poor correlation, large bias, unknown calibration scale
        ;; Dwejra (goz): only few years, not included in DLR output
        ;; Other sites that could potentially be removed: 
        ;;  Anmyeon-Do (amy): poor correlation before 2009, strange pos and neg peaks
        ;;  Moody (wkt): quite strong underestimation. Scot Miller reports strong 
        ;;              underestimation of emissions in this area by EDGAR.
        ;;  Sable Island (wsa): poor correlation, wrong seasonal cyle, really strange
        ;;  Argyle (amt): strong overestimation by model in summer, possibly because it
        ;;              is a 100m tower but model is evaluated at surface.
        ;;  Bukit Koto Tabang (bkt): wildfire influence in model seems overestimation
        ;; Note: station tdf has been renamed by NOAA to ush (Ushuaia)
        stats  =   [   'alt',   'brw',   'llb',   'cdl',   'zgt',   'etl',            'mhd',   'ngl',$
                       'deu',   'fsd',   'ssl',   'jfj',   'zsf',   'prs',   'egb',   'wsa',   'coi',   'thd',$
                       'ryo',   'amy',            'izo',   'hat',   'mnm',   'yon',   'mlo',   'rpb',   'smo',$
                       'cpt',   'cgo',   'zep',   'sum',   'ter',   'pal',   'ice',   'sis',   'cba',   'bal',$
                       'shm',   'oxk',   'lpo',   'esp',   'hpb',   'hun',   'puy',   'lef',   'amt',   $
                       'kzd',   'uum',   'pdm',   'bgu',   'nwr',   'uta',   'azr',   'pta',   'sgp',$
                       'tap',   'wlg',   'lmp',   'bmw',   'bme',   'wkt',   'wis',   'key',   'ask',   'lln',$
                       'kum',   'cri',   'gmi',   'abp',   'chr',   'bkt',   'mkn',   'sey',   'asc',   'cfa',$
                       'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',   'crz',   'mqa',   'tdf',   'psa',$
                       'cya',   'syo',   'hba',   'chm']
        
        ufact = 1. + FltArr(n_elements(stats))
     END
     'special': BEGIN           ; same as 'all' but excluding sites llb, kmw, bkt (total 88 sites)
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
        ufact = 1. + FltArr(n_elements(stats))
     END
     ELSE: BEGIN
        print,'invalid station configuration sconfig'
        RETURN
     END
  ENDCASE
  ok = 1

END
