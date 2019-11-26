;+
; NAME:
;  station_info
;
; PURPOSE:
;  For a given station ID return detailed information on position, altitude, full name
;  etc.
;
; CALLING SEQUENCE:
;  info = station_info(statid)
;
; HISTORY:
;  Dominik Brunner (DB), Empa
;  04 Feb 2017, DB first implementation
;  29 Dec 2017, DB several coordinates adjusted to true FLEXPART receptor point locations
;               Note that in few cases (denoted "wrong" below), the FLEXPART receptor
;               locations do not exactly match the true station coordinate according to GAWSIS
;  30 Dec 2017, DB added information on site type (flask, continuous, agage and network)
;-
FUNCTION station_info,statid
  
  IF n_elements(statid) EQ 0 THEN BEGIN
     message,'parameter statid missing',/continue
     RETURN,-1
  ENDIF
  surf='surface' & mount='mountain' & tower='tower'
  CASE strlowcase(statid) OF
     'abp': statrec={id:statid,name:'Arembepe',lon:-38.17,lat:-12.77,alt:1.,type:surf}
     'alt': statrec={id:statid,name:'Alert',lon:-62.5167,lat:82.45,alt:0.,type:surf}
     'ams': statrec={id:statid,name:'Amsterdam Island',lon:77.53,lat:-37.8,alt:55.,type:surf}
     'amt': statrec={id:statid,name:'Argyle',lon:-68.68,lat:45.03,alt:157.,type:tower} ; 100 m.a.g. 
     'amy': statrec={id:statid,name:'Anmyeon-do',lon:126.33,lat:36.5383,alt:46.,type:surf}
     'asc': statrec={id:statid,name:'Ascension Island',lon:-14.42,lat:-7.98,alt:54.,type:surf}; wrongly used
     ;;'asc': statrec={id:statid,name:'Ascension Island',lon:-14.40,lat:-7.97,alt:54.,type:surf} ; true coos
     'arh': statrec={id:statid,name:'Arrival Heights',lon:166.660,lat:-77.832,alt:184.,type:surf}
     'ask': statrec={id:statid,name:'Assekrem',lon:5.63,lat:23.27,alt:2710.,type:mount}
     'azr': statrec={id:statid,name:'Terceira Island',lon:-27.38,lat:38.77,alt:40.,type:surf}
     'bal': statrec={id:statid,name:'Baltic Sea',lon:16.67,lat:55.50,alt:7.,type:surf}
     'bgu': statrec={id:statid,name:'Begur',lon:3.23,lat:41.97,alt:13.,type:surf}
     'bhd': statrec={id:statid,name:'Baring Head',lon:174.87,lat:-41.41,alt:85.,type:surf}
     'bkt': statrec={id:statid,name:'Bukit Koto Tabang',lon:100.32,lat:-0.2,alt:864.5,type:surf}
     'bme': statrec={id:statid,name:"St. David's Head",lon:-64.65,lat:32.37,alt:30.,type:surf}
     'bmw': statrec={id:statid,name:'Tudor Hill',lon:-64.87,lat:32.27,alt:30.,type:surf}
     'brw': statrec={id:statid,name:'Barrow',lon:-156.611,lat:71.323,alt:11.,type:surf} 
     'bsc': statrec={id:statid,name:'Black Sea',lon:28.67,lat:44.17,alt:3.,type:surf}
     'cba': statrec={id:statid,name:'Cold Bay',lon:-162.72,lat:55.2,alt:25.,type:surf}
     'cdl': statrec={id:statid,name:'Candle Lake',lon:-104.65,lat:53.87,alt:489.,type:surf}
     'cfa': statrec={id:statid,name:'Cape Ferguson',lon:147.05,lat:-19.28,alt:2.,type:surf}
     'cgo': statrec={id:statid,name:'Cape Grim',lon:144.68,lat:-40.68,alt:94.,type:surf}
     'chm': statrec={id:statid,name:'Chibougamau',lon:-74.3423,lat:49.6925,alt:383.,type:surf}
     'chr': statrec={id:statid,name:'Christmas Island',lon:-157.17,lat:1.7,alt:3.,type:surf}
     'cpt': statrec={id:statid,name:'Cape Point',lon:18.4897,lat:-34.3535,alt:230.,type:surf}
     'coi': statrec={id:statid,name:'Cape Ochiishi',lon:145.5000,lat:43.16667,alt:49.,type:surf}
     'cri': statrec={id:statid,name:'Cape Rama',lon:73.83,lat:15.08,alt:60.,type:surf}
     'crz': statrec={id:statid,name:'Crozet',lon:51.8336,lat:-46.4333,alt:120.,type:surf}
     'cya': statrec={id:statid,name:'Casey',lon:110.517,lat:-66.2833,alt:51.,type:surf}
     'deu': statrec={id:statid,name:'Deuselbach',lon:7.05,lat:49.77,alt:480.,type:surf}
     'egb': statrec={id:statid,name:'Egbert',lon:-79.78333,lat:44.23,alt:253.,type:surf}
     'eic': statrec={id:statid,name:'Easter Island',lon:-109.417,lat:-27.1667,alt:41.,type:surf}
     'esp': statrec={id:statid,name:'Estevan Point',lon:-126.55,lat:49.38,alt:39.,type:surf}
     'etl': statrec={id:statid,name:'East Trout Lake',lon:-104.9834,lat:54.3501,alt:492.,type:surf}
     'fsd': statrec={id:statid,name:'Fraserdale',lon:-81.517,lat:49.84,alt:210.,type:surf}
     'goz': statrec={id:statid,name:'Dwejra Point',lon:14.180,lat:36.049999,alt:30.,type:surf}
     'gmi': statrec={id:statid,name:'Guam',lon:144.78,lat:13.43,alt:2.,type:surf}
     'hat': statrec={id:statid,name:'Hateruma Island',lon:123.81084,lat:24.05389,alt:10.,type:surf}
     'hba': statrec={id:statid,name:'Halley Bay',lon:-26.71,lat:-75.58,alt:30.,type:surf} ; wrongly used
     ;;'hba': statrec={id:statid,name:'Halley Bay',lon:-26.18,lat:-75.62,alt:30.,type:surf} ; true coos
     'hpb': statrec={id:statid,name:'Hohenpeissenberg',lon:11.02,lat:47.8,alt:985.,type:mount}
     'hun': statrec={id:statid,name:'Hegyhatsal',lon:16.65,lat:46.95,alt:344.,type:tower} ; 96 m.a.g. 
     'ice': statrec={id:statid,name:'Storhofdi Iceland',lon:-20.2833,lat:63.400,alt:118.,type:surf}
     'izo': statrec={id:statid,name:'Izana',lon:-16.4994,lat:28.309,alt:2373.,type:mount}
     'jfj': statrec={id:statid,name:'Jungfraujoch',lon:7.9851,lat:46.5475,alt:3580.,type:mount} 
     'key': statrec={id:statid,name:'Key Biscane',lon:-80.2,lat:25.67,alt:3.,type:surf}
     'kmw': statrec={id:statid,name:'Kollumerwaard',lon:6.2666,lat:53.33,alt:0.,type:surf}
     'kum': statrec={id:statid,name:'Cape Kumukahi',lon:-154.82,lat:19.52,alt:3.,type:surf}
     'kzd': statrec={id:statid,name:'Sary Taukum',lon:77.5699,lat:44.45,alt:412.,type:surf} ;Kazakhstan
     'kzm': statrec={id:statid,name:'Plateau Assy',lon:77.87,lat:43.25,alt:2519.,type:surf} 
                                ; Kazakhstan, surrounded by high mountains, therefore surf
     'lef': statrec={id:statid,name:'Park Falls',lon:-90.27,lat:45.92,alt:868.,type:tower} ; 396 m.a.g.
     'llb': statrec={id:statid,name:'Lac La Biche',lon:-112.45,lat:54.95,alt:540.,type:surf} ; peatland site
     'lln': statrec={id:statid,name:'Lulin',lon:120.87,lat:23.47,alt:2867.,type:mount} ; top of ridge
     'lmp': statrec={id:statid,name:'Lampedusa',lon:12.63,lat:35.52,alt:45.,type:surf}
     'lpo': statrec={id:statid,name:'Ile Grande',lon:-3.5839,lat:48.8036,alt:10.,type:surf}
     'maa': statrec={id:statid,name:'Mawson',lon:62.8706,lat:-67.6047,alt:20.,type:surf}
     'mhd': statrec={id:statid,name:'Mace Head',lon:-9.9,lat:53.33,alt:8.,type:surf}
     'mkn': statrec={id:statid,name:'Mt. Kenya',lon:37.297,lat:-0.062,alt:3678.,type:mount}
     'mlo': statrec={id:statid,name:'Mauna Loa',lon:-155.578,lat:19.539,alt:3397.,type:mount}
     'mnm': statrec={id:statid,name:'Minamitorishima',lon:153.9833,lat:24.2883,alt:7.1,type:surf}
     'mqa': statrec={id:statid,name:'Macquarie Island',lon:158.939,lat:-54.4985,alt:12.,type:surf}
     'ngl': statrec={id:statid,name:'Neuglobsow',lon:13.0333,lat:53.1428,alt:65.,type:surf}
     'nmb': statrec={id:statid,name:'Namibia',lon:15.02,lat:-23.57,alt:461.,type:surf}
     'oxk': statrec={id:statid,name:'Ochsenkopf',lon:11.8,lat:50.03,alt:1185.,type:tower} ; 163 m.a.g.
     'pal': statrec={id:statid,name:'Pallas-Sammaltunturi',lon:24.12,lat:67.97,alt:560.,type:surf}
     'pdm': statrec={id:statid,name:'Pic du Midi',lon:0.1411,lat:42.9372,alt:2877.,type:mount}
     'prs': statrec={id:statid,name:'Plateau Rosa',lon:7.7,lat:45.93,alt:3480.,type:mount}
     'psa': statrec={id:statid,name:'Palmer Station',lon:-64.0,lat:-64.92,alt:10.,type:surf}
     'pta': statrec={id:statid,name:'Point Arena',lon:-123.73,lat:38.95,alt:17.,type:surf}
     'puy': statrec={id:statid,name:'Puy de Dome',lon:2.9658,lat:45.7719,alt:1465.,type:mount} ; isolated peak
     'nwr': statrec={id:statid,name:'Niwot Ridge',lon:-105.586,lat:40.053,alt:3523.,type:mount} ; near top of ridge
     'rpb': statrec={id:statid,name:'Ragged Point',lon:-59.43,lat:13.17,alt:45.,type:surf}
     'ryo': statrec={id:statid,name:'Ryori',lon:141.8222,lat:39.0319,alt:260.,type:surf}
     'shm': statrec={id:statid,name:'Shemya Island',lon:174.10,lat:52.72,alt:40.,type:surf}
     'sis': statrec={id:statid,name:'Shetland Lerwick',lon:-1.1833,lat:60.1333,alt:84.,type:surf}
     'sey': statrec={id:statid,name:'Mahe Island',lon:55.52,lat:-4.67,alt:2.,type:surf} ;wrongly used for FLEXPART
     ;;'sey': statrec={id:statid,name:'Mahe Island',lon:55.17,lat:-4.67,alt:2.,type:surf} ;true longitude
     'sgp': statrec={id:statid,name:'Southern Great Plains',lon:-97.5,lat:36.60,alt:318.,type:surf}
     'smo': statrec={id:statid,name:'Samoa',lon:-170.563,lat:-14.2319,alt:77.,type:surf} ; wrongly used
     ;;'smo': statrec={id:statid,name:'Samoa',lon:-170.5645,lat:-14.2475,alt:77.,type:surf} ; true coos
     'ssl': statrec={id:statid,name:'Schauinsland',lon:7.9167,lat:47.900,alt:1205.,type:mount}
     'sum': statrec={id:statid,name:'Summit',lon:-38.48,lat:72.58,alt:3238,type:surf} ; flat area on Greenland
     'syo': statrec={id:statid,name:'Syowa Station',lon:39.58,lat:-69.0,alt:16.,type:surf}
     'tap': statrec={id:statid,name:'Tae-ahn Peninsula',lon:126.13,lat:36.73,alt:20.,type:surf}
     'ter': statrec={id:statid,name:'Teriberka',lon:35.1,lat:69.2,alt:40.,type:surf}
     'thd': statrec={id:statid,name:'Trinidad Head',lon:-124.15,lat:41.05,alt:120.,type:surf}
     'tdf': statrec={id:statid,name:'Tierra del Fuego',lon:-68.32,lat:-54.82,alt:30.,type:surf}
     'tkb': statrec={id:statid,name:'Tsukuba',lon:140.13,lat:36.05,alt:26.,type:surf}
     'uum': statrec={id:statid,name:'Ulaan Uul',lon:111.10,lat:44.45,alt:914.,type:surf}
     'ush': statrec={id:statid,name:'Ushuaia',lon:-68.32,lat:-54.82,alt:30.,type:surf}     ; same as Tierra del Fuego
     'uta': statrec={id:statid,name:'Wendover',lon:-113.72,lat:39.900,alt:1320.,type:surf} ; flat salt lake area
     'wis': statrec={id:statid,name:'Sede Boker',lon:34.88,lat:31.13,alt:400.,type:surf}
     'wkt': statrec={id:statid,name:'Moody',lon:-97.62,lat:31.32,alt:708.,type:tower} ; sampling 457 m.a.g.
     'wlg': statrec={id:statid,name:'Mt. Waliguan',lon:100.9,lat:36.28,alt:3810.,type:mount}
     'wsa': statrec={id:statid,name:'Sable Island',lon:-60.02,lat:43.93,alt:5.,type:surf}
     'yon': statrec={id:statid,name:'Yonaginijima',lon:123.0106,lat:24.4667,alt:30.,type:surf}
     'zgt': statrec={id:statid,name:'Zingst',lon:12.73,lat:54.43,alt:1.,type:surf}
     'zsf': statrec={id:statid,name:'Zugspitze / Schneefernerhaus',lon:10.98,lat:47.42,alt:2656.,type:mount}
     'zep': statrec={id:statid,name:'Zeppelinfjellet',lon:11.88,lat:78.9,alt:45.,type:surf}
     ELSE: BEGIN
        message,'station '+statid+' not found',/cont
        RETURN,''
     END

  ENDCASE

  ;; add information on type of site, i.e. flask or continuous, and on contributing network
  
  ;; flask sites with event data
  statsev  = [  'alt',   'brw',    'mhd',  'mlo',   'rpb',   'smo',   'thd',   'wsa',   'cgo',   'izo',$
                'zep',   'sum',$
                'ter',   'pal',    'ice',  'sis',   'cba',   'bal',   'shm',   'oxk',   'lpo',   'esp',$
                'hpb',   'hun',    'puy',  'lef',   'amt',   'bsc',   'kzd',   'uum',   'pdm',   'bgu',$
                'nwr',   'uta',    'azr',  'pta',   'sgp',   'tap',   'wlg',   'lmp',   'bmw',   'bme',$
                'wkt',   'wis',    'key',  'ask',   'lln',   'kum',   'cri',   'gmi',   'abp',   'chr',$
                'mkn',   'sey',    'asc',  'cfa',   'nmb',   'eic',   'ams',   'maa',   'arh',   'bhd',$
                'crz',   'mqa',    'tdf',  'psa',   'cya',   'syo',   'hba',   'bkt',   'goz']
  
  ;; contrev  = [  'ec',   'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',    'ec',  'noaa',  'noaa',$
  ;;               'noaa', 'noaa',$
  ;;               'mgo',  'noaa',   'noaa','csiro',  'noaa',  'noaa',  'noaa',  'noaa',  'lsce',    'ec',$
  ;;               'noaa', 'noaa',   'lsce', 'cmdl',  'noaa',  'noaa',  'noaa',  'noaa',  'lsce',  'lsce',$
  ;;               'noaa', 'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',  'enea',  'noaa',  'noaa',$
  ;;               'noaa', 'noaa',   'noaa', 'noaa',  'noaa',  'noaa', 'csiro',  'noaa',  'noaa',  'noaa',$
  ;;               'noaa', 'noaa',   'noaa','csiro',  'noaa',  'noaa',  'lsce', 'csiro',  'niwa',  'niwa',$
  ;;               'noaa', 'csiro',  'noaa',  'noaa', 'csiro', 'noaa',  'noaa',  'noaa',
             ;;               'noaa']

  ;; new GAW2017 version
  contrev  = [  'ec',   'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'noaa',    'ec',  'noaa',  'noaa',$
                'noaa', 'noaa',$
                'mgo',  'noaa',   'noaa','csiro',  'noaa',  'noaa',  'noaa',  'noaa',  'lsce',    'ec',$
                'noaa', 'noaa',   'lsce', 'noaa',  'noaa',  'noaa',  'noaa',  'noaa',  'lsce',  'lsce',$
                'noaa', 'noaa',   'noaa', 'noaa',  'noaa',  'noaa',  'cma_noaa','enea','noaa',  'noaa',$
                'noaa', 'noaa',   'noaa', 'noaa',  'noaa',  'noaa', 'csiro',  'noaa',  'noaa',  'noaa',$
                'noaa', 'noaa',   'noaa','csiro',  'noaa',  'noaa',  'lsce', 'csiro',  'niwa',  'niwa',$
                'noaa', 'csiro',  'noaa',  'noaa', 'csiro', 'noaa',  'noaa',  'noaa',  'noaa']

  index = WHERE(statid EQ statsev,cnt)
  flask = 0B & flasknetwork=''
  IF cnt GT 0 THEN BEGIN
     flask = 1B
     flasknetwork = contrev[index[0]]
  ENDIF

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

  index = WHERE(statid EQ statscn,cnt)
  continuous = 0B & contnetwork=''
  IF cnt GT 0 THEN BEGIN
     continuous = 1B
     contnetwork = contrcn[index[0]]
  ENDIF

  ;; AGAGE sites with quasi-continuous measurements
  statsagage   = [ 'mhd', 'thd', 'rpb','smo','cgo']
  index = WHERE(statid EQ statsagage,cnt)
  agage = 0B
  IF cnt GT 0 THEN agage = 1B
  
  ;; define y-axis range for plotting purposes
  yrange = [1600.,2000.]

  ;; create structure
  statrec = create_struct('flask',flask,'flasknetwork',flasknetwork,'continuous',continuous,$
                          'contnetwork',contnetwork,'agage',agage,'yrange',yrange,statrec)

  return,statrec

END
