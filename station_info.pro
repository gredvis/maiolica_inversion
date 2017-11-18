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
;  Dominik Brunner, Empa
;  04 Feb 2017, first implementation
;-
FUNCTION station_info,statid
  
  statrec = {id:'',name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}

  ;; define y-axis range for plotting purposes
  yrange = [1600.,2000.]

  CASE strlowcase(statid) OF
     'abp': statrec={id:statid,name:'Arembepe',lon:-38.17,lat:-12.77,alt:0.,yrange:yrange}
     'alt': statrec={id:statid,name:'Alert',lon:-62.5167,lat:82.45,alt:0.,yrange:yrange}
     'ams': statrec={id:statid,name:'Amsterdam Island',lon:77.53,lat:-37.8,alt:55.,yrange:yrange}
     'amt': statrec={id:statid,name:'Argyle',lon:-68.68,lat:45.03,alt:50.,yrange:yrange}
     'amy': statrec={id:statid,name:'Anmyeon-do',lon:126.32,lat:36.53,alt:47.,yrange:yrange}
     'asc': statrec={id:statid,name:'Ascension Island',lon:-14.42,lat:-7.92,alt:54.,yrange:yrange}
     'arh': statrec={id:statid,name:'Arrival Heights',lon:166.67,lat:-77.8,alt:184.,yrange:yrange}
     'ask': statrec={id:statid,name:'Assekrem',lon:5.63,lat:23.27,alt:2710.,yrange:yrange}
     'azr': statrec={id:statid,name:'Terceira Island',lon:-27.37,lat:38.77,alt:40.,yrange:yrange}
     'bal': statrec={id:statid,name:'Baltic Sea',lon:17.22,lat:55.35,alt:28.,yrange:yrange}
     'bgu': statrec={id:statid,name:'Begur',lon:3.23,lat:41.97,alt:13.,yrange:yrange}
     'bhd': statrec={id:statid,name:'Baring Head',lon:174.87,lat:-41.41,alt:85.,yrange:yrange}
     'bkt': statrec={id:statid,name:'Bukit Koto Tabang',lon:100.32,lat:-0.2,alt:864.5,yrange:yrange}
     'bme': statrec={id:statid,name:"St. David's Head",lon:-64.65,lat:32.37,alt:30.,yrange:yrange}
     'bmw': statrec={id:statid,name:'Tudor Hill',lon:-64.87,lat:32.27,alt:30.,yrange:yrange}
     'brw': statrec={id:statid,name:'Barrow',lon:-156.611,lat:71.323,alt:11.,yrange:yrange} 
     'bsc': statrec={id:statid,name:'Black Sea',lon:28.67,lat:44.17,alt:3.,yrange:yrange}
     'cba': statrec={id:statid,name:'Cold Bay',lon:-162.72,lat:55.2,alt:25.,yrange:yrange}
     'cdl': statrec={id:statid,name:'Candle Lake',lon:-104.65,lat:53.87,alt:489.,yrange:yrange}
     'cfa': statrec={id:statid,name:'Cape Ferguson',lon:147.05,lat:-19.28,alt:2.,yrange:yrange}
     'cgo': statrec={id:statid,name:'Cape Grim',lon:144.68,lat:-40.68,alt:94.,yrange:yrange}
     'chr': statrec={id:statid,name:'Christmas Island',lon:-157.17,lat:1.7,alt:3.,yrange:yrange}
     'cpt': statrec={id:statid,name:'Cape Point',lon:0.,lat:0.,alt:0.,yrange:yrange}
     'cri': statrec={id:statid,name:'Cape Rama',lon:73.83,lat:15.08,alt:60.,yrange:yrange}
     'crz': statrec={id:statid,name:'Crozet',lon:51.85,lat:-46.45,alt:120.,yrange:yrange}
     'cya': statrec={id:statid,name:'Casey Station',lon:110.53,lat:-66.28,alt:60.,yrange:yrange}
     'deu': statrec={id:statid,name:'Deuselbach',lon:7.05,lat:49.77,alt:480.,yrange:yrange}
     'egb': statrec={id:statid,name:'Egbert',lon:-79.78333,lat:44.23,alt:253.,yrange:yrange}
     'eic': statrec={id:statid,name:'Easter Island',lon:-109.45,lat:-27.13,alt:50.,yrange:yrange}
     'esp': statrec={id:statid,name:'Estevan Point',lon:-126.55,lat:49.38,alt:39.,yrange:yrange}
     'etl': statrec={id:statid,name:'East Trout Lake',lon:-104.9834,lat:54.3501,alt:492.,yrange:yrange}
     'fsd': statrec={id:statid,name:'Fraserdale',lon:-81.57,lat:49.88,alt:210.,yrange:yrange}
     'gmi': statrec={id:statid,name:'Guam',lon:144.78,lat:13.43,alt:2.,yrange:yrange}
     'hba': statrec={id:statid,name:'Halley Bay',lon:-26.5,lat:-75.57,alt:33.,yrange:yrange}
     'hpb': statrec={id:statid,name:'Hohenpeissenberg',lon:11.02,lat:47.8,alt:985.,yrange:yrange}
     'hun': statrec={id:statid,name:'Hegyhatsal',lon:16.65,lat:46.95,alt:248.,yrange:yrange}
     'ice': statrec={id:statid,name:'Storhofdi Iceland',lon:-20.2833,lat:63.400,alt:118.,yrange:yrange}
     'izo': statrec={id:statid,name:'Izana',lon:0.,lat:0.,alt:0.,yrange:yrange}
     'jfj': statrec={id:statid,name:'Jungfraujoch',lon:0.,lat:0.,alt:0.,yrange:yrange} 
     'key': statrec={id:statid,name:'Key Biscane',lon:-80.2,lat:25.67,alt:3.,yrange:yrange}
     'kmw': statrec={id:statid,name:'Kollumerwaard',lon:6.28,lat:53.33,alt:0.,yrange:yrange}
     'kum': statrec={id:statid,name:'Cape Kumukahi',lon:-154.82,lat:19.52,alt:3.,yrange:yrange}
     'kzd': statrec={id:statid,name:'Sary Taukum',lon:75.57,lat:44.45,alt:412.,yrange:yrange}   ;Kazakhstan
     'kzm': statrec={id:statid,name:'Plateau Assy',lon:77.87,lat:43.25,alt:2519.,yrange:yrange} ; Kazakhstan
     'lef': statrec={id:statid,name:'Park Falls',lon:-90.27,lat:45.92,alt:868.,yrange:yrange}
     'llb': statrec={id:statid,name:'Lac La Biche',lon:-112.45,lat:54.95,alt:540.,yrange:yrange}
     'lln': statrec={id:statid,name:'Lulin',lon:120.87,lat:23.47,alt:2867.,yrange:yrange}
     'lmp': statrec={id:statid,name:'Lampedusa',lon:12.63,lat:35.52,alt:45.,yrange:yrange}
     'lpo': statrec={id:statid,name:'Ile Grande',lon:-3.5839,lat:48.8036,alt:10.,yrange:yrange}
     'maa': statrec={id:statid,name:'Mawson',lon:62.87,lat:-67.62,alt:32.,yrange:yrange}
     'mhd': statrec={id:statid,name:'Mace Head',lon:-9.9,lat:53.33,alt:8.,yrange:yrange}
     'mkn': statrec={id:statid,name:'Mt. Kenya',lon:37.297,lat:-0.062,alt:3678.,yrange:yrange}
     'mlo': statrec={id:statid,name:'Mauna Loa',lon:-155.578,lat:19.539,alt:3397.,yrange:yrange}
     'mnm': statrec={id:statid,name:'Minamitorishima',lon:0.,lat:0.,alt:0.,yrange:yrange}
     'mqa': statrec={id:statid,name:'Macquarie Island',lon:158.97,lat:-54.48,alt:12.,yrange:yrange}
     'ngl': statrec={id:statid,name:'Neuglobsow',lon:13.03,lat:53.17,alt:65.,yrange:yrange}
     'nmb': statrec={id:statid,name:'Namibia',lon:15.02,lat:-23.57,alt:461.,yrange:yrange}
     'oxk': statrec={id:statid,name:'Ochsenkopf',lon:11.8,lat:50.03,alt:1185.,yrange:yrange}
     'pal': statrec={id:statid,name:'Pallas-Sammaltunturi',lon:24.12,lat:67.97,alt:560.,yrange:yrange}
     'pdm': statrec={id:statid,name:'Pic du Midi',lon:0.1411,lat:42.9372,alt:2877.,yrange:yrange}
     'prs': statrec={id:statid,name:'Plateau Rosa',lon:7.7,lat:45.93,alt:3480.,yrange:yrange}
     'psa': statrec={id:statid,name:'Palmer Station',lon:-64.0,lat:-64.92,alt:10.,yrange:yrange}
     'pta': statrec={id:statid,name:'Point Arena',lon:-123.72,lat:38.95,alt:17.,yrange:yrange}
     'puy': statrec={id:statid,name:'Puy de Dome',lon:2.9658,lat:45.7719,alt:1465.,yrange:yrange}
     'nwr': statrec={id:statid,name:'Niwot Ridge',lon:-105.586,lat:40.053,alt:3523.,yrange:yrange}
     'rpb': statrec={id:statid,name:'Ragged Point',lon:-59.43,lat:13.17,alt:45.,yrange:yrange}
     'shm': statrec={id:statid,name:'Shemya Island',lon:174.08,lat:52.72,alt:40.,yrange:yrange}
     'sis': statrec={id:statid,name:'Shetland Lerwick',lon:-1.275,lat:59.854,alt:84.,yrange:yrange}
     'sey': statrec={id:statid,name:'Mahe Island',lon:55.17,lat:-4.67,alt:0.,yrange:yrange} ;'Seychelles'
     'sgp': statrec={id:statid,name:'Southern Great Plains',lon:-97.5,lat:36.78,alt:314.,yrange:yrange}
     'smo': statrec={id:statid,name:'Samoa',lon:0.,lat:0.,alt:0.,yrange:yrange}
     'ssl': statrec={id:statid,name:'Schauinsland',lon:7.92,lat:47.92,alt:1205.,yrange:yrange}
     'sum': statrec={id:statid,name:'Summit',lon:-38.48,lat:72.58,alt:3238,yrange:yrange}
     'syo': statrec={id:statid,name:'Syowa Station',lon:39.58,lat:-69.0,alt:16.,yrange:yrange}
     'tap': statrec={id:statid,name:'Tae-ahn Peninsula',lon:126.12,lat:36.72,alt:20.,yrange:yrange}
     'ter': statrec={id:statid,name:'Teriberka',lon:35.1,lat:69.2,alt:40.,yrange:yrange}
     'thd': statrec={id:statid,name:'Trinidad Head',lon:-124.15,lat:41.05,alt:120.,yrange:yrange}
     'tdf': statrec={id:statid,name:'Tierra del Fuego',lon:-68.48,lat:-54.87,alt:20.,yrange:yrange}
     'tkb': statrec={id:statid,name:'Tsukuba',lon:140.13,lat:36.05,alt:26.,yrange:yrange}
     'uum': statrec={id:statid,name:'Ulaan Uul',lon:111.08,lat:44.45,alt:914.,yrange:yrange}
     'uta': statrec={id:statid,name:'Wendover',lon:-113.72,lat:39.88,alt:1320.,yrange:yrange}
     'wis': statrec={id:statid,name:'Sede Boker',lon:34.87,lat:31.12,alt:400.,yrange:yrange}
     'wkt': statrec={id:statid,name:'Moody',lon:-97.32,lat:31.32,alt:708.,yrange:yrange}
     'wlg': statrec={id:statid,name:'Mt. Waliguan',lon:100.9,lat:36.28,alt:3810.,yrange:yrange}
     'wsa': statrec={id:statid,name:'Sable Island',lon:-60.02,lat:43.93,alt:5.,yrange:yrange}
     'yon': statrec={id:statid,name:'Yonaginijima',lon:0.,lat:0.,alt:0.,yrange:yrange}
     'zgt': statrec={id:statid,name:'Zingst',lon:12.73,lat:54.43,alt:1.,yrange:yrange}
     'zsf': statrec={id:statid,name:'Zugspitze / Schneefernerhaus',lon:10.98,lat:47.42,alt:2656.,yrange:yrange}
     'zep': statrec={id:statid,name:'Zeppelinfjellet',lon:11.88,lat:78.9,alt:45.,yrange:yrange}
     ELSE: BEGIN
        message,'station '+statid+' not found',/cont
        RETURN,''
     END

  ENDCASE

  ;; IF statid eq 'chr' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'gmi' THEN yrange = [1600.,1850.]
  ;; IF statid eq 'cgo' THEN yrange = [1600.,1850.]
  ;; IF statid eq 'cpt' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'smo' THEN yrange = [1600.,1850.]
  ;; IF statid eq 'sey' THEN yrange = [1600.,1850.]
  ;; IF statid eq 'nwr' THEN yrange = [1650.,1900.]
  ;; IF statid eq 'alt ' THEN yrange = [1750.,1950.]
  ;; IF statid eq 'hba' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'syo' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'ams' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'maa' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'mlo' THEN yrange = [1650.,1900.]
  ;; IF statid eq 'asc' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'psa' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'nmb' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'crz' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'tdf' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'mqa' THEN yrange = [1600.,1800.]
  ;; IF statid eq 'rpb' THEN yrange = [1650.,1900.]
  ;; IF statid eq 'izo' THEN yrange = [1700.,1950.]
  
  statrec.yrange = yrange

  return,statrec

END
