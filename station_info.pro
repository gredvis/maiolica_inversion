FUNCTION station_info,statid
  
  statrec = {id:'',name:'',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
  
  CASE strlowcase(statid) OF
     'abp': statrec={id:statid,name:'Arembepe',lon:-38.17,lat:-12.77,alt:0.,yrange:[0.,0.]}
     'alt': statrec={id:statid,name:'Alert',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'ams': statrec={id:statid,name:'Amsterdam Island',lon:77.53,lat:-37.8,alt:55.,yrange:[0.,0.]}
     'amt': statrec={id:statid,name:'Argyle',lon:-68.68,lat:45.03,alt:50.,yrange:[0.,0.]}
     'amy': statrec={id:statid,name:'Anmyeon-do',lon:126.32,lat:36.53,alt:47.,yrange:[0.,0.]}
     'asc': statrec={id:statid,name:'Ascension Island',lon:-14.42,lat:-7.92,alt:54.,yrange:[0.,0.]}
     'arh': statrec={id:statid,name:'Arrival Heights',lon:166.67,lat:-77.8,alt:184.,yrange:[0.,0.]}
     'ask': statrec={id:statid,name:'Assekrem',lon:5.63,lat:23.27,alt:2710.,yrange:[0.,0.]}
     'azr': statrec={id:statid,name:'Terceira Island',lon:-27.37,lat:38.77,alt:40.,yrange:[0.,0.]}
     'bal': statrec={id:statid,name:'Baltic Sea',lon:17.22,lat:55.35,alt:28.,yrange:[0.,0.]}
     'bgu': statrec={id:statid,name:'Begur',lon:3.23,lat:41.97,alt:13.,yrange:[0.,0.]}
     'bhd': statrec={id:statid,name:'Baring Head',lon:174.87,lat:-41.41,alt:85.,yrange:[0.,0.]}
     'bkt': statrec={id:statid,name:'Bukit Koto Tabang',lon:100.32,lat:-0.2,alt:864.5,yrange:[0.,0.]}
     'bme': statrec={id:statid,name:"St. David's Head",lon:-64.65,lat:32.37,alt:30.,yrange:[0.,0.]}
     'bmw': statrec={id:statid,name:'Tudor Hill',lon:-64.87,lat:32.27,alt:30.,yrange:[0.,0.]}
     'brw': statrec={id:statid,name:'Barrow',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} 
     'bsc': statrec={id:statid,name:'Black Sea',lon:28.67,lat:44.17,alt:3.,yrange:[0.,0.]}
     'cba': statrec={id:statid,name:'Cold Bay',lon:-162.72,lat:55.2,alt:25.,yrange:[0.,0.]}
     'cdl': statrec={id:statid,name:'Candle Lake',lon:-104.65,lat:53.87,alt:489.,yrange:[0.,0.]}
     'cfa': statrec={id:statid,name:'Cape Ferguson',lon:147.05,lat:-19.28,alt:2.,yrange:[0.,0.]}
     'cgo': statrec={id:statid,name:'Cape Grim',lon:144.68,lat:-40.68,alt:94.,yrange:[0.,0.]}
     'chr': statrec={id:statid,name:'Christmas Island',lon:-157.17,lat:1.7,alt:3.,yrange:[0.,0.]}
     'cpt': statrec={id:statid,name:'Cape Point',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'cri': statrec={id:statid,name:'Cape Rama',lon:73.83,lat:15.08,alt:60.,yrange:[0.,0.]}
     'crz': statrec={id:statid,name:'Crozet',lon:51.85,lat:-46.45,alt:120.,yrange:[0.,0.]}
     'cya': statrec={id:statid,name:'Casey Station',lon:110.53,lat:-66.28,alt:60.,yrange:[0.,0.]}
     'deu': statrec={id:statid,name:'Deuselbach',lon:7.05,lat:49.77,alt:480.,yrange:[0.,0.]}
     'egb': statrec={id:statid,name:'Egbert',lon:-79.78333,lat:44.23,alt:253.,yrange:[0.,0.]}
     'eic': statrec={id:statid,name:'Easter Island',lon:-109.45,lat:-27.13,alt:50.,yrange:[0.,0.]}
     'esp': statrec={id:statid,name:'Estevan Point',lon:-126.55,lat:49.38,alt:39.,yrange:[0.,0.]}
     'etl': statrec={id:statid,name:'East Trout Lake',lon:-104.9834,lat:54.3501,alt:492.,yrange:[0.,0.]}
     'fsd': statrec={id:statid,name:'Fraserdale',lon:-81.57,lat:49.88,alt:210.,yrange:[0.,0.]}
     'gmi': statrec={id:statid,name:'Guam',lon:144.78,lat:13.43,alt:2.,yrange:[0.,0.]}
     'hba': statrec={id:statid,name:'Halley Bay',lon:-26.5,lat:-75.57,alt:33.,yrange:[0.,0.]}
     'hpb': statrec={id:statid,name:'Hohenpeissenberg',lon:11.02,lat:47.8,alt:985.,yrange:[0.,0.]}
     'hun': statrec={id:statid,name:'Hegyhatsal',lon:16.65,lat:46.95,alt:248.,yrange:[0.,0.]}
     'izo': statrec={id:statid,name:'Izana',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'jfj': statrec={id:statid,name:'Jungfraujoch',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]} 
     'key': statrec={id:statid,name:'Key Biscane',lon:-80.2,lat:25.67,alt:3.,yrange:[0.,0.]}
     'kmw': statrec={id:statid,name:'Kollumerwaard',lon:6.28,lat:53.33,alt:0.,yrange:[0.,0.]}
     'kum': statrec={id:statid,name:'Cape Kumukahi',lon:-154.82,lat:19.52,alt:3.,yrange:[0.,0.]}
     'kzd': statrec={id:statid,name:'Sary Taukum',lon:75.57,lat:44.45,alt:412.,yrange:[0.,0.]}   ;Kazakhstan
     'kzm': statrec={id:statid,name:'Plateau Assy',lon:77.87,lat:43.25,alt:2519.,yrange:[0.,0.]} ; Kazakhstan
     'lef': statrec={id:statid,name:'Park Falls',lon:-90.27,lat:45.92,alt:868.,yrange:[0.,0.]}
     'llb': statrec={id:statid,name:'Lac La Biche',lon:-112.45,lat:54.95,alt:540.,yrange:[0.,0.]}
     'lln': statrec={id:statid,name:'Lulin',lon:120.87,lat:23.47,alt:2867.,yrange:[0.,0.]}
     'lmp': statrec={id:statid,name:'Lampedusa',lon:12.63,lat:35.52,alt:45.,yrange:[0.,0.]}
     'lpo': statrec={id:statid,name:'Ile Grande',lon:-3.5839,lat:48.8036,alt:10.,yrange:[0.,0.]}
     'maa': statrec={id:statid,name:'Mawson',lon:62.87,lat:-67.62,alt:32.,yrange:[0.,0.]}
     'mhd': statrec={id:statid,name:'Mace Head',lon:-9.9,lat:53.33,alt:8.,yrange:[0.,0.]}
     'mkn': statrec={id:statid,name:'Mt. Kenya',lon:37.297,lat:-0.062,alt:3678.,yrange:[0.,0.]}
     'mlo': statrec={id:statid,name:'Mauna Loa',lon:-155.578,lat:19.539,alt:3397.,yrange:[0.,0.]}
     'mnm': statrec={id:statid,name:'Minamitorishima',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'mqa': statrec={id:statid,name:'Macquarie Island',lon:158.97,lat:-54.48,alt:12.,yrange:[0.,0.]}
     'ngl': statrec={id:statid,name:'Neuglobsow',lon:13.03,lat:53.17,alt:65.,yrange:[0.,0.]}
     'nmb': statrec={id:statid,name:'Namibia',lon:15.02,lat:-23.57,alt:461.,yrange:[0.,0.]}
     'oxk': statrec={id:statid,name:'Ochsenkopf',lon:11.8,lat:50.03,alt:1185.,yrange:[0.,0.]}
     'pal': statrec={id:statid,name:'Pallas-Sammaltunturi',lon:24.12,lat:67.97,alt:560.,yrange:[0.,0.]}
     'pdm': statrec={id:statid,name:'Pic du Midi',lon:0.1411,lat:42.9372,alt:2877.,yrange:[0.,0.]}
     'prs': statrec={id:statid,name:'Plateau Rosa',lon:7.7,lat:45.93,alt:3480.,yrange:[0.,0.]}
     'psa': statrec={id:statid,name:'Palmer Station',lon:-64.0,lat:-64.92,alt:10.,yrange:[0.,0.]}
     'pta': statrec={id:statid,name:'Point Arena',lon:-123.72,lat:38.95,alt:17.,yrange:[0.,0.]}
     'puy': statrec={id:statid,name:'Puy de Dome',lon:2.9658,lat:45.7719,alt:1465.,yrange:[0.,0.]}
     'nwr': statrec={id:statid,name:'Niwot Ridge',lon:-105.586,lat:40.053,alt:3523.,yrange:[0.,0.]}
     'rpb': statrec={id:statid,name:'Ragged Point',lon:-59.43,lat:13.17,alt:45.,yrange:[0.,0.]}
     'shm': statrec={id:statid,name:'Shemya Island',lon:174.08,lat:52.72,alt:40.,yrange:[0.,0.]}
     'sey': statrec={id:statid,name:'Mahe Island',lon:55.17,lat:-4.67,alt:0.,yrange:[0.,0.]} ;'Seychelles'
     'sgp': statrec={id:statid,name:'Southern Great Plains',lon:-97.5,lat:36.78,alt:314.,yrange:[0.,0.]}
     'smo': statrec={id:statid,name:'Samoa',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'ssl': statrec={id:statid,name:'Schauinsland',lon:7.92,lat:47.92,alt:1205.,yrange:[0.,0.]}
     'sum': statrec={id:statid,name:'Summit',lon:-38.48,lat:72.58,alt:3238,yrange:[0.,0.]}
     'syo': statrec={id:statid,name:'Syowa Station',lon:39.58,lat:-69.0,alt:16.,yrange:[0.,0.]}
     'tap': statrec={id:statid,name:'Tae-ahn Peninsula',lon:126.12,lat:36.72,alt:20.,yrange:[0.,0.]}
     'ter': statrec={id:statid,name:'Teriberka',lon:35.1,lat:69.2,alt:40.,yrange:[0.,0.]}
     'thd': statrec={id:statid,name:'Trinidad Head',lon:-124.15,lat:41.05,alt:120.,yrange:[0.,0.]}
     'tdf': statrec={id:statid,name:'Tierra del Fuego',lon:-68.48,lat:-54.87,alt:20.,yrange:[0.,0.]}
     'tkb': statrec={id:statid,name:'Tsukuba',lon:140.13,lat:36.05,alt:26.,yrange:[0.,0.]}
     'uum': statrec={id:statid,name:'Ulaan Uul',lon:111.08,lat:44.45,alt:914.,yrange:[0.,0.]}
     'uta': statrec={id:statid,name:'Wendover',lon:-113.72,lat:39.88,alt:1320.,yrange:[0.,0.]}
     'wis': statrec={id:statid,name:'Sede Boker',lon:34.87,lat:31.12,alt:400.,yrange:[0.,0.]}
     'wkt': statrec={id:statid,name:'Moody',lon:-97.32,lat:31.32,alt:708.,yrange:[0.,0.]}
     'wlg': statrec={id:statid,name:'Mt. Waliguan',lon:100.9,lat:36.28,alt:3810.,yrange:[0.,0.]}
     'wsa': statrec={id:statid,name:'Sable Island',lon:-60.02,lat:43.93,alt:5.,yrange:[0.,0.]}
     'yon': statrec={id:statid,name:'Yonaginijima',lon:0.,lat:0.,alt:0.,yrange:[0.,0.]}
     'zgt': statrec={id:statid,name:'Zingst',lon:12.73,lat:54.43,alt:1.,yrange:[0.,0.]}
     'zsf': statrec={id:statid,name:'Zugspitze / Schneefernerhaus',lon:10.98,lat:47.42,alt:2656.,yrange:[0.,0.]}
     'zep': statrec={id:statid,name:'Zeppelinfjellet',lon:11.88,lat:78.9,alt:45.,yrange:[0.,0.]}
     ELSE: BEGIN
        message,'station '+statid+' not found',/cont
        RETURN,''
     END

  ENDCASE

  ;; define y-axis range for plotting purposes
  yrange = [1750.,1980.]

  IF statid eq 'chr' THEN yrange = [1600.,1800.]
  IF statid eq 'gmi' THEN yrange = [1600.,1850.]
  IF statid eq 'cgo' THEN yrange = [1600.,1850.]
  IF statid eq 'cpt' THEN yrange = [1600.,1800.]
  IF statid eq 'smo' THEN yrange = [1600.,1850.]
  IF statid eq 'sey' THEN yrange = [1600.,1850.]
  IF statid eq 'nwr' THEN yrange = [1650.,1900.]
  IF statid eq 'alt ' THEN yrange = [1750.,1950.]
  IF statid eq 'hba' THEN yrange = [1600.,1800.]
  IF statid eq 'syo' THEN yrange = [1600.,1800.]
  IF statid eq 'ams' THEN yrange = [1600.,1800.]
  IF statid eq 'maa' THEN yrange = [1600.,1800.]
  IF statid eq 'mlo' THEN yrange = [1650.,1900.]
  IF statid eq 'asc' THEN yrange = [1600.,1800.]
  IF statid eq 'psa' THEN yrange = [1600.,1800.]
  IF statid eq 'nmb' THEN yrange = [1600.,1800.]
  IF statid eq 'crz' THEN yrange = [1600.,1800.]
  IF statid eq 'tdf' THEN yrange = [1600.,1800.]
  IF statid eq 'mqa' THEN yrange = [1600.,1800.]
  IF statid eq 'rpb' THEN yrange = [1650.,1900.]
  IF statid eq 'izo' THEN yrange = [1700.,1950.]
  
  statrec.yrange = yrange

  return,statrec

END
