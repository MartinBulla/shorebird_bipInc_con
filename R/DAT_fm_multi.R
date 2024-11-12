# load packages
  require(data.table)
# function
  getime = function (x) {ifelse(is.na(x), as.numeric(NA), as.numeric(difftime(x, trunc(x,"day"), units = "hours")))}
# set R-system time to UTC 
  Sys.setenv(TZ="UTC")

# load extracted data
  load(here::here("Data/Bulla_et_al_2016-comparative_all.RData"))
  d = data.table(d)
  d = d[sex%in%c('m','f')]

# fix missed bouts
    di = d[pk == 15593]
    d[pk == 15593, datetime_off := as.POSIXct('1997-05-09 09:12:09')]
    d[, bout_length := as.numeric(difftime(datetime_off, datetime_on, units = 'hours'))]
    #d[pk == 15593]

    di[, datetime_on := as.POSIXct('1997-05-09 09:15:09')]
    di[, datetime_off := as.POSIXct('1997-05-09 10:08:49')]
    di[, bout_length := as.numeric(difftime(datetime_off, datetime_on, units = 'hours'))]
    di[, sex := 'f']
    di[, pk := 15593.5]

    d = rbind(d,di)
    #d[pk == 15593.5]
    
    di = d[pk == 15766]
    d[pk == 15766, datetime_off := as.POSIXct('1997-05-25 09:05:29')]
    d[, bout_length := as.numeric(difftime(datetime_off, datetime_on, units = 'hours'))]
    #d[pk == 15593]

    di[, datetime_on := as.POSIXct('1997-05-25 09:14:09')]
    di[, datetime_off := as.POSIXct('1997-05-25 09:24:29')]
    di[, bout_length := as.numeric(difftime(datetime_off, datetime_on, units = 'hours'))]
    di[, sex := 'm']
    di[, pk := 15766.5]

    d = rbind(d,di)

    d[pk == 9752, datetime_off:=d[pk == 9753, datetime_off]]
    d[pk == 9752, bout_length:= bout_length+d[pk == 9753, bout_length]]
    d = d[pk != 9753]
    #d[pk == 15766.5]
    d = d[order(pk)]

# fix issue with non-alternating sex
  d = d[!pk == 15799] # fixed for KEPL tuzl 1997 1997-D-76 1  
  d = d[!pk %in%c(15035,15036,15037)] # fixed for KEPL ddll 2007 AC07 1
  d = d[!pk%in%c(14975,14976,14977,14978,14979)] # fixed for KEPL ddll 2006 V2 1

  # other cases
    d[ , sex_next := data.table::shift(sex, type = 'lead'), by = pk_nest]
    #nrow(d[!is.na(sex_next) & sex == sex_next])

    d = d[!(!is.na(sex_next) & sex == sex_next)]
    
    #nrow(d)

    # DONE check
      #d[ , sex_prev := data.table::shift(sex), by = pk_nest]
      #d[ , sex_next := data.table::shift(sex, type = 'lead'), by = pk_nest]
      #nrow(d[!is.na(sex_next) & sex == sex_next])
      #d[ , off_prev := data.table::shift(datetime_off), by = pk_nest]
      #d[ , on_next := data.table::shift(datetime_on, type = 'lead'), by = pk_nest]
      #d[ , pk_next := data.table::shift(pk, type = 'lead'), by = pk_nest]
      
     
      #d[!is.na(sex_next) & sex == sex_next, unique(pk_nest)]
      #d[!is.na(sex_next) & sex == sex_next, .(pk_nest, bout_length, sex, sex_prev, sex_next, off_prev, datetime_on, datetime_off, on_next, pk, pk_next)]
      #d[!is.na(sex_next) & sex == sex_next, .(pk_nest, bout_length, sex, sex_next,  datetime_on, datetime_off, pk)]
      #print(d[!is.na(sex_next) & pk_nest == 'BLGO frie 2013 NA-B319 2', .(pk, pk_nest, bout_length, sex, sex_prev, sex_next, off_prev, datetime_on, datetime_off, on_next)], nrow=1000)
      # some bouts for these nests were removed #d[!is.na(sex_next) & sex == sex_next & pk_nest %in% c('BLGO frie 2013 NA-B319 2','BLOY hafj 2005 101-05A 1','BLOY hafj 2005 114-05A 1','BLOY hafj 2006 114-06B 2','BLOY hafj 2006 115-06B 2','BLOY hafj 2006 118-06A 1','KEPL tuzl 1997 1997-D-37 1','LRPL czrp 2014 LR504_CZ2014 0','SAND zack 2007 S30 1','GRPL kois 2011 KR24 1','KEPL ddll 2006 A16 1','KEPL ddll 2006 A20 1','KEPL ddll 2006 CA3 1','KEPL ddll 2007 AL07 1','KEPL ddll 2007 MA07 1','KEPL ddll 2008 AB08 1','KEPL ddll 2008 CAA08 1')]
                                                                                                                                                                      
# create dataset
  d[ , bout_m := data.table::shift(bout_length, type = 'lead'), by = pk_nest]
  d = d[sex=='f' & !is.na(bout_m)]
  setnames(d, old = 'bout_length', new = 'bout_f')
  d[ ,(c('sex', 'bird_ID', 'sex_next')) := NULL]
  
# end