recode_FIPS<-function(x){
  #### THESE ARE FIPSCODE-FYS IN AFCARS NOT MATCHED IN SEER POP
  ### weld county CO: FIPSCODE 8123, enters pop data in 2002, 08914 prior
  x$FIPSCODE<-ifelse((x$FIPSCODE==8123)&(x$FY<2002), 
                     8914, x$FIPSCODE)
  ### Boulder county CO: FIPSCODE 8013, enters pop data in 2002, 08912 prior
  x$FIPSCODE<-ifelse((x$FIPSCODE==8013)&(x$FY<2002), 
                     8912, x$FIPSCODE)
  ### Adams county CO: FIPSCODE 8001, enters pop data in 2002, 8911 prior
  x$FIPSCODE<-ifelse((x$FIPSCODE==8001)&(x$FY<2002), 
                     8911, x$FIPSCODE)
  ### Jefferson county CO: FIPSCODE 8059, enteres pop data in 2002, 8913 prior
  x$FIPSCODE<-ifelse((x$FIPSCODE==8059)&(x$FY<2002), 
                     8913, x$FIPSCODE)
  ### Bedford city, bedford county, VA: FIPSCODE 51515, 51019, mapped to 51917
  x$FIPSCODE<-ifelse((x$FIPSCODE==51515)|(x$FIPSCODE==51019), 
                     51917, x$FIPSCODE)
  ### Clifton Forge, VA: FIPSCODE 51560 not included in SEER, mapped to 51005
  x$FIPSCODE<-ifelse(x$FIPSCODE==51560, 
                     51005, x$FIPSCODE)
  ### Oglala Lakota County, SD: FIPSCODE 46102, not remapped in SEER, mapped to 46113
  x$FIPSCODE<-ifelse(x$FIPSCODE==46102, 
                     46113, x$FIPSCODE)
  
  ### Wrangell Petersburg Census Area, AK: FIPSCODE 02280, mapped in SEER after 2013 to 2275
  x$FIPSCODE<-ifelse((x$FIPSCODE==2280)&(x$FY>2013), 
                     2275, x$FIPSCODE)
  ### Prince of Wales Census Area, AK: FIPSCODE 02201, mapped in SEER after 2013 to 2130
  x$FIPSCODE<-ifelse((x$FIPSCODE==2201)&(x$FY>2013), 
                     2130, x$FIPSCODE)
  ### Kusilvak census area, AK: FIPSCODE 2158, not mapped in SEER, recode to 2270
  x$FIPSCODE<-ifelse(x$FIPSCODE==2158, 
                     2270, x$FIPSCODE)
  return(x)
}