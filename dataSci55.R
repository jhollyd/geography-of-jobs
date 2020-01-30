#dataSci55.R
#geography-of-jobs
#J.H.DeBlois
#30 January 2020
#Load, join, get 2 plots
# look at population

#install.packages("ggplot2", "dplyr", "tidyr")
  
library("ggplot2")
library("dplyr","tidyr")

theme_set(theme_bw(base_size=12) + 
            theme(panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank()))

#cities data with lat long population
cit1<-read.csv(file="uscities.csv", header=TRUE, sep=",")
dff<-as_tibble(cit1)
print("head of cit1 as dff1:")
print(head(dff))

#job listing data - 100,000 records at a time
job1 <- read.csv(file="j100000.csv", header=TRUE, sep = ',')
job2 <- read.csv(file="jsecond100000.csv", header=FALSE, sep=',', col.names=c("dataset_id","listing_id","domain","as_of_date","title","url","brand","category", "locality","region","country","number_of_openings","date_added","date_updated","posted_date","location_string","description","entity_id","city_lat","city_lng","cusip","isin"))
job3 <- read.csv(file="jfourth100000.csv", header=FALSE, sep=",", col.names=c("dataset_id","listing_id","domain","as_of_date","title","url","brand","category", "locality","region","country","number_of_openings","date_added","date_updated","posted_date","location_string","description","entity_id","city_lat","city_lng","cusip","isin"))

#make tibbles
df1 <- as_tibble(job1)
print("head of df1, the tibble:")
print(head(df))
print("nrows(df1):")
n1<-nrow(df1)
print(n1)
df2<-as_tibble(job2)
print("head of df2:")
print(head(df2))
print("nrows(df2):")
n2<-nrow(df2)
print(n2)
df3<-as_tibble(job3)

#look at column names
print("colnames of dff, the cities:")
c3<-colnames(dff)
print(c3)
print("colnames of df1, df2:")
c1<-colnames(df1)
print(c1)
c2<-colnames(df2)
print(c2)

#use 300,000 dataset size
df<-rbind(df1,df2,df3)
#jfourth100000 is df3, j100000 is df1
#or USE just one: df2 is jsecond100000
#df<-df2

#select city columns: city state_id and population (all USA)
print("select 6 cols and print head of dff4, cities:")
dff4<-dff[,c("city","state_id","state_name","population","lat","lng")]
print(head(dff4))
#UNNECESSARY sort by state_id and city
#print("sort cities by state_id and city and print head dff7:")
#dff7<-dff4[with(dff4,order(state_id,city)),]
#print(head(dff7))
dff7<-dff4

#select jobs columns dataset_id(1),domain(3), as_of_date(4), 
#title(5), locality(9), region(10), country(11), posted_date?
print("select 8 cols and print head of df4, job listings:")
df4<-df[,c("dataset_id","domain","as_of_date","title","locality","region","country","posted_date")]
print(head(df4))
#sort or change to use arrange(domain, desc(domain))
#UNNECESSARY print("sort by country,region,locality and print head df7:")
#df7<-df4[with(df4,order(country,region,locality)),]
#print(head(df7))
df7<-df4

#run distinct values tests
#  jobs dataset_id, 151 in jsecond
ds<-df4[,c("dataset_id")]
ds_usa<-subset(ds,country="USA") #get number in USA
distinct_ds<-distinct(ds)
distinct_ds_usa<-distinct(ds_usa)
print("# distinct dataset_id in df: all and USA only:")
print(distinct_ds)
print(distinct_ds_usa)

#run distinct test
#  jobs domains, 143 domains in jsecond (same for all, USA)
#    =companies with job to list
dom<-df4[,c("domain")]
dom_usa<-subset(dom,country="USA")
distinct_dom<-distinct(dom)
distinct_dom_usa<-distinct(dom_usa)
print("# distinct domain in df: all and USA only:")
print(distinct_dom)
print(distinct_dom_usa)

#run distinct test
#   jobs title, 58,942 titles in all countries
#      these are distinct job titles of which there are multiples
ti<-df4[,c("title")]
distinct_ti<-distinct(ti)
print("# distinct titles in df: all:")
print(distinct_ti)
print("# titles in df: all:")
print(count(ti))

#****check dataset_id vs domain and make plot1 (near EOF)
#   this is plot of number of jobs listed by company all time
ds_dom1<-df4[,c("dataset_id","domain")] #amazon 6 digits, kroger...
dsdompd1<-df4[,c("dataset_id","domain","posted_date")]
ds_dom2<-arrange(ds_dom1,desc(dataset_id)) #90541 5 digits jobs.sears.com
dsdompd2<-arrange(dsdompd1,posted_date)
ds_dom3<- ds_dom2 %>% group_by(dataset_id) #same print out
dsdompd3<-dsdompd2 %>% group_by(dataset_id,posted_date)
ds_dom4<- ds_dom3 %>% summarise(domain_count=n(),.group_by=TRUE)
dsdompd4<-dsdompd3 %>% summarise(jobs_listed=n())
print("************here is ds_dom's dataset_id with count=n(domain), not distinct:")
print("ds_dom1 basic data:")
print(ds_dom1)
print("ds_dom2, sorted:")
print(ds_dom2)
print("ds_dom3, grouped:")
print(ds_dom3)
print("ds_dom4: jsecond shows 151 dataset_ids, count of job listings=domains per ds_id:")
print("Here is dataset_ids grouping summarised by domain_count (#jobs listed by company):")
print(ds_dom4)
print("Here is by posted_date:")
print("dsdompd1 basic data:")
print(dsdompd1)
print("dsdompd2 sorted:")
print(dsdompd2)
print("dsdompd4 summarised as jobs count by period, by company:")
print(dsdompd4)
#check dataset_id, domain, posted_date and plot it

#run more distinct tests
#  jobs countries, 115 countries in jsecond
#    Note: but we know from above that there are no dataset_ids for
#    companies in other countries; only USA companies
#    so the country is for when a USA company expands there
df8<-df7[,c("country")]
distinct_country<-distinct(df8)
print("number of distinct values of country in jobs:")
print(distinct_country)

#run count test
#   jobs countries=USA, jsecond: found 160,813 out of 199,999 are job listings in USA
#      subset job listings by country=USA; 220619 out of 300000
#      using uscities means no lat/long for other countries yet
df9<-subset(df7,country=="USA")
count<-count(df9)
print("here is the count of rows for jobs in country=usa:")
print(count)
print("Here is df9 usa:")
print(df9)

#run more count tests
#   jobs titles with word 'software'
#   jobs titles with word 'data'
df9b<-filter(df9,grepl('software',title)) #nice!
df9c<-filter(df9,grepl('data',title))
#df9b<-select(df9,dataset_id,domain,as_of_date,title(contains("software"),locality,region,country)) #or select
print("here is the count of rows for jobs in usa w/ title has software:")
count2<-count(df9b)
print(count2)
count3<-count(df9c)
print("Here is count for 'data' in title:")
print(count3)
print("Here is df9b dataset software in titles:")
print(df9b)
print("Here is df9c dataset with software and data in titles:")
print(df9c)
df9c<-rbind(df9b,df9c)
count4<-count(df9c)
print("Here is the count for 'data' or 'software' in title:")
print(count4)
print("Here is rbind of dataset for both:")
print(df9c)

#test more distinct
#  jobs regions, found 2,477 distinct "region" entries AR, Arizona,Altoona,Arkansas
#  distinct regions (states) in jobs with country="USA"
#UNNECESSARY df10<-df9[,c("region")]
#distinct_region_inUSA<-distinct(df10)
#print("# distinct values of region in job for country=usa:")
#print(distinct_region_inUSA)

#test more distinct
#  jobs locality, found 6,968 distinct "locality" in jobs for country=USA
#  and 19,387 city names in uscities data
df11<-df9[,c("locality")]  #df9 is country=USA, selected cols
distinct_locality_inUSA<-distinct(df11)
print("# distinct values of locality in job for country=USA:")
print(distinct_locality_inUSA)

#test more distinct
#   cities state_id, found 52 state_id in uscities
#UNNECESSARY dff8<-dff7[,c("state_id")]
#distinct_stateid<-distinct(dff8)
#print("# distinct values of state_id in uscities:")
#print(distinct_stateid)
#   cities state_name, found 52 state_name in uscities
#UNNECESSARY dff9<-dff7[,c("state_name")]
#distinct_statename<-distinct(dff9)
#print("# distinct values of state_name in uscities:")
#print(distinct_statename)

#test more distinct
#  cities city, found 19,387 values of city in uscities
dff10<-dff7[,c("city")]
distinct_city<-distinct(dff10)
print("# distinct values of city in uscities:")
print(distinct_city)

#test more distinct
#  cities population, with 10 that are over 4 million, hi=19 million
#     lo is 0,1,2,3,4,5...
dff20<-dff7[,c("population")]
dff21<-arrange(dff20,desc(population))
dff22<-arrange(dff20,population)
distinct_pop_city<-distinct(dff21)
distinct_pop_city_lotohi<-distinct(dff22)
print("# distinct (sort desc,asc) entries of population in uscities-- is it there?:")
print(distinct_pop_city)
print(distinct_pop_city_lotohi)

#test more distinct
#  jobs posted_date for now; we know we can do it
df30<-df7[,c("posted_date")] #includes overseas
df31<-arrange(df30,desc(posted_date))
df32<-arrange(df30,posted_date)
df33<-distinct(df31)
df34<-distinct(df32)
count_pd<-count(df32)
print("# distinct sort desc posted_date in jobs:")
print(count_pd)
print("posted_dates, desc:")
print(df33)
print("posted_dates, asc:")
print(df34)

#Using just locality (city) does run into some glitches
#  when dataset_id has joblisting in non-USA place
#  and lists only region and country,  use domain to see?? 
#workingatbooking.com domain has "" locality 
#          and region country of Jeju-do,KO  RETHINK!!
#Got 92649 seattle 9295  is amazon
#868988 "" 1483  nothing in locality??  weird #, jobid
#92972 New York  842   also a jobid in vi job.csv
# 92585 ""
# 92972 KA 794 MH 549 ??
# 92649 Sunnyvale 439 amazon
#92972 London 401
#
#Got 14,157 rows in the counts of joblistings by company/city
#used df7, df9 is just usa, usa has 19K cities, 151 dataset_ids
#IDEALLY WE WOULD FIND NEW CITIES, just having companies go in!
#do the count first, then left join: use df7
#select dataset_id, domain, locality where domain is for listing
#   many domains are given with same dataset_id
dfbycity1<-df7[,c("dataset_id","domain","locality")]
#group_by dataset_id, locality 
dfbycity2<-dfbycity1 %>% group_by(domain,locality)
#which is employers in cities
#   and then summarise by count using n() of domains or jobs
dfbycity3<-dfbycity2 %>% summarise(count_joblistings_byco_bycity=n())
#   to give count of jobs listed in a city for an employer
#Keeping all dates together for now, so we don't infer 
#anything from the number over time, just the total by city/co.
dfbycity4<-dfbycity3 %>% arrange(desc(count_joblistings_byco_bycity))
print("Here is dfbycity4:")
print(dfbycity4)

#rename column 1 city in uscities to "locality" (or vice-versa)
colnames(dff7)[1]<-"locality"

#for use below, get all 19,387 cities with lat, lng
dff11<-dff7[,c("locality","lat","lng","population")]
print("Here is dff11 locality(city), lat, lng (count is 19387 cities:")
print(dff11) #use below in 2nd left join to restore columns

#NEW5 add population?
#NEW6 in ds48 used group_by(dataset_id, locality), not just locality
#NEW6 sort hicount to smallcount so graphics...?
df21<-df9 %>% group_by(locality) %>% summarise(count_dom=n()) %>% arrange(desc(count_dom),.by_group=TRUE)
print("NEW6 group_by 2 or not: Next df21 compute the count of companies(domains) in each city:")
print(df21)
#df9 %>% select(title(contains("software))) #do above via subset
df22<-df9c %>% group_by(locality) %>% summarise(count_ti=n()) %>% arrange(desc(count_ti),.by_group=TRUE)
print("NEW6 group_by 2: Also df22 compute the count of distinct joblistings(title) in each city:")
print(df22)

#NEW5 now use df23, df24, below used df14, df15 to get df17, df18
df23<-dff11 %>% left_join(df21,.,by="locality") #distinct companies
df24<-dff11 %>% left_join(df22,.,by="locality") #distinct jobs
print("NEW5 Here left join is df23 with count of domains (companies) by city:")
print(df23)
print("NEW7: population added, Here is left join is df24 with count of titles (job listings) by city:")
print(df24)

#plot1
print("Create plot1:")
p<-ggplot(ds_dom4, x=dataset_id, y=domain_count) +
  geom_point(aes(x=dataset_id, y=domain_count, color=factor(domain_count))) +
  labs(title="p1: JOB LISTINGS: color shows increasing #",subtitle="credits: Thinknum job listings data",y="total # domains listed per company", x="company dataset_id (5-digit,6-digit)") +
  theme(legend.position="none")
print("type p to see plot1")
ggsave("Rplot_ds55_1_dataset_id_by_domain_count_with_count_colored.png")
#ggsave("Rplot1_ds50.pdf")
print("plot1 saved, type p to see it.")

#plot1b - developed to examine feas. posted_date time periods
#pp<-ggplot(dsdompd4, x=dataset_id, y= posted_date) +
# geom_point(aes(x=dataset_id,y=posted_date,color=factor(jobs_listed))) +
#  labs(y="job listing posted_date",x="job listing dataset_id (company)") +
#  theme(legend.position="none") #nowork:c(".5,.5"))
#print("type pp to see plot1b")
#ggsave("Rplot_ds54_1b.png")
#print("plot1b saved")

#plot2
#print("Create plot2 next: x=country, y=domain(company hiring)")
print("Create plot2: x=lng, y=lat, AND count_bycity_domain or title:")
p2<-ggplot(data=df23,x=lng,y=lat) + #was df21,wasdf20, was df12, now use df17 dom
  #               df18 title or with group_by df19, df20
  #geom_point(mapping=aes(as_of_date, region)) #x,y HUGE cool, ontitle
  #below was df21, df20, df21 has no distinct in the count, df21 uses domain
  geom_point(data=df23,mapping=aes(x=lng,y=lat,color=factor(count_dom),shape=".")) +
  theme(legend.position="none") +
  geom_point(data=df24,mapping=aes(x=lng,y=lat,shape=".")) +
  labs(title="p2: GEO of JOBS: black is 'software' and 'data' jobs",subtitle="credits: Thinknum job listings; geo: uscities database",x="longitude of job listing locality",y="latitide of job listing locality")
  #x is longitude, y is the lat so write it: lat by long
ggsave("Rplot_ds55_2_lat_by_long_color_count_by_citydom.png") #, dpi=300, dev='png', height=4, width=5, units="in")
#ggsave("Rplot2_ds50.pdf")
print("plot2 saved, type p2 to see plot - 300,000 pts with:count_bycity_domain")
 
#plot3 - developed to use as overlay for plot2
#print("Create plot3")
#p3<-ggplot(data=df24,x=lng,y=lat) +
#  geom_point(mapping=aes(x=lng,y=lat,color=factor(count_ti),shape=".")) +
#  theme(legend.position="bot")
#ggsave("Rplot_ds54_3.png")
#ggsave("Rplot3_ds50.pdf")
#print("plot3 saved, type p3 to see it")