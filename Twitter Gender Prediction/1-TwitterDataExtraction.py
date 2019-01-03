#This code extracts the twitter user and tweet object for top 3600 names from a USA census list of the year 2000

import credentials as cr 
import pandas as pd
import tweepy
from tweepy import OAuthHandler
import chardet
import datetime

currentDT1 = datetime.datetime.now()
def Import_Name_Gender_Data(path):    
    #"https://www.ssa.gov/oact/babynames/limits.html"
    """This Fuction downloads popular names with gender dataset(above url) 
    from USA census database and returns a new data frame containing first name and gender"""  
    rawdata = open(path, 'rb').read()
    charenc = chardet.detect(rawdata)['encoding']
    name_gender_data=pd.read_csv(path,sep=",",encoding=charenc,header=None,names=["firstname","gender","count"])
    print(list(name_gender_data.columns.values))
    name_gender_data=name_gender_data.sort_values("count",ascending= False).groupby("gender").head(3600)
    name_gender_data= name_gender_data[["firstname","gender"]]
    #Number of Male and Female names
    print(name_gender_data.groupby("gender").count())
    name_gender_data= name_gender_data.apply(lambda x: x.str.lower())
    name_gender_data=name_gender_data.drop_duplicates("firstname")
    print(name_gender_data.groupby("gender").count())
    return (name_gender_data)


def Create_Twitter_API_Connection(CONSUMER_KEY,CONSUMER_SECRET,ACCESS_KEY,ACCESS_SECRET):
    #This Function create a connection to twitter using tweepy
    auth = OAuthHandler(CONSUMER_KEY,CONSUMER_SECRET)
    api = tweepy.API(auth)
    auth.set_access_token(ACCESS_KEY, ACCESS_SECRET)
    api = tweepy.API(auth,wait_on_rate_limit=True)
    return(api)

def Extract_Twitter_User_Data(api,user_name_list=None,user_id_list=None):
    '''This function Downloads User Object or Tweet Object based on the 
    input passed and converts it into a pandas dataframe'''
    flag=-1
    if(user_name_list is None and user_id_list is None):
        print("No aruguement Passed")
        return -1        
    if(user_name_list is None):
        lookup=user_id_list
        flag=1
    else:
        lookup=user_name_list
        flag=0                
    ext_data= pd.DataFrame()
    count=0
    
    for name in lookup:
        try:
            print(count," ",name)
            if(flag==0):
                results=api.search_users(name,7)
            else:
                results=api.user_timeline(name)
            temp=pd.DataFrame(vars(results[cnt]) for cnt in range(len(results)))
            if not temp.empty:
                ext_data=ext_data.append(temp,ignore_index=True,sort=True)
            count=count+1
        except KeyboardInterrupt: 
            raise
        except:
            pass
    return(ext_data)

def Clean_User_Data(user_data,name_gender_data) :
    #Basic cleaning of the User Object DataFrame
    attribute_list=['id',
    'name',
    'screen_name',
    'created_at',
    'description',
    'geo_enabled',
    'favourites_count',
    'follow_request_sent',
    'followers_count',
    'following',
    'friends_count',
    'has_extended_profile',
    'is_translation_enabled',
    'is_translator',
    'lang',
    'listed_count',
    'location',
    'notifications',
    'profile_background_color',
    'profile_background_image_url_https',
    'profile_background_tile',
    'profile_banner_url',
    'profile_image_url_https',
    'profile_link_color',
    'profile_sidebar_border_color',
    'profile_sidebar_fill_color',
    'profile_text_color',
    'profile_use_background_image',
    'default_profile',
    'default_profile_image',
    'contributors_enabled',
    'protected',
    'statuses_count',
    'verified',
    'translator_type',
    'url']
    user_data=user_data.loc[:,attribute_list]
    user_data['firstname']=user_data.name.str.split().str.get(0).str.lower()
    user_data=user_data[user_data.firstname.isin(name_gender_data.firstname)]
    user_data=user_data[user_data.lang == 'en']
    user_data=user_data[user_data.protected == False ]
    user_data2=pd.merge(user_data, name_gender_data, how='inner', on='firstname')
    user_data2=user_data2.drop(['firstname','protected','lang'],axis=1) 
    user_data2=user_data2.drop_duplicates("id")
    user_data2.to_csv(r'user_data.csv',encoding='utf-8')
    return(user_data2)
    
def Clean_Tweet_Data(tweet_data):
    #Basic cleaning of the Twitter Object DataFrame
    attr_list=['id','text','user','is_quote_status','retweeted','truncated','favorite_count','favorited','retweet_count','lang']
    tweet_data=tweet_data.loc[:,attr_list]
    tweet_data['usr_id'] = pd.DataFrame(vars(tweet_data.user[cnt]) for cnt in range(len(tweet_data)))['id']
    tweet_data=tweet_data[tweet_data.lang=='en']
    tweet_data=tweet_data.drop(['user','lang'],axis=1)
    tweet_data.to_csv(r'tweet_data.csv',encoding='utf-8',sep=",")
    
name_gender_data=Import_Name_Gender_Data(r"name_gender_yr2000.txt")
api=Create_Twitter_API_Connection(cr.CONSUMER_KEY,cr.CONSUMER_SECRET,cr.ACCESS_KEY,cr.ACCESS_SECRET)
user_data=Extract_Twitter_User_Data(api,user_name_list=name_gender_data.firstname)
user_data=Clean_User_Data(user_data,name_gender_data)
tweet_data=Extract_Twitter_User_Data(api,user_id_list=user_data.id)
tweet_data=Clean_Tweet_Data(tweet_data)
currentDT = datetime.datetime.now()
print ( currentDT-currentDT1  )


