import pandas as pd

#pca=pd.read_csv(r'pca.csv',sep=",",index_col='Unnamed: 0')
f_tweet_data= pd.read_csv(r'featured_tweet_data.csv',sep=",",index_col='Unnamed: 0')
t_attr_to_rm=['id', 'text']
f_tweet_data=f_tweet_data.drop(t_attr_to_rm,axis=1)
tweet_data_summary=f_tweet_data.groupby('usr_id',as_index=False).mean()
tweet_data_summary.columns=['id', 'avg_is_quote_status', 'avg_retweeted', 'avg_truncated', 'avg_favorite_count',
       'avg_favorited', 'avg_retweet_count', 'avg_no_of_usr_mentions', 'avg_no_of_hashtags',
       'avg_no_of_emoji', 'avg_no_of_urls', 'avg_no_of_char', 'avg_perc_of_lower_char',
       'avg_perc_of_upper_char', 'avg_perc_of_digits', 'avg_perc_of_punc', 'avg_no_of_words',
       'avg_word_to_char_length', 'avg_hapax_legomena', 'avg_hapax_dislegomena']

#Merging tweet and user Data
f_user_data= pd.read_csv(r'featured_user_data.csv',sep=",",index_col='Unnamed: 0')
twitter_data=pd.merge(tweet_data_summary,f_user_data,how='inner',on='id')
twitter_data.columns



#Removing Column with no variation and unnecessary text column
#print(twitter_data.nunique())
col_to_rm= ['id','name','description','d_text_filtered' ,'screen_name', 'created_at','location','avg_retweeted','avg_favorited',
'follow_request_sent','following','notifications','profile_image_url_https', 'contributors_enabled','location' ]
#print(f_user_data.columns)
twitter_data=twitter_data.drop(col_to_rm,axis=1)
#print(twitter_data.isna().sum())
twitter_data.nunique()
#f_user_data.select_dtypes(['int64','float64']).describe(percentiles=[.25,.5,.75,.90,.95,.99])

#COnverting boolean into 0 and 1
twitter_data.geo_enabled=twitter_data.geo_enabled.astype('int64')
twitter_data.has_extended_profile=twitter_data.has_extended_profile.astype('int64')
twitter_data.profile_background_image_url_https=twitter_data.profile_background_image_url_https.astype('int64')
twitter_data.profile_background_tile=twitter_data.profile_background_tile.astype('int64')
twitter_data.profile_banner_url=twitter_data.profile_banner_url.astype('int64')
twitter_data.profile_use_background_image=twitter_data.profile_use_background_image.astype('int64')
twitter_data.default_profile=twitter_data.default_profile.astype('int64')
twitter_data.default_profile_image=twitter_data.default_profile_image.astype('int64')
twitter_data.verified=twitter_data.verified.astype('int64')
twitter_data.url=twitter_data.url.astype('int64')
twitter_data.is_translation_enabled=twitter_data.is_translation_enabled.astype('int64')
twitter_data.is_translator=twitter_data.is_translator.astype('int64')
twitter_data.gender=(twitter_data.gender=='f').astype('int64')


#Creating Dummy variable
color_attr=['profile_background_color','profile_link_color','profile_sidebar_border_color', 'profile_sidebar_fill_color',
'profile_text_color','translator_type']
d1 = pd.get_dummies(twitter_data['profile_background_color'],prefix='profile_background_color',drop_first=True)
d2 = pd.get_dummies(twitter_data['profile_link_color'],prefix='profile_link_color',drop_first=True)
d3 = pd.get_dummies(twitter_data['profile_sidebar_border_color'],prefix='profile_sidebar_border_color',drop_first=True)
d4 = pd.get_dummies(twitter_data['profile_sidebar_fill_color'],prefix='profile_sidebar_fill_color',drop_first=True)
d5 = pd.get_dummies(twitter_data['profile_text_color'],prefix='profile_text_color',drop_first=True)
d5 = pd.get_dummies(twitter_data['translator_type'],prefix='translator_type',drop_first=True)
twitter_data=pd.concat([twitter_data,d1,d2,d3,d4,d5], axis=1)
twitter_data=twitter_data.drop(color_attr,axis=1)
#twitter_data.head()

twitter_data.columns

twitter_data.to_csv(r'twitter_data_model_duilding.csv')
