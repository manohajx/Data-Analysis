#This code reads the raw user and twitter data and generates various features
import pandas as pd
import string
from collections import Counter
import re
import emoji
import webcolors as wc
import datetime

currentDT1 = datetime.datetime.now()
def n_lower_chars(stringx):
    return sum(1 for c in stringx if c.islower())

def n_upper_chars(stringx):
    return sum(1 for c in stringx if c.isupper())

def n_digit_chars(stringx):
    return sum(1 for c in stringx if c.isdigit())

def n_punctuation_chars(stringx):
    return sum(1 for c in stringx if c in string.punctuation)

def n_words(text):
    num_words = text.split()
    return len(num_words);

def n_user_mentions(stringx):
    stringx = re.sub('@\S+', 'THIS_IS_A_MENTION', stringx)
    return (stringx.count('THIS_IS_A_MENTION'))

def n_hashtags(text):
    matched = 0
    for word in text.split():
        matches = re.findall(r'^[#]\w+',word)
        if len(matches)>=1:
            matched = matched + 1
    return matched;

def n_urls(stringx):
        stringx = re.sub('http\S+', 'THIS_IS_A_URL', stringx)
        return (stringx.count('THIS_IS_A_URL'))
    
def n_emoticons(strngx):
    emojis_list = map(lambda x: ''.join(x.split()), emoji.UNICODE_EMOJI.keys())
    r = re.compile('|'.join(re.escape(p) for p in emojis_list))
    x = r.findall(strngx)
    print(strngx)
    return (len(x))

def replace_nonwords(text):
    stringx = re.sub('http\S+', '', text)
    stringx = re.sub('@\S+', '', stringx)
    newtext = " ".join(filter(lambda x:x[0]!='#', stringx.split()))
    return newtext;

def hapax_legomena(text):
    words = []
    for sentence in text:
        words+= sentence.split()
    c = Counter(words)
    sum1 = 0
    for w in words:
        if c[w]==1:
            sum1 = sum1 + 1;
    return sum1

def hapax_dislegomena(text):
    words = []
    for sentence in text:
        words+= sentence.split()
    c = Counter(words)
    sum1 = 0
    for w in words:
        if c[w]==2:
            sum1 = sum1 + 1;
    return sum1

def closest_colour(requested_colour):
    color_list={ '#87ceeb': 'skyblue', '#ffd700': 'gold','#ffc0cb': 'pink'
            , '#ee82ee': 'violet','#7cfc00' :'lawngreen', '#deb887': 'burlywood' ,'#800000': 'maroon'}
    min_colours = {}
    for key, name in color_list.items():
        r_c, g_c, b_c = wc.hex_to_rgb(key)
        rd = (r_c - requested_colour[0]) ** 2
        gd = (g_c - requested_colour[1]) ** 2
        bd = (b_c - requested_colour[2]) ** 2
        min_colours[(rd + gd + bd)] = name
    return min_colours[min(min_colours.keys())]


#Generating Features for user_data
user_data= pd.read_csv(r'user_data.csv',sep=",",index_col='Unnamed: 0')

user_data.loc[user_data['description'].isna(),'description'] = ""
user_data['d_no_of_emoji']=user_data['description'].apply(n_emoticons)
user_data['d_no_of_urls']=user_data['description'].apply(n_urls)
user_data['d_text_filtered']=user_data['description'].apply(replace_nonwords).apply(str.strip)
user_data['d_no_of_char']=user_data['d_text_filtered'].apply(lambda x: len(x.replace(" ","")))
user_data['d_perc_of_lower_char']=user_data['d_text_filtered'].apply(n_lower_chars)/user_data['d_no_of_char'] 
user_data.loc[user_data.d_perc_of_lower_char.isna(),'d_perc_of_lower_char']=0
user_data['d_perc_of_upper_char']=user_data['d_text_filtered'].apply(n_upper_chars)/user_data['d_no_of_char'] 
user_data.loc[user_data.d_perc_of_upper_char.isna(),'d_perc_of_upper_char']=0
user_data['d_perc_of_digits']=user_data['d_text_filtered'].apply(n_digit_chars)/user_data['d_no_of_char']
user_data.loc[user_data.d_perc_of_digits.isna(),'d_perc_of_digits']=0
user_data['d_perc_of_punc']=user_data['d_text_filtered'].apply(n_punctuation_chars)/user_data['d_no_of_char'] 
user_data.loc[user_data.d_perc_of_punc.isna(),'d_perc_of_punc']=0
user_data['d_no_of_words']=user_data['d_text_filtered'].apply(n_words)
user_data.loc[user_data.d_no_of_words.isna(),'d_no_of_words']=0
user_data['d_hapax_legomena']=user_data['d_text_filtered'].apply(hapax_legomena)/user_data['d_no_of_words']
user_data.loc[user_data.d_hapax_legomena.isna(),'d_hapax_legomena']=0
user_data['d_hapax_dislegomena']=user_data['d_text_filtered'].apply(hapax_dislegomena)/user_data['d_no_of_words']
user_data.loc[user_data.d_hapax_dislegomena.isna(),'d_hapax_dislegomena']=0
attr_to_bool=['profile_background_image_url_https','profile_banner_url',
              'profile_image_url_https','url']
user_data.loc[:,attr_to_bool]=user_data.loc[:,attr_to_bool].apply(lambda x: x.isna())
color_attr=['profile_background_color','profile_link_color','profile_sidebar_border_color', 'profile_sidebar_fill_color',
'profile_text_color']
user_data.loc[:,color_attr]=user_data.loc[:,color_attr].apply(lambda x: "#"+x)
user_data.loc[:,color_attr]=user_data.loc[:,color_attr].applymap(wc.hex_to_rgb).applymap(closest_colour)
user_data.to_csv(r'featured_user_data.csv')
print("User Data Done...")






#Generating Features for twitter data
tweet_data= pd.read_csv(r'tweet_data.csv',sep=",",index_col='Unnamed: 0')
tweet_data['no_of_usr_mentions']=tweet_data['no_of_usr_mentions']=tweet_data['text'].apply(n_user_mentions)
tweet_data['no_of_hashtags']=tweet_data['text'].apply(n_hashtags)
tweet_data['no_of_emoji']=tweet_data['text'].apply(n_emoticons)
tweet_data['no_of_urls']=tweet_data['text'].apply(n_urls)
tweet_data['text_filtered']=tweet_data['text'].apply(replace_nonwords).apply(str.strip)
tweet_data['no_of_char']=tweet_data['text_filtered'].apply(lambda x: len(x.replace(" ","")))
tweet_data['perc_of_lower_char']=tweet_data['text_filtered'].apply(n_lower_chars)/tweet_data['no_of_char'] 
tweet_data.loc[tweet_data.perc_of_lower_char.isna(),'perc_of_lower_char']=0
tweet_data['perc_of_upper_char']=tweet_data['text_filtered'].apply(n_upper_chars)/tweet_data['no_of_char'] 
tweet_data.loc[tweet_data.perc_of_upper_char.isna(),'perc_of_upper_char']=0
tweet_data['perc_of_digits']=tweet_data['text_filtered'].apply(n_digit_chars)/tweet_data['no_of_char']
tweet_data.loc[tweet_data.perc_of_digits.isna(),'perc_of_digits']=0
tweet_data['perc_of_punc']=tweet_data['text_filtered'].apply(n_punctuation_chars)/tweet_data['no_of_char'] 
tweet_data.loc[tweet_data.perc_of_punc.isna(),'perc_of_punc']=0
tweet_data['no_of_words']=tweet_data['text_filtered'].apply(n_words)
tweet_data.loc[tweet_data.no_of_words.isna(),'no_of_words']=0
tweet_data['word_to_char_length']=tweet_data['no_of_words']/tweet_data['no_of_char']
tweet_data.loc[tweet_data.word_to_char_length.isna(),'word_to_char_length']=0
tweet_data['hapax_legomena']=tweet_data['text_filtered'].apply(hapax_legomena)/tweet_data['no_of_words']
tweet_data.loc[tweet_data.hapax_legomena.isna(),'hapax_legomena']=0
tweet_data['hapax_dislegomena']=tweet_data['text_filtered'].apply(hapax_dislegomena)/tweet_data['no_of_words']
tweet_data.loc[tweet_data.hapax_dislegomena.isna(),'hapax_dislegomena']=0
tweet_data.to_csv(r'featured_tweet_data.csv')
print("Twitter data done")
currentDT = datetime.datetime.now()
print ( currentDT-currentDT1  )
