from twython import Twython
import os

twitter = Twython(os.environ['CONSUMER_KEY'], os.environ['CONSUMER_SECRET'],
                  os.environ['ACCESS_TOKEN'], os.environ['ACCESS_TOKEN_SECRET'])

res = twitter.show_lists(screen_name='gdequeiroz', reverse=True)

res = twitter.get_list_members(list_id=783772667635564544, include_entities=False, skip_status=True, count=100)
screen_names = [e['screen_name'] for e in res['users']]

with open('rladies-chapters.csv', 'w') as f:
    f.write('\n'.join(screen_names))