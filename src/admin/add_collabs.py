#
# usage: cat users.txt | python add_collabs.py
#

import requests
import sys

# set the github repo
repo = '5harad/crowds'

# read and set github credentials
with open('.github-creds') as f:
    user, passwd = f.readline().strip().split()

# load collaborators
collabs = set([line.strip() for line in sys.stdin.readlines()])

# get the current list of collaborators
url = 'https://api.github.com/repos/%s/collaborators' % repo
r = requests.get(url, auth=(user, passwd))
cur_collabs = set([item['login'] for item in r.json()])

# add the additional collaborators
collabs_to_add = collabs - cur_collabs
for c in collabs_to_add:
    print 'adding: %s' % c
    url = 'https://api.github.com/repos/%s/collaborators/%s' % (repo, c)
    requests.put(url, auth=(user, passwd))
