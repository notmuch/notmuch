# generate a test input for the 'export' subcommand of the
# git-remote-notmuch helper

from notmuch2 import Database
from time import time
from hashlib import sha1

def hexencode(str):
    output_charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-_@=.,"
    out = ""
    for char in str:
        if not char in output_charset:
            out+= f"%{ord(char):x}"
        else:
            out+= char
    return out

db = Database(config=Database.CONFIG.SEARCH)

count=1
print("export")
mark={}

for msg in db.messages(""):
    mark[msg.messageid]=count
    blob=""
    for tag in msg.tags:
        blob += f"{tag}\n"
    print (f"blob\nmark :{count}");
    print (f"data {len(blob)}\n{blob}")
    count=count+1

print (f"\ncommit refs/heads/master\nmark :{count+1}")
ctime = int(time())
print (f"author Notmuch Test Suite <notmuch@example.com> {ctime} +0000")
print (f"committer Notmuch Test Suite <notmuch@example.com> {ctime} +0000")
print (f"data 8\nignored")

for msg in db.messages(""):
    digest = sha1(msg.messageid.encode('utf8')).hexdigest()
    filename = hexencode(msg.messageid)
    print (f"M 100644 :{mark[msg.messageid]} {digest[0:2]}/{digest[2:4]}/{filename}/tags")

print("\ndone\n")
