from BeautifulSoup import BeautifulSoup
from datetime import datetime
from time import strptime
import urllib2
from urlparse import urljoin
import re
from xml.sax import saxutils
import StringIO
import cPickle
import sys
import os.path
import getopt

import pprint
import time

logging_level = 2

def endsWith(s, suffix):
  return s.rfind(suffix) + len(suffix) == len(s)

def items_to_dict(items, key):
  dict = {}
  for item in items:
    dict[key(item)] = item
  return dict

def merge_dict(old, new):
  result = {}
  for key in old:
    result[key] = old[key]
  for key in new:
    result[key] = new[key]
  return result

def log(level, str):
  if level <= logging_level:
    sys.stdout.write(str + "\n")
  
class BBScraper:
  def __init__(self, url):
    self.url = url
    self.posts = {}
    self.num_items = {}

  def get_url(self, url):
    log(1, "Fetching %s" % (url,))
    f = urllib2.urlopen(url)
    try:
      return f.read()
    finally:
      f.close()

  def fetch_items(self, parent, url, fil, par):
    # Get the items.
    items = self.fetch_items2(parent, url, fil, par)

    # Set up the proper parent/child relationships.
    if parent:
      parent.children = items_to_dict(items, lambda i: i.id())
    for item in items:
      item.parent = parent
    
    return items

  def fetch_items2(self, parent, url, fil, par):
    # Fetch items for this page.
    html = self.get_url(url)
    tags = fil(html)
    items = map(lambda r: par(parent, url, r), tags)

    item_class = "item"
    if len(items) > 0:
      item_class = items[0].__class__.__name__
    self.num_items[item_class] = self.num_items.get(item_class, 0) + len(items)

    # Is there a 'next' page?
    next_url = self.next_page(url, html)
    if next_url:
      log(1, "  Next page: %s" % (next_url,))
      return items + self.fetch_items2(parent, next_url, fil, par)
    else:
      return items

  def next_page(self, base_url, html_str):
    html = BeautifulSoup(html_str)
    next_a = html.find('a', attrs={'id': lambda id: id and endsWith(id, 'nextPage')})
    if (not next_a) or next_a['class'] == 'IBBnavLinkDisabled':
      return None
    else:
      return urljoin(base_url, saxutils.unescape(next_a['href']))

  def scrape(self):
    forums = self.fetch_forum_infos()
    for forum in forums:
      topics = self.fetch_topic_infos(forum, forum.url)
      for topic in topics:
        posts = self.fetch_post_infos(topic, topic.url)
    return forums

  def update(self, reference_forums):
    reference_forums = items_to_dict(reference_forums, lambda f: f.url)
    update_occurred = False
    for forum in self.fetch_forum_infos():
      reference_forum = reference_forums.get(forum.id(), None)
      log(1, "Comparing %s to %s" % (forum, reference_forum))
      if (not reference_forum) or forum.timestamp > reference_forum.timestamp:
        # Need to update this forum.
        if reference_forum:
          log(0, "New post in %s." % (forum,))
        else:
          log(0, "New forum %s." % (forum,))
        reference_topics = {}
        if reference_forum:
          reference_topics = reference_forum.children
        for topic in self.fetch_topic_infos(forum, forum.url):
          reference_topic = reference_topics.get(topic.id(), None)
          if (not reference_topic) or topic.timestamp > reference_topic.timestamp:
            # Update this topic.
            if reference_topic:
              log(0, "New post in %s." % (topic,))
            else:
              log(0, "New topic %s." % (topic,))
            posts = self.fetch_post_infos(topic, topic.url)
            update_occurred = True
            reference_topics[topic.id()] = topic
        forum.children = reference_topics
        for topic in reference_topics.values():
          topic.parent = forum
        reference_forums[forum.id()] = forum
    return (reference_forums.values(), update_occurred)
    
  def forum_info_tags(self, html):
    soup = BeautifulSoup(html)
    for tr in soup.findAll('tr'):
      if tr.find('a', attrs={'id': is_forum_id}):
        yield tr

  def fetch_forum_infos(self,):
    return self.fetch_items(None, self.url, self.forum_info_tags, self.parse_forum_info)

  def parse_forum_info(self, parent, base_url, row):
    # Get forum name
    a = row.find('a', attrs={'id': is_forum_id})
    name = unicode(a.string)
    url = a['href']
    forum = Forum(name, urljoin(base_url, url))
    last_post_date = row.find('a', attrs={'id': is_last_post_id}).contents[0]
    forum.timestamp = parse_date(last_post_date)
    return forum

  def topic_info_tags(self, html):
    soup = BeautifulSoup(html)
    for tr in soup.findAll('tr'):
      if tr.find('a', attrs={'id': is_topic_id}):
        yield tr

  def parse_topic_info(self, forum, base_url, row):
    # Get forum name
    a = row.find('a', attrs={'id': is_topic_id})
    name = unicode(a.string)
    url = a['href']
    topic = Topic(name, urljoin(base_url, url))
    last_post_date = row.find('a', attrs={'id': is_last_post_id}).contents[0]
    topic.timestamp = parse_date(last_post_date)
    return topic

  def fetch_topic_infos(self, forum, url):
    topics = self.fetch_items(forum, url, self.topic_info_tags, self.parse_topic_info)
    return topics

  
  def post_info_tags(self, html):
    soup = BeautifulSoup(html)
    for tr in soup.findAll('span', attrs={'id': is_post_id}):
      yield tr

  def parse_body_tag(self, tag):
    # collect children until we hit a <div>
    body_elements = []
    for e in tag.contents:
      if isinstance(e, unicode) or e.name != 'div':
        body_elements.append(e)
      else:
        break

    # Strip that last 3 <br>s before the <div>.
    body_elements = body_elements[:-3]
    strings = map(lambda e: u'%s' % (e,), body_elements)
    return u''.join(strings)

  TIMESTAMP_RE = re.compile('([0-9]+/[0-9]+/[0-9]+ [0-9]+(?::[0-9]+){1,2} (?:AM|PM)?)',
                            re.MULTILINE)

  def parse_post_info(self, topic, base_url, tag):
    # permalink
    anchor = tag.find('a', attrs={'id': is_anchor_id,
                                  'name': True})
    permalink = urljoin(base_url, "posts.aspx?postID=%s" % (anchor['name'],))

    # date
    date_td = tag.find('td', attrs={'class': 'IBBpostTableCellHeadingSpacer'})
    timestamp = None
    for s in date_td.contents:
      if isinstance(s, unicode):
        match = self.TIMESTAMP_RE.search(s)
        if match:
          timestamp = match.group(1)
          break
    if not timestamp:
      print date_td.contents
    assert timestamp

    # author
    user_anchor = tag.find('a', attrs={'id': lambda id: id and endsWith(id, 'usernameLink')})
    if not user_anchor:
      print tag
    assert user_anchor

    # body
    body_td = tag.find('td', attrs={'class': 'IBBpostTextCellClass'})

    attachment_span = body_td.find('span', attrs={'id': lambda id: id and endsWith(id, 'frmAttach')})
    attachments = []
    if attachment_span:
      attachments = self.parse_attachments(attachment_span)
      attachment_span.extract()

    # Fix relative image URLs (e.g. for emoticons).
    for img in body_td.findAll('img'):
      img['src'] = urljoin(base_url, img['src'])

    body = self.parse_body_tag(body_td)
    post = Post(None, permalink)
    post.timestamp = datetime(*strptime(timestamp, "%m/%d/%Y %H:%M %p")[0:6])
    bodys = StringIO.StringIO()
    bodys.write('<p><b>Posted by: ' + str(user_anchor) + ' to ')
    bodys.write('<a href="' + topic.parent.url + '">' + topic.parent.name + '</a>:')
    bodys.write('<a href="' + topic.url + '">' + topic.name + '</a></b></p>\n<hr>\n')
    bodys.write(apply_fixups(body, FIXUPS))

    if attachments:
      bodys.write("<table><tr>\n")
      for attachment in attachments:
        bodys.write('<td><a href="' + urljoin(base_url, attachment.url) + '">')
        bodys.write('<img src="' + urljoin(base_url, attachment.img) + '">')
        bodys.write(" " + attachment.description + " ")
        bodys.write("(" + attachment.size + ")</td>\n")
      bodys.write("</table>")

    post.body = bodys.getvalue()
    post.author = unicode(user_anchor.string)
    return post

  def fetch_post_infos(self, topic, url):
    posts = self.fetch_items(topic, url, self.post_info_tags, self.parse_post_info)
    return posts

  ATTACHMENT_URL_RE = re.compile(r"(?:href='(.*?))'|(?:openWindow.'(.*?)')")
  ATTACHMENT_SIZE_RE = re.compile('\((.*?)\)')

  def parse_attachments(self, html):
    attachments = []
    imgs = html.findAll('img', attrs={'id': lambda id: id and endsWith(id, 'imgIcon')})
    for img in imgs:
      preview_url = img['src']
      match = self.ATTACHMENT_URL_RE.search(img['onclick'])

      if not match:
        print "NO MATCH: %s" % (img,)
      assert match
      url = match.group(1) or match.group(2)

      a = img.findNext('a', attrs={'id': lambda id: id and endsWith(id, 'hlFileLoc')})

      description = a.contents[0]
      size_match = self.ATTACHMENT_SIZE_RE.search(unicode(a.nextSibling))
      size = size_match.group(1)

      attachment = Attachment(url, description, size, preview_url)
      attachments.append(attachment)
    return attachments
  


class Item:
  def __init__(self, name, url):
    self.name = name
    self.url = url
    self.timestamp = None
    self.parent = None
    self.children = {}

  def id(self):
    return self.url


def is_last_post_id(id):
  return id and endsWith(id, 'LastPost')
  

# ------------------------------
# Forum
# ------------------------------

class Forum(Item):
  def __init__(self, name, url):
    Item.__init__(self, name, url)

  def __str__(self):
    return "{Forum %s %s %s}" % (repr(self.name),
                                 self.url,
                                 self.timestamp)
    
  def __repr__(self):
    return self.__str__()

  def all_posts(self):
    posts = []
    for topic in self.children.values():
      posts = topic.children.values() + posts
    return posts


def is_forum_id(id):
  return id and endsWith(id, 'forumName')

def parse_date(str):
  tuple = strptime(str, "%m/%d/%Y %H:%M %p")
  return datetime(*tuple[0:6])
  


# ------------------------------
# Topic
# ------------------------------

class Topic(Item):
  def __init__(self, name, url):
    Item.__init__(self, name, url)

  def __str__(self):
    return "{Topic %s %s %s}" % (repr(self.name),
                                 self.url,
                                 self.timestamp)
    
  def __repr__(self):
    return self.__str__()
  

def is_topic_id(id):
  return id and endsWith(id, 'hSubject')

# ------------------------------
# Post
# ------------------------------

class Post(Item):
  def __init__(self, name, url):
    Item.__init__(self, name, url)

  def __str__(self):
    return "{Post %s %s %s}" % (repr(self.name),
                                self.url,
                                self.timestamp)
    
  def __repr__(self):
    return self.__str__()

  def to_rss(self):
    s = StringIO.StringIO()
    s.write("  <item>\n")
    s.write("    <title>" + saxutils.escape(self.parent.name) + "</title>\n")
    s.write("    <link>" + saxutils.escape(self.url) + "</link>\n")
    s.write("    <pubDate>")
    s.write(saxutils.escape(self.timestamp.strftime("%d %b %Y %H:%M:%S GMT")))
    s.write("</pubDate>\n")
    s.write("    <author>" + saxutils.escape(str(self.author)) + "</author>\n")
    s.write("    <description>" + saxutils.escape(self.body) + "</description>\n")
    s.write("  </item>\n")
    return s.getvalue()
  

def is_post_id(id):
  return id and endsWith(id, 'postControl')

def is_anchor_id(id):
  return id and endsWith(id, 'hlAnchor')

class Attachment:
  def __init__(self, url, description, size, img):
    self.url = url
    self.description = description
    self.size = size
    self.img = img

  def __str__(self):
    return "{Attachment %s %s (%s)}" % (self.description, self.url, self.size)
    
  def __repr__(self):
    return self.__str__()

def as_rss(forums, num_items=0):
  s = StringIO.StringIO()
  s.write("<rss version='2.0'>\n")
  s.write("  <channel>\n")
  s.write("  <title>DARPA Urban Challenge Forum</title>\n")
  s.write("  <link>http://dtsn.darpa.mil/ibb/categoryindex.aspx?boardID=2</link>\n")
  s.write("  <description>The DARPA Urban Challenge Forum</description>\n")
  s.write("  <language>en-us</language>\n")
  

  posts = []
  for forum in forums:
    posts = forum.all_posts() + posts
  posts.sort(key=lambda p: p.timestamp, reverse=True)
  if num_items > 0:
    posts = posts[0:min(num_items, len(posts))]
  for post in posts:
    rss = post.to_rss()
    s.write(rss)

  s.write("  </channel>\n")
  s.write("</rss>\n")
  return s.getvalue()


FIXUPS = [[r'<strong>Subject.*?topicAnchor.*?</table>', ''],
          [r'<table class="IBBthinlineDark">.*?</table>', r'<hr><br>']]

for i, f in enumerate(FIXUPS):
  pattern, replacement = f
  FIXUPS[i] = [re.compile(pattern, re.MULTILINE | re.DOTALL), replacement]
  

def apply_fixups(html, fixups):
  for pattern, replacement in fixups:
    html = re.sub(pattern, replacement, html)
  return html


class BBScraperApp:
  def backup(self, path):
    (dirname, basename) = os.path.split(path)
    backup_path = os.path.join(dirname, basename + ".bak")
    if os.path.exists(path):
      log(1, "Backing up %s to %s." % (path, backup_path))
      os.rename(path, backup_path)
    
  def run(self, argv):
    optlist, args = getopt.getopt(argv[1:], 'n:')
    print args
    if len(args) != 3:
      self.usage(args)
      sys.exit(1)
    url = args[0]
    db_path = args[1]
    rss_path = args[2]

    num_items = 0
    for (o, v) in optlist:
      if o == '-n':
        num_items = int(v)
      else:
        sys.stderr.write("%s: illegal option '%s'\n" % (argv[0], o,))
        self.usage(argv)

    scraper = BBScraper(url)
    forums = []
    full_scrape = False
    if os.path.exists(db_path):
      log(1, "Loading database %s" % (db_path,))
      db = open(db_path, "rb")
      try:
        forums = cPickle.load(db)
      finally:
        db.close()
    else:
      log(0, "No database %s, performing full scrape." % (db_path,))
      forums = scraper.scrape()
      full_scrape = True

    if not full_scrape:
      log(0, "Checking for updates.")
      (forums, update_occurred) = scraper.update(forums)

    # Save the DB if there were any changes.
    if full_scrape or update_occurred:
      log(1, "Saving database %s." % (db_path,))
      if not full_scrape:
        self.backup(db_path)
      db = open(db_path, "wb")
      try:
        cPickle.dump(forums, db, -1)
      finally:
        db.close()

    # Write the RSS if there were any changes or it doesn't already
    # exist.
    if full_scrape or update_occurred or \
       not os.path.exists(rss_path):
      log(1, "Writing RSS %s." % (rss_path,))
      if not full_scrape:
        try:
          self.backup(rss_path)
        except:
          pass
      rss_str = as_rss(forums, num_items)
      rss = open(rss_path, "wb")
      try:
        rss.write(rss_str.encode('utf-8'))
      finally:
        rss.close()


  def usage(self, args):
    print "Usage: %s [-n num-items] <url> <db path> <rss path>" % (args[0],)
  

    
if __name__ == '__main__':
  app = BBScraperApp()
  app.run(sys.argv)


