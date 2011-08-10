#!/usr/bin/env python

import datetime
import doctest
import re
import sys
import urllib
import xml.sax.saxutils


SHOWS = [
    'Curb Your Enthusiasm', 'Futurama', 'Game of Thrones', 'Top Gear',
    'Dexter', 'South Park', 'IT Crowd, The'
]


def get_shows_airings():
    html_lines = urllib.urlopen('http://eztv.it/showlist/').readlines()
    shows_airings = parse_eztv_html(html_lines)
    return shows_airings


def parse_eztv_html(html_lines):
    """
    >>> parse_eztv_html(["<td class=\\"forum_thread_post\\"><a href=\\"/shows/66/curb-your-enthusiasm/\\" class=\\"thread_link\\">Curb Your Enthusiasm</a></td>", "<td class=\\"forum_thread_post\\"><font class=\\"airing\\">Airing: Sunday</font></td>"])
    {'Curb Your Enthusiasm': 'Sunday'}
    """
    shows = {}
    for line in html_lines:
        match = re.match(r'.*class="thread_link">(.*?)</a>', line)
        if match:
            show_name = match.group(1)
        match = re.match(r'.*class="airing">Airing: (.*?)</font>', line)
        if match and show_name:
            airing_day = match.group(1)
            shows[show_name] = airing_day
    return shows


def weekday_name_to_weekday(weekday_name):
    """
    >>> weekday_name_to_weekday('Sunday')
    6
    >>> weekday_name_to_weekday('Tuesday')
    1
    """
    weekdays_names = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
        'Saturday', 'Sunday']
    if weekday_name in weekdays_names:
        return weekdays_names.index(weekday_name)


def http_response(content_type, body):
    print "Content-Type: " + content_type
    print
    print body


def main():
    body = """<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0">
  <channel>
    <title>What's on TV?</title>
    <description>Keep up with airdates of your favourite shows via RSS</description>
"""
    current_weekday = datetime.date.weekday(datetime.datetime.now())
    shows_airings = get_shows_airings()
    for show, airing in shows_airings.iteritems():
        airing_weekday = weekday_name_to_weekday(airing)
        if show in SHOWS and current_weekday == airing_weekday:
            body += "    <item>\n"
            body += "      <title>"
            body += xml.sax.saxutils.escape(show) + ' airing on ' + airing
            body += "</title>\n"
            body += "    </item>\n"
    body += """  </channel>
</rss>"""

    http_response('text/xml', body)


if __name__ == '__main__':
    doctest.testmod()
    main()

