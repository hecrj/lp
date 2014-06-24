#!/usr/bin/python
# coding:utf-8

import csv
import ast
import urllib2
import math
from HTMLParser import HTMLParser
from xml.etree import ElementTree
from rdf2csv import Restaurant


class Bicing(object):
    def __init__(self, url='http://wservice.viabicing.cat/getstations.php?v=1'):
        data = urllib2.urlopen(url).read()
        xml = ElementTree.fromstring(data)
        self.stations = [self.Station(station) for station in xml.iter('station')]

    def near(self, place, max_distance=1000):
        station_distances = []

        for station in self.stations:
            dist = distance(place, station)
            if dist <= max_distance:
                station_distances.append((station, dist))

        return sorted(station_distances, key=lambda tup: tup[1])

    def parking(self, place):
        return filter(lambda station: station[0].slots > 0, self.near(place))

    def depart(self, place):
        return filter(lambda station: station[0].bikes > 0, self.near(place))

    class Station(object):
        def __init__(self, xml):
            self.id = int(xml.find('id').text)

            for attr in ['lat', 'long']:
                setattr(self, attr, float(xml.find(attr).text))

            for attr in ['bikes', 'slots']:
                setattr(self, attr, int(xml.find(attr).text))

            self.street = HTMLParser().unescape(xml.find('street').text).encode("utf-8").strip()
            self.streetnumber = xml.find('streetNumber').text or "N/A"
            self.status = xml.find('status').text

        @property
        def address(self):
            return "%s, %s" % (self.street, self.streetnumber)

        @property
        def coords(self):
            return (self.lat, self.long)

        def is_open(self):
            return self.status == 'OPN'


def distance(place1, place2):
    """
    Calculates the distance in meters between two places with coordinates
    Based on http://www.johndcook.com/python_longitude_latitude.html
    """
    lat1, long1 = place1.coords
    lat2, long2 = place2.coords

    degrees_to_radians = math.pi/180.0

    phi1 = (90.0 - lat1)*degrees_to_radians
    phi2 = (90.0 - lat2)*degrees_to_radians

    theta1 = long1*degrees_to_radians
    theta2 = long2*degrees_to_radians

    cos = (math.sin(phi1)*math.sin(phi2)*math.cos(theta1 - theta2) +
           math.cos(phi1)*math.cos(phi2))
    arc = math.acos(cos)

    return arc * 6373000.0


def match(name, query):
    """
    Tells whether a name satisfies a given query
    """
    if isinstance(query, basestring):
        return query in name

    if isinstance(query, list):
        for subquery in query:
            if match(name, subquery):
                return True
        return False

    if isinstance(query, tuple):
        for subquery in query:
            if not match(name, subquery):
                return False
        return True


def table_cols(row, type="td"):
    print "\n".join(["<%s>%s</%s>" % (type, col, type) for col in row])


# A simple context manager to write fancy HTML
class tag:
    def __init__(self, name):
        self.name = name

    def __enter__(self):
        print "<%s>" % self.name

    def __exit__(self, exc_type, exc_val, exc_tb):
        print "</%s>" % self.name


if __name__ == "__main__":
    import sys

    try:
        # Safe eval
        query = ast.literal_eval(sys.argv[1])
    except IndexError:
        query = ''
    except SyntaxError:
        raise RuntimeError("Invalid query.")

    with open('restaurants.csv', 'r') as f:
        reader = csv.reader(f, delimiter='\t')
        reader.next()
        # Select the restaurants that match the query
        restaurants = [Restaurant.load(row) for row in reader if match(row[0], query)]

    # Load bicing stations
    bicing = Bicing()

    # The output is an HTML table
    print "<!html>"

    with tag("head"):
        print '<meta charset="UTF-8">'

    with tag('body'):
        with tag('table'):
            # Header row
            with tag('tr'):
                table_cols(Restaurant.ATTRS, type="th")
                table_cols(['parking stations', 'departure stations'], type="th")

            # Table contents
            for restaurant in restaurants:
                with tag('tr'):
                    table_cols(restaurant.attrs())

                    with tag('td'):
                        with tag('ol'):
                            for station in bicing.parking(restaurant):
                                print "<li>%s (%.3f m)</li>" % (station[0].address, station[1])

                    with tag('td'):
                        with tag('ol'):
                            for station in bicing.depart(restaurant):
                                print "<li>%s (%f m)</li>" % (station[0].address, station[1])
