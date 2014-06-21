#!/usr/bin/python

import sys
import csv

from HTMLParser import HTMLParser

restaurants = []


class Restaurant(object):
    ATTRS = ['name', 'address', 'district', 'neighborhood',
             'postal_code', 'locality', 'region', 'country_name',
             'telephone', 'fax', 'email', 'latitude', 'longitude']

    def __init__(self):
        self.name = []
        self.street_name = []
        self.streetnumber = "N/A"
        self.latitude = None
        self.longitude = None
        self.district = None
        self.neighborhood = None
        self.postal_code = None
        self.locality = None
        self.region = None
        self.country_name = None
        self.telephone = None
        self.fax = None
        self.email = None

    def clean(self):
        # Better ask for forgiveness than permission
        try:
            self.name = ' '.join(self.name).split(' *')[0]
        except TypeError:
            pass

        try:
            self.street_name = ' '.join(self.street_name)
        except TypeError:
            pass

        self.streetnumber = self.streetnumber.replace("*", "-")
        self.latitude = float(self.latitude)
        self.longitude = float(self.longitude)

    @property
    def address(self):
        return "%s, %s" % (self.street_name, self.streetnumber)

    def attrs(self):
        return (getattr(self, attr) for attr in self.ATTRS)

    def load(self, attrs):
        for name, value in zip(self.ATTRS, attrs):
            setattr(self, name, value)


class MHTMLParser(HTMLParser):
    DIRECTLY_SET = ["xv:streetnumber", "xv:district", "xv:neighborhood", "v:postal-code",
                    "v:locality", "v:region", "v:country-name", "v:latitude", "v:longitude"]

    def __init__(self):
        HTMLParser.__init__(self)
        self.ctag = None
        self.crest = None
        self.elems = [None]
        self.tmp_tel = None

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        celem = self.elems[-1]
        self.ctag = tag

        if tag == 'v:vcard':
            self.crest = Restaurant()
        elif self.ctag in ['v:tel', 'v:email']:
            self.elems.append(self.ctag)
        elif self.ctag == 'rdf:type' and celem == 'v:tel':
            resource = attrs['rdf:resource']
            if resource.endswith("Work"):
                self.crest.telephone = self.tmp_tel
            elif resource.endswith("Fax"):
                self.crest.fax = self.tmp_tel
        elif self.ctag == 'rdf:description' and celem == 'v:email':
            self.crest.email = attrs['rdf:about'][7:]

    def handle_endtag(self, tag):
        self.ctag = ""

        if tag == 'v:vcard':
            self.crest.clean()
            restaurants.append(self.crest)
        elif tag in ['v:tel', 'v:email']:
            self.elems.pop()

    def handle_data(self, data):
        if self.ctag == 'v:fn':
            self.crest.name.append(data)
        elif self.ctag == 'xv:streetname':
            self.crest.street_name.append(data)
        elif self.ctag == "rdf:value" and self.elems[-1] == "v:tel":
            self.tmp_tel = data
        elif self.ctag in self.DIRECTLY_SET:
            setattr(self.crest, self.ctag.split(':')[1].replace('-', '_'), data)


class ProgressBar(object):
    def __init__(self, message, total):
        self.message = message
        self.total = total - 1
        self.percentil = total / 100

        if self.percentil == 0:
            self.percentil = 1

    def update(self, current):
        if current % self.percentil == 0 or current == self.total:
            progress = int((100 * current) / float(self.total))
            step = progress / 10
            sys.stdout.write('\r{0}... [{1}{2}] {3}%'.format(self.message, '#'*step, ' '*(10-step), progress))
            sys.stdout.flush()


if __name__ == "__main__":
    parser = MHTMLParser()

    with open('restaurants.rdf', 'rb') as f:
        # To show some progress
        progress = ProgressBar("Parsing restaurants.rdf", sum(1 for line in f))

        # Go to the beginning
        f.seek(0)

        # Parsing
        for i, line in enumerate(f):
            progress.update(i)
            parser.feed(line)

    print

    with open('restaurants.csv', 'w') as f:
        progress = ProgressBar("Writing restaurants.csv", len(restaurants))

        writer = csv.writer(f, delimiter='\t')
        writer.writerow(Restaurant.ATTRS)

        for i, restaurant in enumerate(restaurants):
            progress.update(i)
            writer.writerow(list(restaurant.attrs()))

    print
    print "Done."
