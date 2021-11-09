# Copyright (c) 2013-2019 Siphon Contributors.
# Distributed under the terms of the BSD 3-Clause License.
# SPDX-License-Identifier: BSD-3-Clause
"""Utility code to support making requests using HTTP."""

from collections import OrderedDict
from datetime import datetime, timedelta, tzinfo
import gzip
from io import BytesIO
from itertools import chain
import posixpath
import warnings
try:
    from urllib.parse import urlencode, urljoin  # noqa
except ImportError:
    from urllib import urlencode
    from urlparse import urljoin  # noqa

import requests

HTTPError = requests.HTTPError


class BadQueryError(Exception):
    """Exception raised when a query fails."""


# A UTC class. Taken from standard library docs.
class UTC(tzinfo):
    """Represent UTC timezone."""

    ZERO = timedelta(0)

    def utcoffset(self, dt):  # pylint:disable=unused-argument
        """Get the offset from UTC."""
        return self.ZERO

    def tzname(self, dt):  # pylint:disable=unused-argument
        """Get the name of the timezone."""
        return r'UTC'

    def dst(self, dt):  # pylint:disable=unused-argument
        """Get whether the timezone uses Daylight Savings Time."""
        return self.ZERO


utc = UTC()


class HTTPSessionManager(object):
    """Manage the creation of sessions for HTTP access."""

    def __init__(self):
        """Initialize ``HTTPSessionManager``."""
        self.user_agent = 'Siphon ({})'.format("__version__")
        self.options = {}

    def set_session_options(self, **kwargs):
        """Set options for created session instances.

        Takes keyword arguments and sets them as attributes on the returned
        :class:`requests.Session` instance.

        See Also
        --------
        create_session

        """
        self.options = kwargs

    def create_session(self):
        """Create a new HTTP session with our user-agent set.

        Returns
        -------
        session : requests.Session
            The created session

        See Also
        --------
        urlopen, set_session_options

        """
        ret = requests.Session()
        ret.headers['User-Agent'] = self.user_agent
        for k, v in self.options.items():
            setattr(ret, k, v)
        return ret

    def urlopen(self, url, decompress=False, **kwargs):
        """GET a file-like object for a URL using HTTP.

        This is a thin wrapper around :meth:`requests.Session.get` that returns a file-like
        object wrapped around the resulting content.

        Parameters
        ----------
        url : str
            The URL to request

        kwargs : arbitrary keyword arguments
            Additional keyword arguments to pass to :meth:`requests.Session.get`.

        Returns
        -------
        fobj : file-like object
            A file-like interface to the content in the response

        See Also
        --------
        :meth:`requests.Session.get`

        """
        fobj = BytesIO(self.create_session().get(url, **kwargs).content)
        if decompress:
            fobj = gzip.GzipFile(fileobj=fobj)
        return fobj


session_manager = HTTPSessionManager()


def parse_iso_date(s):
    """Parse a string containing an ISO-8601 formatted date.

    Parameters
    ----------
    s : str
        The string to be parsed

    Returns
    -------
    dt : datetime.datetime
        The results of parsing the string

    """
    return datetime.strptime(s, '%Y-%m-%dT%H:%M:%SZ').replace(tzinfo=utc)


class DataQuery(object):
    """Represent a query for data from a THREDDS server.

    This object provides a clear API to formulate a query for data, including
    a spatial query, a time query, and possibly some variables or other parameters.
    These objects provide a dictionary-like interface, (:meth:`items` and :meth:`__iter__`)
    sufficient to be passed to functions expecting a dictionary representing a URL query.
    Instances of this object can also be turned into a string, which will yield a
    properly escaped string for a URL.
    """

    def __init__(self):
        """Construct an empty :class:`DataQuery`."""
        self.var = set()
        self.time_query = OrderedDict()
        self.spatial_query = OrderedDict()
        self.extra_params = OrderedDict()

    def variables(self, *var_names):
        """Specify one or more variables for the query.

        This function ensures that variable names are not repeated.

        This modifies the query in-place, but returns `self` so that multiple
        queries can be chained together on one line.

        Parameters
        ----------
        var_names : one or more strings
            One or more names of variables to request. Use 'all' to request all.

        Returns
        -------
        self : DataQuery
            Returns self for chaining calls

        """
        self.var.update(set(var_names))
        return self

    def add_query_parameter(self, **kwargs):
        """Add arbitrary query element (name=value) to the request.

        This modifies the query in-place, but returns `self` so that multiple
        queries can be chained together on one line.

        Parameters
        ----------
        kwargs : one or more strings passed as keyword arguments
            Names and values of parameters to add to the query

        Returns
        -------
        self : DataQuery
            Returns self for chaining calls

        """
        self.extra_params.update(kwargs)
        return self

    def lonlat_box(self, west, east, south, north):
        """Add a latitude/longitude bounding box to the query.

        This adds a request for a spatial bounding box, bounded by ('north', 'south')
        for latitude and ('east', 'west') for the longitude. This modifies the query
        in-place, but returns `self` so that multiple queries can be chained together
        on one line.

        This replaces any existing spatial queries that have been set.

        Parameters
        ----------
        west: float
            The bounding longitude to the west, in degrees east of the prime meridian
        east : float
            The bounding longitude to the east, in degrees east of the prime meridian
        south : float
            The bounding latitude to the south, in degrees north of the equator
        north : float
            The bounding latitude to the north, in degrees north of the equator

        Returns
        -------
        self : DataQuery
            Returns self for chaining calls

        """
        self._set_query(self.spatial_query, west=west, east=east, south=south,
                        north=north)
        return self

    def lonlat_point(self, lon, lat):
        """Add a latitude/longitude point to the query.

        This adds a request for a (`lon`, `lat`) point. This modifies the query
        in-place, but returns `self` so that multiple queries can be chained together on
        one line.

        This replaces any existing spatial queries that have been set.

        Parameters
        ----------
        lon: float
            The longitude to request
        lat : float
            The latitude to request

        Returns
        -------
        self : DataQuery
            Returns self for chaining calls

        """
        self._set_query(self.spatial_query, longitude=lon, latitude=lat)
        return self

    # Helper for resetting a dict
    @staticmethod
    def _set_query(query, **kwargs):
        query.clear()
        query.update(kwargs)

    def all_times(self):
        """Add a request for all times to the query.

        This adds a request for all times (`temporal=all`). This modifies the query
        in-place, but returns `self` so that multiple queries can be chained together on
        one line.

        This replaces any existing temporal queries that have been set.

        Returns
        -------
        self : DataQuery
            Returns self for chaining calls

        """
        self._set_query(self.time_query, temporal='all')
        return self

    def time(self, time):
        """Add a request for a specific time to the query.

        This modifies the query in-place, but returns `self` so that multiple queries
        can be chained together on one line.

        This replaces any existing temporal queries that have been set.

        Parameters
        ----------
        time : datetime.datetime
            The time to request

        Returns
        -------
        self : DataQuery
            Returns self for chaining calls

        """
        self._set_query(self.time_query, time=self._format_time(time))
        return self

    def time_range(self, start, end):
        """Add a request for a time range to the query.

        This modifies the query in-place, but returns `self` so that multiple queries
        can be chained together on one line.

        This replaces any existing temporal queries that have been set.

        Parameters
        ----------
        start : datetime.datetime
            The start of the requested time range
        end : datetime.datetime
            The end of the requested time range

        Returns
        -------
        self : DataQuery
            Returns self for chaining calls

        """
        if start > end:
            warnings.warn('The provided start time comes after the end time. No data will '
                          'be returned.', UserWarning)
        self._set_query(self.time_query, time_start=self._format_time(start),
                        time_end=self._format_time(end))
        return self

    # Helper for formatting times appropriately
    @staticmethod
    def _format_time(dt):
        return dt.isoformat()

    def __iter__(self):
        """Return an iterator of the various items (name=value pairs) that compose the query.

        Returns
        -------
        items : iterator
            Sequence of tuples of name, value representing the query.

        """
        return chain([('var', self.var)], self.time_query.items(),
                     self.spatial_query.items(), self.extra_params.items())

    def items(self):
        """Return the various name=value pairs that compose the query.

        Returns
        -------
        items : iterator
            Sequence of tuples of name, value representing the query.

        """
        return iter(self)

    def __str__(self):
        """Format query as a urlencoded string."""
        return urlencode(self, doseq=True)

    def __repr__(self):
        """Format query as a urlencoded string."""
        return str(self)


class HTTPEndPoint(object):
    def __init__(self, url):
        self._base = url
        self._session = session_manager.create_session()
        self._get_metadata()

    def get_query(self, query):
        url = self._base[:-1] if self._base[-1] == '/' else self._base
        return self.get(url, query)

    def url_path(self, path):
        return posixpath.join(self._base, path)

    def get_path(self, path, query=None):
        return self.get(self.url_path(path), query)

    def get(self, path, params=None):
        resp = self._session.get(path, params=params)
        if resp.status_code != 200:
            if resp.headers.get('Content-Type', '').startswith('text/html'):
                text = resp.reason
            else:
                text = resp.text
            raise requests.HTTPError('Error accessing {0}\n'
                                     'Server Error ({1:d}: {2})'.format(resp.request.url,
                                                                        resp.status_code,
                                                                        text))
        return resp

    def _get_metadata(self):
        """Get the metadata associated with the endpoint.

        It is intended that this be implemented by subclasses as necessary.
        """

    def validate_query(self, query):
        return len(str(query)) > 0  # Ensure not empty

    def query(self):
        return DataQuery()
