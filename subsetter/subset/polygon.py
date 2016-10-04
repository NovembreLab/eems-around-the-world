from shapely.geometry.polygon import Polygon
from shapely.geometry import Point, MultiPoint
import shapely.ops as ops
import numpy as np
from ..load import wrap_america
from .geoloc2 import load_countries
#from mpl_toolkits.basemap import Basemap


def get_polygon(polygon, wrap=True):
    """get_polygon
    gets the boundary polygon tu run eems on
    
    Parameters
    ----------
    polygon : int or str
        if str, file with polygon in eems format
        if set to `0`, get a  convex hull around the sample points
        if set to `1`, use the countries the points are in
        if set to `2`, get continent data
    wrap : bool
        should coordinates in the americas be wrapped s.t they appear in the
        east?
    
    Returns
    -------
    polygon: shapely Polygon object
        the polygon to be used in the eems analysis
    """
    if polygon == 0:
        pass
    elif polygon == 1:
        raise NotImplementedError("country mode nyi")
    elif polygon == 2:
        raise NotImplementedError("continent mode nyi")
    else:
        polygon = np.loadtxt(polygon)
        polygon[:, 1] = wrap_america(polygon[:, 1])
        polygon = polygon[..., ::-1]
        return Polygon(polygon)


def filter_individuals_based_on_location(meta_data, polygon, add_pop=[], wrap=True):
    """filter_individuals
    only retains individuals that are inside the polygon
    
    Parameters
    ----------
    meta_data : pd.data_frame
        data frame with individuals and their location
    polygon : list of (int, int)
        a polygon, output from get_polygon
    
    Returns
    -------
    filtered_meta_data : pd.DataFrmae
        a data frame only with individuals used for analysis
    """
    if add_pop is None: add_pop = []
    if wrap:
        meta_data.longitude = wrap_america(meta_data.longitude)
    create_points(meta_data)
    to_keep = [polygon.contains(pt) for pt in meta_data['POINTS']]
    to_add = [pt in add_pop for pt in meta_data.popId]
    print(sum(to_keep))
    to_keep = np.logical_or(to_keep, to_add)
    print(sum(to_add))
    print(sum(to_keep))
        
    return meta_data[to_keep]


def create_points(meta_data):
    """ adds a point object for meta data """
    meta_data['POINTS'] = [Point(a[1]['longitude'], a[1]['latitude']) for a in
                           meta_data.iterrows()]
    return meta_data


def apply_map_projection(polygon, meta_data, projection):
    m = Basemap(llcrnrlon=-50.,llcrnrlat=-50.,urcrnrlon=340.,urcrnrlat=65.,\
                resolution='h',projection=projection,
                lat_0=40.,lon_0=-20.,lat_ts=20.)
    polygon = ops.transform(m, polygon)
    meta_data.POINTS = [ops.transform(m, p) for p in meta_data.POINTS]
    meta_data.longitude = [p.coords[0][0] for p in meta_data.POINTS]
    meta_data.latitude = [p.coords[0][1] for p in meta_data.POINTS]

    return polygon, meta_data


def get_hull(meta_data):
    pts = MultiPoint(list(meta_data.POINTS))
    return pts.convex_hull


def get_envelope(meta_data):
    pts = MultiPoint(list(meta_data.POINTS))
    return pts.envelope


def get_region_polygon(region, map_file='', rbuffer=0, wrap=True,
                       min_area=0.9):
    if region is None:
        return None

    countries = load_countries(map_file, wrap_americas=wrap)
    print('loaded countries', region)
    eems_region = countries[region]
    polygon = eems_region.get_boundary_polygon(min_area=min_area,
                                               buffer_lvl=rbuffer,
                                               return_type="polygon")
    print('got boundary polygon')
    return polygon


def get_polygon_from_extrema(extrema):
        polygon=[[extrema[0], extrema[2]],
                 [extrema[0], extrema[3]],
                 [extrema[1], extrema[3]],
                 [extrema[1], extrema[2]]]
        return Polygon(polygon)

def _get_subset_area(meta_data, population=None,
                     exclude_pop=None,
                     add_pop=None,
                     exclude_source=None,
                     polygon=None, region=None,
                     extrema=None,
                     convex_hull=False, envelope=False, map_projection=None,
                     _map=None, region_buffer=2, sample_buffer=2,
                     min_area=0.9,
                     wrap=True):
    """_get_subset_area

    The (internal) subset area function. Takes a bunch of options and
    returns a polygon along the specifications
    
    Parameters
    ----------
    meta_data : pd.DataFrame
        Describes the sample locations for all samples,
        in minimum, contains latitude and longitude columns
    population : list or None
        A list of populations to be kept. Is compared to the
            meta_data.POP column (popLabel in PGS framework).
            Samples not having an ID are removed.
            If None, all samples are retained
    exclude_pop : list or None
        A list of populations to be specifically excluded. Is compared to the
            meta_data.POP column (popLabel in PGS framework).
            Samples not having an ID are removed.
            If None, all samples are retained
    add_pop : list or None
        A list of populations to keep no matter what
    polygon : str or None
        A Polygon with samples to be kept.
        if None, this filter is ignored
        if str, file with polygon in eems format
        if set to `0`, get a  convex hull around the sample points
        if set to `1`, use the countries the points are in
        if set to `2`, get continent data
    region : list or None
        A list of regions to be kept. Compared to database. If None,
        filter is ignored
    convex_hull : bool
        should a convex hull be drawn around all retained samples?
    envelope : bool
        should an envelope (= rectangular regions) be kept around
        retained samples?
    map_projections : special
        should the coordinates of samples be
        changed according to some map-projection? See
        plt_toolkits.Basemap for a list of all
            possible projections.
    map : str
        what map file should be loaded. Expects a
        shapefile *.shp/*.shx combo of different countries.
        The default is %s, which was downloaded from
        http://www.naturalearthdata.com/downloads/
    region_buffer : float
       how much space should be added to region (in lat/long) 
    sample_buffer : float
       how much space should be added to samples (in lat/long) 
    wrap : bool
        should the Americas be wrapped?
    
    Returns
    -------
    """


    if population is not None:
        print(meta_data.columns)
        to_keep = [s in population
                   for s in meta_data.popId]
        meta_data = meta_data[to_keep]
        meta_data.index = range(len(meta_data))

    if exclude_pop is not None:
        print("excluding pop")
        print("before:", meta_data.shape)
        to_keep = [s not in exclude_pop
                   for s in meta_data.popId]
        meta_data = meta_data[to_keep]
        meta_data.index = range(len(meta_data))
        print("after: ", meta_data.shape)

    if exclude_source is not None:
        print("excluding source")
        print("before:", meta_data.shape)
        to_keep = [s not in exclude_source
                   for s in meta_data.wasDerivedFrom]
        meta_data = meta_data[to_keep]
        meta_data.index = range(len(meta_data))
        print("after: ", meta_data.shape)

    create_points(meta_data)
    poly1 = None
    if extrema  is not None and extrema is not False:
        poly1 = get_polygon_from_extrema(extrema)
        print("case1")
    elif polygon is not None:
        poly1 = get_polygon(polygon, wrap=True)
        print("case2")

    if region is not None and poly1 is None:
        unbuffered_poly = get_region_polygon(region, _map,
                                   rbuffer=2.2, wrap=wrap, 
                                             min_area=min_area)
        poly1 = get_region_polygon(region, _map,
                                   rbuffer=region_buffer, wrap=wrap,
                                             min_area=min_area)
        meta_data = filter_individuals_based_on_location(meta_data,
                                                         unbuffered_poly,
                                                         add_pop)
        print("case3")

    elif region is not None and poly1 is not None:
        unbuffered_poly = get_region_polygon(region, _map,
                                   rbuffer=2.2, wrap=wrap,
                                             min_area=min_area)
        poly_region = get_region_polygon(region, _map,
                                         rbuffer=region_buffer, wrap=wrap,
                                         min_area=min_area)
        meta_data = filter_individuals_based_on_location(meta_data,
                                                         unbuffered_poly,
                                                         add_pop)
        poly1 = poly_region.intersection(poly1)
        print("case4")
    print("loaded polygons")

    # hulls depend on the data
    if poly1 is not None:
        meta_data = filter_individuals_based_on_location(meta_data, poly1,
                                                         add_pop)
        print("META DATA:", meta_data.shape)
        poly1 = poly1.buffer(region_buffer)

    hull = None
    if convex_hull:
        hull = get_hull(meta_data)
        hull = hull.buffer(sample_buffer)
        if poly1 is None:
            poly1 = hull

    elif envelope:
        hull = get_envelope(meta_data)
        hull = hull.buffer(sample_buffer)
        if poly1 is None:
            poly1 = hull

    if hull is not None:
        poly1 = poly1.intersection(hull)

    if map_projection is not None:
        poly1, meta_data = apply_map_projection(poly1, meta_data, map_projection)

    return poly1, meta_data

def get_subset_area(params, meta_data):
    """get_subset_area
    The main function that creates a Polygon around the samples
    for subsetting.
    
    Parameters
    ----------
    params : Parameters
        a parameters object
    
    Returns
    -------
    polygon : shapely.geometry.Polygon
        a polygon describing the plotting region
    meta_data : pd.DataFrame
        the potentially filtered meta_data object
    """
    return _get_subset_area(meta_data=meta_data, 
                            population=params.population,
                            polygon=params.polygon,
                            region=params.region,
                            convex_hull=params.hull,
                            envelope=params.envelope,
                            map_projection=params.map_projection,
                            _map=params.map,
                            region_buffer=params.region_buffer,
                            sample_buffer=params.sample_buffer,
                            wrap=params.wrap)

def create_polygon_file(polygon, outname, add_outer=True):
    """create_polygon_file
    writes a polygon to outname, for input in eems
    
    Parameters
    ----------
    polygon : shapely Polygon
        the polygon to be written
        
    outname : str
        file name of the polygon
    
    Returns
    -------
    """
    if add_outer: outname += ".outer"
    try:
        points = list(polygon.exterior.coords)
        np.savetxt(outname, points, fmt="%f")
    except:
        points = list(polygon.convex_hull.exterior.coords)
        np.savetxt(outname, points, fmt="%f")
