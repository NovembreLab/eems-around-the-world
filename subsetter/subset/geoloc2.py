import fiona
from shapely.geometry import shape, MultiPolygon
from shapely.affinity import translate
import descartes
#import matplotlib.pyplot as plt
import shapely.ops as ops
from collections import defaultdict
import numpy as np
#from mpl_toolkits.basemap import Basemap

WRAP_POINT=-25.

s = 'subsetter/maps/ne_50m_admin_0_map_subunits.shp'


class CountryContainer(object):
    """CountryContainer
    
    The CountryContainer is an object to store countries
    """

    def __init__(self):
        self.regions = defaultdict(list)
        self.countries = dict()
        self.c = []
    
    def add_country(self, c, min_area=0):
        if c in self.c:
            return

        if np.sum(c.area) < min_area:
            return
        
        self.c.append(c)

        self.countries[c.name.lower()] = c
        self.countries[c.name_long.lower()] = c
        self.countries[c.abbrev.lower()] = c

        if c.homepart == 1.0:
            self.countries[c.gu_a3.lower()] = c
            self.countries[c.postal.lower()] = c
    
        self.regions[c.sovereignt.lower()].append(c)
        self.regions[c.continent.lower()].append(c)
        self.regions[c.region_un.lower()].append(c)
        self.regions[c.subregion.lower()].append(c)
        self.regions[c.region_wb.lower()].append(c)

        self.regions[c.name.lower()].append(c)

        if c.continent not in ['Oceania', 'Antarctica'] and \
                c.name not in ['Greenland', 'Madagascar', 'Argentina', 'Chile']:
            self.regions['world'].append(c)
            self.regions['World'].append(c)

    def add_countries(self, countries, **kwargs):
        for c in countries:
            self.add_country(c, **kwargs)
    
    def __iter__(self):
        return self.c.__iter__()

    def __getitem__(self, i):
        if type(i) is int or type(i) is slice:
            return self.c.__getitem__(i)
        if type(i) is str:
            if i.lower() in self.regions:
                opt = CountryContainer()
                opt.add_countries(self.regions[i.lower()])
                return opt
            if i.lower() in self.countries:
                opt = CountryContainer()
                opt.add_country(self.countries[i.lower()])
                return opt
            raise IndexError()

        opt = CountryContainer()
        for j in i:
            if type(j) is list:
                for k in j:
                    if k.lower() in self.regions:
                        opt.add_countries(self[k.lower()])
                    else:
                        opt.add_country(self[k.lower()])
            else:
                if j.lower() in self.regions:
                    opt.add_countries(self[j.lower()])
                else:
                    opt.add_country(self[j.lower()])
        return opt

    def remove_northern_islands(self, limit=70):
        for c in self:
            c.remove_northern_islands(limit)

    def get_boundary_polygon(self, simplify_level=0.5, buffer_lvl=0,
                             min_area=0.95, return_type="array"):
        """get_boundary_polygon
        Gets a polygon around all countries in the CountryContainer

        There are two modes, which are selected automatically:
        First, if the polygons simplify to a single polygon, this
        polygon is returned. If not, a convex hull around the
        resulting multipolygon is returned.

        Parameters
        ----------
        simplify_level : float
            how much the polygon should be simplified. A higher 
            number results in a polygon with fewer points
        buffer_lvl : float
            how much the polygon should be buffered. This is 
            helpful with connecting islands to coasts. Therefore,
            levels below 1 are not recommended
        min_area : float
            regions with areas < min_area are excluded
        return_type : str
            either "array" or "polygon". If "array", a np.ndarray
            is returned, otherwise a polygon object
        
        Returns
        -------
        boundary : return_type
            array with the coordinates of the resulting 
            boundary polygon
        """

        patches = []
        for c in self:
            patches.extend(c.get_patch_area(min_area))
        continent = ops.cascaded_union(patches)

        continent = continent.buffer(buffer_lvl)

        if return_type == "array":
            try:
                return np.array(continent.exterior.coords.xy)
            except:
                return np.array(continent.convex_hull.exterior.coords.xy)
        elif return_type == 'polygon':
            try:
                np.array(continent.exterior.coords.xy)
                print("continent is single")
                return continent
            except:
                print("continent is nonsingle", len(continent))
                return continent
                return continent.convex_hull
        raise ValueError("invalid return_type: %s", return_type)

    def subset(self, countries=[], regions=[], min_area=0.5):
        cc = CountryContainer()
        for c in countries:
            cc.add_country(self.countries[c], min_area)
        for r in regions:
            for c in self.regions[r]:
                cc.add_country(c, min_area)
        return cc

    def append(self, *args):
        return self.subset(*args)

    def plot(self, ax=None, new=True, basemap=None, **kwargs):
        if basemap is not None:
            patches = [ops.transform(basemap, country.patch) for country
                       in self]
        else:
            patches = [country.patch for country in self]

        if new:
            ax, fig = new_fig()

        for p in patches:
            plot_mpp(ax, p, **kwargs)

        if new:
            finalize_plot(fig, ax)
    
    def __contains__(self, p):
        for c in self:
            if p in c:
                return True
        return False

    def get_country_from_point(self, p):
        for c in self:
            if p in c:
                return c.name
        return False
    
    def wrap_americas(self, wrapping_point=WRAP_POINT):
        for c in self:
            c.wrap_americas(wrapping_point)

    def unwrap_americas(self, wrapping_point=360):
        for c in self:
            self.unwrap_americas(wrapping_point)

    @property
    def area(self):
        return sum(sum(c.area) for c in self)

        
class Country(object):
    def __init__(self, patch, meta_data):
        self.patch = shape(patch)
        self.patch = self.patch.buffer(.0001)
        for k, v in meta_data.items():
            setattr(self, k.lower(), v)
        self.meta_data = meta_data

    def __contains__(self, p):
        return self.patch.contains(p)

    def __hash__(self):
        return self.name.__hash__()

    @property
    def area(self):
        return self.patch.area
    
    def get_plot_patch(self, area_limit=0, **kwargs):
        patches = self.get_patch_area(area_limit)
        q = [descartes.PolygonPatch(p, **kwargs) for p in patches]
        return q

    def get_patch_area(self, area_limit=0):
        raise NotImplementedError()
    
    def get_minx(self):
        return min(self.patch.envelope.exterior.xy[0])

    def get_maxx(self):
        return max(self.patch.envelope.exterior.xy[0])

    def get_miny(self):
        return min(self.patch.envelope.exterior.xy[1])

    def get_maxy(self):
        return max(self.patch.envelope.exterior.xy[1])
    
    def wrap_americas(self, wrapping_point=WRAP_POINT):
        if self.get_maxx() < wrapping_point or\
                self.get_minx() < wrapping_point:
            self.patch = translate(self.patch, xoff=360.)

    def unwrap_americas(self, wrapping_point=360):
        if self.get_maxx() > wrapping_point or\
                self.get_minx() > wrapping_point:
            self.patch = translate(self.patch, xoff=-360.)

    def plot(self, ax=None, new=True, basemap=None, **kwargs):
        if basemap is not None:
            patch = ops.transform(basemap, self.patch)
        else:
            patch = self.patch

        if new:
            ax, fig = new_fig()

        plot_mpp(ax, patch, **kwargs)

        if new:
            finalize_plot(fig, ax)

    def remove_northern_islands(self, limit=None):
        pass


class CountryP(Country):
    def get_patch_area(self, area_limit=0):
        if self. patch.area <= area_limit:
            return []
        return [self.patch]
    
    @staticmethod
    def from_mp(mp):
        c = CountryP(mp.patch, mp.meta_data)
        return c


class CountryMP(Country):
    def wrap_americas(self, wrapping_point=WRAP_POINT):
        new_mp = []
        try:
            for i, p in enumerate(self.patch):
                if min(p.envelope.exterior.xy[0]) < wrapping_point or\
                        max(p.envelope.exterior.xy[0]) < wrapping_point:
                    new_mp.append(translate(p, xoff=360.))
                else:
                    new_mp.append(p)
            self.patch = MultiPolygon(new_mp)
        except TypeError:
            if self.get_maxx() < wrapping_point or\
                    self.get_minx() < wrapping_point:
                self.patch = translate(self.patch, xoff=360.)

    def get_patch_area(self, area_limit=0):
        try:
            return [p for p in self.patch if p.area > area_limit]
        except TypeError:
            if self. patch.area <= area_limit:
                return []
            return [self.patch]

    def remove_northern_islands(self, limit=75.):
        if not hasattr(self.patch, 'geoms'):
            return
        q = [g for g in self.patch.geoms
             if max(g.envelope.exterior.xy[1]) < limit or g.area > 100]
        self.patch = MultiPolygon(q)


    
    @property
    def area(self):
        try:
            return [p.area for p in self.patch]
        except TypeError:
            return self.patch.area


def handle_special_countries(c):
    print(c.keys())
    c['Iceland'].patch = ops.cascaded_union([c['Iceland'].patch, c['Norway'].patch,
                                             c['Ireland'].patch]).convex_hull
    c['Madagascar'].patch = ops.cascaded_union([c['Madagascar'].patch,
                                             c['Mozambique'].patch]).convex_hull
    c['North I.'].patch = ops.cascaded_union([c['North I.'].patch,
                                             c['South I.'].patch]).convex_hull
    c['North I.'].patch = ops.cascaded_union([c['North I.'].patch,
                                             c['Australia'].patch]).convex_hull
    c['Madagascar'].__class__ = c['Iceland'].__class__
    c['Malaysia'].patch = c['Malaysia'].patch.buffer(1)
    c['Indonesia'].patch = c['Indonesia'].patch.buffer(1)
    c['Indonesia'].__class__ = c['Iceland'].__class__
    c['Malaysia'].__class__ = c['Iceland'].__class__
#    c['New Zealand'].__class__ = c['Iceland'].__class__
    c['Hawaii'].continent = ''
    c['Falkland Is.'].continent = ''
    

def load_countries(s, wrap_americas=True, remove_northern_islans=True):
    patches = fiona.collection(s)
    countries = CountryContainer()
    special_countries = dict()
    special_country_ids = ['England', 'Ireland', 'Iceland', 'Madagascar', 'Mozambique',
                           'Malaysia', 'Indonesia', 'Hawaii', 'Falkland Is.',
                           'Norway']
    special_country_ids.extend(['Australia', 'North I.', 'South I.'])
    for p in patches:
        if p['geometry']['type'] is 'Polygon':
            c = CountryP(p['geometry'], p['properties'])
        elif p['geometry']['type'] is 'MultiPolygon':
            c = CountryMP(p['geometry'], p['properties'])
        else:
            raise ValueError(p['geometry']['type'])

        if not hasattr(c, 'name'):
            return c
        if c.name in special_country_ids:
            special_countries[c.name] = c
        countries.add_country(c)

    handle_special_countries(special_countries)
    if wrap_americas:
        countries.wrap_americas()

    if remove_northern_islans:
        russia = ['Russia', 'Norway', 'Greenland']
        for r in russia:
            r = countries[r]
            r.remove_northern_islands(limit=72)

    return countries


def new_fig():
    fig = plt.figure(1, figsize=(10, 4), dpi=180)
    ax = fig.add_subplot(111)
    return ax, fig


def plot_multipolygon(ax, mp, **kwargs):
    area_limit = 0.9
    q = [descartes.PolygonPatch(p, **kwargs) for p in mp
         if p.area > area_limit]
    for p in q:
        ax.add_patch(p)
    

def plot_polygon(ax, p, **kwargs):
    pp = descartes.PolygonPatch(p, **kwargs)
    ax.add_patch(pp)


def plot_mpp(ax, p, **kwargs):
    try:
        plot_multipolygon(ax, p, **kwargs)
    except TypeError:
        plot_polygon(ax, p, **kwargs)


def finalize_plot(fig, ax):
    ax.relim()
    ax.autoscale()

    fig.show()


def draw_preview_plot(polygon):
    ax, fig = new_fig()
    m = Basemap(llcrnrlon=-20., llcrnrlat=WRAP_POINT,
                urcrnrlon=330., urcrnrlat=65.,
                rsphere=(6378137.00, 6356752.3142),
                resolution='h', projection='merc',
                lat_0=40., lon_0=-20., lat_ts=20.)
    m.drawcoastlines()
    m.fillcontinents()

    ops.transform(m, polygon)
    plot_mpp(ax, polygon, fc="red")

    finalize_plot(fig, ax)


if __name__ == '__main__':
    countries = load_countries(s)
    #clist = countries.subset(regions=['Europe', 'North America'], min_area=.5)
