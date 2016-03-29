import fiona
from shapely.geometry import Polygon, Point, shape
from shapely.affinity import translate

import numpy as np

def load_tiles(s):
    tiles = fiona.collection(s)
    return [shape(t['geometry']) for t in tiles]


def create_tile_dict(tiles, bpoly):
    pts = dict() #dict saving ids
    rev_pts = dict()
    edges = set()
    pts_in = dict() #dict saving which points are in region

    for c, poly in enumerate(tiles):
        x, y = poly.exterior.xy
        points = zip(np.round(x, 3), np.round(y, 3))
        points = [wrap_america(p) for p in points]
        for p in points:
            if p not in pts_in:
                pts_in[p] = bpoly.intersects(Point(p))  # check if point is in region
                if pts_in[p]:
                    pts[p] = len(pts)  # if so, give id
                    rev_pts[len(rev_pts)] = p

        for i in range(3):
            pi, pj = points[i], points[i + 1]
            if pts_in[pi] and pts_in[pj]:
                if pts[pi] < pts[pj]:
                    edges.add((pts[pi] + 1, pts[pj] + 1))
                else:
                    edges.add((pts[pj] + 1, pts[pi] + 1))

        if c % 100 == 0:
            print(c, len(tiles))

    pts = [Point(rev_pts[p]) for p in range(len(rev_pts))]
    return pts, rev_pts, edges


def get_closest_point_to_sample(points, samples):
    usamples = unique2d(samples)
    dists = dict((tuple(s), np.argmin([Point(s).distance(Point(p))
                                       for p in points])) for s in usamples)

    res = [dists[tuple(s)] for s in samples]

    return np.array(res)


def wrap_america(tile):
    tile = Point(tile)
    if np.max(tile.xy[0]) < -40 or \
            np.min(tile.xy[0]) < -40:
        tile = translate(tile, xoff=360.)
    
    return tile.xy[0][0], tile.xy[1][0]


def unique2d(a):
    x, y = a.T
    b = x + y * 1.0j
    idx = np.unique(b, return_index=True)[1]
    return a[idx]
        

def write(name, rev_pts, edges, ipmap):
    np.savetxt("%s.ipmap" % name, ipmap + 1, fmt="%s")
    with open("%s.edges" % name, 'w') as f:
        for e in edges:
            f.write("%s %s\n" % e)

    with open("%s.demes" % name, 'w') as f:
        for i, v in rev_pts.items():
            f.write("%s %s\n" % v)


def intersect(grid, boundary, samples, output):
    bpoly = Polygon(np.loadtxt(boundary))
    bpoly2 = translate(bpoly, xoff=-360.)
    samples = np.loadtxt(samples)
    tiles = load_tiles(grid)
    #tiles = [wrap_america(t) for t in tiles]
    tiles = [t for t in tiles if bpoly.intersects(t) or bpoly2.intersects(t)]
    pts, rev_pts, edges = create_tile_dict(tiles, bpoly)
    ipmap = get_closest_point_to_sample(pts, samples)

    write(output, rev_pts, edges, ipmap)

if __name__ == "__main__":
    s = "/data/programs/dggrid.v61/examples/triangle/grid250.shp"
    boundary_file = "/data/eems-project/merged/v1/input/america.outer"
    samples = np.loadtxt("/data/eems-project/merged/v1/input/america.coord")

    bpoly = Polygon(np.loadtxt(boundary_file))
    bpoly2 = translate(bpoly, xoff=-360.)
    tiles2 = load_tiles(s)
    print("done loading")
    #tiles = [wrap_america(t) for t in tiles]
    print("done wrapping")
    tiles3 = [t for t in tiles2 if bpoly.intersects(t) or bpoly2.intersects(t)]
    pts, rev_pts, edges = create_tile_dict(tiles3, bpoly)
    ipmap = get_closest_point_to_sample(pts, samples)

    write("test", rev_pts, edges, ipmap)
