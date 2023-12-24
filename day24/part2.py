from z3 import Solver, Real, Reals
import sys

def parse_input(filename):
    hailstones = []
    for line in open(filename).read().split("\n"):
        # Of form [[x,y,z], [vx, vy, vz]]
        [position, velocity] = [[int(x) for x in s.split(", ")] for s in line.split(" @ ")]
        hailstones.append((position, velocity))
    return hailstones

def compute_rock_position(hailstones):
    x, y, z = Reals("x y z")
    vx, vy, vz = Reals("vx vy vz")

    solver = Solver()
    for hailstone in hailstones[:3]:
        (px, py, pz), (pvx, pvy, pvz) = hailstone
        t = Real(f"t_{hailstone}")
        solver.add(x + vx * t == px + pvx * t)
        solver.add(y + vy * t == py + pvy * t)
        solver.add(z + vz * t == pz + pvz * t)

    solver.check()
    model = solver.model()
    retrieve = lambda s: model.eval(s).as_long()
    return retrieve(x) + retrieve(y) + retrieve(z)

print(compute_rock_position(parse_input(sys.argv[1]))) # Call it with for example "python part2.py input.txt"