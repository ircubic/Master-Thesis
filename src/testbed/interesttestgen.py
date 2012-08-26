import pprint
from game.interest import interest
from game.datalogger import _gamestats
import random

def make_cons(data, cons_name="cons", nil_name="nil", separator=" ", prefix=""):
    strarr = []
    for point in data:
        strarr.append(prefix+"%s(%s," % (cons_name, point))
    strarr.append(prefix + nil_name + (")"*len(data)))
    return separator.join(strarr)

def gen_cells(field=(16,16)):
    cells = []
    for i in range(field[0]):
        cells.append([])
        for j in range(field[1]):
            val = random.randint(0,100)
            if val > 70:
                val -= 70
            else:
                val = 0
            cells[i].append(float(val))
    return cells

def gen_game(num_dogs=4, field=(16,16)):
    game = _gamestats(field[0], field[1], num_dogs)
    game.entropy = []
    for i in range(num_dogs):
        cells = gen_cells()
        game.entropy.append(cells)
    game.ticks = float(random.randint(3,50))
    return game

def gen_visits(N=50):
    all_visits = []
    for i in range(N):
        all_visits.append(gen_game())
    return all_visits

def gen_sml(visits):
    N = float(len(visits))
    sml_ticks = make_cons([g.ticks for g in visits], "tick_cons", "tick_nil")
    sml_visits = make_cons(
        [make_cons(
                [make_cons([item for sublist in dog_visit for item in sublist],
                           "cell_cons", "cell_nil")
                 for dog_visit in g.entropy],
                "cell_list_cons", "cell_list_nil", separator="\n", prefix="\t\t")
         for g in visits],
        "visit_cons", "visit_nil", separator = "\n", prefix = "\t"
    )
    return "result(\n\t%s,\n\t%s,\n\t%s\n)" % (N, sml_ticks, sml_visits)

visits = gen_visits(3)
print gen_sml(visits)
ticks = [v.ticks for v in visits]
print "ticks =", repr(ticks)
visits = [v.entropy for v in visits]
print "visits =", repr(visits)
I = interest(
    ticks,
    visits,
    0.5, 1.0, 4.0
)
print """I = interest(
    ticks,
    visits,
    0.5, 1.0, 4.0
)"""
print I
