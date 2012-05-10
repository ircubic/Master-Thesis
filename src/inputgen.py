from pprint import PrettyPrinter
import random

DOGSIZE = (1.5, 1.5)
CATRADIUS = 0.75

def gen_data(num=100, dognum=4, runs=50, ais=(1,2,3), field=(16.,16.), pct_test=0.33):
    global DOGSIZE, CATRADIUS
    data = []
    dogrange = ((DOGSIZE[0]/2.0, field[0]-DOGSIZE[0]/2.0),
                (DOGSIZE[1]/2.0, (field[1]-DOGSIZE[1])/2.0))
    catrange = (CATRADIUS, field[0]-CATRADIUS)
    caty = field[1]-CATRADIUS

    for i in range(num):
        dogpos = []
        for j in range(dognum):
            dogpos.append((round(random.uniform(dogrange[0][0],
                                                dogrange[0][1]), 2),
                           round(random.uniform(dogrange[1][0],
                                                dogrange[1][1]), 2)))
        cats = []
        for j in range(runs):
            cats.append((round(random.uniform(catrange[0], catrange[1]), 2),
                         caty))
        data.append((dogpos, ais, cats))
    amount_test = int(round(num*pct_test))
    test_data = data[:amount_test]
    train_data = data[amount_test:]
    return train_data, test_data

def make_cons(data, cons_name="cons", nil_name="nil", separator=" ", prefix=""):
    strarr = []
    for point in data:
        strarr.append(prefix+"%s(%s," % (cons_name, point))
    strarr.append(prefix + nil_name + (")"*len(data)))
    return separator.join(strarr)

def make_sml(dataset, prefix=""):
    dogsize_str = "size%s" % (DOGSIZE,)
    catradius_str = "radius(%s)" % CATRADIUS
    text = []
    for datapoint in dataset:
        dogs, ais, cats = datapoint
        dogs = ["rect(point%s, %s)" % (dogpos, dogsize_str)
                for dogpos in dogs]
        cats = ["circle(point%s, %s)" % (catpos, catradius_str)
                for catpos in cats]
        text.append("(\n\t%s,\n\t%s,\n%s\n)" %
        (
                make_cons(dogs, "entity_cons", "entity_nil"),
                make_cons(ais, "cat_ai_cons", "cat_ai_nil"),
                make_cons(cats, "entity_cons", "entity_nil", separator="\n", prefix="\t"),
        ))
    return prefix + "[\n" + ",\n".join(text) + "\n]"

py_file = open("dogs.py", "w+")
sml_file = open("dogs.sml", "w+")
training_data, test_data = gen_data()

pp = PrettyPrinter(stream=py_file)

py_file.write("training_data = ")
pp.pprint(training_data)

py_file.write("\ntest_data = ")
pp.pprint(test_data)
py_file.close()

sml_file.write(make_sml(training_data, prefix="val Inputs = "))
sml_file.write("\n\n")
sml_file.write(make_sml(test_data, prefix="val Test_inputs = "))
sml_file.close()
