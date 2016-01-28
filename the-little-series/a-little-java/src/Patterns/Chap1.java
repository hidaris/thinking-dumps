package Patterns;

abstract class SeasoningD {}
class Salt extends SeasoningD {}
//class Pepper extends SeasoningD {}
class Thyme extends SeasoningD {}
class Sage extends SeasoningD {}


abstract class NumD {}
class Zero extends NumD {
    @Override
    public boolean equals(Object o) {
        return (o instanceof Zero);
    }
}
class OneMoreThan extends NumD {
    NumD predecessor;
    OneMoreThan(NumD _p) {
        predecessor = _p;
    }
    //--------------------------

    @Override
    public boolean equals(Object o) {
        return predecessor.equals(
                ((OneMoreThan)o).predecessor);
    }
}

abstract class LayerD {}
class Base extends LayerD {
    Object o;
    Base(Object _o) {
        o = _o;
    }
}
class Slice extends LayerD {
    LayerD l;
    Slice(LayerD _l) {
        l = _l;
    }
}